/*This module reads in the ip base table, determines which users have changed IPs 
significantly over a certain time period and returns a table with risk scores.  The final risk score
is represented by the percentile to which claimant belongs in terms of a score that weights
(1) The number of unique 24-bit ips a claimant has had in the time period of interest
(2) The percentage of time a claimant has used a unique 24-bit ip address during the time period of interest

The following parameters are pulled from a parameter table to run this model:
-ip_min_num: The minimum number of ip changes (at the 24-bit level) to have to be considered for scoring
-${APM_CYCL_DT}: Kettle date insert
*/

/* Roll up to the claimt_id level and count the number of distinct ips used by each claimant
   based on the first 24 bits of the IP.  Also, count total claims filed by the claimant and 
   only keep records that have at least 'ip_min_num' number of unique 24-bit IP addresses */
DROP TABLE IF EXISTS tmp.ip_change_temp_rolled_up;
CREATE TABLE tmp.ip_change_temp_rolled_up AS
	SELECT 
		claimt_id, 
		COUNT(DISTINCT orig_ip_net_val) as num_unique_ips,
		COUNT(orig_ip_net_val) as num_of_filings
	FROM nrd.aggr_claimt_sessn
	GROUP BY claimt_id
	HAVING COUNT(DISTINCT orig_ip_net_val) >= 
						(SELECT CAST((SELECT parm_val 
						FROM ref.parm 
						WHERE parm_cd = 'IP_CHANGE_MIN_NUM') AS integer)); --getting ip_min_num;

/* Calculating:
   - perc_unique: The percentage of claims filed that are unique at the 24-bit level of the IP addresses
   - scaled_ips: The z-score of the log of the number of unique IP addresses at the 24-bit level
*/
DROP TABLE IF EXISTS tmp.ip_change_temp_score_precursor;
CREATE TABLE tmp.ip_change_temp_score_precursor AS
	SELECT *,
		(t1.logged_ips - avg(t1.logged_ips) over()) / stddev(t1.logged_ips) over() as scaled_ips
	FROM (SELECT *, 
		CAST(num_unique_ips as real) / num_of_filings * 100 as perc_unique, 
		LN(num_unique_ips) as logged_ips
		FROM tmp.ip_change_temp_rolled_up) t1;

/* Creating the for claiamnts by doing the following:
   1. Multiplying scaled_ips by perc_unique in order to combine the number of unique in order to combine the raw number
      of times a claimant has had a unique IP with the frequency with which this occurs.
   2. Calculate the percentile in which each score falls (final_score)
 Note: In the next step, we subset down to only keep claimants that filed in the current week. However, we're the score for these claimants
 is calculated relative to all claimants in present in the data set for the time period of interest.
*/
DROP TABLE IF EXISTS tmp.ip_change_precursor_claimant_changing_ips;
CREATE TABLE tmp.ip_change_precursor_claimant_changing_ips AS
	SELECT
		t1.claimt_id,
		t1.num_unique_ips,
		t1.num_of_filings,
		t1.perc_unique,
		ntile(100) over (order by t1.score) as final_score
		FROM (SELECT *,
				scaled_ips * perc_unique as score
			FROM tmp.ip_change_temp_score_precursor) t1;

-- Only keeping scores belonging to claimants who have logged-in in the current week.
DROP TABLE IF EXISTS tmp.ip_change_precursor_claimant_changing_ips2;
CREATE TABLE tmp.ip_change_precursor_claimant_changing_ips2 AS
	SELECT DISTINCT
		t2.claimt_id,
		t1.bus_perd_end_dt,
		t2.num_unique_ips,
		t2.num_of_filings,
		t2.perc_unique,
		t2.final_score
	FROM nrd.aggr_claimt_sessn t1
	JOIN tmp.ip_change_precursor_claimant_changing_ips t2
	ON t1.claimt_id = t2.claimt_id
	WHERE t1.bus_perd_end_dt = (SELECT MAX(bus_perd_end_dt) FROM nrd.aggr_claimt_sessn);

-- Insert supplementary score data into the attribute table for changing_ip scores
DELETE FROM nrd.ipa_chip_ds WHERE cycl_dt = '${APM_CYCL_DT}'::date;
INSERT INTO nrd.ipa_chip_ds (cycl_dt, claimt_id, uniq_ip_addr_cnt, uniq_sessn_cnt, uniq_pct, crt_tmstmp)
SELECT
	'${APM_CYCL_DT}'::date,
	claimt_id,
	num_unique_ips,
	num_of_filings,
	perc_unique,
	CURRENT_TIMESTAMP
FROM tmp.ip_change_precursor_claimant_changing_ips2;
	
-- Inserting final scores into the entity score table for the week of interest.
DELETE FROM nrd.enty_score WHERE cycl_dt = '${APM_CYCL_DT}'::date AND score_cd = 'IPA_CHIP';
INSERT INTO nrd.enty_score (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)
SELECT 
	'${APM_CYCL_DT}'::date,
	'CLAIMT',
	claimt_id,
	'IPA_CHIP',
	final_score,
	CURRENT_TIMESTAMP
FROM tmp.ip_change_precursor_claimant_changing_ips2;

-- Drop all temporary tables that were created in the process
DROP TABLE IF EXISTS tmp.ip_change_precursor_claimant_changing_ips2;
DROP TABLE IF EXISTS tmp.ip_change_precursor_claimant_changing_ips;
DROP TABLE IF EXISTS tmp.ip_change_temp_score_precursor;
DROP TABLE IF EXISTS tmp.ip_change_temp_rolled_up;