/* This module reads in the ip base table, determines which users have have frequently
shared IP addresses over a certain time period and inserts risk scores into the entity score table indicating
the severity of the sharing behavior for the most recent bus_perd_end_dt.  The (quantile) scores
are calculated using ip sharing behavior from the entire date range, but only the last bus_perd_end_dt
is kept.


The following variables/parameters are set at the beginning of the script:
-IP_MIN_USERS: The minimum # of users sharing an IP address within the same week required
for those to be eligible for a risk score.
-IP_DECAY_FACTOR: The factor to be used for a time decay of scores (in e^(-kt), k = decay factor)

NOTE: This module also uses IP location to complement its score. If a user's ip is shared and it's coming from outside
The states listed in the claimant's file, it's more risky
*/

/*Get minimum amount of time between any two users filing in the same week
  Returns all pairwise combinations of users filing from same ip in same week along with
  the number of seconds between their submissions
*/
DROP TABLE IF EXISTS tmp.ip_share_claimantdiffs;
CREATE TABLE tmp.ip_share_claimantdiffs AS
SELECT 
	c.orig_ip_addr, 
	c.bus_perd_end_dt, 
	c.claimt_id as firstclaimant, 
	s.claimt_id as secondclaimant, 
	ABS(date_part('epoch',(c.claimt_sessn_tmstmp - s.claimt_sessn_tmstmp))) as diffseconds
FROM aggr_claimt_sessn c
JOIN aggr_claimt_sessn s 
ON
    s.orig_ip_addr = c.orig_ip_addr
    AND s.bus_perd_end_dt = c.bus_perd_end_dt
    AND s.claimt_id <> c.claimt_id;

-- Get a table with the minimum number of seconds per week/claimant
DROP TABLE IF EXISTS tmp.ip_share_claimantweek;
CREATE TABLE tmp.ip_share_claimantweek AS
SELECT 
	firstclaimant, 
	bus_perd_end_dt, 
	MIN(diffseconds) as min_seconds
FROM tmp.ip_share_claimantdiffs
GROUP BY firstclaimant, bus_perd_end_dt;

--Use previous table to get the minimum time difference between two different claimants.
DROP TABLE IF EXISTS tmp.ip_share_claimantweek2;
CREATE TABLE tmp.ip_share_claimantweek2 AS
SELECT  
	t1.firstclaimant, 
	array_agg(DISTINCT t1.secondclaimant) as matched_claimants, 
	array_agg(DISTINCT t1.orig_ip_addr) as shared_ips, 
	t1.bus_perd_end_dt, 
	t1.diffseconds
FROM tmp.ip_share_claimantdiffs t1
JOIN tmp.ip_share_claimantweek t2
ON 
	t1.firstclaimant = t2.firstclaimant AND
	t1.bus_perd_end_dt = t2.bus_perd_end_dt AND
	t1.diffseconds = t2.min_seconds
GROUP BY t1.firstclaimant, t1.bus_perd_end_dt, t1.diffseconds;

-- Using the original table, calculate the number of unique users for each orig_ip_addr for a given bus_perd_end_dt
-- Only keep IPs that have been shared by at least IP_MIN_USERS users
DROP TABLE IF EXISTS tmp.ip_share_all_users1;
CREATE TABLE tmp.ip_share_all_users1 AS
SELECT 
	t1.bus_perd_end_dt, 
	t1.orig_ip_addr, 
	COUNT(DISTINCT(t2.claimt_id)) as prev_unique_users
FROM aggr_claimt_sessn t1
LEFT JOIN (
    SELECT 
	claimt_sessn_tmstmp, 
	orig_ip_addr, 
	claimt_id
    FROM aggr_claimt_sessn ) t2
ON 
	t2.orig_ip_addr = t1.orig_ip_addr AND 
	t2.claimt_sessn_tmstmp <= t1.claimt_sessn_tmstmp
GROUP BY t1.bus_perd_end_dt, t1.orig_ip_addr
HAVING COUNT(DISTINCT(t2.claimt_id)) >= (SELECT CAST((SELECT parm_val 
										FROM ref.parm 
										WHERE parm_cd = 'IP_MIN_USERS') AS INTEGER));

-- Join the above table on the table that already contains the minimum difference in seconds for each user/week
DROP TABLE IF EXISTS tmp.ip_share_claimantweek3;
CREATE TABLE tmp.ip_share_claimantweek3 AS
SELECT 
	t1.firstclaimant as claimt_id, 
	t1.matched_claimants, 
	t1.shared_ips[1] as shared_ip, 
	t1.bus_perd_end_dt,
	t1.diffseconds, 
	t2.prev_unique_users
FROM tmp.ip_share_claimantweek2 t1
LEFT JOIN tmp.ip_share_all_users1 t2
ON 
	t1.shared_ips[1] = t2.orig_ip_addr AND 
	t1.bus_perd_end_dt = t2.bus_perd_end_dt;

-- Obtain the number of unique users with each ip in the week
DROP TABLE IF EXISTS tmp.ip_share_claimantweek4;
CREATE TABLE tmp.ip_share_claimantweek4 AS
SELECT 
	t1.*, 
	t2.unique_users_in_week
FROM tmp.ip_share_claimantweek3 t1
LEFT JOIN
	(SELECT 
		orig_ip_addr, 
		bus_perd_end_dt, 
		COUNT(DISTINCT claimt_id) as unique_users_in_week
	FROM aggr_claimt_sessn
	GROUP BY orig_ip_addr, bus_perd_end_dt) t2
ON 
	t1.shared_ip = t2.orig_ip_addr AND 
	t1.bus_perd_end_dt = t2.bus_perd_end_dt;

-- Add IP location from original table
DROP TABLE IF EXISTS tmp.ip_share_claimantweek5;
CREATE TABLE tmp.ip_share_claimantweek5 AS
SELECT 
	t1.*, 
	t2.ip_outsd_local_st_flag, 
	t2.ip_outsd_cntry_flag
FROM tmp.ip_share_claimantweek4 t1
LEFT JOIN (
	SELECT 
		DISTINCT claimt_id, 
		orig_ip_addr, 
		ip_outsd_local_st_flag, 
		ip_outsd_cntry_flag
	FROM aggr_claimt_sessn) t2
ON 
	t1.claimt_id = t2.claimt_id AND 
	t1.shared_ip = t2.orig_ip_addr;

-- Apply exponential decay and quantile scoring of to obtain different score components
DROP TABLE IF EXISTS tmp.ip_share_claimantweek6;
CREATE TABLE tmp.ip_share_claimantweek6 AS
SELECT 
	*,
	NTILE(20) OVER (ORDER BY unique_users_in_week) * 5 as weekly_users_score,
	NTILE(20) OVER (ORDER BY prev_unique_users) * 5 as all_time_users_score,
	ROUND(EXP(diffseconds * -1 *
							(SELECT CAST((SELECT parm_val 
							FROM ref.parm 
							WHERE parm_cd = 'IP_DECAY_FACTOR') AS FLOAT))) --decay_factor (chosen such that a score of 100 is received up to 15 minutes in between submissions)
						  * 100) as time_between_submissions_score
FROM tmp.ip_share_claimantweek5;

--Create the final table with the overall score from these variables and subset down to only include scores from the most recent bus_perd_end_dt
DROP TABLE IF EXISTS tmp.precursor_claimant_sharing_ips;
CREATE TABLE tmp.precursor_claimant_sharing_ips AS
SELECT
	t1.claimt_id,
	t1.matched_claimants,
	t1.shared_ip,
	t1.bus_perd_end_dt,
	t1.diffseconds,
	t1.prev_unique_users,
	t1.unique_users_in_week,
	t1.weekly_users_score,
	t1.all_time_users_score,
	t1.time_between_submissions_score,
	t1.adj_final_score,
	CASE WHEN t1.adj_final_score > 100 THEN 100
	ELSE t1.adj_final_score
	END AS final_score
FROM (
	SELECT
		*,
		CASE WHEN ip_outsd_local_st_flag IS TRUE AND ip_outsd_local_st_flag IS NOT NULL THEN
			weekly_users_score * 0.4 + time_between_submissions_score * 0.4 + all_time_users_score * 0.2 + 20
		ELSE
			weekly_users_score * 0.4 + time_between_submissions_score * 0.4 + all_time_users_score * 0.2
		END AS adj_final_score
	FROM tmp.ip_share_claimantweek6
	WHERE bus_perd_end_dt = (SELECT MAX(bus_perd_end_dt) FROM tmp.ip_share_claimantdiffs)) t1;

-- Insert Score attributes into claimant sharing attributes table
DELETE FROM nrd.ipa_ship_ds WHERE cycl_dt = '${APM_CYCL_DT}'::date;
INSERT INTO nrd.ipa_ship_ds (cycl_dt, claimt_id, orig_ip_addr, matchd_claimt_id_array, uniq_claimt_cnt, 
							cuml_uniq_claimt_cnt, min_sessn_gap_cnt, crt_tmstmp)
SELECT
	'${APM_CYCL_DT}'::date,
	claimt_id,
	shared_ip,
	matched_claimants,
	unique_users_in_week,
	prev_unique_users,
	diffseconds,
	CURRENT_TIMESTAMP
FROM tmp.precursor_claimant_sharing_ips;



/* Insert subscores into scoring table */
DELETE FROM nrd.enty_score WHERE cycl_dt = '${APM_CYCL_DT}'::date AND score_cd IN ('IPA_SHIP_PERD','IPA_SHIP_ALL','IPA_SHIP_TIME','IPA_SHIP_RAW','IPA_SHIP');
--Inserting weekly users score
INSERT INTO nrd.enty_score (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)
SELECT
	'${APM_CYCL_DT}'::date,
	'CLAIMT',
	claimt_id,
	'IPA_SHIP_PERD',
	weekly_users_score,
	CURRENT_TIMESTAMP
FROM tmp.precursor_claimant_sharing_ips;

--Inserting all-time users score
INSERT INTO nrd.enty_score (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)
SELECT
	'${APM_CYCL_DT}'::date,
	'CLAIMT',
	claimt_id,
	'IPA_SHIP_ALL',
	all_time_users_score,
	CURRENT_TIMESTAMP
FROM tmp.precursor_claimant_sharing_ips;

--Inserting time between claims score
INSERT INTO nrd.enty_score (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)
SELECT
	'${APM_CYCL_DT}'::date,
	'CLAIMT',
	claimt_id,
	'IPA_SHIP_TIME',
	time_between_submissions_score,
	CURRENT_TIMESTAMP
FROM tmp.precursor_claimant_sharing_ips;

--Inserting the adjusted final score (before flooring any scores above 100 back down to 100)
INSERT INTO nrd.enty_score (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)
SELECT
	'${APM_CYCL_DT}'::date,
	'CLAIMT',
	claimt_id,
	'IPA_SHIP_RAW',
	adj_final_score,
	CURRENT_TIMESTAMP
FROM tmp.precursor_claimant_sharing_ips;

--Basic Insert of final scores
INSERT INTO nrd.enty_score (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)
SELECT 
	'${APM_CYCL_DT}'::date,
	'CLAIMT',
	claimt_id,
	'IPA_SHIP',
	final_score,
	CURRENT_TIMESTAMP
FROM tmp.precursor_claimant_sharing_ips;

DROP TABLE IF EXISTS tmp.precursor_claimant_sharing_ips;
DROP TABLE IF EXISTS tmp.ip_share_claimantweek6;
DROP TABLE IF EXISTS tmp.ip_share_claimantweek5;
DROP TABLE IF EXISTS tmp.ip_share_claimantweek4;
DROP TABLE IF EXISTS tmp.ip_share_claimantweek3;
DROP TABLE IF EXISTS tmp.ip_share_all_users1;
DROP TABLE IF EXISTS tmp.ip_share_claimantweek2;
DROP TABLE IF EXISTS tmp.ip_share_claimantweek;
DROP TABLE IF EXISTS tmp.ip_share_claimantdiffs;