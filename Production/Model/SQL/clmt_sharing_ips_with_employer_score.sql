--Sharing IPs with employers
/* This module reads in the ip base table and determines which users have IPs that have been shared with
employers' IP addresses used when filing their Quarterly Wage Reports.  It assigns a risk score based on the total number of
times that sharing has occurred and the percentage of time a claimant has shared with an employer's IP. The final score is an equally
weighted combination of these two factors.  One thing to note is that a claimant can legitimately file multiple claims
in a single week in order to claim money for previous weeks. If they use the same IP which matches with a single employer,
the claimant will be counted as sharing the ip 5x. This was left as-is because claiming multiple weeks at once
can be considered slightly suspicious behavior, thus allowing the count to reflect that suspicion. 

The following parameters are pulled from a parameter table to run this model:
 -${APM_CYCL_DT}: Kettle date insert
*/

/*Join the employer IP activity onto the claimant ip activity, adding a flag to the original 
aggr_claimt_sessn table indicating whether or not the claim shared an IP address with an 
employer in the past 3 months.
*/
DROP TABLE IF EXISTS tmp.emp_share_ip_merged_emps_and_claimants;
CREATE TABLE tmp.emp_share_ip_merged_emps_and_claimants AS
SELECT 
	t1.claimt_id,
	t1.orig_ip_addr,
	t1.claimt_sessn_tmstmp,
	t1.bus_perd_end_dt,
	CASE WHEN SUM(CASE WHEN t2.emplr_id IS NULL THEN 0 ELSE 1 END) > 0 
		THEN 1 
		ELSE 0 END AS shares_w_emp
FROM nrd.aggr_claimt_sessn t1
LEFT JOIN nrd.aggr_emplr_sessn t2
ON t1.orig_ip_addr = t2.orig_ip_addr
GROUP BY t1.claimt_id,
	 t1.orig_ip_addr,
	 t1.claimt_sessn_tmstmp,
	 t1.bus_perd_end_dt;

/* Count the total number of times a claimant has shared an IP with an employer in the past three months,
   Count the total number of times a claimant has filed in the past three months,
   Calculate the percentage of times the claimant has shared with an employer in the past three months
   This is all performed by looking at claimants that have filed within the most recent bus_perd_end_dt
   Only keep claimants that filed in the most recent week.
*/
DROP TABLE IF EXISTS tmp.emp_share_ip_ips_shared_per_week_claimant;
CREATE TABLE tmp.emp_share_ip_ips_shared_per_week_claimant AS
SELECT DISTINCT
	t1.claimt_id,
	t1.bus_perd_end_dt,
	t2.total_emp_shared_ct,
	t2.tot_claims_ct,
	t2.total_emp_shared_ct::float / t2.tot_claims_ct * 100 as perc_shared_w_emp
FROM 
	tmp.emp_share_ip_merged_emps_and_claimants t1
JOIN
	--Getting the total number of claims and times claims were shared with emps in the 3-month period
	(SELECT
		claimt_id,
		SUM(shares_w_emp) as total_emp_shared_ct,
		COUNT(1) AS tot_claims_ct
	FROM tmp.emp_share_ip_merged_emps_and_claimants
	GROUP BY 
		claimt_id) t2
ON t1.claimt_id = t2.claimt_id
--Ensuring that we only keep the most recent bus_perd_end_dt and only give scores to those who have
--shared with an employer at least once
WHERE t1.bus_perd_end_dt = (SELECT MAX(bus_perd_end_dt) FROM tmp.emp_share_ip_merged_emps_and_claimants)
      AND
      t2.total_emp_shared_ct > 0;

/*Give a percent rank score to each observation for both total and percent shared with employers*/
DROP TABLE IF EXISTS tmp.emp_share_ips_sharing_with_emps_scoring;
CREATE TABLE tmp.emp_share_ips_sharing_with_emps_scoring AS
	SELECT
		*,
		ROUND(percent_rank() over (order by total_emp_shared_ct) * 100) AS tot_emp_share_score,
		ROUND(percent_rank() over (order by perc_shared_w_emp) * 100) AS perc_emp_share_score
	FROM tmp.emp_share_ip_ips_shared_per_week_claimant;

/*Make the highest score for each of these be 100 to counteract the weirdness that can happen with using percent_rank*/
DROP TABLE IF EXISTS tmp.ceiling_to_100;
CREATE TABLE tmp.ceiling_to_100 AS
SELECT 
	claimt_id,
	bus_perd_end_dt,
	total_emp_shared_ct,
	tot_claims_ct,
	perc_shared_w_emp,
	tot_emp_share_score + (100 - (SELECT MAX(tot_emp_share_score) FROM tmp.emp_share_ips_sharing_with_emps_scoring)) AS tot_emp_share_score,
	perc_emp_share_score + (100 - (SELECT MAX(perc_emp_share_score) FROM tmp.emp_share_ips_sharing_with_emps_scoring)) AS perc_emp_share_score
FROM 	tmp.emp_share_ips_sharing_with_emps_scoring;

/*Performing the following:
  1. Re-calibrate these two scores for individuals that only match up with one employer (because their percent rank is zero).
     This is accomplished by taking the next lowest percent rank score and cutting it in half.
  2. Combine the resulting two scores into a final score, and subset to only include the most recent bus_perd_end_dt
*/
DROP TABLE IF EXISTS tmp.emp_share_ips_sharing_with_emps_scoring2;
CREATE TABLE tmp.emp_share_ips_sharing_with_emps_scoring2 AS
	SELECT
		t1.*,
		ROUND(t1.perc_emp_share_score * 0.5 + t1.tot_emp_share_score * 0.5) AS final_score
	FROM
		(SELECT
			claimt_id,
			bus_perd_end_dt,
			total_emp_shared_ct,
			tot_claims_ct,
			perc_shared_w_emp,
			CASE WHEN perc_emp_share_score = 0
				THEN (SELECT MIN(perc_emp_share_score)
					FROM tmp.emp_share_ips_sharing_with_emps_scoring
					WHERE perc_emp_share_score <> 0) / 2
				ELSE perc_emp_share_score END AS perc_emp_share_score, --taking next lowest score and dividing by 2
			CASE WHEN tot_emp_share_score = 0
				THEN (SELECT MIN(tot_emp_share_score)
					FROM tmp.emp_share_ips_sharing_with_emps_scoring
					WHERE tot_emp_share_score <> 0) / 2
				ELSE tot_emp_share_score END AS tot_emp_share_score
		FROM tmp.ceiling_to_100) t1;

-- Write attributes to employer attributes table
DELETE FROM nrd.ipa_elr_ds WHERE cycl_dt = '${APM_CYCL_DT}'::date;
INSERT INTO nrd.ipa_elr_ds
SELECT
	'${APM_CYCL_DT}'::date,
	claimt_id,
	perc_shared_w_emp,
	total_emp_shared_ct,
	CURRENT_TIMESTAMP
FROM tmp.emp_share_ips_sharing_with_emps_scoring2;
	

/*Insert sub-scores into scoring table*/
DELETE FROM nrd.enty_score WHERE cycl_dt = '${APM_CYCL_DT}'::date AND score_cd IN ('IPA_ELR_PCT','IPA_ELR_TOT','IPA_ELR');
-- Inserting unique employers shared subscore
INSERT INTO nrd.enty_score (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)
SELECT 
	'${APM_CYCL_DT}'::date,
	'CLAIMT',
	claimt_id,
	'IPA_ELR_PCT',
	perc_emp_share_score,
	CURRENT_TIMESTAMP
FROM tmp.emp_share_ips_sharing_with_emps_scoring2;

-- Inserting total shares with employer subscore
INSERT INTO nrd.enty_score (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)
SELECT 
	'${APM_CYCL_DT}'::date,
	'CLAIMT',
	claimt_id,
	'IPA_ELR_TOT',
	tot_emp_share_score,
	CURRENT_TIMESTAMP
FROM tmp.emp_share_ips_sharing_with_emps_scoring2;

-- Inserting final score into scoring table
INSERT INTO nrd.enty_score (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)
SELECT 
	'${APM_CYCL_DT}'::date,
	'CLAIMT',
	claimt_id,
	'IPA_ELR',
	final_score,
	CURRENT_TIMESTAMP
FROM tmp.emp_share_ips_sharing_with_emps_scoring2;

-- Drop all temporary tables created in the process
DROP TABLE IF EXISTS tmp.emp_share_ip_merged_emps_and_claimants,
			tmp.emp_share_ip_ips_shared_per_week_claimant,
			tmp.emp_share_ips_sharing_with_emps_scoring,
			tmp.ceiling_to_100,
			tmp.emp_share_ips_sharing_with_emps_scoring2;