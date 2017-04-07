/* This module reads in the ip base table, looks at the ip location flags and assigns a location score
for each claim using the following methodology:

-If someone filed a claim from a different state/province than was listed in their file and they're outside US OR they used an anonymous proxy (at least once in the past "x" days): 100
-If someone filed a claim from a different state/province than was listed in their file and they're inside the US but outside the the state and neighboring states (at least once in the past "x" days): 50
-If someone filed a claim from a different state/province than was listed in their file and they're inside the US AND in the local area (all the time in the past "x" days): 25
-If no state of record is associated with claimant: 10
-If they've always filed from the same state as was listed in their file (in the past "x" days): 0 (no scores of 0 written to final score table)

Assumption(s):
-Those serving in the armed forces abroad (state code = 'AE' or 'AP') are excluded

The following parameters are pulled from a parameter table to run this model:
-${APM_CYCL_DT}: Kettle date insert
*/

/*Get all the claims for the relevant time period and subset down to exclude those serving abroad.*/
DROP TABLE IF EXISTS tmp.ip_location_ip_subset;
CREATE TABLE tmp.ip_location_ip_subset AS 
	SELECT *
	FROM aggr_claimt_sessn
	WHERE ((NOT claimt_st_cd_array && ARRAY['AP', 'AE', 'AA']::varchar[]) OR claimt_st_cd_array IS NULL);
		
/*Group by claimt_id and bus_perd_end_dt (only for the most recent bus_perd_end_dt) and calculate binary flags indicating:
  - whether a matched transaction occurred outside the US (ip_outsd_cntry_and_st)
  - whether a matched transaction occurred outside local states (ip_outsd_st_and_listed_st)
  - whether an IP belongs to a know anonymous proxy server (ip_anon_proxy_flag)
  - whether a matched transaction occurred in a state not listed as regular/resident state/province (ip_outsd_claimt_st_flag)
  - whether a claimant has any state listed on record (no_st_on_record)  
 */
DROP TABLE IF EXISTS tmp.ip_location_ip_subset_agg;
CREATE TABLE tmp.ip_location_ip_subset_agg AS
	SELECT 
		claimt_id,
		bus_perd_end_dt,
		MAX (CASE WHEN ip_outsd_cntry_flag AND ip_outsd_claimt_st_flag --Shouldn't flag US Territories
			THEN 1
			ELSE 0 END) as ip_outsd_cntry_and_st,
		MAX (CASE WHEN ip_outsd_local_st_flag AND ip_outsd_claimt_st_flag
			THEN 1
			ELSE 0 END) as ip_outsd_st_and_listed_st,
		MAX (CASE WHEN ip_anon_proxy_flag
			THEN 1
			ELSE 0 END) as ip_anon_proxy,
		MAX (CASE WHEN ip_outsd_claimt_st_flag
			THEN 1
			ELSE 0 END) as ip_outsd_listed_st,
		MAX (CASE WHEN claimt_st_cd_array IS NULL
			THEN 1
			ELSE 0 END) as no_st_on_record
	FROM tmp.ip_location_ip_subset
	WHERE bus_perd_end_dt = (SELECT MAX(bus_perd_end_dt) FROM tmp.ip_location_ip_subset)
	GROUP BY claimt_id,
		bus_perd_end_dt;

/* Create the final score table and get rid of anyone with a score of 0*/
DROP TABLE IF EXISTS tmp.ip_location_claimant_location_score;
CREATE TABLE tmp.ip_location_claimant_location_score AS
	SELECT t1.*
	FROM (
		SELECT
			*,
			CASE 
				WHEN ip_outsd_cntry_and_st = 1 OR ip_anon_proxy = 1 THEN 100
				WHEN ip_outsd_st_and_listed_st = 1 THEN 50
				WHEN ip_outsd_listed_st = 1 THEN 25
				WHEN no_st_on_record = 1 THEN 10
				ELSE 0
			END AS final_score
		FROM tmp.ip_location_ip_subset_agg) t1
	WHERE t1.final_score != 0;

-- Write attributes to location attributes table
DELETE FROM nrd.ipa_clr_ds WHERE cycl_dt = '${APM_CYCL_DT}'::date;
INSERT INTO nrd.ipa_clr_ds (cycl_dt, claimt_id, ip_anon_proxy_flag, ip_outsd_cntry_claimt_st_flag, 
							ip_outsd_local_claimt_st_flag, ip_outsd_claimt_st_flag, 
							missg_claimt_st_flag, crt_tmstmp)
SELECT 
	'${APM_CYCL_DT}'::date,
	claimt_id,
	CASE WHEN ip_anon_proxy = 1 THEN TRUE ELSE FALSE END,
	CASE WHEN ip_outsd_cntry_and_st = 1 THEN TRUE ELSE FALSE END,
	CASE WHEN ip_outsd_st_and_listed_st = 1 THEN TRUE ELSE FALSE END,
	CASE WHEN ip_outsd_listed_st = 1 THEN TRUE ELSE FALSE END,
	CASE WHEN no_st_on_record = 1 THEN TRUE ELSE FALSE END,
	CURRENT_TIMESTAMP
FROM tmp.ip_location_claimant_location_score;

-- Inserting final scores into the entity score table for the week of interest.
DELETE FROM nrd.enty_score WHERE cycl_dt = '${APM_CYCL_DT}'::date AND score_cd = 'IPA_CLR';
INSERT INTO nrd.enty_score (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)
SELECT 
	'${APM_CYCL_DT}'::date, 
	'CLAIMT',
	claimt_id,
	'IPA_CLR',
	final_score,
	CURRENT_TIMESTAMP AS crt_tmstmp
FROM tmp.ip_location_claimant_location_score;

-- Drop all temporary tables created in the process
DROP TABLE IF EXISTS tmp.ip_location_ip_subset,
			tmp.ip_location_ip_subset_agg,
			tmp.ip_location_claimant_location_score;