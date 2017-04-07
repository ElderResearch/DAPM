DELETE FROM nrd.aggr_claimt_benf;

INSERT INTO nrd.aggr_claimt_benf (
 claimt_id,
 cert_perd_end_dt,
 cert_perd_end_qtr_num,
 claimt_age,
 claim_start_dt,
 claim_year_seq,
 last_emplr_naics2_cd,
 last_emplr_end_dt,
 last_emplr_end_qtr_num,
 last_emplr_days_emplyd_cnt,
 perds_to_claim_start_cnt,
 perds_to_cert_start_cnt,
 base_perd_paymt_amt,
 st_base_perd_paymt_amt,
 base_paymt_perds_cnt,
 max_paymt_amt,
 st_max_paymt_amt,
 reptd_incm_amt_array,
 tot_reglr_reptd_incm_amt,
 tot_reglr_adjstd_incm_amt,
 days_workd_cnt,
 incm_abv_threshld_flag,
 max_days_workd_cnt,
 paymt_type_cd_array,
 paymt_amt_array,
 tot_reglr_paymt_amt,
 cuml_paymt_amt,
 cuml_paymt_perds_cnt,
 cuml_zero_incm_cert_perd_cnt,
 cuml_lt_min_incm_cert_perd_cnt,
 cuml_gt_max_incm_cert_perd_cnt,
 cuml_reptd_incm_amt,
 cuml_adjstd_incm_amt,
 cuml_days_workd_cnt,
 cuml_perds_zero_days_workd_cnt,
 cuml_perds_abv_threshld_cnt,
 paymt_to_base_paymt_ratio,
 incm_to_base_paymt_ratio,
 paymt_to_base_paymt_perds_ratio,
 progm_cd_cnt,
 seprtn_reas_cd,
 educ_lvl_cd,
 workrs_comp_cd,
 pensn_cd,
 subst_workr_flag,
 school_empl_flag,
 filg_outsd_st_flag,
 union_flag,
 corp_flag,
 sevrnc_flag,
 commtr_flag,
 crt_tmstmp
)
SELECT DISTINCT
 claimt.claimt_id,
 cert.cert_perd_end_dt,
 TO_CHAR(cert.cert_perd_end_dt, 'YYYYQ')::int,
 FLOOR((CURRENT_DATE - claimt.birth_dt) / 365),
 claim.claim_start_dt,
 COALESCE(claim.claim_year_seq, -1),
 SUBSTRING(emplr.naics_cd FROM 1 FOR 2),
 COALESCE(claim.last_emplr_end_dt, '1900-01-01'),
 COALESCE(TO_CHAR(claim.last_emplr_end_dt, 'YYYYQ')::int, -1),
 COALESCE((claim.last_emplr_end_dt - claim.last_emplr_start_dt), -1),
 COALESCE(FLOOR((claim.claim_start_dt - claim.last_emplr_end_dt) / 7), -1),
 COALESCE(FLOOR((cert.cert_perd_end_dt - claim.last_emplr_end_dt) / 7), -1),
 claim.base_perd_paymt_amt,
 claim.st_base_perd_paymt_amt,
 claim.base_paymt_perds_cnt,
 claim.max_paymt_amt,
 claim.st_max_paymt_amt,
 inc.reptd_incm_amt_array,
 inc.tot_reglr_reptd_incm_amt,
 inc.tot_reglr_adjstd_incm_amt,
 
 inc.days_workd_cnt,
 inc.incm_abv_threshld_flag,
 MAX(inc.days_workd_cnt) OVER (PARTITION BY claimt.claimt_id, claim.claim_year_seq ORDER BY cert.cert_perd_end_dt),
 
 
 pay.paymt_type_cd_array,
 pay.paymt_amt_array,
 pay.tot_reglr_paymt_amt,
 SUM(pay.tot_paymt_amt) OVER (PARTITION BY claimt.claimt_id, claim.claim_year_seq ORDER BY cert.cert_perd_end_dt),
 COUNT(*) OVER (PARTITION BY claimt.claimt_id, claim.claim_year_seq ORDER BY cert.cert_perd_end_dt),
 SUM (CASE WHEN inc.tot_reptd_incm_amt = 0 THEN 1 ELSE 0 END)
    OVER (PARTITION BY claimt.claimt_id, claim.claim_year_seq ORDER BY cert.cert_perd_end_dt),
 SUM (CASE WHEN claim.base_perd_paymt_amt > 0 AND inc.tot_reptd_incm_amt > 0 AND (inc.tot_reptd_incm_amt / claim.base_perd_paymt_amt) <= (
       SELECT sys_parm_val::numeric FROM ref.sys_parm WHERE sys_parm_cd = 'INCM_MIN_THRESH_PCT'
     ) THEN 1 ELSE 0 END) OVER (PARTITION BY claimt.claimt_id, claim.claim_year_seq ORDER BY cert.cert_perd_end_dt),
 SUM (CASE WHEN claim.base_perd_paymt_amt > 0 AND inc.tot_reptd_incm_amt > 0 AND (inc.tot_reptd_incm_amt / claim.base_perd_paymt_amt) >= (
       SELECT sys_parm_val::numeric FROM ref.sys_parm WHERE sys_parm_cd = 'INCM_MAX_THRESH_PCT'
     ) THEN 1 ELSE 0 END) OVER (PARTITION BY claimt.claimt_id, claim.claim_year_seq ORDER BY cert.cert_perd_end_dt),
 SUM(inc.tot_reptd_incm_amt) OVER (PARTITION BY claimt.claimt_id, claim.claim_year_seq ORDER BY cert.cert_perd_end_dt),
 SUM(inc.tot_adjstd_incm_amt) OVER (PARTITION BY claimt.claimt_id, claim.claim_year_seq ORDER BY cert.cert_perd_end_dt),
 
 SUM(inc.days_workd_cnt) OVER (PARTITION BY claimt.claimt_id, claim.claim_year_seq ORDER BY cert.cert_perd_end_dt),
 SUM(CASE WHEN inc.days_workd_cnt = 0 THEN 1 ELSE 0 END) OVER (PARTITION BY claimt.claimt_id, claim.claim_year_seq ORDER BY cert.cert_perd_end_dt),
 SUM(CASE WHEN inc.incm_abv_threshld_flag = TRUE THEN 1 ELSE 0 END) OVER (PARTITION BY claimt.claimt_id, claim.claim_year_seq ORDER BY cert.cert_perd_end_dt),
 
 CASE WHEN claim.base_perd_paymt_amt > 0 THEN pay.tot_reglr_paymt_amt / claim.base_perd_paymt_amt ELSE 0 END,
 CASE WHEN claim.base_perd_paymt_amt > 0 THEN inc.tot_reglr_reptd_incm_amt / claim.base_perd_paymt_amt ELSE 0 END,
 CASE WHEN claim.base_paymt_perds_cnt > 0
    THEN COALESCE(pay_cnt.cnt, 0)::double precision / claim.base_paymt_perds_cnt::double precision
    ELSE -1
 END,
 pay.progm_cd_cnt,
 claim.seprtn_reas_cd,
 claim.educ_lvl_cd,
 claim.workrs_comp_cd,
 claim.pensn_cd,
 claim.subst_workr_flag,
 claim.school_empl_flag,
 claim.filg_outsd_st_flag,
 claim.union_flag,
 claim.corp_flag,
 claim.sevrnc_flag,
 claim.commtr_flag,
 CURRENT_TIMESTAMP
FROM nrd.claimt
JOIN nrd.claim
 ON claimt.claimt_id = claim.claimt_id
JOIN nrd.cert
 ON claim.claim_id = cert.claim_id
LEFT JOIN nrd.emplr
 ON claim.last_emplr_id = emplr.emplr_id
LEFT JOIN (
  SELECT
	claim.claimt_id,
	cert.cert_perd_end_dt,
	ARRAY_AGG(paymt.paymt_type_cd) AS paymt_type_cd_array,
	COUNT(DISTINCT paymt.progm_cd) AS progm_cd_cnt,
	ARRAY_AGG(COALESCE(paymt.paymt_amt, 0)) AS paymt_amt_array,
	COALESCE(SUM(paymt.paymt_amt), 0) AS tot_paymt_amt,
	SUM(COALESCE(CASE WHEN paymt.reglr_paymt_flag = TRUE THEN paymt.paymt_amt ELSE 0 END, 0)) AS tot_reglr_paymt_amt
  FROM nrd.claim claim
  JOIN nrd.cert cert
   ON claim.claim_id = cert.claim_id
  LEFT JOIN nrd.paymt paymt
   ON cert.cert_id = paymt.cert_id
  GROUP BY claim.claimt_id, cert.cert_perd_end_dt
) AS pay
 ON cert.cert_perd_end_dt = pay.cert_perd_end_dt AND claim.claimt_id = pay.claimt_id
LEFT JOIN (
  SELECT
	claim.claimt_id,
	cert.cert_perd_end_dt,
	COUNT(*) OVER (PARTITION BY claim.claimt_id, claim.claim_year_seq ORDER BY cert.cert_perd_end_dt) AS cnt 
  FROM nrd.claim claim
  JOIN nrd.cert cert
   ON claim.claim_id = cert.claim_id
  JOIN nrd.paymt paymt
   ON cert.cert_id = paymt.cert_id
  GROUP BY claim.claimt_id, claim.claim_year_seq, cert.cert_perd_end_dt
 ) AS pay_cnt
 ON cert.cert_perd_end_dt = pay_cnt.cert_perd_end_dt AND claim.claimt_id = pay_cnt.claimt_id
LEFT JOIN (
  SELECT
	claim.claimt_id,
	cert.cert_perd_end_dt,
	ARRAY_AGG(COALESCE(incm.reptd_incm_amt, 0)) AS reptd_incm_amt_array,
	COALESCE(SUM(CASE WHEN incm.reglr_incm_flag = TRUE THEN incm.reptd_incm_amt ELSE 0 END), 0) AS tot_reglr_reptd_incm_amt,
	COALESCE(SUM(incm.reptd_incm_amt), 0) AS tot_reptd_incm_amt,
	COALESCE(SUM(CASE WHEN incm.reglr_incm_flag = TRUE THEN incm.adjstd_incm_amt ELSE 0 END), 0) AS tot_reglr_adjstd_incm_amt,
	COALESCE(SUM(incm.adjstd_incm_amt), 0) AS tot_adjstd_incm_amt,
	COALESCE(SUM(incm.days_workd_cnt), 0) AS days_workd_cnt,
	MAX(CASE WHEN incm.incm_abv_threshld_flag = TRUE THEN 1 ELSE 0 END)::boolean AS incm_abv_threshld_flag
  FROM nrd.claim claim
  JOIN nrd.cert cert
   ON claim.claim_id = cert.claim_id
  LEFT JOIN nrd.incm incm
   ON cert.cert_id = incm.cert_id
  GROUP BY claim.claimt_id, cert.cert_perd_end_dt
) AS inc
 ON cert.cert_perd_end_dt = inc.cert_perd_end_dt AND claim.claimt_id = inc.claimt_id;