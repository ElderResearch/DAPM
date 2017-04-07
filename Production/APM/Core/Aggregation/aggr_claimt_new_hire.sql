DELETE FROM nrd.aggr_claimt_new_hire;

INSERT INTO nrd.aggr_claimt_new_hire
(claimt_id, cert_perd_end_dt, min_incm_threshld_amt, max_incm_threshld_amt, reptd_incm_amt_array, max_reptd_incm_amt, paymt_amt_array, tot_paymt_amt, empl_start_dt, crt_tmstmp)
SELECT
 claimt.claimt_id,
 benf.cert_perd_end_dt,
 benf.base_perd_paymt_amt * (
   SELECT sys_parm_val::numeric FROM ref.sys_parm WHERE sys_parm_cd = 'INCM_MIN_THRESH_PCT'
 ),
 benf.base_perd_paymt_amt * (
   SELECT sys_parm_val::numeric FROM ref.sys_parm WHERE sys_parm_cd = 'INCM_MAX_THRESH_PCT'
 ),
 benf.reptd_incm_amt_array,
 (SELECT MAX(x) FROM UNNEST(benf.reptd_incm_amt_array) x),
 benf.paymt_amt_array,
 (SELECT SUM(x) FROM UNNEST(benf.paymt_amt_array) x),
 MAX(empl.empl_start_dt),
 CURRENT_TIMESTAMP
FROM nrd.claimt
JOIN nrd.empl ON claimt.claimt_ref_num = empl.empl_ref_num
JOIN nrd.aggr_claimt_benf benf ON benf.claimt_id = claimt.claimt_id
 AND benf.cert_perd_end_dt BETWEEN empl.empl_start_dt AND empl.empl_start_dt + INTERVAL '2 months'
WHERE benf.cert_perd_end_dt IN (
 SELECT MAX(cert_perd_end_dt) FROM nrd.cert_perd WHERE cert_perd_end_dt < '${APM_CYCL_DT}'::DATE
)
GROUP BY claimt.claimt_id, benf.cert_perd_end_dt, benf.base_perd_paymt_amt, benf.reptd_incm_amt_array, benf.paymt_amt_array;
 