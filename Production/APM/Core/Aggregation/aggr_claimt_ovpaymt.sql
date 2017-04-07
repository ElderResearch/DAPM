DELETE FROM nrd.aggr_claimt_ovpaymt;

INSERT INTO nrd.aggr_claimt_ovpaymt
(claimt_id, ovpaymt_start_dt, ovpaymt_start_qtr_num, ovpaymt_end_dt, ovpaymt_end_qtr_num, ovpaymt_amt, laws_cd_array, ovpaymt_dt_range, ovpaymt_perds_cnt,
 ovpaymt_range_perds_cnt, fraud_flag, reas_cd, eff_dt, progm_cd, due_amt,
 crt_tmstmp)
SELECT
 claim.claimt_id,
 cert_perd.cert_perd_start_dt,
 TO_CHAR (cert_perd.cert_perd_start_dt, 'YYYYQ')::int,
 cert_perd.cert_perd_end_dt,
 TO_CHAR (cert_perd.cert_perd_end_dt, 'YYYYQ')::int,
 SUM(ovpaymt.ovpaymt_amt),
 NULL,
 DATERANGE(cert_perd.cert_perd_start_dt, cert_perd.cert_perd_end_dt, '[]'),
 1,
 1,
 ovpaymt.fraud_flag,
 ovpaymt.reas_cd,
 cert_perd.cert_perd_start_dt,
 ovpaymt.progm_cd,
 SUM(ovpaymt.due_amt),
 CURRENT_TIMESTAMP 
FROM nrd.ovpaymt
JOIN nrd.paymt
 ON ovpaymt.paymt_id = paymt.paymt_id
JOIN nrd.cert
 ON paymt.cert_id = cert.cert_id
JOIN nrd.cert_perd
 ON cert.cert_perd_end_dt = cert_perd.cert_perd_end_dt
JOIN nrd.claim
 ON cert.claim_id = claim.claim_id
GROUP BY claim.claimt_id, cert_perd.cert_perd_start_dt, cert_perd.cert_perd_end_dt, ovpaymt.fraud_flag, ovpaymt.reas_cd, ovpaymt.progm_cd;