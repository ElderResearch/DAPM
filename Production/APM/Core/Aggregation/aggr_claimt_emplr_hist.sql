DELETE FROM nrd.aggr_claimt_emplr_hist;

INSERT INTO nrd.aggr_claimt_emplr_hist
(claimt_id, emplr_id, empl_start_qtr_num, empl_end_qtr_num, crt_tmstmp)
SELECT
 claimt.claimt_id,
 wage_rept.emplr_id,
 TO_CHAR(MIN(wage_rept.wage_dt), 'YYYYQ')::int,
 TO_CHAR(MAX(wage_rept.wage_dt), 'YYYYQ')::int,
 CURRENT_TIMESTAMP
FROM nrd.claimt
JOIN nrd.empl ON claimt.claimt_ref_num = empl.empl_ref_num
JOIN nrd.wage ON empl.empl_id = wage.empl_id
JOIN nrd.wage_rept ON wage.wage_rept_id = wage_rept.wage_rept_id
GROUP BY claimt.claimt_id, wage_rept.emplr_id;