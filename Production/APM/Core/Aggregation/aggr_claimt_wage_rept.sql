DELETE FROM nrd.aggr_claimt_wage_rept;

INSERT INTO nrd.aggr_claimt_wage_rept
(claimt_id, emplr_id_array, wage_dt, wage_qtr_num, tot_wage_amt, wage_amt_array, payroll_pct_array, crt_tmstmp)
SELECT
 claimt.claimt_id,
 ARRAY_AGG(emplr.emplr_id),
 wage_rept.wage_dt,
 TO_CHAR(wage_rept.wage_dt, 'YYYYQ')::int,
 SUM(wage.wage_amt),
 ARRAY_AGG(wage.wage_amt),
 ARRAY_AGG(CASE WHEN total_wages.sum_wage_amt IS NULL OR total_wages.sum_wage_amt = 0 THEN 0 ELSE wage.wage_amt/total_wages.sum_wage_amt END),
 CURRENT_TIMESTAMP
FROM nrd.claimt
JOIN nrd.empl
 ON claimt.claimt_ref_num = empl.empl_ref_num
JOIN nrd.wage
 ON empl.empl_id = wage.empl_id
JOIN nrd.wage_rept
 ON wage.wage_rept_id = wage_rept.wage_rept_id
JOIN nrd.emplr
 ON wage_rept.emplr_id = emplr.emplr_id
JOIN (
  SELECT wage_rept_id, SUM(wage_amt) AS sum_wage_amt FROM nrd.wage GROUP BY wage_rept_id
) AS total_wages
 ON wage_rept.wage_rept_id = total_wages.wage_rept_id
WHERE TO_CHAR(wage_rept.wage_dt, 'YYYYQ')::int IN (
 SELECT MAX(TO_CHAR(wage_rept.wage_dt, 'YYYYQ')::int) FROM nrd.wage_rept WHERE TO_CHAR(wage_dt, 'YYYYQ')::int < TO_CHAR('${APM_CYCL_DT}'::DATE, 'YYYYQ')::int
)
GROUP BY  claimt.claimt_id, wage_rept.wage_dt;
 