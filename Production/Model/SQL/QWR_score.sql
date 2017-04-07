--
--  QWR2 Score -- WF, 04 Aug 2015
--
--  Create the QWR+ score.
--
--  This metric is aimed at folks who continue to collect benefits once they
--  start a new job, or otherwise are double-dipping.

--  It gives no score for a quarter in which a person is not found in the QWR reports.

--  The f1 and f2 components are looking for a large gap between (employer-reported) wages
--  and (claimant-reported) earnings: both in dollar terms and also as a percentage.
--
--  The f3 and f4 components are looking for claimants who claimed for a large percentage
--  of the quarter and of their possible benefits. And the f5 component is a bonus for
--  folks reporting a high percentage of benefit-awarding weeks with zero earnings.
--
--     USES:     benefits, quarterly_wage
--
--     CREATES:  qwr_score
--

--  Note that I use some crazy math to calculate `f1` and `f2`, below, which expresses
--  a Gompertz curve (not to be confused with a Gompertz distribution), which is a generalization
--  of a logit curve, and provides a way to have bounded scores with a useful, smooth logit-like shape:
--
--  100 * exp (-5 * exp (-0.0009 * x))
--  ^           ^         ^
--  |           |         |
--  a=100       b=5       c=0.0009
--
--  Where:
--         a is asymptote (max)
--         b is x-axis shift
--         c is growth rate
--
-- The values I chose are based on my eyeballing of the data.
--
-- Note the crazy LEASE and GREATEST stuff, which is to work around postgreSQL's silly overflow/underflow
-- issues with EXP. Other languages, like R, don't need that.

--- NEED TO ADD calculation of the number of employers reporting wages in quarterly_wages
--- and to create an f6 which reflects how many employers there are. Maybe bonus points like
--- f5, but at any rate 1 employer should not decrease the score, and I've seen up to 6 employers, so
--- must scale well.
---
--- ALSO NEED TO ADD using add aggr_claimt_wage_rept::payroll_pct_array into the equation
--- so that people who make up a larger percent of the payroll get a higher
--- level of risk than those who make up a smaller percentage.
	
---
--- Could be: s * (1 + gompertz (x, 0.25, 30, 2)) or craziness: s + (1 + gompertz (2, 0.25, 30, 2)) * ((100 - s) / 3)
---
DELETE FROM nrd.enty_score WHERE cycl_dt = '${APM_CYCL_DT}'::date AND score_cd = 'QWR_QWR';
INSERT INTO nrd.enty_score (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)

WITH a AS (
	SELECT  claimt_id,
		cert_perd_end_qtr_num,
		sum (tot_reglr_reptd_incm_amt)                                  as total_earnings,
		sum (tot_reglr_paymt_amt)                                       as total_benefit,
		sum (base_perd_paymt_amt)                                       as total_wba,
		sum (CASE WHEN tot_reglr_paymt_amt      > 0 THEN 1 ELSE 0 END)  as benefit_weeks,
		sum (CASE WHEN tot_reglr_reptd_incm_amt > 0 THEN 0 ELSE 1 END)  as zero_earnings_weeks
	FROM nrd.aggr_claimt_benf
	-- WHERE cert_perd_end_dt BETWEEN (SELECT starting_date from qwr_parameters) AND (SELECT ending_date from qwr_parameters)   ----------
	GROUP BY claimt_id, cert_perd_end_qtr_num
        ), 
		
     b AS (
	SELECT a.*, tot_wage_amt,
	      nrd.uf_gompertz (tot_wage_amt - total_earnings,         100,  4, 0.001)               as f1, -- [0,100] score gap
	      nrd.uf_gompertz (tot_wage_amt / (1.0 + total_earnings),   1, 12, 2.75)                as f2, -- [0,1] score ratio
	      CASE WHEN total_wba = 0     THEN 1.0
		   ELSE
		      CASE WHEN ((total_benefit / total_wba) > 0.5)                                   -- Under normal circumstances, can't collect more than roughly
			   THEN nrd.uf_gompertz (total_benefit / total_wba,     1, 5, 4.5)                   -- half of total_wba (~26 weeks) in one quarter (13 weeks)
			   ELSE nrd.uf_gompertz (2 * total_benefit / total_wba, 1, 5, 4.5) END END  as f3, -- [0,1] score percentage.
	      LEAST (1.0, benefit_weeks / 13.0)                                              as f4, -- [0,1] score percentage
	      CASE WHEN benefit_weeks = 0 THEN 0.0
		   ELSE LEAST (1.0, zero_earnings_weeks / benefit_weeks) END                 as f5  -- [0,1] score percentage
	FROM a JOIN nrd.aggr_claimt_wage_rept AS bb ON a.claimt_id = bb.claimt_id AND a.cert_perd_end_qtr_num = bb.wage_qtr_num   --- LEFT OUTER JOIN ?? ---
	)

SELECT
	'${APM_CYCL_DT}'::date,
	'CLAIMT',
	claimt_id,
	'QWR_QWR',
	ROUND (LEAST (100.0, f1 * f2 * f3 * f4 + (5.0 * f5)), 2),
	CURRENT_TIMESTAMP
FROM b;
