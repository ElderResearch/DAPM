/* This function checks for un/under reported income from claimants. It reads in the new hire 
table (NH_EMPLOYEE)and checks it against the list of claimants who received benefits in the @time_window 
following their start date. Not all claims will be fraudulent, since underemployment is still an issue.
A claimants risk will decrease with respect to their reported income relative to their total benefits.
Since the amount a claimant can earn before loosing benefits changes state to state, a variable @wage_threshold
is included.

Variables/Parameters:
starts_after - Gives lower bound to search window for new hire list
time_window - The length of time from start date of the new hire to check for benefits being awarded.
wage_threshold_low -The fraction of WBA a claimant can earn before benefits are reduced.
wage_threshold_high -The fraction of WBA a claimant can earn before not receiving benefits.

act_wage_threshold_high = total_wba*wage_threshold_high
act_wage_threshold_low = total_wba*wage_threshold_low

Score:
if(claimed_earnings > act_wage_threshold_low):
	100 - 100*(claimed_earnings/act_wage_threshold_high)
else:
	100
*/

--Creates the score table.
DELETE FROM nrd.enty_score WHERE cycl_dt = '${APM_CYCL_DT}'::date AND score_cd = 'NHI_NHR';
INSERT INTO nrd.enty_score (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)
SELECT
	'${APM_CYCL_DT}'::date,
	'CLAIMT',
	claimt_id,
	'NHI_NHR',
	CASE WHEN max_reptd_incm_amt > min_incm_threshld_amt THEN GREATEST(ROUND((100-100*(max_reptd_incm_amt/(max_incm_threshld_amt+0.01))),0),0)ELSE 100 END,
	CURRENT_TIMESTAMP
FROM aggr_claimt_new_hire;