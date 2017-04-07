DELETE FROM nrd.aggr_emplr_sessn;

INSERT INTO nrd.aggr_emplr_sessn
(emplr_id, emplr_sessn_tmstmp, orig_ip_addr, third_party_entry_flag, crt_tmstmp)
SELECT
 sessn.emplr_id, sessn.emplr_sessn_tmstmp, sessn.orig_ip_addr, sessn.third_party_entry_flag, CURRENT_TIMESTAMP 
FROM nrd.emplr_sessn sessn
JOIN ref.parm
 ON sessn.emplr_sessn_tmstmp BETWEEN '${APM_CYCL_DT}'::DATE - INTERVAL '1 month' * parm.parm_val::int AND '${APM_CYCL_DT}'::DATE + INTERVAL '1 day'
 AND parm.parm_cd = 'AGGR_CLAIMT_SESSN_MON_CNT';