DELETE FROM nrd.aggr_claimt_sessn;

INSERT INTO nrd.aggr_claimt_sessn
(claimt_id, claimt_sessn_tmstmp, bus_perd_end_dt, sessn_enty_type_cd, orig_ip_addr, orig_ip_net_val, ip_city_name, ip_st_cd,
 ip_cntry_cd, ip_anon_proxy_flag, ip_conn_type_cd, ip_subscr_type_cd, ip_lat_val, ip_long_val, ip_outsd_local_st_flag, ip_outsd_claimt_st_flag,
 ip_outsd_cntry_flag, claimt_st_cd_array, claimt_addr_chg_flag,
 crt_tmstmp)

SELECT
 sessn.claimt_id,
 sessn.claimt_sessn_tmstmp,
 DATE_TRUNC('week', sessn.claimt_sessn_tmstmp) - INTERVAL '1 day',
 sessn.sessn_enty_type_cd,
 sessn.orig_ip_addr,
 NETWORK(SET_MASKLEN(sessn.orig_ip_addr, 24)),
 sessn.ip_city_name,
 sessn.ip_st_cd,
 sessn.ip_cntry_cd,
 sessn.ip_anon_proxy_flag,
 sessn.ip_conn_type_cd,
 sessn.ip_subscr_type_cd,
 sessn.ip_lat_val,
 sessn.ip_long_val,
 CASE WHEN sessn.ip_st_cd IS NULL OR sessn.ip_st_cd IN (
  SELECT lkup_cd
  FROM ref.parnt_lkup_cd lk
  JOIN ref.sys_parm sp
  ON lk.parnt_lkup_cd = sp.sys_parm_val
  WHERE sp.sys_parm_cd = 'CLIENT_DOL_ST_CD'
  AND lk.lkup_tabl_id = 5
 )
  THEN false
  ELSE true
 END,
 CASE WHEN sessn.ip_st_cd IS NULL OR sessn.ip_st_cd = ANY(y.st_cd_array)
  THEN false
  ELSE true
 END,
 CASE WHEN sessn.ip_cntry_cd IS NULL OR sessn.ip_cntry_cd = 'US'
  THEN false
  ELSE true
 END,
 y.st_cd_array,
 CASE WHEN z.prev_addr_cnt IS NULL OR z.prev_addr_cnt = 0
  THEN false
  ELSE true
 END,
 CURRENT_TIMESTAMP 
FROM nrd.claimt_sessn sessn
JOIN ref.parm
  ON sessn.claimt_sessn_tmstmp BETWEEN '${APM_CYCL_DT}'::DATE - INTERVAL '1 month' * parm.parm_val::int AND '${APM_CYCL_DT}'::DATE + INTERVAL '1 day'
  AND parm.parm_cd = 'AGGR_CLAIMT_SESSN_MON_CNT'
LEFT JOIN (
  SELECT
   addr_idfr.enty_id,
   ARRAY_AGG(DISTINCT addr.st_cd) AS st_cd_array
  FROM nrd.addr_idfr
  JOIN nrd.addr
   ON addr_idfr.addr_id = addr.addr_id AND addr.exp_dt IS NULL
  WHERE addr_idfr.enty_type_cd = 'CLAIMT' AND addr.st_cd IS NOT NULL
  GROUP BY addr_idfr.enty_id
) y
 ON sessn.claimt_id = y.enty_id
LEFT JOIN (
  SELECT
   addr_idfr.enty_id,
   COUNT(*) AS prev_addr_cnt
  FROM nrd.addr_idfr
  JOIN nrd.addr
   ON addr_idfr.addr_id = addr.addr_id AND addr.exp_dt IS NOT NULL
  WHERE addr_idfr.enty_type_cd = 'CLAIMT'
  GROUP BY addr_idfr.enty_id
) z
 ON sessn.claimt_id = z.enty_id;
 
-- remove details for MaxMind center of US coordinates
UPDATE nrd.aggr_claimt_sessn
 SET ip_city_name = NULL, ip_st_cd = NULL, ip_lat_val = NULL, ip_long_val = NULL 
 WHERE (ip_lat_val = 38 and ip_long_val = -97) OR (ip_lat_val = 37.751 and ip_long_val = -97.822);