DELETE FROM nrd.aggr_shared_claimt_attr;

INSERT INTO nrd.aggr_shared_claimt_attr
(claimt_attr_type_cd, claimt_attr_val, claimt_id_array, claimt_attr_crt_tmstmp_array, claimt_cnt, crt_tmstmp)
SELECT
 claimt_attr_type_cd,
 claimt_attr_val,
 ARRAY_AGG(claimt_id),
 ARRAY_AGG(claimt_attr_crt_tmstmp),
 COUNT(claimt_id),
 CURRENT_TIMESTAMP 
FROM nrd.claimt_attr
GROUP BY claimt_attr_type_cd, claimt_attr_val;
