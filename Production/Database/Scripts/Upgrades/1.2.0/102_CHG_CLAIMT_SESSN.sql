/*==============================================================*/
/* NRD Schema: Result Table, Index and Constraint Definitions   */
/*==============================================================*/

SET SEARCH_PATH = nrd;

ALTER TABLE claimt_sessn
RENAME COLUMN crt_tmstmp TO crt_tmstmp1;

ALTER TABLE claimt_sessn
RENAME COLUMN last_updt_tmstmp TO last_updt_tmstmp1;

ALTER TABLE claimt_sessn
ADD COLUMN ip_city_name         VARCHAR(100)         NULL,
ADD COLUMN ip_st_cd             VARCHAR(10)          NULL,
ADD COLUMN ip_cntry_cd          VARCHAR(10)          NULL,
ADD COLUMN ip_anon_proxy_flag   BOOLEAN              NULL,
ADD COLUMN ip_conn_type_cd      VARCHAR(10)          NULL,
ADD COLUMN ip_subscr_type_cd    VARCHAR(10)          NULL,
ADD COLUMN ip_lat_val           NUMERIC(8,4)         NULL,
ADD COLUMN ip_long_val          NUMERIC (8,4)        NULL,
ADD COLUMN crt_tmstmp           TIMESTAMP            NULL,
ADD COLUMN last_updt_tmstmp     TIMESTAMP            NULL;

UPDATE claimt_sessn
SET crt_tmstmp = crt_tmstmp1,
last_updt_tmstmp = last_updt_tmstmp1;

ALTER TABLE claimt_sessn
ALTER COLUMN crt_tmstmp SET NOT NULL;

ALTER TABLE claimt_sessn
DROP COLUMN crt_tmstmp1,
DROP COLUMN last_updt_tmstmp1;

