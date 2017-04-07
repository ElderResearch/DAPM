/*==============================================================*/
/* NRD Schema: Result Table, Index and Constraint Definitions   */
/*==============================================================*/

SET SEARCH_PATH = nrd;

ALTER TABLE incm
RENAME COLUMN crt_tmstmp TO crt_tmstmp1;

ALTER TABLE incm
RENAME COLUMN last_updt_tmstmp TO last_updt_tmstmp1;

ALTER TABLE incm
ALTER COLUMN incm_type_cd DROP NOT NULL,
ALTER COLUMN reglr_incm_flag DROP NOT NULL,
ALTER COLUMN reptd_incm_amt DROP NOT NULL,
ADD COLUMN days_workd_cnt       INTEGER              NULL,
ADD COLUMN incm_abv_threshld_flag BOOLEAN            NULL,
ADD COLUMN crt_tmstmp           TIMESTAMP            NULL,
ADD COLUMN last_updt_tmstmp     TIMESTAMP            NULL;

UPDATE incm
SET crt_tmstmp = crt_tmstmp1,
last_updt_tmstmp = last_updt_tmstmp1;

ALTER TABLE incm
ALTER COLUMN crt_tmstmp SET NOT NULL;

ALTER TABLE incm
DROP COLUMN crt_tmstmp1,
DROP COLUMN last_updt_tmstmp1;

