/*==============================================================*/
/* NRD Schema: Result Table, Index and Constraint Definitions   */
/*==============================================================*/

SET SEARCH_PATH = nrd;

DROP INDEX emplr_id_wage_dt_ix;

ALTER TABLE wage_rept
ADD CONSTRAINT ak_wage_rept_ai01_wage_rep UNIQUE (emplr_id, wage_dt);

