/*==============================================================*/
/* NRD Schema: Result Table, Index and Constraint Definitions   */
/*==============================================================*/

SET SEARCH_PATH = nrd;

DROP TABLE aggr_claimt_benf;

/*==============================================================*/
/* Table: aggr_claimt_benf                                      */
/*==============================================================*/

CREATE TABLE aggr_claimt_benf (
   claimt_id            BIGINT               NOT NULL,
   cert_perd_end_dt     DATE                 NOT NULL,
   cert_perd_end_qtr_num INTEGER             NOT NULL,
   claimt_age           INTEGER              NULL,
   claim_start_dt       DATE                 NOT NULL,
   claim_year_seq       INTEGER              NULL,
   last_emplr_naics2_cd VARCHAR(10)          NULL,
   last_emplr_end_dt    DATE                 NULL,
   last_emplr_end_qtr_num INTEGER            NULL,
   last_emplr_days_emplyd_cnt INTEGER        NULL,
   perds_to_claim_start_cnt INTEGER          NULL,
   perds_to_cert_start_cnt INTEGER           NULL,
   base_perd_paymt_amt  NUMERIC(16,2)        NULL,
   st_base_perd_paymt_amt NUMERIC(16,2)      NULL,
   base_paymt_perds_cnt INTEGER              NULL,
   max_paymt_amt        NUMERIC(16,2)        NULL,
   st_max_paymt_amt     NUMERIC(16,2)        NULL,
   reptd_incm_amt_array NUMERIC(16,2) ARRAY  NULL,
   tot_reglr_reptd_incm_amt NUMERIC(16,2)    NULL,
   tot_reglr_adjstd_incm_amt NUMERIC(16,2)   NULL,
   days_workd_cnt       INTEGER              NULL,
   incm_abv_threshld_flag BOOLEAN            NULL,
   max_days_workd_cnt   INTEGER              NULL,
   paymt_type_cd_array  VARCHAR(10) ARRAY    NULL,
   paymt_amt_array      NUMERIC(16,2) ARRAY  NULL,
   tot_reglr_paymt_amt  NUMERIC(16,2)        NULL,
   cuml_paymt_amt       NUMERIC(16,2)        NULL,
   cuml_paymt_perds_cnt INTEGER              NOT NULL,
   cuml_zero_incm_cert_perd_cnt INTEGER      NOT NULL,
   cuml_lt_min_incm_cert_perd_cnt INTEGER    NOT NULL,
   cuml_gt_max_incm_cert_perd_cnt INTEGER    NOT NULL,
   cuml_reptd_incm_amt  NUMERIC(16,2)        NULL,
   cuml_adjstd_incm_amt NUMERIC(16,2)        NULL,
   cuml_days_workd_cnt  INTEGER              NULL,
   cuml_perds_zero_days_workd_cnt INTEGER    NULL,
   cuml_perds_abv_threshld_cnt INTEGER       NULL,
   paymt_to_base_paymt_ratio NUMERIC(12,6)   NULL,
   incm_to_base_paymt_ratio NUMERIC(12,6)    NULL,
   paymt_to_base_paymt_perds_ratio NUMERIC(12,6) NULL,
   progm_cd_cnt         INTEGER              NULL,
   seprtn_reas_cd       VARCHAR(10)          NULL,
   educ_lvl_cd          VARCHAR(10)          NULL,
   workrs_comp_cd       VARCHAR(10)          NULL,
   pensn_cd             VARCHAR(10)          NULL,
   subst_workr_flag     BOOLEAN              NULL,
   school_empl_flag     BOOLEAN              NULL,
   filg_outsd_st_flag   BOOLEAN              NULL,
   union_flag           BOOLEAN              NULL,
   corp_flag            BOOLEAN              NULL,
   sevrnc_flag          BOOLEAN              NULL,
   commtr_flag          BOOLEAN              NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_aggr_claimt_benf PRIMARY KEY (claimt_id, cert_perd_end_dt)
);

ALTER TABLE aggr_claimt_benf
  OWNER TO sidapmprd;
GRANT ALL ON TABLE aggr_claimt_benf TO sidapmprd;
GRANT SELECT, UPDATE, INSERT, DELETE ON TABLE aggr_claimt_benf TO sidapmprdapp;
GRANT SELECT ON TABLE aggr_claimt_benf TO sidapmprdusr;

/*======================*/
/* RE-ADD PERMISSIONS   */
/*======================*/

/*==============================================================*/
/* NOTE: NEED TO RE-RUN AGGREGATION TO RE-POPULATE THIS TABLE   */
/*==============================================================*/
