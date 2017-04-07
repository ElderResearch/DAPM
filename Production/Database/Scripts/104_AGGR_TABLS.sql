/*==============================================================*/
/* NRD Schema: Aggregate Table, Index and Constraint Definitions*/
/*==============================================================*/

SET SEARCH_PATH = nrd;

/*==============================================================*/
/* Table: aggr_claimt_sessn                                     */
/*==============================================================*/

CREATE TABLE aggr_claimt_sessn (
   claimt_id            BIGINT               NOT NULL,
   claimt_sessn_tmstmp  TIMESTAMP            NOT NULL,
   sessn_enty_type_cd   VARCHAR(10)          NOT NULL,
   bus_perd_end_dt      DATE                 NOT NULL,
   orig_ip_addr         INET                 NOT NULL,
   orig_ip_net_val      INET                 NOT NULL,
   ip_city_name         VARCHAR(100)         NULL,
   ip_st_cd             VARCHAR(10)          NULL,
   ip_cntry_cd          VARCHAR(10)          NULL,
   ip_anon_proxy_flag   BOOLEAN              NULL,
   ip_conn_type_cd      VARCHAR(10)          NULL,
   ip_subscr_type_cd    VARCHAR(10)          NULL,
   ip_lat_val           NUMERIC(8,4)         NULL,
   ip_long_val          NUMERIC(8,4)         NULL,
   claimt_st_cd_array   VARCHAR(10) ARRAY    NULL,
   ip_outsd_local_st_flag BOOLEAN            NOT NULL,
   ip_outsd_claimt_st_flag BOOLEAN           NOT NULL,
   ip_outsd_cntry_flag  BOOLEAN              NOT NULL,
   claimt_addr_chg_flag BOOLEAN              NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_aggr_claimt_sessn PRIMARY KEY (claimt_id, claimt_sessn_tmstmp)
);

/*==============================================================*/
/* Index: acs_claimt_sessn_tmstmp_ix                            */
/*==============================================================*/

CREATE INDEX acs_claimt_sessn_tmstmp_ix ON aggr_claimt_sessn (
claimt_sessn_tmstmp
);

/*==============================================================*/
/* Table: aggr_shared_claimt_attr                               */
/*==============================================================*/

CREATE TABLE aggr_shared_claimt_attr (
   claimt_attr_type_cd  VARCHAR(100)         NOT NULL,
   claimt_attr_val      VARCHAR(100)         NOT NULL,
   claimt_cnt           INTEGER              NOT NULL,
   claimt_id_array      BIGINT ARRAY         NOT NULL,
   claimt_attr_crt_tmstmp_array TIMESTAMP ARRAY NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_aggr_shared_claimt_attr PRIMARY KEY (claimt_attr_type_cd, claimt_attr_val)
);

/*==============================================================*/
/* Index: asca_claimt_id_array_ixgin                            */
/*==============================================================*/

CREATE INDEX asca_claimt_id_array_ixgin ON aggr_shared_claimt_attr USING GIN (
claimt_id_array
);

/*==============================================================*/
/* Table: aggr_claimt_shared_attr_cnts (No longer being used)   */
/*==============================================================*/

-- CREATE TABLE aggr_claimt_shared_attr_cnts (
   -- claimt_id            BIGINT               NOT NULL,
   -- shared_email_cnt     INTEGER              NOT NULL,
   -- shared_phon_cnt      INTEGER              NOT NULL,
   -- shared_passwd_cnt    INTEGER              NOT NULL,
   -- shared_passphrs_cnt  INTEGER              NOT NULL,
   -- shared_pin_cnt       INTEGER              NOT NULL,
   -- shared_email_phon_cnt INTEGER             NOT NULL,
   -- shared_email_passwd_cnt INTEGER           NOT NULL,
   -- shared_email_passphrs_cnt INTEGER         NOT NULL,
   -- shared_email_pin_cnt INTEGER              NOT NULL,
   -- shared_phon_passwd_cnt INTEGER            NOT NULL,
   -- shared_phon_passphrs_cnt INTEGER          NOT NULL,
   -- shared_phon_pin_cnt  INTEGER              NOT NULL,
   -- shared_passwd_passphrs_cnt INTEGER        NOT NULL,
   -- shared_passwd_pin_cnt INTEGER             NOT NULL,
   -- shared_passphrs_pin_cnt INTEGER           NOT NULL,
   -- crt_tmstmp           TIMESTAMP            NOT NULL,
   -- last_updt_tmstmp     TIMESTAMP            NULL,
   -- CONSTRAINT pk_aggr_claimt_shared_attr_cnt PRIMARY KEY (claimt_id)
-- );

/*==============================================================*/
/* Table: aggr_claimt_shared_attr_edges                         */
/*==============================================================*/

CREATE TABLE aggr_claimt_shared_attr_edges (
   from_claimt_id       BIGINT               NOT NULL,
   to_claimt_id         BIGINT               NOT NULL,
   email_addr_array     VARCHAR(100) ARRAY   NULL,
   email_link_wgt_val   NUMERIC(12,6)        NULL,
   phon_num_array       VARCHAR(100) ARRAY   NULL,
   phon_link_wgt_val    NUMERIC(12,6)        NULL,
   passwd_val_array     VARCHAR(100) ARRAY   NULL,
   passwd_link_wgt_val  NUMERIC(12,6)        NULL,
   orig_ip_addr_array   INET ARRAY           NULL,
   orig_ip_link_wgt_val NUMERIC(12,6)        NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_aggr_claimt_shared_attr_edg PRIMARY KEY (from_claimt_id, to_claimt_id)
);

/*==============================================================*/
/* Table: aggr_emplr_sessn                                      */
/*==============================================================*/

CREATE TABLE aggr_emplr_sessn (
   emplr_id             BIGINT               NOT NULL,
   emplr_sessn_tmstmp   TIMESTAMP            NOT NULL,
   orig_ip_addr         INET                 NOT NULL,
   third_party_entry_flag BOOLEAN            NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_aggr_emplr_sessn PRIMARY KEY (emplr_id, emplr_sessn_tmstmp)
);

/*==============================================================*/
/* Table: aggr_claimt_new_hire                                  */
/*==============================================================*/

CREATE TABLE aggr_claimt_new_hire (
   claimt_id            BIGINT               NOT NULL,
   cert_perd_end_dt     DATE                 NOT NULL,
   empl_start_dt        DATE                 NOT NULL,
   reptd_incm_amt_array NUMERIC(16,2) ARRAY  NULL,
   max_reptd_incm_amt   NUMERIC(16,2)        NULL,
   paymt_amt_array      NUMERIC(16,2) ARRAY  NOT NULL,
   tot_paymt_amt        NUMERIC(16,2)        NOT NULL,
   min_incm_threshld_amt NUMERIC(16,2)       NULL,
   max_incm_threshld_amt NUMERIC(16,2)       NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_aggr_claimt_new_hire PRIMARY KEY (claimt_id, cert_perd_end_dt)
);

/*==============================================================*/
/* Table: aggr_claimt_emplr_hist                                */
/*==============================================================*/

CREATE TABLE aggr_claimt_emplr_hist (
   claimt_id            BIGINT               NOT NULL,
   emplr_id             BIGINT               NOT NULL,
   empl_start_qtr_num   INTEGER              NOT NULL,
   empl_end_qtr_num     INTEGER              NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_aggr_claimt_emplr_hist PRIMARY KEY (claimt_id, emplr_id, empl_start_qtr_num)
);

/*==============================================================*/
/* Table: aggr_claimt_wage_rept                                 */
/*==============================================================*/

CREATE TABLE aggr_claimt_wage_rept (
   claimt_id            BIGINT               NOT NULL,
   wage_dt              DATE                 NOT NULL,
   wage_qtr_num         INTEGER              NOT NULL,
   emplr_id_array       BIGINT ARRAY         NOT NULL,
   wage_amt_array       NUMERIC(16,2) ARRAY  NOT NULL,
   payroll_pct_array    NUMERIC(8,5) ARRAY   NOT NULL,
   tot_wage_amt         NUMERIC(16,2)        NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_aggr_claimt_wage_rept PRIMARY KEY (claimt_id, wage_dt)
);

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
   
/*==============================================================*/
/* Table: aggr_claimt_ovpaymt                                   */
/*==============================================================*/

CREATE TABLE aggr_claimt_ovpaymt (
   claimt_id            BIGINT               NOT NULL,
   ovpaymt_start_dt     DATE                 NOT NULL,
   ovpaymt_start_qtr_num INTEGER             NOT NULL,
   ovpaymt_end_dt       DATE                 NOT NULL,
   ovpaymt_end_qtr_num  INTEGER              NOT NULL,
   ovpaymt_amt          NUMERIC(16,2)        NOT NULL,
   ovpaymt_perds_cnt    INTEGER              NOT NULL,
   ovpaymt_range_perds_cnt INTEGER           NOT NULL,
   ovpaymt_dt_range     daterange            NOT NULL,
   fraud_flag           BOOLEAN              NOT NULL,
   reas_cd              VARCHAR(10)          NOT NULL,
   eff_dt               DATE                 NULL,
   progm_cd             VARCHAR(10)          NOT NULL,
   laws_cd_array        VARCHAR(10) ARRAY    NULL,
   due_amt              NUMERIC(16,2)        NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_aggr_claimt_ovpaymt PRIMARY KEY (claimt_id, ovpaymt_start_dt, ovpaymt_end_dt, ovpaymt_perds_cnt, fraud_flag, reas_cd, progm_cd)
);

