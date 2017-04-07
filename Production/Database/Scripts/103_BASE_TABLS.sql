/*==============================================================*/
/* NRD Schema: Base Table, Index and Constraint Definitions     */
/*==============================================================*/

SET SEARCH_PATH = nrd;

/*==============================================================*/
/* Table: emplr                                                 */
/*==============================================================*/

CREATE TABLE emplr (
   emplr_id             BIGSERIAL            NOT NULL,
   emplr_ref_num        VARCHAR(100)         NULL,
   emplr_name           VARCHAR(100)         NOT NULL,
   naics_cd             VARCHAR(10)          NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_emplr PRIMARY KEY (emplr_id),
   CONSTRAINT ak_emplr_ai_emplr UNIQUE (emplr_ref_num)
);
  
/*==============================================================*/
/* Table: emplr_sessn                                           */
/*==============================================================*/

CREATE TABLE emplr_sessn (
   emplr_id             BIGINT               NOT NULL,
   emplr_sessn_tmstmp   TIMESTAMP            NOT NULL,
   orig_ip_addr         INET                 NOT NULL,
   third_party_entry_flag BOOLEAN            NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_emplr_sessn PRIMARY KEY (emplr_id, emplr_sessn_tmstmp)
);

ALTER TABLE emplr_sessn
   ADD CONSTRAINT fk_emplr_se_sessn_emp_emplr FOREIGN KEY (emplr_id)
      REFERENCES emplr (emplr_id)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: corp_offcr                                            */
/*==============================================================*/

CREATE TABLE corp_offcr (
   corp_offcr_id        BIGSERIAL            NOT NULL,
   emplr_id             BIGINT               NOT NULL,
   corp_offcr_last_name VARCHAR(100)         NOT NULL,
   corp_offcr_first_name VARCHAR(100)        NOT NULL,
   corp_offcr_mi        CHAR(1)              NULL,
   corp_offcr_titl      VARCHAR(100)         NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_corp_offcr PRIMARY KEY (corp_offcr_id)
);

/*==============================================================*/
/* Index: corp_offcr_emplr_fk                                   */
/*==============================================================*/

CREATE INDEX corp_offcr_emplr_fk ON corp_offcr (
emplr_id
);

/*==============================================================*/
/* Index: emplr_name_ix                                         */
/*==============================================================*/

CREATE INDEX emplr_name_ix ON emplr (
emplr_name
);

ALTER TABLE corp_offcr
   ADD CONSTRAINT fk_corp_off_corp_offc_emplr FOREIGN KEY (emplr_id)
      REFERENCES emplr (emplr_id)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: empl                                                  */
/*==============================================================*/

CREATE TABLE empl (
   empl_id              BIGSERIAL            NOT NULL,
   empl_ref_num         VARCHAR(100)         NOT NULL,
   emplr_id             BIGINT               NOT NULL,
   empl_last_name       VARCHAR(100)         NULL,
   empl_first_name      VARCHAR(100)         NULL,
   empl_mi              CHAR(1)              NULL,
   empl_titl            VARCHAR(100)         NULL,
   empl_start_dt        DATE                 NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_empl PRIMARY KEY (empl_id)
);

/*==============================================================*/
/* Index: empl_emplr_fk                                         */
/*==============================================================*/

CREATE INDEX empl_emplr_fk ON empl (
emplr_id
);

/*==============================================================*/
/* Index: empl_ref_num_ix                                       */
/*==============================================================*/

CREATE INDEX empl_ref_num_ix ON empl (
empl_ref_num
);

ALTER TABLE empl
   ADD CONSTRAINT fk_empl_empl_empl_emplr FOREIGN KEY (emplr_id)
      REFERENCES emplr (emplr_id)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: wage_rept                                             */
/*==============================================================*/

CREATE TABLE wage_rept (
   wage_rept_id         BIGSERIAL            NOT NULL,
   emplr_id             BIGINT               NOT NULL,
   wage_dt              DATE                 NOT NULL,
   wage_rept_dt         DATE                 NOT NULL,
   min_empl_cnt         INTEGER              NOT NULL,
   max_empl_cnt         INTEGER              NOT NULL,
   mean_empl_cnt        INTEGER              NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_wage_rept PRIMARY KEY (wage_rept_id),
   CONSTRAINT ak_wage_rept_ai01_wage_rep UNIQUE (emplr_id, wage_dt)
);

ALTER TABLE wage_rept
   ADD CONSTRAINT fk_wage_rep_emplr_wag_emplr FOREIGN KEY (emplr_id)
      REFERENCES emplr (emplr_id)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: wage                                                  */
/*==============================================================*/

CREATE TABLE wage (
   wage_rept_id         BIGINT               NOT NULL,
   empl_id              BIGINT               NOT NULL,
   wage_amt             NUMERIC(16,2)        NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_wage PRIMARY KEY (wage_rept_id, empl_id)
);

/*==============================================================*/
/* Index: wage_rept_fk                                          */
/*==============================================================*/

CREATE INDEX wage_rept_fk ON wage (
wage_rept_id
);

/*==============================================================*/
/* Index: wage_empl_fk                                          */
/*==============================================================*/

CREATE INDEX wage_empl_fk ON wage (
empl_id
);

ALTER TABLE wage
   ADD CONSTRAINT fk_wage_wage_rept_wage_rep FOREIGN KEY (wage_rept_id)
      REFERENCES wage_rept (wage_rept_id)
      ON DELETE CASCADE ON UPDATE CASCADE;

ALTER TABLE wage
   ADD CONSTRAINT fk_wage_wage_empl_empl FOREIGN KEY (empl_id)
      REFERENCES empl (empl_id)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: claimt                                                */
/*==============================================================*/

CREATE TABLE claimt (
   claimt_id            BIGSERIAL            NOT NULL,
   claimt_ref_num       VARCHAR(100)         NOT NULL,
   claimt_last_name     VARCHAR(100)         NULL,
   claimt_first_name    VARCHAR(100)         NULL,
   claimt_mi            CHAR(1)              NULL,
   birth_dt             DATE                 NULL,
   last_login_tmstmp    TIMESTAMP            NULL,
   faild_login_cnt      INTEGER              NOT NULL,
   profl_crt_tmstmp     TIMESTAMP            NOT NULL,
   profl_last_updt_tmstmp TIMESTAMP          NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_claimt PRIMARY KEY (claimt_id),
   CONSTRAINT ak_claimt_ai_claimt UNIQUE (claimt_ref_num)
);

/*==============================================================*/
/* Index: CLAIMT_LAST_NAME_IX                                   */
/*==============================================================*/

create  index CLAIMT_LAST_NAME_IX on CLAIMT (
CLAIMT_LAST_NAME
);

/*==============================================================*/
/* Table: claimt_attr                                           */
/*==============================================================*/

CREATE TABLE claimt_attr (
   claimt_id            BIGINT               NOT NULL,
   claimt_attr_type_cd  VARCHAR(10)          NOT NULL,
   claimt_attr_sub_type_cd VARCHAR(10)       NOT NULL,
   claimt_attr_val      VARCHAR(100)         NOT NULL,
   claimt_attr_crt_tmstmp TIMESTAMP          NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_claimt_attr PRIMARY KEY (claimt_id, claimt_attr_type_cd, claimt_attr_sub_type_cd, claimt_attr_val)
);

/*==============================================================*/
/* Index: claimt_attr_type_val_ix                               */
/*==============================================================*/

CREATE INDEX claimt_attr_type_val_ix ON claimt_attr (
claimt_attr_type_cd,
claimt_attr_val
);


ALTER TABLE claimt_attr
   ADD CONSTRAINT fk_claimt_a_attr_clai_claimt FOREIGN KEY (claimt_id)
      REFERENCES claimt (claimt_id)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: claimt_sessn                                          */
/*==============================================================*/

CREATE TABLE claimt_sessn (
   claimt_id            BIGINT               NOT NULL,
   claimt_sessn_tmstmp  TIMESTAMP            NOT NULL,
   sessn_enty_type_cd   VARCHAR(10)          NOT NULL,
   orig_ip_addr         INET                 NOT NULL,
   ip_city_name         VARCHAR(100)         NULL,
   ip_st_cd             VARCHAR(10)          NULL,
   ip_cntry_cd          VARCHAR(10)          NULL,
   ip_anon_proxy_flag   BOOLEAN              NULL,
   ip_conn_type_cd      VARCHAR(10)          NULL,
   ip_subscr_type_cd    VARCHAR(10)          NULL,
   ip_lat_val           NUMERIC(8,4)         NULL,
   ip_long_val          NUMERIC (8,4)        NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_claimt_sessn PRIMARY KEY (claimt_id, claimt_sessn_tmstmp)
);

/*==============================================================*/
/* Index: claimt_sessn_tmstmp_ix                                */
/*==============================================================*/

CREATE INDEX claimt_sessn_tmstmp_ix ON claimt_sessn (
claimt_sessn_tmstmp
);

/*==============================================================*/
/* Index: claimt_sessn_ip_addr_ixgist                           */
/*==============================================================*/

CREATE INDEX claimt_sessn_ip_addr_ix ON claimt_sessn USING GIST(
orig_ip_addr inet_ops
);

ALTER TABLE claimt_sessn
   ADD CONSTRAINT fk_claimt_s_sessn_cla_claimt FOREIGN KEY (claimt_id)
      REFERENCES claimt (claimt_id)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: claim                                                 */
/*==============================================================*/

CREATE TABLE claim (
   claim_id             BIGSERIAL            NOT NULL,
   claimt_id            BIGINT               NOT NULL,
   claim_start_dt       DATE                 NOT NULL,
   claim_end_dt         DATE                 NULL,
   claim_year_seq       INTEGER              NULL,
   last_emplr_id        BIGINT               NULL,
   last_emplr_start_dt  DATE                 NULL,
   last_emplr_end_dt    DATE                 NULL,
   base_perd_paymt_amt  NUMERIC(16,2)        NULL,
   st_base_perd_paymt_amt NUMERIC(16,2)      NULL,
   base_paymt_perds_cnt INTEGER              NULL,
   max_paymt_amt        NUMERIC(16,2)        NULL,
   st_max_paymt_amt     NUMERIC(16,2)        NULL,
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
   CONSTRAINT pk_claim PRIMARY KEY (claim_id)
);

/*==============================================================*/
/* Index: claim_claimt_start_ix                                 */
/*==============================================================*/

CREATE INDEX claim_claimt_start_ix ON claim (
claimt_id,
claim_start_dt
);

/*==============================================================*/
/* Index: claim_claimt_last_emplr_ix                            */
/*==============================================================*/

CREATE INDEX claim_claimt_last_emplr_ix ON claim (
claimt_id,
last_emplr_id
);
ALTER TABLE claim
   ADD CONSTRAINT fk_claim_claim_cla_claimt FOREIGN KEY (claimt_id)
      REFERENCES claimt (claimt_id)
      ON DELETE CASCADE ON UPDATE CASCADE;

ALTER TABLE claim
   ADD CONSTRAINT fk_claim_claim_las_emplr FOREIGN KEY (last_emplr_id)
      REFERENCES emplr (emplr_id)
      ON delete set NULL ON UPDATE CASCADE;
	  
/*==============================================================*/
/* Table: cert_perd                                             */
/*==============================================================*/

CREATE TABLE cert_perd (
   cert_perd_end_dt     DATE                 NOT NULL,
   cert_perd_start_dt   DATE                 NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_cert_perd PRIMARY KEY (cert_perd_end_dt)
);

/*==============================================================*/
/* Table: cert                                                  */
/*==============================================================*/

CREATE TABLE cert (
   cert_id              BIGSERIAL            NOT NULL,
   claim_id             BIGINT               NOT NULL,
   cert_perd_end_dt     DATE                 NOT NULL,
   cert_dt              DATE                 NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_cert PRIMARY KEY (cert_id)
);

/*==============================================================*/
/* Index: cert_claim_fk                                         */
/*==============================================================*/

CREATE INDEX cert_claim_fk ON cert (
claim_id
);

/*==============================================================*/
/* Index: cert_perd_fk                                          */
/*==============================================================*/

CREATE INDEX cert_perd_fk ON cert (
cert_perd_end_dt
);

ALTER TABLE cert
   ADD CONSTRAINT fk_cert_cert_clai_claim FOREIGN KEY (claim_id)
      REFERENCES claim (claim_id)
      ON DELETE CASCADE ON UPDATE CASCADE;

ALTER TABLE cert
   ADD CONSTRAINT fk_cert_cert_perd_cert_per FOREIGN KEY (cert_perd_end_dt)
      REFERENCES cert_perd (cert_perd_end_dt)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: incm                                                  */
/*==============================================================*/

CREATE TABLE incm (
   incm_id              BIGSERIAL            NOT NULL,
   cert_id              BIGINT               NOT NULL,
   incm_type_cd         VARCHAR(10)          NULL,
   reglr_incm_flag      BOOLEAN              NULL,
   reptd_incm_amt       NUMERIC(16,2)        NULL,
   adjstd_incm_amt      NUMERIC(16,2)        NULL,
   days_workd_cnt       INTEGER              NULL,
   incm_abv_threshld_flag BOOLEAN            NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_incm PRIMARY KEY (incm_id)
);

/*==============================================================*/
/* Index: incm_cert_fk                                          */
/*==============================================================*/

CREATE INDEX incm_cert_fk ON incm (
cert_id
);

ALTER TABLE incm
   ADD CONSTRAINT fk_incm_incm_cert_cert FOREIGN KEY (cert_id)
      REFERENCES cert (cert_id)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: paymt                                                 */
/*==============================================================*/

CREATE TABLE paymt (
   paymt_id             BIGSERIAL            NOT NULL,
   cert_id              BIGINT               NOT NULL,
   paymt_type_cd        VARCHAR(10)          NOT NULL,
   reglr_paymt_flag     BOOLEAN              NOT NULL,
   paymt_amt            NUMERIC(16,2)        NOT NULL,
   progm_cd             VARCHAR(10)          NOT NULL,
   paid_dt              DATE                 NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_paymt PRIMARY KEY (paymt_id)
);

/*==============================================================*/
/* Index: paymt_cert_fk                                         */
/*==============================================================*/

CREATE INDEX paymt_cert_fk ON paymt (
cert_id
);

ALTER TABLE paymt
   ADD CONSTRAINT fk_paymt_paymt_cer_cert FOREIGN KEY (cert_id)
      REFERENCES cert (cert_id)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: ovpaymt                                               */
/*==============================================================*/

CREATE TABLE ovpaymt (
   ovpaymt_id           BIGSERIAL            NOT NULL,
   paymt_id             BIGINT               NOT NULL,
   ovpaymt_amt          NUMERIC(16,2)        NOT NULL,
   fraud_flag           BOOLEAN              NOT NULL,
   reas_cd              VARCHAR(10)          NULL,
   progm_cd             VARCHAR(10)          NULL,
   due_amt              NUMERIC(16,2)        NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_ovpaymt PRIMARY KEY (ovpaymt_id)
);

/*==============================================================*/
/* Index: ovpaymt_paymt_fk                                      */
/*==============================================================*/

CREATE INDEX ovpaymt_paymt_fk ON ovpaymt (
paymt_id
);

ALTER TABLE ovpaymt
   ADD CONSTRAINT fk_ovpaymt_ovpaymt_p_paymt FOREIGN KEY (paymt_id)
      REFERENCES paymt (paymt_id)
      ON DELETE CASCADE ON UPDATE CASCADE;
	  
/*==============================================================*/
/* Table: invstgn                                               */
/*==============================================================*/

CREATE TABLE invstgn (
   invstgn_id           BIGSERIAL            NOT NULL,
   cert_id              BIGINT               NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_invstgn PRIMARY KEY (invstgn_id)
);

/*==============================================================*/
/* Index: invstgn_cert_fk                                       */
/*==============================================================*/

CREATE INDEX invstgn_cert_fk ON invstgn (
cert_id
);

ALTER TABLE invstgn
   ADD CONSTRAINT fk_invstgn_invstgn_c_cert FOREIGN KEY (cert_id)
      REFERENCES cert (cert_id)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: addr_idfr                                             */
/*==============================================================*/

CREATE TABLE addr_idfr (
   addr_id              BIGSERIAL            NOT NULL,
   enty_type_cd         VARCHAR(20)          NOT NULL,
   enty_id              BIGINT               NOT NULL,
   addr_type_cd         VARCHAR(10)          NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_addr_idfr PRIMARY KEY (addr_id)
);

/*==============================================================*/
/* Index: addr_idfr_enty_type_id_ix                             */
/*==============================================================*/

CREATE INDEX addr_idfr_enty_type_id_ix ON addr_idfr (
enty_type_cd,
enty_id
);

/*==============================================================*/
/* Table: addr                                                  */
/*==============================================================*/

CREATE TABLE addr (
   addr_id              BIGINT               NOT NULL,
   eff_dt               DATE                 NOT NULL,
   exp_dt               DATE                 NULL,
   addr_line_1          VARCHAR(100)         NULL,
   addr_line_2          VARCHAR(100)         NULL,
   city_name            VARCHAR(100)         NULL,
   st_cd                VARCHAR(10)          NULL,
   zip_cd               CHAR(5)              NULL,
   zip_cd_plus_4        CHAR(4)              NULL,
   cnty_name            VARCHAR(100)         NULL,
   cntry_cd             VARCHAR(10)          NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_addr PRIMARY KEY (addr_id, eff_dt)
);

/*==============================================================*/
/* Index: addr_addr_id_exp_ix                                   */
/*==============================================================*/

CREATE INDEX addr_addr_id_exp_ix ON addr (
addr_id,
exp_dt
);

/*==============================================================*/
/* constraint: addr                                             */
/*==============================================================*/

ALTER TABLE addr
   ADD CONSTRAINT fk_addr_addr_idfr_addr_idf FOREIGN KEY (addr_id)
      REFERENCES addr_idfr (addr_id)
      ON DELETE CASCADE ON UPDATE CASCADE;
