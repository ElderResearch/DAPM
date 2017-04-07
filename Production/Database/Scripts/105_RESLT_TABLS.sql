/*==============================================================*/
/* NRD Schema: Result Table, Index and Constraint Definitions   */
/*==============================================================*/

SET SEARCH_PATH = nrd, ref;

/*==============================================================*/
/* Table: cycl                                                  */
/*==============================================================*/

CREATE TABLE cycl (
   cycl_dt              DATE                 NOT NULL,
   prod_source_cd_vers_num VARCHAR(20)       NOT NULL,
   cust_source_cd_vers_num VARCHAR(20)       NOT NULL,
   univ_model_vers_num  VARCHAR(20)          NOT NULL,
   cycl_start_tmstmp    TIMESTAMP            NOT NULL,
   cycl_end_tmstmp      TIMESTAMP            NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_cycl PRIMARY KEY (cycl_dt)
);

/*==============================================================*/
/* Table: cycl_score                                            */
/*==============================================================*/

CREATE TABLE cycl_score (
   cycl_dt              DATE                 NOT NULL,
   score_cd             VARCHAR(100)         NOT NULL,
   parnt_score_cd       VARCHAR(100)         NULL,
   score_lvl_num        INTEGER              NOT NULL,
   score_wgt_val        INTEGER              NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_cycl_score PRIMARY KEY (cycl_dt, score_cd)
);

/*==============================================================*/
/* Index: score_cycl_fk                                         */
/*==============================================================*/

CREATE  INDEX score_cycl_fk ON cycl_score (
cycl_dt
);

/*==============================================================*/
/* Index: cycl_score_fk                                         */
/*==============================================================*/

CREATE  INDEX cycl_score_fk ON cycl_score (
score_cd
);

ALTER TABLE cycl_score
   ADD CONSTRAINT fk_cycl_sco_cycl_scor_score FOREIGN KEY (score_cd)
      REFERENCES score (score_cd)
      ON DELETE CASCADE ON UPDATE CASCADE;

ALTER TABLE cycl_score
   ADD CONSTRAINT fk_cycl_sco_score_cyc_cycl FOREIGN KEY (cycl_dt)
      REFERENCES cycl (cycl_dt)
      ON DELETE CASCADE ON UPDATE CASCADE;
	  
/*==============================================================*/
/* Table: enty_score                                            */
/*==============================================================*/

CREATE TABLE enty_score (
   cycl_dt              DATE                 NOT NULL,
   enty_type_cd         VARCHAR(20)          NOT NULL,
   enty_id              BIGINT               NOT NULL,
   score_cd             VARCHAR(100)         NOT NULL,
   score_val            NUMERIC(12,6)        NOT NULL,
   normlzd_score_val    NUMERIC(12,6)        NULL,
   wgtd_score_val       NUMERIC(12,6)        NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_enty_score PRIMARY KEY (cycl_dt, enty_type_cd, enty_id, score_cd)
);

/*==============================================================*/
/* Index: enty_score_fk                                         */
/*==============================================================*/

CREATE INDEX enty_score_fk ON enty_score (
score_cd
);

/*==============================================================*/
/* Index: enty_score_type_id_cycl_ix                            */
/*==============================================================*/

CREATE INDEX enty_score_type_id_cycl_ix ON enty_score (
enty_type_cd,
enty_id,
cycl_dt
);

ALTER TABLE enty_score
   ADD CONSTRAINT fk_enty_sco_enty_scor_score FOREIGN KEY (score_cd)
      REFERENCES score (score_cd)
      ON DELETE CASCADE ON UPDATE CASCADE;

ALTER TABLE enty_score
   ADD CONSTRAINT fk_enty_sco_enty_scor_cycl FOREIGN KEY (cycl_dt)
      REFERENCES cycl (cycl_dt)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: enty_flag                                             */
/*==============================================================*/

CREATE TABLE enty_flag (
   cycl_dt              DATE                 NOT NULL,
   enty_type_cd         VARCHAR(20)          NOT NULL,
   enty_id              BIGINT               NOT NULL,
   flag_cd              VARCHAR(10)          NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_enty_flag PRIMARY KEY (cycl_dt, enty_type_cd, enty_id, flag_cd)
);

/*==============================================================*/
/* Index: enty_flag_fk                                          */
/*==============================================================*/

CREATE INDEX enty_flag_fk ON enty_flag (
flag_cd
);

ALTER TABLE enty_flag
   ADD CONSTRAINT fk_enty_fla_enty_flag_flag FOREIGN KEY (flag_cd)
      REFERENCES flag (flag_cd)
      ON DELETE CASCADE ON UPDATE CASCADE;

ALTER TABLE enty_flag
   ADD CONSTRAINT fk_enty_fla_enty_flag_cycl FOREIGN KEY (cycl_dt)
      REFERENCES cycl (cycl_dt)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: ipa_ship_ds                                           */
/*==============================================================*/

CREATE TABLE ipa_ship_ds (
   cycl_dt              DATE                 NOT NULL,
   claimt_id            BIGINT               NOT NULL,
   orig_ip_addr         INET                 NOT NULL,
   matchd_claimt_id_array BIGINT ARRAY       NOT NULL,
   uniq_claimt_cnt      INTEGER              NOT NULL,
   cuml_uniq_claimt_cnt INTEGER              NOT NULL,
   min_sessn_gap_cnt    BIGINT               NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_ipa_ship_ds PRIMARY KEY (cycl_dt, claimt_id)
);

ALTER TABLE ipa_ship_ds
   ADD CONSTRAINT fk_ipa_ship_ipa_ship__cycl FOREIGN KEY (cycl_dt)
      REFERENCES cycl (cycl_dt)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: ipa_chip_ds                                           */
/*==============================================================*/

CREATE TABLE ipa_chip_ds (
   cycl_dt              DATE                 NOT NULL,
   claimt_id            BIGINT               NOT NULL,
   uniq_ip_addr_cnt     INTEGER              NOT NULL,
   uniq_sessn_cnt       INTEGER              NOT NULL,
   uniq_pct             NUMERIC(8,5)         NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_ipa_chip_ds PRIMARY KEY (cycl_dt, claimt_id)
);

ALTER TABLE ipa_chip_ds
   ADD CONSTRAINT fk_ipa_chip_ipa_chip__cycl FOREIGN KEY (cycl_dt)
      REFERENCES cycl (cycl_dt)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: ipa_clr_ds                                            */
/*==============================================================*/

CREATE TABLE ipa_clr_ds (
   cycl_dt              DATE                 NOT NULL,
   claimt_id            BIGINT               NOT NULL,
   ip_anon_proxy_flag   BOOLEAN              NOT NULL,
   ip_outsd_cntry_claimt_st_flag BOOLEAN     NOT NULL,
   ip_outsd_local_claimt_st_flag BOOLEAN     NOT NULL,
   ip_outsd_claimt_st_flag BOOLEAN           NOT NULL,
   missg_claimt_st_flag BOOLEAN              NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_ipa_clr_ds PRIMARY KEY (cycl_dt, claimt_id)
);

ALTER TABLE ipa_clr_ds
   ADD CONSTRAINT fk_ipa_clr__ipa_clr_d_cycl FOREIGN KEY (cycl_dt)
      REFERENCES cycl (cycl_dt)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: ipa_elr_ds                                            */
/*==============================================================*/

CREATE TABLE ipa_elr_ds (
   cycl_dt              DATE                 NOT NULL,
   claimt_id            BIGINT               NOT NULL,
   emplr_shared_ip_addr_cnt INTEGER          NOT NULL,
   emplr_shared_ip_addr_pct NUMERIC(8,5)     NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_ipa_elr_ds PRIMARY KEY (cycl_dt, claimt_id)
);

ALTER TABLE ipa_elr_ds
   ADD CONSTRAINT fk_ipa_elr__ipa_elr_d_cycl FOREIGN KEY (cycl_dt)
      REFERENCES cycl (cycl_dt)
      ON DELETE CASCADE ON UPDATE CASCADE;
	  
	  