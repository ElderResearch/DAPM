/*==============================================================*/
/* STG Schema: Staging Table, Index and Constraint Definitions  */
/*==============================================================*/

SET SEARCH_PATH = stg, ref;

/*==============================================================*/
/* Table: data_source                                           */
/*==============================================================*/

CREATE TABLE data_source (
   data_source_cd       VARCHAR(10)          NOT NULL,
   data_source_name     VARCHAR(100)         NOT NULL,
   data_source_desc     VARCHAR(500)         NULL,
   data_source_type_desc VARCHAR(500)        NULL,
   crt_user_id          VARCHAR(50)          NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_user_id    VARCHAR(50)          NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_data_source PRIMARY KEY (data_source_cd)
);

/*==============================================================*/
/* Table: data_source_lkup_tabl                                 */
/*==============================================================*/

CREATE TABLE data_source_lkup_tabl (
   data_source_cd       VARCHAR(10)          NOT NULL,
   data_source_lkup_tabl_id BIGINT           NOT NULL,
   data_source_lkup_tabl_name VARCHAR(100)   NOT NULL,
   data_source_lkup_tabl_desc VARCHAR(500)   NULL,
   lkup_tabl_id         BIGINT               NULL,
   crt_user_id          VARCHAR(50)          NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_user_id    VARCHAR(50)          NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_data_source_lkup_tabl PRIMARY KEY (data_source_cd, data_source_lkup_tabl_id)
);

/*==============================================================*/
/* Index: data_source_lkup_tabl_fk                              */
/*==============================================================*/

CREATE INDEX data_source_lkup_tabl_fk ON data_source_lkup_tabl (
lkup_tabl_id
);

ALTER TABLE data_source_lkup_tabl
   ADD CONSTRAINT fk_data_sou_data_sour_lkup_tab FOREIGN KEY (lkup_tabl_id)
      REFERENCES lkup_tabl (lkup_tabl_id)
      ON DELETE CASCADE ON UPDATE CASCADE;

ALTER TABLE data_source_lkup_tabl
   ADD CONSTRAINT fk_data_sou_lkup_tabl_data_sou FOREIGN KEY (data_source_cd)
      REFERENCES data_source (data_source_cd)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: data_source_lkup_cd                                   */
/*==============================================================*/

CREATE TABLE data_source_lkup_cd (
   data_source_cd       VARCHAR(10)          NOT NULL,
   data_source_lkup_tabl_id BIGINT           NOT NULL,
   data_source_lkup_cd  VARCHAR(100)         NOT NULL,
   data_source_lkup_val VARCHAR(100)         NOT NULL,
   data_source_lkup_desc VARCHAR(500)        NULL,
   lkup_tabl_id         BIGINT               NULL,
   lkup_cd              VARCHAR(10)          NULL,
   crt_user_id          VARCHAR(50)          NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_user_id    VARCHAR(50)          NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_data_source_lkup_cd PRIMARY KEY (data_source_cd, data_source_lkup_tabl_id, data_source_lkup_cd)
);

/*==============================================================*/
/* Index: data_source_lkup_cd_fk                                */
/*==============================================================*/

CREATE INDEX data_source_lkup_cd_fk ON data_source_lkup_cd (
lkup_tabl_id,
lkup_cd
);

ALTER TABLE data_source_lkup_cd
   ADD CONSTRAINT fk_data_sou_data_sour_lkup_cd FOREIGN KEY (lkup_tabl_id, lkup_cd)
      REFERENCES lkup_cd (lkup_tabl_id, lkup_cd)
      ON DELETE CASCADE ON UPDATE CASCADE;

ALTER TABLE data_source_lkup_cd
   ADD CONSTRAINT fk_data_sou_data_sour_data_sou FOREIGN KEY (data_source_cd, data_source_lkup_tabl_id)
      REFERENCES data_source_lkup_tabl (data_source_cd, data_source_lkup_tabl_id)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: enty_idfr                                             */
/*==============================================================*/

CREATE TABLE enty_idfr (
   data_source_cd       VARCHAR(10)          NOT NULL,
   enty_type_cd         VARCHAR(20)          NOT NULL,
   data_source_uuid     VARCHAR(100)         NOT NULL,
   enty_id              BIGINT               NOT NULL,
   enty_ref_num         VARCHAR(100)         NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   CONSTRAINT pk_enty_idfr PRIMARY KEY (data_source_cd, enty_type_cd, data_source_uuid),
   CONSTRAINT ak_enty_idfr_ai01_enty_idf UNIQUE (data_source_cd, enty_type_cd, enty_ref_num, data_source_uuid)
);

ALTER TABLE enty_idfr
   ADD CONSTRAINT fk_enty_idf_enty_idfr_data_sou FOREIGN KEY (data_source_cd)
      REFERENCES data_source (data_source_cd)
      ON DELETE CASCADE ON UPDATE CASCADE;
