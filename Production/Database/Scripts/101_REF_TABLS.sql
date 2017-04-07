/*==============================================================*/
/* REF Schema: Reference Table, Index and Constraint Definitions*/
/*==============================================================*/

SET SEARCH_PATH = ref;

/*==============================================================*/
/* Table: sys_parm                                              */
/*==============================================================*/

CREATE TABLE sys_parm (
   sys_parm_cd          VARCHAR(100)         NOT NULL,
   sys_parm_desc        VARCHAR(500)         NOT NULL,
   sys_parm_val         VARCHAR(100)         NOT NULL,
   crt_user_id          VARCHAR(50)          NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_user_id    VARCHAR(50)          NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_sys_parm PRIMARY KEY (sys_parm_cd)
);

/*==============================================================*/
/* Table: lkup_tabl                                             */
/*==============================================================*/

CREATE TABLE lkup_tabl (
   lkup_tabl_id         BIGINT               NOT NULL,
   lkup_tabl_name       VARCHAR(100)         NOT NULL,
   lkup_tabl_desc       VARCHAR(500)         NULL,
   crt_user_id          VARCHAR(50)          NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_user_id    VARCHAR(50)          NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_lkup_tabl PRIMARY KEY (lkup_tabl_id)
);

/*==============================================================*/
/* Table: lkup_cd                                               */
/*==============================================================*/

CREATE TABLE lkup_cd (
   lkup_tabl_id         BIGINT               NOT NULL,
   lkup_cd              VARCHAR(10)          NOT NULL,
   lkup_val             VARCHAR(100)         NOT NULL,
   crt_user_id          VARCHAR(50)          NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_user_id    VARCHAR(50)          NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_lkup_cd PRIMARY KEY (lkup_tabl_id, lkup_cd)
);

ALTER TABLE lkup_cd
   ADD CONSTRAINT fk_lkup_cd_lkup_cd_t_lkup_tab FOREIGN KEY (lkup_tabl_id)
      REFERENCES lkup_tabl (lkup_tabl_id)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: parnt_lkup_tabl                                       */
/*==============================================================*/

CREATE TABLE parnt_lkup_tabl (
   parnt_lkup_tabl_id   BIGINT               NOT NULL,
   lkup_tabl_id         BIGINT               NOT NULL,
   crt_user_id          VARCHAR(50)          NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_user_id    VARCHAR(50)          NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_parnt_lkup_tabl PRIMARY KEY (parnt_lkup_tabl_id, lkup_tabl_id),
   CONSTRAINT ak_parnt_lkup_tabl_ai_parnt_lk UNIQUE (lkup_tabl_id, parnt_lkup_tabl_id)
);

ALTER TABLE parnt_lkup_tabl
   ADD CONSTRAINT fk_parnt_lk_lkup_tabl_lkup_tab FOREIGN KEY (lkup_tabl_id)
      REFERENCES lkup_tabl (lkup_tabl_id)
      ON DELETE CASCADE ON UPDATE CASCADE;

ALTER TABLE parnt_lkup_tabl
   ADD CONSTRAINT fk_parnt_lk_parnt_lku_lkup_tab FOREIGN KEY (parnt_lkup_tabl_id)
      REFERENCES lkup_tabl (lkup_tabl_id)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: parnt_lkup_cd                                         */
/*==============================================================*/

CREATE TABLE parnt_lkup_cd (
   parnt_lkup_tabl_id   BIGINT               NOT NULL,
   parnt_lkup_cd        VARCHAR(10)          NOT NULL,
   lkup_tabl_id         BIGINT               NOT NULL,
   lkup_cd              VARCHAR(10)          NOT NULL,
   crt_user_id          VARCHAR(50)          NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_user_id    VARCHAR(50)          NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_parnt_lkup_cd PRIMARY KEY (parnt_lkup_tabl_id, parnt_lkup_cd, lkup_tabl_id, lkup_cd),
   CONSTRAINT ak_parnt_lkup_cd_ai01_parnt_lk UNIQUE (lkup_tabl_id, lkup_cd, parnt_lkup_tabl_id, parnt_lkup_cd)
);

ALTER TABLE parnt_lkup_cd
   ADD CONSTRAINT fk_parnt_lk_lkup_cd_lkup_cd FOREIGN KEY (lkup_tabl_id, lkup_cd)
      REFERENCES lkup_cd (lkup_tabl_id, lkup_cd)
      ON DELETE CASCADE ON UPDATE CASCADE;

ALTER TABLE parnt_lkup_cd
   ADD CONSTRAINT fk_parnt_lk_parnt_lku_lkup_cd FOREIGN KEY (parnt_lkup_tabl_id, parnt_lkup_cd)
      REFERENCES lkup_cd (lkup_tabl_id, lkup_cd)
      ON DELETE CASCADE ON UPDATE CASCADE;

ALTER TABLE parnt_lkup_cd
   ADD CONSTRAINT fk_parnt_lk_parnt_lku_parnt_lk FOREIGN KEY (parnt_lkup_tabl_id, lkup_tabl_id)
      REFERENCES parnt_lkup_tabl (parnt_lkup_tabl_id, lkup_tabl_id)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: model                                                 */
/*==============================================================*/

CREATE TABLE model (
   model_cd             VARCHAR(10)          NOT NULL,
   model_name           VARCHAR(100)         NOT NULL,
   model_desc           VARCHAR(1000)        NULL,
   crt_user_id          VARCHAR(50)          NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_user_id    VARCHAR(50)          NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_model PRIMARY KEY (model_cd)
);

/*==============================================================*/
/* Table: script                                                */
/*==============================================================*/

CREATE TABLE script (
   script_id            BIGINT               NOT NULL,
   script_name          VARCHAR(100)         NOT NULL,
   script_desc          VARCHAR(1000)        NULL,
   script_filename      VARCHAR(100)         NOT NULL,
   script_type_cd       VARCHAR(10)          NOT NULL,
   script_lang_cd       VARCHAR(10)          NOT NULL,
   run_flag             BOOLEAN              NOT NULL,
   run_seq              INTEGER              NOT NULL,
   crt_user_id          VARCHAR(50)          NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_user_id    VARCHAR(50)          NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_script PRIMARY KEY (script_id)
);

/*==============================================================*/
/* Table: model_script                                          */
/*==============================================================*/

CREATE TABLE model_script (
   model_cd             VARCHAR(10)          NOT NULL,
   script_id            BIGINT               NOT NULL,
   crt_user_id          VARCHAR(50)          NOT NULL,
   crt_tmstmp           DATE                 NOT NULL,
   CONSTRAINT pk_model_script PRIMARY KEY (model_cd, script_id)
);

/*==============================================================*/
/* Index: model_script_fk                                       */
/*==============================================================*/

CREATE INDEX model_script_fk ON model_script (
script_id
);

ALTER TABLE model_script
   ADD CONSTRAINT fk_model_sc_model_scr_script FOREIGN KEY (script_id)
      REFERENCES script (script_id)
      ON DELETE CASCADE ON UPDATE CASCADE;

ALTER TABLE model_script
   ADD CONSTRAINT fk_model_sc_script_mo_model FOREIGN KEY (model_cd)
      REFERENCES model (model_cd)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: parm                                                  */
/*==============================================================*/

CREATE TABLE parm (
   parm_cd              VARCHAR(100)         NOT NULL,
   parm_name            VARCHAR(100)         NOT NULL,
   parm_desc            VARCHAR(1000)        NULL,
   parm_data_type_cd    VARCHAR(10)          NOT NULL,
   parm_val             VARCHAR(100)         NOT NULL,
   crt_user_id          VARCHAR(50)          NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_user_id    VARCHAR(50)          NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_parm PRIMARY KEY (parm_cd)
);

/*==============================================================*/
/* Table: model_parm                                            */
/*==============================================================*/

CREATE TABLE model_parm (
   model_cd             VARCHAR(10)          NOT NULL,
   parm_cd              VARCHAR(100)         NOT NULL,
   crt_user_id          VARCHAR(50)          NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   CONSTRAINT pk_model_parm PRIMARY KEY (model_cd, parm_cd)
);

/*==============================================================*/
/* Index: model_parm_fk                                         */
/*==============================================================*/

CREATE INDEX model_parm_fk ON model_parm (
parm_cd
);

ALTER TABLE model_parm
   ADD CONSTRAINT fk_model_pa_model_par_parm FOREIGN KEY (parm_cd)
      REFERENCES parm (parm_cd)
      ON DELETE CASCADE ON UPDATE CASCADE;

ALTER TABLE model_parm
   ADD CONSTRAINT fk_model_pa_parm_mode_model FOREIGN KEY (model_cd)
      REFERENCES model (model_cd)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: score                                                 */
/*==============================================================*/

CREATE TABLE score (
   score_cd             VARCHAR(100)         NOT NULL,
   score_name           VARCHAR(100)         NOT NULL,
   score_desc           VARCHAR(1000)        NULL,
   model_cd             VARCHAR(10)          NOT NULL,
   parnt_score_cd       VARCHAR(100)         NULL,
   enty_type_cd         VARCHAR(10)          NOT NULL,
   score_lvl_num        INTEGER              NOT NULL,
   curr_score_wgt_val   INTEGER              NULL,
   crt_user_id          VARCHAR(50)          NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_user_id    VARCHAR(50)          NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_score PRIMARY KEY (score_cd)
);

/*==============================================================*/
/* Index: score_model_fk                                        */
/*==============================================================*/

CREATE INDEX score_model_fk ON score (
model_cd
);

/*==============================================================*/
/* Index: parnt_score_fk                                        */
/*==============================================================*/

CREATE INDEX parnt_score_fk ON score (
parnt_score_cd
);

ALTER TABLE score
   ADD CONSTRAINT fk_score_parnt_sco_score FOREIGN KEY (parnt_score_cd)
      REFERENCES score (score_cd)
      ON DELETE CASCADE ON UPDATE CASCADE;

ALTER TABLE score
   ADD CONSTRAINT fk_score_score_mod_model FOREIGN KEY (model_cd)
      REFERENCES model (model_cd)
      ON DELETE CASCADE ON UPDATE CASCADE;

/*==============================================================*/
/* Table: flag                                                  */
/*==============================================================*/

CREATE TABLE flag (
   flag_cd              VARCHAR(10)          NOT NULL,
   flag_name            VARCHAR(100)         NOT NULL,
   flag_desc            VARCHAR(1000)        NULL,
   model_cd             VARCHAR(10)          NOT NULL,
   enty_type_cd         VARCHAR(10)          NOT NULL,
   crt_user_id          VARCHAR(50)          NOT NULL,
   crt_tmstmp           TIMESTAMP            NOT NULL,
   last_updt_user_id    VARCHAR(50)          NULL,
   last_updt_tmstmp     TIMESTAMP            NULL,
   CONSTRAINT pk_flag PRIMARY KEY (flag_cd)
);

/*==============================================================*/
/* Index: flag_model_fk                                         */
/*==============================================================*/

CREATE INDEX flag_model_fk ON flag (
model_cd
);

ALTER TABLE flag
   ADD CONSTRAINT fk_flag_flag_mode_model FOREIGN KEY (model_cd)
      REFERENCES model (model_cd)
      ON DELETE CASCADE ON UPDATE CASCADE;