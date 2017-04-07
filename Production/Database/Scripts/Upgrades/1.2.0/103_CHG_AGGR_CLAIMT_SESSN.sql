/*==============================================================*/
/* NRD Schema: Result Table, Index and Constraint Definitions   */
/*==============================================================*/

SET SEARCH_PATH = nrd;

DROP TABLE aggr_claimt_sessn;

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
/* NOTE: NEED TO RE-RUN AGGREGATION TO RE-POPULATE THIS TABLE   */
/*==============================================================*/
