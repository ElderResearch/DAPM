/*==============================================================*/
/* DROP_ALL_TABLES: Drop all tables in order of dependence.     */
/*==============================================================*/

/*==============================================================*/
/* RESLT_TABLS                                                  */
/*==============================================================*/

set SEARCH_PATH to NRD;

drop table if exists IPA_ELR_DS;
drop table if exists IPA_CLR_DS;
drop table if exists IPA_CHIP_DS;
drop table if exists IPA_SHIP_DS;
drop table if exists ENTY_FLAG;
drop table if exists ENTY_SCORE;
drop table if exists CYCL_SCORE;
drop table if exists CYCL;

/*==============================================================*/
/* AGGR_TABLS                                                   */
/*==============================================================*/

set SEARCH_PATH to NRD;

drop table if exists AGGR_CLAIMT_OVPAYMT;
drop table if exists AGGR_CLAIMT_BENF;

drop table if exists AGGR_CLAIMT_WAGE_REPT;
drop table if exists AGGR_CLAIMT_EMPLR_HIST;
drop table if exists AGGR_CLAIMT_NEW_HIRE;

drop table if exists AGGR_EMPLR_SESSN;

drop table if exists AGGR_CLAIMT_SHARED_ATTR_EDGES;
drop table if exists AGGR_CLAIMT_SHARED_ATTR_CNTS;
drop table if exists AGGR_SHARED_CLAIMT_ATTR;
drop table if exists AGGR_CLAIMT_SESSN;

/*==============================================================*/
/* BASE_TABLS                                                   */
/*==============================================================*/

set SEARCH_PATH to NRD;

drop table if exists ADDR;
drop table if exists ADDR_IDFR;
drop table if exists INVSTGN;
drop table if exists OVPAYMT;
drop table if exists PAYMT;
drop table if exists INCM;
drop table if exists CERT;
drop table if exists CERT_PERD;
drop table if exists CLAIM;
drop table if exists CLAIMT_SESSN;
drop table if exists CLAIMT_ATTR;
drop table if exists CLAIMT;
drop table if exists WAGE;
drop table if exists WAGE_REPT;
drop table if exists EMPL;
drop table if exists CORP_OFFCR;
drop table if exists EMPLR_SESSN;
drop table if exists EMPLR;

/*==============================================================*/
/* STG_TABLS                                                    */
/*==============================================================*/

set SEARCH_PATH to STG;

drop table if exists ENTY_IDFR;
drop table if exists DATA_SOURCE_LKUP_CD;
drop table if exists DATA_SOURCE_LKUP_TABL;
drop table if exists DATA_SOURCE;

/*==============================================================*/
/* REF_TABLS                                                    */
/*==============================================================*/

set SEARCH_PATH to REF;

drop table if exists FLAG;
drop table if exists SCORE;
drop table if exists MODEL_PARM;
drop table if exists PARM;
drop table if exists MODEL_SCRIPT;
drop table if exists SCRIPT;
drop table if exists MODEL;

drop table if exists PARNT_LKUP_CD;
drop table if exists PARNT_LKUP_TABL;
drop table if exists LKUP_CD;
drop table if exists LKUP_TABL;

drop table if exists SYS_PARM;


