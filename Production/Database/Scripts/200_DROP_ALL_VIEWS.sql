/*==============================================================*/
/* DROP_ALL_VIEWS: Drop all views in order of dependence.       */
/*==============================================================*/

/*==============================================================*/
/* BID_VIEWS                                                    */
/*==============================================================*/

SET SEARCH_PATH = bid;

DROP MATERIALIZED VIEW bid_claimt_list;
DROP MATERIALIZED VIEW scored_aggr_claimt_emplr_hist;
DROP MATERIALIZED VIEW scored_claimt_attr;
DROP MATERIALIZED VIEW scored_claimt_sessn;
DROP MATERIALIZED VIEW scored_claimt_wage;
