CREATE MATERIALIZED VIEW bid.bid_claimt_list AS
 SELECT x.claimt_id,
    x.normlzd_score_val,
    x.cycl_dt,
    x.claimt_last_name,
    x.claimt_first_name,
    x.claimt_ref_num,
    x.email_addr,
    x.phon_num,
    x.city_name,
    x.st_cd
   FROM ( SELECT DISTINCT ON (c.claimt_id) c.claimt_id,
            es.normlzd_score_val,
            es.cycl_dt,
            c.claimt_last_name,
            c.claimt_first_name,
            c.claimt_ref_num,
            cae.claimt_attr_val AS email_addr,
            cap.claimt_attr_val AS phon_num,
            a.city_name,
            a.st_cd
           FROM claimt c
             JOIN enty_score es ON es.enty_type_cd::text = 'CLAIMT'::text AND es.enty_id = c.claimt_id AND es.score_cd::text = 'CLA_RISK'::text
             LEFT JOIN addr_idfr ai ON ai.enty_type_cd::text = 'CLAIMT'::text AND ai.enty_id = c.claimt_id AND ai.addr_type_cd::text = 'RESD'::text
             LEFT JOIN addr a ON a.addr_id = ai.addr_id AND a.exp_dt IS NULL
             LEFT JOIN claimt_attr cae ON cae.claimt_id = c.claimt_id AND cae.claimt_attr_type_cd::text = 'EMAIL'::text AND cae.claimt_attr_sub_type_cd::text = 'HOME'::text
             LEFT JOIN claimt_attr cap ON cap.claimt_id = c.claimt_id AND cap.claimt_attr_type_cd::text = 'PHON'::text AND cap.claimt_attr_sub_type_cd::text = 'HOME'::text
          ORDER BY c.claimt_id, es.cycl_dt DESC) x
  ORDER BY x.normlzd_score_val DESC
WITH DATA;

CREATE INDEX bid_claimt_list_idx1 ON bid.bid_claimt_list (claimt_id);
CREATE INDEX bid_claimt_list_idx2 ON bid.bid_claimt_list (normlzd_score_val DESC);

CREATE MATERIALIZED VIEW bid.scored_aggr_claimt_emplr_hist
 SELECT aceh.claimt_id,
    aceh.emplr_id,
    aceh.empl_start_qtr_num,
    aceh.empl_end_qtr_num,
    aceh.crt_tmstmp,
    aceh.last_updt_tmstmp,
    COALESCE((cl.claimt_first_name::text || ' '::text) || cl.claimt_last_name::text, 'N/A'::text) AS claimt_name,
    emplr.emplr_name
   FROM aggr_claimt_emplr_hist aceh
     JOIN bid_claimt_list cl ON aceh.claimt_id = cl.claimt_id
     JOIN emplr ON aceh.emplr_id = emplr.emplr_id
WITH DATA;

CREATE INDEX scored_aceh_idx ON bid.scored_aggr_claimt_emplr_hist (claimt_id);

CREATE MATERIALIZED VIEW bid.scored_claimt_attr
 SELECT ca.claimt_id,
    ca.claimt_attr_type_cd,
    ca.claimt_attr_val,
    COALESCE((cl.claimt_first_name::text || ' '::text) || cl.claimt_last_name::text, 'N/A'::text) AS claimt_name
   FROM claimt_attr ca
     JOIN bid_claimt_list cl ON ca.claimt_id = cl.claimt_id
  WHERE ca.claimt_attr_type_cd::text = ANY (ARRAY['PHON'::character varying, 'EMAIL'::character varying]::text[])
WITH DATA;

CREATE INDEX scored_ca_idx ON bid.scored_claimt_attr (claimt_id);

CREATE MATERIALIZED VIEW bid.scored_claimt_sessn
 SELECT cs.claimt_id,
    cs.claimt_sessn_tmstmp,
    cs.orig_ip_addr,
    cs.ip_city_name,
    cs.ip_st_cd,
    COALESCE((cl.claimt_first_name::text || ' '::text) || cl.claimt_last_name::text, 'N/A'::text) AS claimt_name
   FROM claimt_sessn cs
     JOIN bid_claimt_list cl ON cs.claimt_id = cl.claimt_id
  WHERE cs.ip_conn_type_cd IS NULL OR cs.ip_conn_type_cd::text <> 'CEL'::text
WITH DATA;

CREATE INDEX scored_cs_idx1 ON bid.scored_claimt_sessn (claimt_id);
CREATE INDEX scored_cs_idx2 ON bid.scored_claimt_sessn USING GIST (orig_ip_addr inet_ops);

CREATE MATERIALIZED VIEW bid.scored_claimt_wage
 SELECT c.claimt_id,
    wr.emplr_id,
    wr.wage_dt,
    to_char(wr.wage_dt::timestamp with time zone, 'YYYY-Q'::text) AS wage_qtr,
    w1.wage_amt,
    wr.mean_empl_cnt,
        CASE
            WHEN w2.sum IS NOT NULL AND w2.sum > 0::numeric THEN round(w1.wage_amt * 100::numeric / w2.sum, 2)
            ELSE 0::numeric
        END AS wage_pct
   FROM bid_claimt_list c
     JOIN empl ee ON ee.empl_ref_num::text = c.claimt_ref_num::text
     JOIN wage_rept wr ON wr.emplr_id = ee.emplr_id
     JOIN wage w1 ON w1.wage_rept_id = wr.wage_rept_id AND w1.empl_id = ee.empl_id
     JOIN ( SELECT wage.wage_rept_id,
            SUM(wage.wage_amt) AS sum
           FROM wage
          GROUP BY wage.wage_rept_id) w2 ON w1.wage_rept_id = w2.wage_rept_id
WITH DATA;

CREATE INDEX scored_cw_idx ON bid.scored_claimt_wage (claimt_id);
