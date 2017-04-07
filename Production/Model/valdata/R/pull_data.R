
#' @title pull data
#' @description Pull all data fields that are used for generating scores.
#' This includes the statistical modules and business rules.
#' The output includes 8 data.frames created from tables in the NRDB
#' @param min_date - the minimum date to filter the query by. of the form 'year-month-day'
#' @param max_date - the maximum date to filter the query by. of the form 'year-month-day'
#' @export


pull_data <- function(min_date = "1900-01-01" , max_date = Sys.Date()){
  APM::initialize_apm ("")
  db_conn <- APM::initialize_db ()
  cat("Start pulling data from the NRDB")
  cat("\n")
  cat("\n")
  #The code for each of the data pulls comes from the github directory, which has more specific comments
  #The variables used in the modules are the ones that are pulled
  date_range <- paste0("'", min_date, "'", " AND ", "'", max_date, "'")
  max_date <<- max_date
  min_date <<- min_date

  #used for statistical modules
  aggr_claimt_benf <<- RJDBC::dbGetQuery(db_conn$conn, paste0("SELECT
                                  last_emplr_naics2_cd, seprtn_reas_cd,
                                  educ_lvl_cd, workrs_comp_cd,
                                  pensn_cd, subst_workr_flag,
                                  school_empl_flag, filg_outsd_st_flag,
                                  union_flag, corp_flag,
                                  sevrnc_flag, commtr_flag,
                                  last_emplr_days_emplyd_cnt, cert_perd_end_qtr_num,
                                  claimt_age, claim_year_seq,
                                  perds_to_claim_start_cnt, perds_to_cert_start_cnt,
                                  base_perd_paymt_amt, st_base_perd_paymt_amt,
                                  base_paymt_perds_cnt, max_paymt_amt,
                                  st_max_paymt_amt, tot_reglr_reptd_incm_amt,
                                  tot_reglr_paymt_amt, cuml_paymt_amt,
                                  cuml_paymt_perds_cnt, cuml_zero_incm_cert_perd_cnt,
                                  cuml_lt_min_incm_cert_perd_cnt, cuml_gt_max_incm_cert_perd_cnt,
                                  cuml_reptd_incm_amt, paymt_to_base_paymt_ratio,
                                  incm_to_base_paymt_ratio, paymt_to_base_paymt_perds_ratio
                                  FROM nrd.aggr_claimt_benf
                                  WHERE cert_perd_end_dt between ", date_range,
                                  " ORDER BY RANDOM()
                                  limit 1000000 ;"))
  check_classes_match(aggr_claimt_benf)
  dimension(aggr_claimt_benf)


  #used for statistical modules
  aggr_claimt_ovpaymt <<- RJDBC::dbGetQuery(db_conn$conn, paste0("SELECT
                                     ovpaymt_amt, fraud_flag,
                                     reas_cd FROM nrd.aggr_claimt_ovpaymt
                                     WHERE ovpaymt_start_dt between ", date_range))
 check_classes_match(aggr_claimt_ovpaymt)
 dimension(aggr_claimt_ovpaymt)


  #used for NewHire Module
  sql_new_hire <<- RJDBC::dbGetQuery(db_conn$conn, paste0("SELECT
                              max_reptd_incm_amt, min_incm_threshld_amt
                              FROM nrd.aggr_claimt_new_hire
                              WHERE cert_perd_end_dt between ", date_range))
  check_classes_match(sql_new_hire)
  dimension(sql_new_hire)


  #used for QWR score module
  sql_QWR <<- RJDBC::dbGetQuery(db_conn$conn, paste0("SELECT
                         sum (tot_reglr_reptd_incm_amt) as total_earnings,
                         sum (tot_reglr_paymt_amt) as total_benefit,
                         sum (base_perd_paymt_amt) as total_wba,
                         sum (CASE WHEN tot_reglr_paymt_amt > 0 THEN 1 ELSE 0 END)  as benefit_weeks,
                         sum (CASE WHEN tot_reglr_reptd_incm_amt > 0 THEN 0 ELSE 1 END)  as zero_earnings_weeks
                         FROM nrd.aggr_claimt_benf
                         WHERE cert_perd_end_dt between ", date_range,
                         " GROUP BY claimt_id, cert_perd_end_qtr_num;"))
  check_classes_match(sql_QWR)
  dimension(sql_QWR)



  #used for changing_ip module
  sql_changing_ip_fn <- function(){
    RJDBC::dbSendUpdate(db_conn$conn, paste0("DROP TABLE IF EXISTS tmp.ip_change_temp_rolled_up;
                 CREATE TABLE tmp.ip_change_temp_rolled_up AS
                 SELECT
                 claimt_id,
                 COUNT(DISTINCT orig_ip_net_val) as num_unique_ips,
                 COUNT(orig_ip_net_val) as num_of_filings
                 FROM nrd.aggr_claimt_sessn
                 WHERE bus_perd_end_dt between ", date_range,
                 " GROUP BY claimt_id
                 HAVING COUNT(DISTINCT orig_ip_net_val) >=
                 (SELECT CAST((SELECT parm_val
                 FROM ref.parm
                 WHERE parm_cd = 'IP_CHANGE_MIN_NUM') AS integer)); --getting ip_min_num;


                 DROP TABLE IF EXISTS tmp.ip_change_temp_score_precursor;
                 CREATE TABLE tmp.ip_change_temp_score_precursor AS
                 SELECT *,
                 (t1.logged_ips - avg(t1.logged_ips) over()) / stddev(t1.logged_ips) over() as scaled_ips
                 FROM (SELECT *,
                 CAST(num_unique_ips as real) / num_of_filings * 100 as perc_unique,
                 LN(num_unique_ips) as logged_ips
                 FROM tmp.ip_change_temp_rolled_up) t1;


                 DROP TABLE IF EXISTS tmp.ip_change_precursor_claimant_changing_ips;
                 CREATE TABLE tmp.ip_change_precursor_claimant_changing_ips AS
                 SELECT
                 t1.claimt_id,
                 t1.num_unique_ips,
                 t1.num_of_filings,
                 t1.perc_unique,
                 ntile(100) over (order by t1.score) as final_score
                 FROM (SELECT *,
                 scaled_ips * perc_unique as score
                 FROM tmp.ip_change_temp_score_precursor) t1;"))


    x <-  RJDBC::dbGetQuery(db_conn$conn, "SELECT DISTINCT
                     t2.claimt_id,
                     t1.bus_perd_end_dt,
                     t2.num_unique_ips,
                     t2.num_of_filings,
                     t2.perc_unique,
                     t2.final_score
                     FROM nrd.aggr_claimt_sessn t1
                     JOIN tmp.ip_change_precursor_claimant_changing_ips t2
                     ON t1.claimt_id = t2.claimt_id
                     WHERE t1.bus_perd_end_dt = (SELECT MAX(bus_perd_end_dt) FROM nrd.aggr_claimt_sessn);")

    x <- x[, names(x) %in% c("num_unique_ips", "num_of_filings", "perc_unique")]

    return(x)

    RJDBC::dbSendUpdate(db_conn$conn, "DROP TABLE IF EXISTS tmp.ip_change_precursor_claimant_changing_ips2;
                 DROP TABLE IF EXISTS tmp.ip_change_precursor_claimant_changing_ips;
                 DROP TABLE IF EXISTS tmp.ip_change_temp_score_precursor;
                 DROP TABLE IF EXISTS tmp.ip_change_temp_rolled_up;")

  }

  sql_changing_ip <<- sql_changing_ip_fn()
  check_classes_match(sql_changing_ip)
  dimension(sql_changing_ip)



  #used for clmt_ip_location module
  sql_clmt_ip_location_fn <- function() {

    RJDBC::dbSendUpdate(db_conn$conn,paste0("DROP TABLE IF EXISTS tmp.ip_location_ip_subset;
                 CREATE TABLE tmp.ip_location_ip_subset AS
                 SELECT *
                 FROM aggr_claimt_sessn
                 WHERE bus_perd_end_dt between ",date_range," AND ((NOT claimt_st_cd_array && ARRAY['AP', 'AE', 'AA']::varchar[]) OR claimt_st_cd_array IS NULL);


                 DROP TABLE IF EXISTS tmp.ip_location_ip_subset_agg;
                 CREATE TABLE tmp.ip_location_ip_subset_agg AS
                 SELECT
                 claimt_id,
                 bus_perd_end_dt,
                 MAX (CASE WHEN ip_outsd_cntry_flag AND ip_outsd_claimt_st_flag --Shouldn't flag US Territories
                 THEN 1
                 ELSE 0 END) as ip_outsd_cntry_and_st,
                 MAX (CASE WHEN ip_outsd_local_st_flag AND ip_outsd_claimt_st_flag
                 THEN 1
                 ELSE 0 END) as ip_outsd_st_and_listed_st,
                 MAX (CASE WHEN ip_anon_proxy_flag
                 THEN 1
                 ELSE 0 END) as ip_anon_proxy,
                 MAX (CASE WHEN ip_outsd_claimt_st_flag
                 THEN 1
                 ELSE 0 END) as ip_outsd_listed_st,
                 MAX (CASE WHEN claimt_st_cd_array IS NULL
                 THEN 1
                 ELSE 0 END) as no_st_on_record
                 FROM tmp.ip_location_ip_subset
                 WHERE bus_perd_end_dt = (SELECT MAX(bus_perd_end_dt) FROM tmp.ip_location_ip_subset)
                 GROUP BY claimt_id,
                 bus_perd_end_dt;"))

    x <- RJDBC::dbGetQuery(db_conn$conn, "SELECT t1.*
                    FROM (
                    SELECT
                    *,
                    CASE
                    WHEN ip_outsd_cntry_and_st = 1 OR ip_anon_proxy = 1 THEN 100
                    WHEN ip_outsd_st_and_listed_st = 1 THEN 50
                    WHEN ip_outsd_listed_st = 1 THEN 25
                    WHEN no_st_on_record = 1 THEN 10
                    ELSE 0
                    END AS final_score
                    FROM tmp.ip_location_ip_subset_agg) t1
                    WHERE t1.final_score != 0;")

    x <- x[, names(x) %in% c("ip_outsd_cntry_and_st", "ip_outsd_st_and_listed_st",
                             "ip_anon_proxy", "ip_outsd_listed_st",
                             "no_st_on_record")]

    x <- data.frame(x)
    for(i in 1:length(x)){
      x[,i] <- as.character(x[,i])
    }

    return(x)

    RJDBC::dbSendUpdate(db_conn$conn,"DROP TABLE IF EXISTS tmp.ip_location_ip_subset,
                 tmp.ip_location_ip_subset_agg,
                 tmp.ip_location_claimant_location_score;")
  }

  sql_clmt_ip_location <<- sql_clmt_ip_location_fn()
  check_classes_match(sql_clmt_ip_location)
  dimension(sql_clmt_ip_location)



  #used for sql_clmt_ip_location module
  sql_clmt_sharing_ips_with_employer_fn <-   function(){
    RJDBC::dbSendUpdate(db_conn$conn, paste0("DROP TABLE IF EXISTS tmp.emp_share_ip_merged_emps_and_claimants;

               CREATE TABLE tmp.emp_share_ip_merged_emps_and_claimants AS
                SELECT
                t1.claimt_id,
                t1.orig_ip_addr,
                t1.claimt_sessn_tmstmp,
                t1.bus_perd_end_dt,
                CASE WHEN SUM(CASE WHEN t2.emplr_id IS NULL THEN 0 ELSE 1 END) > 0
                THEN 1
                ELSE 0 END AS shares_w_emp
                FROM (SELECT * FROM nrd.aggr_claimt_sessn WHERE bus_perd_end_dt between ", date_range,") t1
                LEFT JOIN nrd.aggr_emplr_sessn t2
                ON t1.orig_ip_addr = t2.orig_ip_addr
                GROUP BY t1.claimt_id,
                t1.orig_ip_addr,
                t1.claimt_sessn_tmstmp,
                t1.bus_perd_end_dt;


                 DROP TABLE IF EXISTS tmp.emp_share_ip_ips_shared_per_week_claimant;

                 CREATE TABLE tmp.emp_share_ip_ips_shared_per_week_claimant AS
                 SELECT DISTINCT
                 t1.claimt_id,
                 t1.bus_perd_end_dt,
                 t2.total_emp_shared_ct,
                 t2.tot_claims_ct,
                 t2.total_emp_shared_ct::float / t2.tot_claims_ct * 100 as perc_shared_w_emp
                 FROM
                 tmp.emp_share_ip_merged_emps_and_claimants t1
                 JOIN
                 --Getting the total number of claims and times claims were shared with emps in the 3-month period
                 (SELECT
                 claimt_id,
                 SUM(shares_w_emp) as total_emp_shared_ct,
                 COUNT(1) AS tot_claims_ct
                 FROM tmp.emp_share_ip_merged_emps_and_claimants
                 GROUP BY
                 claimt_id) t2
                 ON t1.claimt_id = t2.claimt_id
                 --Ensuring that we only keep the most recent bus_perd_end_dt and only give scores to those who have
                 --shared with an employer at least once
                 WHERE t1.bus_perd_end_dt = (SELECT MAX(bus_perd_end_dt) FROM tmp.emp_share_ip_merged_emps_and_claimants)
                 AND
                 t2.total_emp_shared_ct > 0;

                 DROP TABLE IF EXISTS tmp.emp_share_ips_sharing_with_emps_scoring;
                 CREATE TABLE tmp.emp_share_ips_sharing_with_emps_scoring AS
                 SELECT
                 *,
                 ROUND(percent_rank() over (order by total_emp_shared_ct) * 100) AS tot_emp_share_score,
                 ROUND(percent_rank() over (order by perc_shared_w_emp) * 100) AS perc_emp_share_score
                 FROM tmp.emp_share_ip_ips_shared_per_week_claimant;


                 DROP TABLE IF EXISTS tmp.ceiling_to_100;
                 CREATE TABLE tmp.ceiling_to_100 AS
                 SELECT
                 claimt_id,
                 bus_perd_end_dt,
                 total_emp_shared_ct,
                 tot_claims_ct,
                 perc_shared_w_emp,
                 tot_emp_share_score + (100 - (SELECT MAX(tot_emp_share_score) FROM tmp.emp_share_ips_sharing_with_emps_scoring)) AS tot_emp_share_score,
                 perc_emp_share_score + (100 - (SELECT MAX(perc_emp_share_score) FROM tmp.emp_share_ips_sharing_with_emps_scoring)) AS perc_emp_share_score
                 FROM 	tmp.emp_share_ips_sharing_with_emps_scoring;"))


    x <-RJDBC::dbGetQuery(db_conn$conn, "SELECT
                   t1.*,
                   ROUND(t1.perc_emp_share_score * 0.5 + t1.tot_emp_share_score * 0.5) AS final_score
                   FROM
                   (SELECT
                   claimt_id,
                   bus_perd_end_dt,
                   total_emp_shared_ct,
                   tot_claims_ct,
                   perc_shared_w_emp,
                   CASE WHEN perc_emp_share_score = 0
                   THEN (SELECT MIN(perc_emp_share_score)
                   FROM tmp.emp_share_ips_sharing_with_emps_scoring
                   WHERE perc_emp_share_score <> 0) / 2
                   ELSE perc_emp_share_score END AS perc_emp_share_score, --taking next lowest score and dividing by 2
                   CASE WHEN tot_emp_share_score = 0
                   THEN (SELECT MIN(tot_emp_share_score)
                   FROM tmp.emp_share_ips_sharing_with_emps_scoring
                   WHERE tot_emp_share_score <> 0) / 2
                   ELSE tot_emp_share_score END AS tot_emp_share_score
                   FROM tmp.ceiling_to_100) t1;")

    x <- x[,names(x) %in% c("total_emp_shared_ct", "tot_claims_ct",
                            "perc_shared_w_emp", "perc_emp_share_score",
                            "tot_emp_share_score")]


    RJDBC::dbSendUpdate(db_conn$conn, "DROP TABLE IF EXISTS tmp.emp_share_ip_merged_emps_and_claimants,
                 tmp.emp_share_ip_ips_shared_per_week_claimant,
                 tmp.emp_share_ips_sharing_with_emps_scoring,
                 tmp.ceiling_to_100,
                 tmp.emp_share_ips_sharing_with_emps_scoring2;")

    return(x)
  }
  sql_clmt_sharing_ips_with_employer <<- sql_clmt_sharing_ips_with_employer_fn()
  check_classes_match(sql_clmt_sharing_ips_with_employer)
  dimension(sql_clmt_sharing_ips_with_employer)



  #used for sql_sharing_ip module
  sql_sharing_ip_fn <- function(){

    RJDBC::dbSendUpdate(db_conn$conn, paste0("DROP TABLE IF EXISTS tmp.ip_share_claimantdiffs;
                 CREATE TABLE tmp.ip_share_claimantdiffs AS
                 SELECT
                 c.orig_ip_addr,
                 c.bus_perd_end_dt,
                 c.claimt_id as firstclaimant,
                 s.claimt_id as secondclaimant,
                 ABS(date_part('epoch',(c.claimt_sessn_tmstmp - s.claimt_sessn_tmstmp))) as diffseconds
                 FROM (SELECT * FROM aggr_claimt_sessn WHERE bus_perd_end_dt between ", date_range,") as c

                 JOIN (SELECT * FROM aggr_claimt_sessn WHERE bus_perd_end_dt between ", date_range,") as s
                 ON
                 s.orig_ip_addr = c.orig_ip_addr
                 AND s.bus_perd_end_dt = c.bus_perd_end_dt
                 AND s.claimt_id <> c.claimt_id;

                 DROP TABLE IF EXISTS tmp.ip_share_claimantweek;
                 CREATE TABLE tmp.ip_share_claimantweek AS
                 SELECT
                 firstclaimant,
                 bus_perd_end_dt,
                 MIN(diffseconds) as min_seconds
                 FROM tmp.ip_share_claimantdiffs
                 GROUP BY firstclaimant, bus_perd_end_dt;

                 DROP TABLE IF EXISTS tmp.ip_share_claimantweek2;
                 CREATE TABLE tmp.ip_share_claimantweek2 AS
                 SELECT
                 t1.firstclaimant,
                 array_agg(DISTINCT t1.secondclaimant) as matched_claimants,
                 array_agg(DISTINCT t1.orig_ip_addr) as shared_ips,
                 t1.bus_perd_end_dt,
                 t1.diffseconds
                 FROM tmp.ip_share_claimantdiffs t1
                 JOIN tmp.ip_share_claimantweek t2
                 ON
                 t1.firstclaimant = t2.firstclaimant AND
                 t1.bus_perd_end_dt = t2.bus_perd_end_dt AND
                 t1.diffseconds = t2.min_seconds
                 GROUP BY t1.firstclaimant, t1.bus_perd_end_dt, t1.diffseconds;

                 DROP TABLE IF EXISTS tmp.ip_share_all_users1;
                 CREATE TABLE tmp.ip_share_all_users1 AS
                 SELECT
                 t1.bus_perd_end_dt,
                 t1.orig_ip_addr,
                 COUNT(DISTINCT(t2.claimt_id)) as prev_unique_users
                 FROM aggr_claimt_sessn t1
                 LEFT JOIN (
                 SELECT
                 claimt_sessn_tmstmp,
                 orig_ip_addr,
                 claimt_id
                 FROM aggr_claimt_sessn ) t2
                 ON
                 t2.orig_ip_addr = t1.orig_ip_addr AND
                 t2.claimt_sessn_tmstmp <= t1.claimt_sessn_tmstmp
                 GROUP BY t1.bus_perd_end_dt, t1.orig_ip_addr
                 HAVING COUNT(DISTINCT(t2.claimt_id)) >= (SELECT CAST((SELECT parm_val
                 FROM ref.parm
                 WHERE parm_cd = 'IP_MIN_USERS') AS INTEGER));

                 DROP TABLE IF EXISTS tmp.ip_share_claimantweek3;
                 CREATE TABLE tmp.ip_share_claimantweek3 AS
                 SELECT
                 t1.firstclaimant as claimt_id,
                 t1.matched_claimants,
                 t1.shared_ips[1] as shared_ip,
                 t1.bus_perd_end_dt,
                 t1.diffseconds,
                 t2.prev_unique_users
                 FROM tmp.ip_share_claimantweek2 t1
                 LEFT JOIN tmp.ip_share_all_users1 t2
                 ON
                 t1.shared_ips[1] = t2.orig_ip_addr AND
                 t1.bus_perd_end_dt = t2.bus_perd_end_dt;

                 DROP TABLE IF EXISTS tmp.ip_share_claimantweek4;
                 CREATE TABLE tmp.ip_share_claimantweek4 AS
                 SELECT
                 t1.*,
                 t2.unique_users_in_week
                 FROM tmp.ip_share_claimantweek3 t1
                 LEFT JOIN
                 (SELECT
                 orig_ip_addr,
                 bus_perd_end_dt,
                 COUNT(DISTINCT claimt_id) as unique_users_in_week
                 FROM aggr_claimt_sessn
                 GROUP BY orig_ip_addr, bus_perd_end_dt) t2
                 ON
                 t1.shared_ip = t2.orig_ip_addr AND
                 t1.bus_perd_end_dt = t2.bus_perd_end_dt;

                 DROP TABLE IF EXISTS tmp.ip_share_claimantweek5;
                 CREATE TABLE tmp.ip_share_claimantweek5 AS
                 SELECT
                 t1.*,
                 t2.ip_outsd_local_st_flag,
                 t2.ip_outsd_cntry_flag
                 FROM tmp.ip_share_claimantweek4 t1
                 LEFT JOIN (
                 SELECT
                 DISTINCT claimt_id,
                 orig_ip_addr,
                 ip_outsd_local_st_flag,
                 ip_outsd_cntry_flag
                 FROM aggr_claimt_sessn) t2
                 ON
                 t1.claimt_id = t2.claimt_id AND
                 t1.shared_ip = t2.orig_ip_addr;

                 DROP TABLE IF EXISTS tmp.ip_share_claimantweek6;
                 CREATE TABLE tmp.ip_share_claimantweek6 AS
                 SELECT
                 *,
                 NTILE(20) OVER (ORDER BY unique_users_in_week) * 5 as weekly_users_score,
                 NTILE(20) OVER (ORDER BY prev_unique_users) * 5 as all_time_users_score,
                 ROUND(EXP(diffseconds * -1 *
                 (SELECT CAST((SELECT parm_val
                 FROM ref.parm
                 WHERE parm_cd = 'IP_DECAY_FACTOR') AS FLOAT))) --decay_factor (chosen such that a score of 100 is received up to 15 minutes in between submissions)
                 * 100) as time_between_submissions_score
                 FROM tmp.ip_share_claimantweek5;"))

    x <- RJDBC::dbGetQuery(db_conn$conn, "SELECT
                    t1.diffseconds,
                    t1.prev_unique_users,
                    t1.unique_users_in_week,
                    t1.weekly_users_score,
                    t1.all_time_users_score,
                    t1.time_between_submissions_score
                    FROM (
                    SELECT
                    *,
                    CASE WHEN ip_outsd_local_st_flag IS TRUE AND ip_outsd_local_st_flag IS NOT NULL THEN
                    weekly_users_score * 0.4 + time_between_submissions_score * 0.4 + all_time_users_score * 0.2 + 20
                    ELSE
                    weekly_users_score * 0.4 + time_between_submissions_score * 0.4 + all_time_users_score * 0.2
                    END AS adj_final_score
                    FROM tmp.ip_share_claimantweek6
                    WHERE bus_perd_end_dt = (SELECT MAX(bus_perd_end_dt) FROM tmp.ip_share_claimantdiffs)) t1;")

    return(x)

    RJDBC::dbSendUpdate(db_conn$conn, "DROP TABLE IF EXISTS tmp.precursor_claimant_sharing_ips;
                 DROP TABLE IF EXISTS tmp.ip_share_claimantweek6;
                 DROP TABLE IF EXISTS tmp.ip_share_claimantweek5;
                 DROP TABLE IF EXISTS tmp.ip_share_claimantweek4;
                 DROP TABLE IF EXISTS tmp.ip_share_claimantweek3;
                 DROP TABLE IF EXISTS tmp.ip_share_all_users1;
                 DROP TABLE IF EXISTS tmp.ip_share_claimantweek2;
                 DROP TABLE IF EXISTS tmp.ip_share_claimantweek;
                 DROP TABLE IF EXISTS tmp.ip_share_claimantdiffs;")

  }
  sql_sharing_ip <<- sql_sharing_ip_fn()
  check_classes_match(sql_sharing_ip)
  dimension(sql_sharing_ip)


  cat("\n")
  cat("\n")
  cat("Done pulling data")
  cat("\n")
  cat("\n")


  check_var_range(aggr_claimt_benf, sql_sharing_ip, sql_clmt_sharing_ips_with_employer,
                  sql_changing_ip, sql_QWR, sql_new_hire, aggr_claimt_ovpaymt)
  cat("\n")
  cat("\n")

  }

#Contagion is left out because run time is so long







