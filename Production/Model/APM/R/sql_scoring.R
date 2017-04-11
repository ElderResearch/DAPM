#' Changing IP Module
#'
#' This module reads in the IP base table, determines which users have changed
#' IPs significantly over a certain time period, and returns a table with risk
#' scores. The final risk score is represented by the percentile to which
#' claimant belongs in terms of a score that weights
#'   (1) The number of unique 24-bit IPs a claimant has had in the time period
#'       of interest
#'   (2) The percentage of time a claimant has used a unique 24-bit IP address
#'       during the time period of interest
#'
## The following parameters are pulled from a parameter table to run this model:
##   - ip_min_num: The minimum number of ip changes (at the 24-bit level) to
##     have to be considered for scoring
##   - ${APM_CYCL_DT}: Kettle date insert (can also be set via function arg)
#'
#' @param cycle_date The APM cycle date.
#' @param db_schema  The database schema to write scores and related tables to.
#'                   Default: "nrd".
#' @param exclude_cellular Whether or not the module should exclude
#'        records identified as originating from cellular IPs.
#'
#' @return An integer status code, like other models.
#'
#' @examples
#' \dontrun{
#' rc <- changing_ip_score()
#' }
#'
#' @export
changing_ip_score <- function(cycle_date       = NULL,
                              db_schema        = "nrd",
                              exclude_cellular = FALSE) {

  # Basic APM init stuff
  if (initialize_apm("Changing IP Scoring") != 0)
    return(1)

  output_message("Start", doMemory=TRUE, clear=TRUE)

  if (is.null(cycle_date))
    cycle_date <- APM_CYCL_DT

  db <- initialize_db()

  if ("error" %in% class(db))
    return(2)


  # Optionally exclude CEL IP sessions by injecting SQL code
  sql_cel_inject <- ""

  if (exclude_cellular) {
    output_message("Excluding CEL sessions")
    sql_cel_inject <- "WHERE (ip_conn_type_cd != 'CEL' or
                              ip_conn_type_cd IS NULL)"
  }


  output_message("Making TMP tables")

  # Roll up to the claimt_id level and count the number of distinct ips used by
  # each claimant based on the first 24 bits of the IP.  Also, count total
  # claims filed by the claimant and only keep records that have at least
  # 'ip_min_num' number of unique 24-bit IP addresses
  sql <- sprintf("
    DROP TABLE IF EXISTS tmp.ip_change_temp_rolled_up;

    CREATE TABLE tmp.ip_change_temp_rolled_up AS
      SELECT
        claimt_id,
        COUNT(DISTINCT orig_ip_net_val) as num_unique_ips,
        COUNT(orig_ip_net_val) as num_of_filings
      FROM nrd.aggr_claimt_sessn
      %s
      GROUP BY claimt_id
      HAVING COUNT(DISTINCT orig_ip_net_val) >=
         --getting ip_min_num;
        (SELECT CAST((SELECT parm_val
         FROM ref.parm
         WHERE parm_cd = 'IP_CHANGE_MIN_NUM') AS integer));", sql_cel_inject)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(3)

  # Calculating:
  # - perc_unique: The percentage of claims filed that are unique at the 24-bit
  #   level of the IP addresses
  # - scaled_ips: The z-score of the log of the number of unique IP addresses
  #   at the 24-bit level
  sql <- "
    DROP TABLE IF EXISTS tmp.ip_change_temp_score_precursor;

    CREATE TABLE tmp.ip_change_temp_score_precursor AS
      SELECT *,
        (t1.logged_ips - avg(t1.logged_ips) over()) / stddev(t1.logged_ips) over() as scaled_ips
      FROM (
        SELECT *,
          CAST(num_unique_ips as real) / num_of_filings * 100 as perc_unique,
          LN(num_unique_ips) as logged_ips
        FROM tmp.ip_change_temp_rolled_up) t1;"

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(4)

  # Creating the for claiamnts by doing the following:
  # 1. Multiplying scaled_ips by perc_unique in order to combine the number of
  #    unique in order to combine the raw number of times a claimant has had a
  #    unique IP with the frequency with which this occurs.
  # 2. Calculate the percentile in which each score falls (final_score)
  #
  # Note: In the next step, we subset down to only keep claimants that filed in
  # the current week. However, we're the score for these claimants is
  # calculated relative to all claimants in present in the data set for the
  # time period of interest.
  sql <- "
    DROP TABLE IF EXISTS tmp.ip_change_precursor_claimant_changing_ips;

    CREATE TABLE tmp.ip_change_precursor_claimant_changing_ips AS
      SELECT
        t1.claimt_id,
        t1.num_unique_ips,
        t1.num_of_filings,
        t1.perc_unique,
        ntile(100) over (order by t1.score) as final_score
      FROM (
        SELECT *,
        scaled_ips * perc_unique as score
        FROM tmp.ip_change_temp_score_precursor) t1;"

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(5)

  # Only keeping scores belonging to claimants who have logged-in in the
  # current week.
  sql <- "
    DROP TABLE IF EXISTS tmp.ip_change_precursor_claimant_changing_ips2;

    CREATE TABLE tmp.ip_change_precursor_claimant_changing_ips2 AS
      SELECT DISTINCT
        t2.claimt_id,
        t1.bus_perd_end_dt,
        t2.num_unique_ips,
        t2.num_of_filings,
        t2.perc_unique,
        t2.final_score
      FROM nrd.aggr_claimt_sessn t1
      JOIN tmp.ip_change_precursor_claimant_changing_ips t2
      ON t1.claimt_id = t2.claimt_id
      WHERE t1.bus_perd_end_dt = (
        SELECT MAX(bus_perd_end_dt) FROM nrd.aggr_claimt_sessn)"

  if (exclude_cellular) {
    sql <- paste(sql, "AND (t1.ip_conn_type_cd != 'CEL'
                         OR t1.ip_conn_type_cd IS NULL)")
  }

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(6)


  output_message("Making NRDB tables")
  output_message(sprintf("Using schema: %s", db_schema))

  # Insert supplementary score data into the attribute table for changing_ip
  # scores
  sql <- sprintf("DELETE FROM %s.ipa_chip_ds WHERE cycl_dt = '%s'::date;",
            db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(7)

  sql <- sprintf("
    INSERT INTO %s.ipa_chip_ds
      (cycl_dt, claimt_id, uniq_ip_addr_cnt, uniq_sessn_cnt, uniq_pct, crt_tmstmp)
    SELECT
      '%s'::date,
      claimt_id,
      num_unique_ips,
      num_of_filings,
      perc_unique,
      CURRENT_TIMESTAMP
    FROM tmp.ip_change_precursor_claimant_changing_ips2;",
    db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(8)

  # Inserting final scores into the entity score table for the week of interest.
  sql <- sprintf("
    DELETE FROM %s.enty_score
    WHERE cycl_dt = '%s'::date
    AND score_cd = 'IPA_CHIP';",
    db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(9)

  # E.g. http://stackoverflow.com/a/19509764/656740
  sql <- sprintf("
    INSERT INTO %s.enty_score
      (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)
    SELECT
      '%s'::date,
      'CLAIMT',
      claimt_id,
      'IPA_CHIP',
      final_score,
      CURRENT_TIMESTAMP
    FROM tmp.ip_change_precursor_claimant_changing_ips2",
    db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(10)

  # Drop all temporary tables that were created in the process
  sql <- "
    DROP TABLE IF EXISTS tmp.ip_change_precursor_claimant_changing_ips2;
    DROP TABLE IF EXISTS tmp.ip_change_precursor_claimant_changing_ips;
    DROP TABLE IF EXISTS tmp.ip_change_temp_score_precursor;
    DROP TABLE IF EXISTS tmp.ip_change_temp_rolled_up;"

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(11)

  close_db(db)
  return(0)
}


#' IP Location Score
#'
#' This module reads in the ip base table, looks at the ip location flags and
#' assigns a location score for each claim using the following methodology:
#'
#' - If someone filed a claim from a different state/province than was listed
#'   in their file and they're outside US OR they used an anonymous proxy (at.
#'   least once in the past "x" days): 100
#' - If someone filed a claim from a different state/province than was listed
#'   in their file and they're inside the US but outside the the state and
#'   neighboring states (at least once in the past "x" days): 50
#' - If someone filed a claim from a different state/province than was listed
#'   in their file and they're inside the US AND in the local area (all the time
#'   in the past "x" days): 25
#' - If no state of record is associated with claimant: 10
#' - If they've always filed from the same state as was listed in their file
#'   (in the past "x" days): 0 (no scores of 0 written to final score table)
#'
#' Assumption(s):
#' - Those serving in the armed forces abroad (state code = 'AE' or 'AP') are
#'   excluded
#'
#' The following parameters are pulled from a parameter table to run this model:
#' - ${APM_CYCL_DT}: Kettle date insert (also set via function arg)
#'
#' @param cycle_date The APM cycle date.
#' @param db_schema  The database schema to write scores and related tables to.
#'                   Default: "nrd".
#' @param drop_tmp_tables Remove created TMP tables.
#' @param exclude_cellular Whether or not the module should exclude
#'        records identified as originating from cellular IPs.
#'
#' @return An integer status code, like other models.
#'
#' @examples
#' \dontrun{
#' rc <- ip_location_score()
#' }
#'
#' @export
ip_location_score <- function(cycle_date       = NULL,
                              db_schema        = "nrd",
                              drop_tmp_tables  = TRUE,
                              exclude_cellular = FALSE) {
  # Basic APM init stuff
  if (initialize_apm("IP Location Scoring") != 0)
    return(1)

  output_message("Start", doMemory=TRUE, clear=TRUE)

  if (is.null(cycle_date))
    cycle_date <- APM_CYCL_DT

  db <- initialize_db()

  if ("error" %in% class(db))
    return(2)


  # The correct business period is the one that ended just before the cycle date
  bus_perd_end_dt <- as.character(RJDBC::dbGetQuery(db$conn,
                        sprintf("select max(bus_perd_end_dt)
                                 from aggr_claimt_sessn
                                 where bus_perd_end_dt <= '%s'", cycle_date)))

  # Print a few statstics on this cycle
  print(paste("cycle_date =", cycle_date))
  print(paste("bus_perd_end_dt =", bus_perd_end_dt))


  output_message("Making TMP tables")

  # Get all the claims for the relevant time period and subset down to exclude
  # those serving abroad.
  sql <- paste0("
    DROP TABLE IF EXISTS tmp.ip_location_ip_subset;

    CREATE TABLE tmp.ip_location_ip_subset AS
      SELECT *
      FROM aggr_claimt_sessn
      WHERE ((NOT claimt_st_cd_array && ARRAY['AP', 'AE', 'AA']::varchar[])
             OR claimt_st_cd_array IS NULL)
      AND bus_perd_end_dt = '", bus_perd_end_dt, "'")

  # Optionally exclude CEL IP sessions
  if (exclude_cellular) {
    output_message("Excluding CEL sessions")

    sql <- paste(sql, "and (ip_conn_type_cd != 'CEL'
                         or ip_conn_type_cd is null)")
  }

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(3)

  # Group by claimt_id and bus_perd_end_dt (only for the business period
  # defined earlier) and calculate binary flags indicating:
  # - whether a matched transaction occurred outside the US
  #   (ip_outsd_cntry_and_st)
  # - whether a matched transaction occurred outside local states
  #   (ip_outsd_st_and_listed_st)
  # - whether an IP belongs to a know anonymous proxy server
  #   (ip_anon_proxy_flag)
  # - whether a matched transaction occurred in a state not listed as
  #   regular/resident state/province (ip_outsd_claimt_st_flag)
  # - whether a claimant has any state listed on record (no_st_on_record)
  sql <- "
    DROP TABLE IF EXISTS tmp.ip_location_ip_subset_agg;

    CREATE TABLE tmp.ip_location_ip_subset_agg AS
      SELECT
        claimt_id,
        bus_perd_end_dt,
        --Shouldn't flag US Territories
        MAX (CASE WHEN ip_outsd_cntry_flag AND ip_outsd_claimt_st_flag
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
    GROUP BY claimt_id, bus_perd_end_dt;"

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(4)

  # Create the final score table and get rid of anyone with a score of 0
  sql <- "
    DROP TABLE IF EXISTS tmp.ip_location_claimant_location_score;

    CREATE TABLE tmp.ip_location_claimant_location_score AS
      SELECT t1.*
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
      WHERE t1.final_score != 0;"

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(5)


  output_message("Making NRD tables")
  output_message(sprintf("Using schema: %s", db_schema))

  # Write attributes to location attributes table
  sql <- sprintf("DELETE FROM %s.ipa_clr_ds WHERE cycl_dt = '%s'::date;",
            db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(6)

  sql <- sprintf("
    INSERT INTO %s.ipa_clr_ds
      (cycl_dt, claimt_id, ip_anon_proxy_flag, ip_outsd_cntry_claimt_st_flag,
       ip_outsd_local_claimt_st_flag, ip_outsd_claimt_st_flag,
       missg_claimt_st_flag, crt_tmstmp)
    SELECT
      '%s'::date,
      claimt_id,
      CASE WHEN ip_anon_proxy = 1 THEN TRUE ELSE FALSE END,
      CASE WHEN ip_outsd_cntry_and_st = 1 THEN TRUE ELSE FALSE END,
      CASE WHEN ip_outsd_st_and_listed_st = 1 THEN TRUE ELSE FALSE END,
      CASE WHEN ip_outsd_listed_st = 1 THEN TRUE ELSE FALSE END,
      CASE WHEN no_st_on_record = 1 THEN TRUE ELSE FALSE END,
      CURRENT_TIMESTAMP
    FROM tmp.ip_location_claimant_location_score;",
    db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(7)

  # Inserting final scores into the entity score table for the week of interest.
  sql <- sprintf("
    DELETE FROM %s.enty_score
    WHERE cycl_dt = '%s'::date
    AND score_cd = 'IPA_CLR';",
    db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(8)

  sql <- sprintf("
    INSERT INTO %s.enty_score
      (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)
    SELECT
      '%s'::date,
      'CLAIMT',
      claimt_id,
      'IPA_CLR',
      final_score,
      CURRENT_TIMESTAMP AS crt_tmstmp
    FROM tmp.ip_location_claimant_location_score;",
    db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(9)

  # Drop all temporary tables created in the process
  if (drop_tmp_tables) {
    sql <- "
      DROP TABLE IF EXISTS
        tmp.ip_location_ip_subset,
        tmp.ip_location_ip_subset_agg,
        tmp.ip_location_claimant_location_score;"

    result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
    if ("error" %in% class(result))
      return(10)
  }

  close_db(db)
  return(0)
}


#' Sharing IPs with employers
#'
#' This module reads in the ip base table and determines which users have IPs
#' that have been shared with employers' IP addresses used when filing their
#' Quarterly Wage Reports.  It assigns a risk score based on the total number of
#' times that sharing has occurred and the percentage of time a claimant has
#' shared with an employer's IP. The final score is an equally weighted
#' combination of these two factors.  One thing to note is that a claimant can
#' legitimately file multiple claims in a single week in order to claim money
#' for previous weeks. If they use the same IP which matches with a single
#' employer, the claimant will be counted as sharing the ip 5x. This was left
#' as-is because claiming multiple weeks at once can be considered slightly
#' suspicious behavior, thus allowing the count to reflect that suspicion.
#'
#' The following parameters are pulled from a parameter table to run this model:
#' - ${APM_CYCL_DT}: Kettle date insert (or passed as function arg)
#'
#' @param cycle_date The APM cycle date.
#' @param db_schema  The database schema to write scores and related tables to.
#'                   Default: "nrd".
#' @param exclude_cellular Whether or not the module should exclude
#'        records identified as originating from cellular IPs.
#'
#' @return An integer status code, like other models.
#'
#' @examples
#' \dontrun{
#' rc <- sharing_ips_with_employer_score()
#' }
#'
#' @export
sharing_ips_with_employer_score <- function(cycle_date       = NULL,
                                            db_schema        = "nrd",
                                            exclude_cellular = FALSE) {

  # Basic APM init stuff
  if (initialize_apm("Claimant + Employer IP Sharing Scoring") != 0)
    return(1)

  output_message("Start", doMemory=TRUE, clear=TRUE)

  if (is.null(cycle_date))
    cycle_date <- APM_CYCL_DT

  db <- initialize_db()

  if ("error" %in% class(db))
    return(2)


  # Cut out CEL IP accesses if requested
  sql_cel_inject <- ""

  if (exclude_cellular) {
    output_message("Excluding CEL sessions")
    sql_cel_inject <- "WHERE (t1.ip_conn_type_cd != 'CEL'
                           OR t1.ip_conn_type_cd IS NULL)"
  }


  output_message("Making TMP tables")

  # Join the employer IP activity onto the claimant ip activity, adding a flag
  # to the original aggr_claimt_sessn table indicating whether or not the claim
  # shared an IP address with an employer in the past 3 months.
  sql <- sprintf("
    DROP TABLE IF EXISTS tmp.emp_share_ip_merged_emps_and_claimants;

    CREATE TABLE tmp.emp_share_ip_merged_emps_and_claimants AS
      SELECT
        t1.claimt_id,
        t1.orig_ip_addr,
        t1.claimt_sessn_tmstmp,
        t1.bus_perd_end_dt,
        CASE WHEN SUM(CASE WHEN t2.emplr_id IS NULL THEN 0 ELSE 1 END) > 0
        THEN 1
        ELSE 0 END AS shares_w_emp
      FROM nrd.aggr_claimt_sessn t1
      LEFT JOIN nrd.aggr_emplr_sessn t2
      ON t1.orig_ip_addr = t2.orig_ip_addr
      %s
      GROUP BY t1.claimt_id,
        t1.orig_ip_addr,
        t1.claimt_sessn_tmstmp,
        t1.bus_perd_end_dt;", sql_cel_inject)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(3)

  # Count the total number of times a claimant has shared an IP with an
  # employer in the past three months, count the total number of times a
  # claimant has filed in the past three months, calculate the percentage of
  # times the claimant has shared with an employer in the past three months.
  # This is all performed by looking at claimants that have filed within the
  # most recent bus_perd_end_dt. Only keep claimants that filed in the most
  # recent week.
  sql <- "
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
        -- Getting the total number of claims and times claims were shared with
        -- emps in the 3-month period
        (SELECT
           claimt_id,
           SUM(shares_w_emp) as total_emp_shared_ct,
           COUNT(1) AS tot_claims_ct
         FROM tmp.emp_share_ip_merged_emps_and_claimants
         GROUP BY
           claimt_id) t2
      ON t1.claimt_id = t2.claimt_id
      -- Ensuring that we only keep the most recent bus_perd_end_dt and only
      -- give scores to those who have shared with an employer at least once
      WHERE t1.bus_perd_end_dt = (
        SELECT MAX(bus_perd_end_dt)
        FROM tmp.emp_share_ip_merged_emps_and_claimants)
      AND
        t2.total_emp_shared_ct > 0;"

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(4)

  # Give a percent rank score to each observation for both total and percent
  # shared with employers
  sql <- "
    DROP TABLE IF EXISTS tmp.emp_share_ips_sharing_with_emps_scoring;

    CREATE TABLE tmp.emp_share_ips_sharing_with_emps_scoring AS
      SELECT
        *,
        ROUND(percent_rank() over (order by total_emp_shared_ct) * 100) AS tot_emp_share_score,
        ROUND(percent_rank() over (order by perc_shared_w_emp) * 100)   AS perc_emp_share_score
      FROM tmp.emp_share_ip_ips_shared_per_week_claimant;"

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(5)

  # Make the highest score for each of these be 100 to counteract the weirdness
  # that can happen with using percent_rank
  sql <- "
    DROP TABLE IF EXISTS tmp.ceiling_to_100;

    CREATE TABLE tmp.ceiling_to_100 AS
      SELECT
        claimt_id,
        bus_perd_end_dt,
        total_emp_shared_ct,
        tot_claims_ct,
        perc_shared_w_emp,
        tot_emp_share_score  + (100 - (SELECT MAX(tot_emp_share_score)  FROM tmp.emp_share_ips_sharing_with_emps_scoring)) AS tot_emp_share_score,
        perc_emp_share_score + (100 - (SELECT MAX(perc_emp_share_score) FROM tmp.emp_share_ips_sharing_with_emps_scoring)) AS perc_emp_share_score
      FROM tmp.emp_share_ips_sharing_with_emps_scoring;"

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(6)

  # Performing the following:
  # 1. Re-calibrate these two scores for individuals that only match up with
  #    one employer (because their percent rank is zero). This is accomplished
  #    by taking the next lowest percent rank score and cutting it in half.
  # 2. Combine the resulting two scores into a final score, and subset to only
  #    include the most recent bus_perd_end_dt
  sql <- "
    DROP TABLE IF EXISTS tmp.emp_share_ips_sharing_with_emps_scoring2;

    CREATE TABLE tmp.emp_share_ips_sharing_with_emps_scoring2 AS
      SELECT
        t1.*,
        ROUND(t1.perc_emp_share_score * 0.5 + t1.tot_emp_share_score * 0.5) AS final_score
      FROM
        (SELECT
          claimt_id,
          bus_perd_end_dt,
          total_emp_shared_ct,
          tot_claims_ct,
          perc_shared_w_emp,
          -- taking next lowest score and dividing by 2
          CASE WHEN perc_emp_share_score = 0
          THEN (
            SELECT MIN(perc_emp_share_score)
            FROM tmp.emp_share_ips_sharing_with_emps_scoring
            WHERE perc_emp_share_score <> 0) / 2
          ELSE perc_emp_share_score END AS perc_emp_share_score,
          CASE WHEN tot_emp_share_score = 0
          THEN (
            SELECT MIN(tot_emp_share_score)
            FROM tmp.emp_share_ips_sharing_with_emps_scoring
            WHERE tot_emp_share_score <> 0) / 2
          ELSE tot_emp_share_score END AS tot_emp_share_score
         FROM tmp.ceiling_to_100) t1;"

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(7)


  output_message("Making NRD tables")
  output_message(sprintf("Using schema: %s", db_schema))

  # Write attributes to employer attributes table
  sql <- sprintf("DELETE FROM %s.ipa_elr_ds WHERE cycl_dt = '%s'::date;",
            db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(8)

  sql <- sprintf("
    INSERT INTO %s.ipa_elr_ds
      SELECT
        '%s'::date,
        claimt_id,
        perc_shared_w_emp,
        total_emp_shared_ct,
        CURRENT_TIMESTAMP
      FROM tmp.emp_share_ips_sharing_with_emps_scoring2;",
      db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(9)

  # Insert sub-scores into scoring table
  sql <- sprintf("
    DELETE FROM %s.enty_score
    WHERE cycl_dt = '%s'::date
    AND score_cd IN ('IPA_ELR_PCT','IPA_ELR_TOT','IPA_ELR');",
    db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(10)

  # Inserting unique employers shared subscore
  sql <- sprintf("
    INSERT INTO %s.enty_score
      (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)
    SELECT
      '%s'::date,
      'CLAIMT',
      claimt_id,
      'IPA_ELR_PCT',
      perc_emp_share_score,
      CURRENT_TIMESTAMP
    FROM tmp.emp_share_ips_sharing_with_emps_scoring2;",
    db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(11)

  # Inserting total shares with employer subscore
  sql <- sprintf("
    INSERT INTO %s.enty_score
      (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)
    SELECT
      '%s'::date,
      'CLAIMT',
      claimt_id,
      'IPA_ELR_TOT',
      tot_emp_share_score,
      CURRENT_TIMESTAMP
    FROM tmp.emp_share_ips_sharing_with_emps_scoring2;",
    db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(12)

  # Inserting final score into scoring table
  sql <- sprintf("
    INSERT INTO %s.enty_score
      (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)
    SELECT
      '%s'::date,
      'CLAIMT',
      claimt_id,
      'IPA_ELR',
      final_score,
      CURRENT_TIMESTAMP
    FROM tmp.emp_share_ips_sharing_with_emps_scoring2;",
    db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(13)

  # Drop all temporary tables created in the process
  sql <- "
    DROP TABLE IF EXISTS
      tmp.emp_share_ip_merged_emps_and_claimants,
      tmp.emp_share_ip_ips_shared_per_week_claimant,
      tmp.emp_share_ips_sharing_with_emps_scoring,
      tmp.ceiling_to_100,
      tmp.emp_share_ips_sharing_with_emps_scoring2;"

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(14)

  close_db(db)
  return(0)
}


#' New Hire Scoring
#'
#' This function checks for un/under reported income from claimants. It reads
#' in the new hire table (NH_EMPLOYEE) and checks it against the list of
#' claimants who received benefits in the @time_window following their start
#' date. Not all claims will be fraudulent, since underemployment is still an
#' issue.
#'
#' A claimant's risk will decrease with respect to their reported income
#' relative to their total benefits. Since the amount a claimant can earn
#' before losing benefits changes state to state, a variable @wage_threshold
#' is included.
#'
#' Variables/Parameters:
#' - starts_after:        Gives lower bound to search window for new hire list
#' - time_window:         The length of time from start date of the new hire to
#'                        check for benefits being awarded.
#' - wage_threshold_low:  The fraction of WBA a claimant can earn before
#'                        benefits are reduced.
#' - wage_threshold_high: The fraction of WBA a claimant can earn before not
#'                        receiving benefits.
#'
#' \code{act_wage_threshold_high = total_wba * wage_threshold_high}
#' \code{act_wage_threshold_low  = total_wba * wage_threshold_low}
#'
#' Score:
#' \code{
#' if(claimed_earnings > act_wage_threshold_low):
#' 	 100 - 100*(claimed_earnings/act_wage_threshold_high)
#' else:
#' 	 100
#' }
#'
#' The following parameters are pulled from a parameter table to run this model:
#' - ${APM_CYCL_DT}: Kettle date insert (or passed as function arg)
#'
#' @param cycle_date The APM cycle date.
#' @param db_schema  The database schema to write scores and related tables to.
#'                   Default: "nrd".
#'
#' @return An integer status code, like other models.
#'
#' @examples
#' \dontrun{
#' rc <- new_hire_score()
#' }
#'
#' @export
new_hire_score <- function(cycle_date=NULL, db_schema="nrd") {
  # Basic APM init stuff
  if(initialize_apm("New Hire Scoring") != 0)
    return(1)

  output_message("Start", doMemory=TRUE, clear=TRUE)

  if(is.null(cycle_date))
    cycle_date <- APM_CYCL_DT

  db <- initialize_db()

  if("error" %in% class(db))
    return(2)


  output_message("Making NRD tables")
  output_message(sprintf("Using schema: %s", db_schema))

  # Creates the score table.
  sql <- sprintf("
    DELETE FROM %s.enty_score
    WHERE cycl_dt = '%s'::date
    AND score_cd = 'NHI_NHR';",
    db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(3)

  sql <- sprintf("
    INSERT INTO %s.enty_score
      (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)
    SELECT
	    '%s'::date,
	    'CLAIMT',
	    claimt_id,
	    'NHI_NHR',
	    CASE WHEN max_reptd_incm_amt > min_incm_threshld_amt
      THEN GREATEST(ROUND((100-100*(max_reptd_incm_amt/(max_incm_threshld_amt+0.01))),0),0)
      ELSE 100 END,
      CURRENT_TIMESTAMP
    FROM aggr_claimt_new_hire;",
    db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(4)

  close_db(db)
  return(0)
}


#' QWR2 Score
#'
#' Create the QWR+ score.
#' 04 Aug 2015
#'
#' This metric is aimed at folks who continue to collect benefits once they
#' start a new job, or otherwise are double-dipping.
#'
#' It gives no score for a quarter in which a person is not found in the QWR reports.
#'
#' The f1 and f2 components are looking for a large gap between (employer-reported) wages
#' and (claimant-reported) earnings: both in dollar terms and also as a percentage.
#'
#' The f3 and f4 components are looking for claimants who claimed for a large percentage
#' of the quarter and of their possible benefits. And the f5 component is a bonus for
#' folks reporting a high percentage of benefit-awarding weeks with zero earnings.
#'
#'    USES:     benefits, quarterly_wage
#'
#'    CREATES:  qwr_score
#'
## Note that I use some crazy math to calculate `f1` and `f2`, below, which expresses
## a Gompertz curve (not to be confused with a Gompertz distribution), which is a generalization
## of a logit curve, and provides a way to have bounded scores with a useful, smooth logit-like shape:
##
## 100 * exp (-5 * exp (-0.0009 * x))
## ^           ^         ^
## |           |         |
## a=100       b=5       c=0.0009
##
## Where:
##        a is asymptote (max)
##        b is x-axis shift
##        c is growth rate
##
##The values I chose are based on my eyeballing of the data.
##
##Note the crazy LEASE and GREATEST stuff, which is to work around postgreSQL's silly overflow/underflow
##issues with EXP. Other languages, like R, don't need that.
##
## NEED TO ADD calculation of the number of employers reporting wages in quarterly_wages
## and to create an f6 which reflects how many employers there are. Maybe bonus points like
## f5, but at any rate 1 employer should not decrease the score, and I've seen up to 6 employers, so
## must scale well.
##
## ALSO NEED TO ADD using add aggr_claimt_wage_rept::payroll_pct_array into the equation
## so that people who make up a larger percent of the payroll get a higher
## level of risk than those who make up a smaller percentage.
##
##
## Could be: s * (1 + gompertz (x, 0.25, 30, 2)) or craziness: s + (1 + gompertz (2, 0.25, 30, 2)) * ((100 - s) / 3)
#'
#' @param cycle_date The APM cycle date.
#' @param db_schema  The database schema to write scores and related tables to.
#'                   Default: "nrd".
#'
#' @return Integer status code, like other models.
#'
#' @examples
#' \dontrun{
#' rc <- qwr_score()
#' }
#'
#' @author Wayne Folta
#' @export
qwr_score <- function(cycle_date=NULL, db_schema="nrd") {
  # Basic APM init stuff
  if(initialize_apm("QWR Scoring") != 0)
    return(1)

  output_message("Start", doMemory=TRUE, clear=TRUE)

  if(is.null(cycle_date))
    cycle_date <- APM_CYCL_DT

  db <- initialize_db()

  if("error" %in% class(db))
    return(2)


  # Clear out scores
  sql <- sprintf("
    DELETE FROM %s.enty_score
    WHERE cycl_dt  = '%s'::date
    AND   score_cd = 'QWR_QWR';",
    db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(3)


  output_message("Inserting new scores")
  output_message(sprintf("Using schema: %s", db_schema))

  # New scores
  sql <- sprintf("
    INSERT INTO %s.enty_score
      (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)
    WITH a AS (
      SELECT
        claimt_id,
        cert_perd_end_qtr_num,
        sum (tot_reglr_reptd_incm_amt)                                  as total_earnings,
        sum (tot_reglr_paymt_amt)                                       as total_benefit,
        sum (base_perd_paymt_amt)                                       as total_wba,
        sum (CASE WHEN tot_reglr_paymt_amt      > 0 THEN 1 ELSE 0 END)  as benefit_weeks,
        sum (CASE WHEN tot_reglr_reptd_incm_amt > 0 THEN 0 ELSE 1 END)  as zero_earnings_weeks
      FROM nrd.aggr_claimt_benf
      -- WHERE cert_perd_end_dt BETWEEN (SELECT starting_date from qwr_parameters)
      -- AND (SELECT ending_date from qwr_parameters)   ----------
      GROUP BY claimt_id, cert_perd_end_qtr_num
    ),
    b AS (
      SELECT a.*, tot_wage_amt,
        nrd.uf_gompertz (tot_wage_amt - total_earnings,         100,  4, 0.001) as f1, -- [0,100] score gap
        nrd.uf_gompertz (tot_wage_amt / (1.0 + total_earnings),   1, 12, 2.75)  as f2, -- [0,1] score ratio
        CASE WHEN total_wba = 0 THEN 1.0
        ELSE
          CASE WHEN ((total_benefit / total_wba) > 0.5)                                -- Under normal circumstances, can't collect more than roughly
          THEN nrd.uf_gompertz (total_benefit / total_wba,     1, 5, 4.5)              -- half of total_wba (~26 weeks) in one quarter (13 weeks)
          ELSE nrd.uf_gompertz (2 * total_benefit / total_wba, 1, 5, 4.5)
          END
        END                                                                     as f3, -- [0,1] score percentage.
        LEAST (1.0, benefit_weeks / 13.0)                                       as f4, -- [0,1] score percentage
        CASE WHEN benefit_weeks = 0 THEN 0.0
        ELSE LEAST (1.0, zero_earnings_weeks / benefit_weeks)
        END                                                                     as f5  -- [0,1] score percentage
      FROM a
      --- LEFT OUTER JOIN ?? ---
      JOIN nrd.aggr_claimt_wage_rept AS bb
      ON a.claimt_id = bb.claimt_id
      AND a.cert_perd_end_qtr_num = bb.wage_qtr_num
    )
    SELECT
      '%s'::date,
      'CLAIMT',
      claimt_id,
      'QWR_QWR',
      ROUND (LEAST (100.0, f1 * f2 * f3 * f4 + (5.0 * f5)), 2),
      CURRENT_TIMESTAMP
    FROM b;",
    db_schema, cycle_date)

    result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
    if ("error" %in% class(result))
      return(4)

    # Made it here? OK to close
    close_db(db)
    return(0)
}


#' Sharing IP Scoring
#'
#' This module reads in the IP base table, determines which users have have
#' frequently shared IP addresses over a certain time period and inserts risk
#' scores into the entity score table indicating the severity of the sharing
#' behavior for the most recent bus_perd_end_dt.  The (quantile) scores are
#' calculated using ip sharing behavior from the entire date range, but only
#' the last bus_perd_end_dt is kept.
#'
#' The following variables/parameters are set at the beginning of the script:
#' - IP_MIN_USERS: The minimum # of users sharing an IP address within the same
#'                 week required for those to be eligible for a risk score.
#' - IP_DECAY_FACTOR: The factor to be used for a time decay of scores (in
#'                    e^(-kt), k = decay factor)
#'
#' NOTE: This module also uses IP location to complement its score. If a user's
#' ip is shared and it's coming from outside the states listed in the
#' claimant's file, it's more risky.
#'
#' @param cycle_date       The APM cycle date.
#' @param db_schema        The database schema to write scores and related tables
#'                         to. Default: "nrd".
#' @param drop_tmp_tables  Should the routine remove temporary tables created
#'                         along the way to risk scores? Default: TRUE.
#' @param exclude_cellular Whether or not the module should exclude
#'                         records identified as originating from cellular IPs.
#'
#' @return Integer status code, like other models.
#'
#' @examples
#' \dontrun{
#' rc <- sharing_ip_score()
#' }
#'
#' @export
sharing_ip_score <- function(cycle_date       = NULL,
                             db_schema        = "nrd",
                             drop_tmp_tables  = TRUE,
                             exclude_cellular = FALSE) {

  if (initialize_apm("Sharing IP Scoring") != 0)
    return(1)

  output_message("Start", doMemory=TRUE, clear=TRUE)

  if (is.null(cycle_date))
    cycle_date <- APM_CYCL_DT

  db <- initialize_db()

  if ("error" %in% class(db))
    return(2)


  output_message("Create TMP tables")

  # Get minimum amount of time between any two users filing in the same week.
  # Returns all pairwise combinations of users filing from same ip in same week
  # along with the number of seconds between their submissions
  sql <- "
    DROP TABLE IF EXISTS tmp.ip_share_claimantdiffs;

    CREATE TABLE tmp.ip_share_claimantdiffs AS
    SELECT
    	c.orig_ip_addr,
    	c.bus_perd_end_dt,
    	c.claimt_id as firstclaimant,
    	s.claimt_id as secondclaimant,
    	ABS(date_part('epoch',(c.claimt_sessn_tmstmp - s.claimt_sessn_tmstmp))) as diffseconds
    FROM aggr_claimt_sessn c
    JOIN aggr_claimt_sessn s
    ON
        s.orig_ip_addr = c.orig_ip_addr
        AND s.bus_perd_end_dt = c.bus_perd_end_dt
        AND s.claimt_id <> c.claimt_id"

  if (exclude_cellular) {
    output_message("Excluding CEL sessions")
    sql <- paste(sql,
            "WHERE (c.ip_conn_type_cd != 'CEL' OR c.ip_conn_type_cd IS NULL)
             AND   (s.ip_conn_type_cd != 'CEL' OR s.ip_conn_type_cd IS NULL)")
  }

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(3)

  # Get a table with the minimum number of seconds per week/claimant
  sql <- "
    DROP TABLE IF EXISTS tmp.ip_share_claimantweek;

    CREATE TABLE tmp.ip_share_claimantweek AS
    SELECT
    	firstclaimant,
    	bus_perd_end_dt,
    	MIN(diffseconds) as min_seconds
    FROM tmp.ip_share_claimantdiffs
    GROUP BY firstclaimant, bus_perd_end_dt;"

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(4)

  # Use previous table to get the minimum time difference between two different
  # claimants.
  sql <- "
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
    GROUP BY t1.firstclaimant, t1.bus_perd_end_dt, t1.diffseconds;"

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(5)

  # Using the original table, calculate the number of unique users for each
  # orig_ip_addr for a given bus_perd_end_dt. Only keep IPs that have been
  # shared by at least IP_MIN_USERS users

  # Remove CEL IPs if requested
  sql_cel_inject  <- ""
  sql_cel_inject2 <- ""

  if (exclude_cellular) {
    sql_cel_inject <-  "WHERE (t1.ip_conn_type_cd != 'CEL'
                            OR t1.ip_conn_type_cd IS NULL)"
    sql_cel_inject2 <- "WHERE (ip_conn_type_cd != 'CEL'
                            OR ip_conn_type_cd IS NULL)"
  }

  sql <- sprintf("
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
        FROM aggr_claimt_sessn
        %s) t2
    ON
    	t2.orig_ip_addr = t1.orig_ip_addr AND
    	t2.claimt_sessn_tmstmp <= t1.claimt_sessn_tmstmp
    %s
    GROUP BY t1.bus_perd_end_dt, t1.orig_ip_addr
    HAVING COUNT(DISTINCT(t2.claimt_id)) >= (SELECT CAST((SELECT parm_val
    										FROM ref.parm
    										WHERE parm_cd = 'IP_MIN_USERS') AS INTEGER));",
                 sql_cel_inject2, sql_cel_inject)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(6)

  # Join the above table on the table that already contains the minimum
  # difference in seconds for each user/week
  sql <- "
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
    	t1.bus_perd_end_dt = t2.bus_perd_end_dt;"

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(7)

  # Obtain the number of unique users with each ip in the week

  # Remove CEL IPs if requested
  sql_cel_inject <- ""
  if (exclude_cellular) {
    sql_cel_inject <- "WHERE (ip_conn_type_cd != 'CEL'
                           OR ip_conn_type_cd IS NULL)"
  }

  sql <- sprintf("
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
      %s
    	GROUP BY orig_ip_addr, bus_perd_end_dt) t2
    ON
    	t1.shared_ip = t2.orig_ip_addr AND
    	t1.bus_perd_end_dt = t2.bus_perd_end_dt;", sql_cel_inject)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(8)

  # Add IP location from original table

  # Remove CEL IPs if requested
  sql_cel_inject <- ""
  if (exclude_cellular) {
    sql_cel_inject <-  "WHERE (ip_conn_type_cd != 'CEL'
                            OR ip_conn_type_cd IS NULL)"
  }

  sql <- sprintf("
    DROP TABLE IF EXISTS tmp.ip_share_claimantweek5;

    CREATE TABLE tmp.ip_share_claimantweek5 AS
    SELECT
      t1.*,
      t2.ip_outsd_local_st_flag,
      t2.ip_outsd_cntry_flag
    FROM tmp.ip_share_claimantweek4 t1
    LEFT JOIN (
      SELECT
        claimt_id,
        orig_ip_addr,
        BOOL_OR(ip_outsd_local_st_flag) as ip_outsd_local_st_flag,
        BOOL_OR(ip_outsd_cntry_flag) as ip_outsd_cntry_flag,
        bus_perd_end_dt
      FROM aggr_claimt_sessn
      %s
      GROUP BY claimt_id, orig_ip_addr, bus_perd_end_dt) t2
    ON
      t1.claimt_id = t2.claimt_id AND
      t1.shared_ip = t2.orig_ip_addr AND
      t1.bus_perd_end_dt = t2.bus_perd_end_dt
    ORDER BY bus_perd_end_dt, claimt_id, shared_ip", sql_cel_inject)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(9)

  # Apply exponential decay and quantile scoring of to obtain different score
  # components
  sql <- "
    DROP TABLE IF EXISTS tmp.ip_share_claimantweek6;

    CREATE TABLE tmp.ip_share_claimantweek6 AS
    SELECT
    	*,
    	NTILE(20) OVER (ORDER BY unique_users_in_week) * 5 as weekly_users_score,
    	NTILE(20) OVER (ORDER BY prev_unique_users) * 5 as all_time_users_score,
    	ROUND(EXP(diffseconds * -1 *
    							(SELECT CAST((SELECT parm_val
    							FROM ref.parm
                  -- decay_factor (chosen such that a score of 100 is received
                  -- up to 15 minutes in between submissions)
    							WHERE parm_cd = 'IP_DECAY_FACTOR') AS FLOAT)))
    						  * 100) as time_between_submissions_score
    FROM (select * from tmp.ip_share_claimantweek5 order by RANDOM()) t;"

  # Set a seed for reproducible randomness
  result <- tryCatch(RJDBC::dbGetQuery(db$conn, "select setseed(0.5);"),
                     error=apm_error_handler)
  if ("error" %in% class(result))
    return(91)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(10)

  # Create the final table with the overall score from these variables and
  # subset down to only include scores from the most recent bus_perd_end_dt
  sql <- "
    DROP TABLE IF EXISTS tmp.precursor_claimant_sharing_ips;

    CREATE TABLE tmp.precursor_claimant_sharing_ips AS
    SELECT
    	t1.claimt_id,
    	t1.matched_claimants,
    	t1.shared_ip,
    	t1.bus_perd_end_dt,
    	t1.diffseconds,
    	t1.prev_unique_users,
    	t1.unique_users_in_week,
    	t1.weekly_users_score,
    	t1.all_time_users_score,
    	t1.time_between_submissions_score,
    	t1.adj_final_score,
    	CASE WHEN t1.adj_final_score > 100 THEN 100
    	ELSE t1.adj_final_score
    	END AS final_score
    FROM (
    	SELECT
    		*,
    		CASE WHEN ip_outsd_local_st_flag IS TRUE AND ip_outsd_local_st_flag IS NOT NULL THEN
    			weekly_users_score * 0.4 + time_between_submissions_score * 0.4 + all_time_users_score * 0.2 + 20
    		ELSE
    			weekly_users_score * 0.4 + time_between_submissions_score * 0.4 + all_time_users_score * 0.2
    		END AS adj_final_score
    	FROM tmp.ip_share_claimantweek6
    	WHERE bus_perd_end_dt = (SELECT MAX(bus_perd_end_dt) FROM tmp.ip_share_claimantdiffs)) t1;"

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(11)


  output_message("Create NRD tables")
  output_message(sprintf("Using schema: %s", db_schema))

  # Insert Score attributes into claimant sharing attributes table
  sql <- sprintf("DELETE FROM %s.ipa_ship_ds WHERE cycl_dt = '%s'::date;",
            db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(12)

  sql <- sprintf("
    INSERT INTO %s.ipa_ship_ds
      (cycl_dt, claimt_id, orig_ip_addr, matchd_claimt_id_array, uniq_claimt_cnt,
  		 cuml_uniq_claimt_cnt, min_sessn_gap_cnt, crt_tmstmp)
    SELECT
    	'%s'::date,
    	claimt_id,
    	shared_ip,
    	matched_claimants,
    	unique_users_in_week,
    	prev_unique_users,
    	diffseconds,
    	CURRENT_TIMESTAMP
    FROM tmp.precursor_claimant_sharing_ips;",
    db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(13)

  # Insert subscores into scoring table
  sql <- sprintf("
    DELETE FROM %s.enty_score
    WHERE cycl_dt = '%s'::date
    AND score_cd IN ('IPA_SHIP_PERD','IPA_SHIP_ALL','IPA_SHIP_TIME','IPA_SHIP_RAW','IPA_SHIP');",
    db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(14)

  # Inserting weekly users score
  sql <- sprintf("
    INSERT INTO %s.enty_score
      (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)
    SELECT
    	'%s'::date,
    	'CLAIMT',
    	claimt_id,
    	'IPA_SHIP_PERD',
    	weekly_users_score,
    	CURRENT_TIMESTAMP
    FROM tmp.precursor_claimant_sharing_ips;",
    db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(15)

  # Inserting all-time users score
  sql <- sprintf("
    INSERT INTO %s.enty_score
      (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)
    SELECT
    	'%s'::date,
    	'CLAIMT',
    	claimt_id,
    	'IPA_SHIP_ALL',
    	all_time_users_score,
    	CURRENT_TIMESTAMP
    FROM tmp.precursor_claimant_sharing_ips;",
    db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(16)

  # Inserting time between claims score
  sql <- sprintf("
    INSERT INTO %s.enty_score
      (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)
    SELECT
    	'%s'::date,
    	'CLAIMT',
    	claimt_id,
    	'IPA_SHIP_TIME',
    	time_between_submissions_score,
    	CURRENT_TIMESTAMP
    FROM tmp.precursor_claimant_sharing_ips;",
    db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(17)

  # Inserting the adjusted final score (before flooring any scores above 100
  # back down to 100)
  sql <- sprintf("
    INSERT INTO %s.enty_score
      (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)
    SELECT
    	'%s'::date,
    	'CLAIMT',
    	claimt_id,
    	'IPA_SHIP_RAW',
    	adj_final_score,
    	CURRENT_TIMESTAMP
    FROM tmp.precursor_claimant_sharing_ips;",
    db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(18)

  # Basic Insert of final scores
  sql <- sprintf("
    INSERT INTO %s.enty_score
      (cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)
    SELECT
    	'%s'::date,
    	'CLAIMT',
    	claimt_id,
    	'IPA_SHIP',
    	final_score,
    	CURRENT_TIMESTAMP
    FROM tmp.precursor_claimant_sharing_ips;",
    db_schema, cycle_date)

  result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
  if ("error" %in% class(result))
    return(19)

  # Clean up
  if (drop_tmp_tables) {
    sql <- "DROP TABLE IF EXISTS
      tmp.precursor_claimant_sharing_ips,
      tmp.ip_share_claimantweek6,
      tmp.ip_share_claimantweek5,
      tmp.ip_share_claimantweek4,
      tmp.ip_share_claimantweek3,
      tmp.ip_share_all_users1,
      tmp.ip_share_claimantweek2,
      tmp.ip_share_claimantweek,
      tmp.ip_share_claimantdiffs;"

    result <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error=apm_error_handler)
    if ("error" %in% class(result))
      return(20)
  }

  # Made it here? OK to close
  close_db(db)
  return(0)
}
