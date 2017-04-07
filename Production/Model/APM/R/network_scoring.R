#' PII and Employment Network Scoring
#'
#' This module connects claimants to others identified as having commited fraud
#' via three factors:
#' 
#' 1. Employment history
#' 2. PII
#' 3. IP address sharing
#' 
#' Because of time and testing constraints (cannot easily test models that use
#' aggregated data, as the data is not available retroactively), all facets
#' of the model are reimplemented in R.
#' 
#' Based on initial testing with 2015 NY data (see Rmd file), the model provides
#' lift by summing up claimants' ranks in the subscores (i.e., average rank). A
#' more probabilistic model is possible (like the 1st version), but needs to
#' be able to combine subscores in a logical way.
#'
#' @return Integer code to Kettle or data frame if `scoreTableName = NULL`
#'
#' @param cycle_date     Weekend date to calculate scores up to
#' @param prevDays       Days prior to the cycle_date to include for scoring
#' @param scoreTableName Postgres table in which to put scores; set NULL to return a data frame
#' @param emplrQuarters  Number of quarters to seek back for employment history
#' @param piiYears       Number of years to seek back for PII connections
#' @param ipWeeks        Number of weeks to seek back for IP connections
#'
#' @author Micah Darrell
#' @author 3-3-2016 Kenny Darrell performace tweaks
#' @author 4-12-2016 Wayne Folta updates to latest APM
#' @author 11-2-2016 Tom Shafer rewrites for better performance with a large network
#' @export
#'
network_score <-
function(cycle_date=NULL, prevDays=90, scoreTableName="nrd.enty_score",
         emplrQuarters=2, piiYears=5, ipWeeks=13) {
  
  # APM setup
  if (initialize_apm("Network Model Scoring") != 0) {
    return(1)
  }

  output_message("Start", doMemory=TRUE, clear=TRUE)
  
  # Print key parameters
  output_message(paste("PARAMETER: emplrQuarters =", emplrQuarters))
  output_message(paste("PARAMETER: piiYears      =", piiYears))
  output_message(paste("PARAMETER: ipWeeks       =", ipWeeks))

  
  if(is.null(cycle_date)) {
    cycle_date <- APM_CYCL_DT
  }

  db_conn <- initialize_db()

  if ("error" %in% class(db_conn)) {
    return(2)
  }
  
  
  ##############################################################################
  # Claimants to score:
  # Same as the machine learning models: find the most recent certification
  # for claimaints that certified within the last [prevDays] days
  ##############################################################################
  earliestDate <- as.Date(cycle_date, "%Y-%m-%d") - prevDays
  
  output_message(sprintf("Loading claimants from date range [%s, %s]", earliestDate, cycle_date))
  
  claimts <- tryCatch(RJDBC::dbGetQuery(db_conn$conn,
               sprintf("SELECT DISTINCT ON(claimt_id) claimt_id, cert_perd_end_dt
                        FROM  nrd.aggr_claimt_benf
                        WHERE cert_perd_end_dt >= '%s'
                        AND   cert_perd_end_dt <= '%s'
                        ORDER BY claimt_id, cert_perd_end_dt DESC",
                       as.character(earliestDate), cycle_date)),
               error=apm_error_handler)
  
  if("error" %in% class(claimts)) {
    return(3)
  }
  
  claimts <- data.table::data.table(claimts)
  
  
  ##############################################################################
  # EMPLOYER NETWORK
  # Select all employment history for a select window of time, including
  # whether or not a claimant has *ever* been flagged as committing fraud.
  # 
  # We then connect this cycle's claimants to known fraud via employment.
  #
  # In testing, a simple `merge`-based calculation was as effective as the more
  # accurate `foverlaps`-based one. So we use `merge()`.
  #
  # In testing, I tentintionally used [cycle_date - 1 week] as the max date to
  # avoid leaks from the future. Here I am using [cycle_date] until/unless I
  # get a better understanding of the different date ranges in production.
  ##############################################################################
  
  output_message("Begin Employer Network Graph")
  
  # How many quarters to use for employment history?
  # More than 4 or 5 is challenging in NY, and 9 takes > 30 GB of memory
  EMPLR_HISTORY_QTRS <- emplrQuarters

  # Generally speaking, we may not have employer wage reports more recent than 2
  # quarters prior: we won't have the current quarter and we may be early in a new
  # quarter and thus will not even have *last* quarter's report. For ease of
  # use and reproducibility, enforce a selection of history from two quarters past.
  
  # This is a very convoluted way to (1) get the quarter of the cycle_date and
  # (2) walk backwards as necessary to form the interval. The factor (- 0.5) walks
  # back two quarters
  
  # EMPLR_HISTORY_QTRS - 1 to get the right number of quarters in total
  emplr_start_qtr <- as.integer(zoo::format.yearqtr(zoo::as.yearqtr(
                        as.Date(cycle_date)) - 0.5 - (EMPLR_HISTORY_QTRS - 1)*0.25, "%Y%q"))
  emplr_end_qtr   <- as.integer(zoo::format.yearqtr(zoo::as.yearqtr(
                        as.Date(cycle_date)) - 0.5, "%Y%q"))

  emplr_hist <- tryCatch(RJDBC::dbGetQuery(db_conn$conn,
                  sprintf("select h.claimt_id, emplr_id, empl_start_qtr_num, empl_end_qtr_num, o.ff
                           from nrd.aggr_claimt_emplr_hist h
                           left join (
                              select claimt_id, bool_or(fraud_flag) as ff
                              from nrd.aggr_claimt_ovpaymt
                              -- limit to fraud we would know about at runtime
                              -- remove for production?
                              where ovpaymt_end_dt < '%3$s'
                              group by claimt_id) o
                           on h.claimt_id = o.claimt_id
                           where empl_start_qtr_num <= %1$d
                           and   empl_end_qtr_num   >= %2$d",
                          emplr_end_qtr, emplr_start_qtr, cycle_date)),
                  error=apm_error_handler)
  
  if ("error" %in% class(emplr_hist)) {
    return(4)
  }

  emplr_hist <- data.table::data.table(emplr_hist)
  
  # Re-code the fraud_flag
  emplr_hist[, ff := ifelse(is.na(ff) | ff == 'f', FALSE, TRUE)]
  
  
  # Subset the data into two parts:
  # - current claimants
  # - claimants that have committed fraud
  # Then we merge() the two sets and sum up the connections as edge weights.
  
  # data.table has .(new_name = old_name) syntax for selecting and renaming columns
  claimts_emplr <- emplr_hist[claimt_id %in% claimts$claimt_id, .(to_claimt = claimt_id, emplr_id)]
  claimts_emplr <- unique(claimts_emplr, by = c("to_claimt", "emplr_id"))
  
  fraud_emplr <- emplr_hist[ff == TRUE, .(from_claimt = claimt_id, emplr_id)]
  fraud_emplr <- unique(fraud_emplr, by = c("from_claimt", "emplr_id"))
  
  # Merge and remove loops (self-edges)
  edges <- merge(claimts_emplr, fraud_emplr, by="emplr_id", allow.cartesian=TRUE)
  edges <- edges[to_claimt != from_claimt]

  # Counts of claimt_ids per employer
  count_emplr <- emplr_hist[, .(count = data.table::uniqueN(claimt_id)), by = emplr_id]
  count_emplr <- unique(count_emplr, by="emplr_id")
  
  # Edge weights are 1/(# of claimts in company in time period)
  # There really should be a numerator, but the final score is a rank in the list,
  # so a common numerator isn't necessary unless we want to be more probabilistic
  edges <- merge(edges, count_emplr, by="emplr_id")
  edges[, score := sum(1.0/count), by = to_claimt]
  
  emplr_scores <- unique(edges, by="to_claimt")[, .(claimt_id = to_claimt, model = "EMPLR", score)]
  
  
  # Free memory for subsequent operations
  rm(claimts_emplr, count_emplr, edges, emplr_hist, fraud_emplr)

  
  ##############################################################################
  # PII NETWORK
  # In this approach, we pull all the PII from the beginning of the NRDB and =
  # produce separate scores for the different PII classes. Realistically, we
  # want to keep the data within memory limits so we impose a 5-year effective
  # cap on the PII.
  ##############################################################################
  
  output_message("Begin PII Network Graph")
  
  # Five years of history
  PII_MIN_DT <- as.character(as.Date(cycle_date) - lubridate::years(piiYears))
  
  pii <- tryCatch(RJDBC::dbGetQuery(db_conn$conn,
            sprintf("select a.claimt_id, claimt_attr_type_cd, claimt_attr_val, ff
                     from nrd.claimt_attr a
                     left join (
                        select claimt_id, bool_or(fraud_flag) as ff
                        from nrd.aggr_claimt_ovpaymt
                        -- prevent leaks form the future
                        -- remove in Production? maybe?
                        where ovpaymt_end_dt < '%s'
                        group by claimt_id) o
                     on a.claimt_id = o.claimt_id
                     where claimt_attr_crt_tmstmp >= '%s'",
                    cycle_date, PII_MIN_DT)),
            error=apm_error_handler)
  
  if ("error" %in% class(pii)) {
    return(5)
  }
  
  pii <- data.table::data.table(pii)

  # Recode the fraud flag
  pii[, ff := ifelse(is.na(ff) | ff == 'f', FALSE, TRUE)]

  
  # Form all possible edges between the current batch of claimants and those that
  # committed fraud -- the edeges are formed over shared PII
  claimts_pii <- pii[claimt_id %in% claimts$claimt_id]
  claimts_pii <- claimts_pii[, .(to_claimt = claimt_id, claimt_attr_type_cd, claimt_attr_val)]
  claimts_pii <- unique(claimts_pii, by = c("to_claimt", "claimt_attr_type_cd", "claimt_attr_val"))
  
  fraud_pii <- pii[ff == TRUE]
  fraud_pii <- fraud_pii[, .(from_claimt = claimt_id, claimt_attr_type_cd, claimt_attr_val)]
  fraud_pii <- unique(fraud_pii, by = c("from_claimt", "claimt_attr_type_cd", "claimt_attr_val"))
  
  # Counts per PII type and PII value for weights
  # This takes a while to calculate
  count_pii <- pii[, .(count = data.table::uniqueN(claimt_id)),
                     by = .(claimt_attr_type_cd, claimt_attr_val)]
  
  count_pii <- unique(count_pii, by = c("claimt_attr_type_cd", "claimt_attr_val"))
  
  
  # Make the edges via merge()
  edges <- merge(claimts_pii, fraud_pii, by = c("claimt_attr_type_cd", "claimt_attr_val"),
                 allow.cartesian=TRUE)
  
  edges <- merge(edges, count_pii, by = c("claimt_attr_type_cd", "claimt_attr_val"),
                 allow.cartesian=TRUE)
  
  # Edge weights are per-claimant and per-PII-type
  edges[, score := sum(1.0/count), by = .(claimt_attr_type_cd, to_claimt)]
  
  pii_scores <- unique(edges, by=c("claimt_attr_type_cd", "to_claimt"))
  pii_scores <- pii_scores[, .(claimt_id = to_claimt, model = claimt_attr_type_cd, score)]
  
  
  # Clean up for the next set of data
  rm(claimts_pii, count_pii, edges, fraud_pii, pii)

  
  ##############################################################################
  # IP ADDRESS NETWORK
  # The table `nrd.aggr_claimt_sessn` contains three months of data, so that
  # is the amount of history we will use for now.
  ##############################################################################
  
  output_message("Begin IP Network Graph")
  
  # Question here as to whether we should start from a Sunday and not a Saturday
  # because bus_perd_end_dt is a Sunday
  IP_MIN_DT <- as.character(as.Date(cycle_date) - lubridate:::weeks(ipWeeks))

  # SQL: select claimants, IP addresses (first 24 bits), times, and fraud_flag
  ipa <- tryCatch(RJDBC::dbGetQuery(db_conn$conn,
            sprintf("select s.claimt_id, claimt_sessn_tmstmp, ff, 
                     substring(orig_ip_addr::varchar(18), '^[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}') as short_ip
                     from nrd.claimt_sessn s
                     left join (
                        select claimt_id, bool_or(fraud_flag) as ff
                        from nrd.aggr_claimt_ovpaymt
                        where ovpaymt_end_dt < '%1$s'
                        group by claimt_id) o
                     on s.claimt_id = o.claimt_id
                     where sessn_enty_type_cd = 'CERT'
                     and claimt_sessn_tmstmp between '%2$s'and '%1$s'",
          cycle_date, IP_MIN_DT)),
          error=apm_error_handler)
  
  if ("error" %in% class(ipa)) {
    return(6)
  }
  
  ipa <- data.table::data.table(ipa)
  
  # Fix up fraud_flag
  ipa[, ff := ifelse(is.na(ff) | ff == 'f', FALSE, TRUE)]

  
  # Scores are computed by counting the number of IP shares.
  # An IP is considered "shared" if two claimants used it within 8 days of each
  # other. (8 days because I'm taking *timestamp +/- 4 days*.)
  
  # 8-day window for IP sharing (symmetric... can't do 3.5 days)
  ipa[, claimt_sessn_tmstmp := lubridate::ymd_hms(claimt_sessn_tmstmp)]
  ipa[, time_a := claimt_sessn_tmstmp - lubridate::days(4)]
  ipa[, time_b := claimt_sessn_tmstmp + lubridate::days(4)]
  
  claimts_ipa <- ipa[claimt_id %in% claimts$claimt_id,
                     .(to_claimt = claimt_id, short_ip, time_a, time_b)]
  
  fraud_ipa   <- ipa[ff == TRUE, .(from_claimt = claimt_id, short_ip, time_a, time_b)]
  
  # Run foverlaps() to find connections between fraud and current claimants
  data.table::setkey(claimts_ipa, short_ip, time_a, time_b)
  data.table::setkey(fraud_ipa,   short_ip, time_a, time_b)
  
  edges <- data.table::foverlaps(claimts_ipa, fraud_ipa, mult="all", nomatch=0)
  edges <- edges[to_claimt != from_claimt]
  edges <- edges[, .(to_claimt, from_claimt, short_ip)]
  
  count_ipa <- ipa[, .(count = data.table::uniqueN(claimt_id)), by = short_ip]
  count_ipa <- unique(count_ipa, by="short_ip")
  
  # Edges are again inverse of counts
  edges <- merge(edges, count_ipa, by="short_ip")
  edges[, score := sum(1.0/count), by = to_claimt]
  
  ipa_scores <- unique(edges, by = "to_claimt")
  ipa_scores <- ipa_scores[, .(claimt_id = to_claimt, model = "IPA", score)]
  
  
  ##############################################################################
  # SUBMODELS ENSEMBLE
  # In testing, I had success adding the ranks of the scores for a final score.
  ##############################################################################
  
  output_message("Combine submodels")
  
  # Combine all the subscores
  all_scores <- data.table::rbindlist(
    list(emplr_scores, pii_scores, ipa_scores), use.names=TRUE)

  # Rank the scores per model
  all_scores[, rank := data.table::frank(score, ties.method="min"), by = model]
  
  # Final score is the average rank
  all_scores[, final_rank := sum(rank), by = claimt_id]
  total_scores <- unique(all_scores[, .(claimt_id, rank = final_rank)], by = "claimt_id")
  total_scores[, score := rank/max(rank)*100]
  
  
  ##############################################################################
  # STORE IN NRDB/DATA FRAME
  ##############################################################################
  
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  if (is.null(scoreTableName)) {
    
    # If scoreTableName is NULL, return the final scores as a data frame
    
    scores_df <- data.frame(total_scores[, .(enty_id = claimt_id, score_val = score)])
    scores_df[, "model"]      <- "NWC_COR"
    scores_df[, "cycle_date"] <- cycle_date
    scores_df[, "timestamp"]  <- ts
    
    close_db(db_conn)
    
    return(scores_df)
    
  } else {
    
    # If scoreTableName is not NULL, write new scores to the NRDB
    
    output_message("Begin Transaction")
  
    s <- db_begin(db_conn$conn)
  
    if ("error" %in% class(s)) {
      return(6)
    }
    
    
    output_message("Delete Previous Scores")
  
    s <- delete_scores(db_conn$conn, "NWC_COR", cycle_date, scoreTableName)
  
    if ("error" %in% class(s)) {
      return(7)
    }
  
    
    output_message("Output Scores")

    s <- tryCatch(write_scores(total_scores$claimt_id, total_scores$score,
                    db_conn$conn, "NWC_COR", cycle_date, scoreTableName, ts),
                  error=apm_error_handler)
    
    if ("error" %in% class(s)) {
      db_rollback(db_conn$conn)   # Don't check because can't do anything
      return(8)
    }
    
    s <- tryCatch(db_commit(db_conn$conn), error=apm_error_handler)
    
    if ("error" %in% class(s)) {
      return(9)
    }
  }
  
  close_db(db_conn)
  
  output_message("End")
}
