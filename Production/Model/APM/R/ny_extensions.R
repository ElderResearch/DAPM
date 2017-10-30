################################################################################
## NYSDOL requests that do not accomodate the other pilot states
################################################################################


#' NYSDOL QWR Cross-check
#'
#' @description
#' Implement NYSDOL's version of the QWR cross-check based on a
#' \code{STORED_VALUE} that estimates a claimant's wages in a quarter.
#'
#' @param cycle_date APM cycle date as a character vector.
#' @param prev_days Claimants who hav certified within `prev_days` days will
#'   be assigned a score.
#' @param score_table_name NRDB score table name or \code{NULL}.
#'   If \code{NULL}, return a data frame instead of writing to the NRDB.
#' @return An integer return code (or a data frame containing the scores if
#'   \code{score_table_name} is \code{NULL}).
#' @export
#' @examples
#' \dontrun{
#' library(APM)
#' qwr <- ny_qwr_score()
#' }
ny_qwr_score <- function(cycle_date = NULL, prev_days = 90,
                         score_table_name = "nrd.enty_score") {

  ##############################################################################
  ## Initialize
  ##############################################################################

  if (initialize_apm("NY QWR Scoring") != 0) {
    return(1)
  }

  output_message("Start", doMemory = TRUE, clear = TRUE)

  if (is.null(cycle_date)) {
    cycle_date <- APM_CYCL_DT
  }

  db <- initialize_db()

  if ("error" %in% class(db)) {
    return(2)
  }

  # The code that goes into the database
  model_code <- "QWR_QWR"


  ##############################################################################
  ## Pull data and score
  ##############################################################################
  # The quarter to use is fixed as the most recent quarter with data
  # in the incm table (where NY's STORED_VALUE goes). **To run this model
  # with historical data would require updating this logic.**
  qtr <- tryCatch(RJDBC::dbGetQuery(db$conn,
            "with dt as (
              select max(cert_perd_end_dt) as cert_perd_end_dt
              from cert
              join incm
              on cert.cert_id = incm.cert_id
              where incm.reglr_incm_flag is TRUE
              and   incm.reptd_incm_amt is not NULL)
            select to_char(cert_perd_end_dt, 'YYYYQ')::int as qtr
            from dt"),
            error = apm_error_handler)

  if ("error" %in% class(qtr)) {
    return(31)
  }

  output_message(sprintf("Using quarter: %d", qtr$qtr[[1]]))

  # Set the date window for the pull
  start_date <- as.character(as.Date(cycle_date, "%Y-%m-%d") - prev_days)
  output_message(sprintf("Claimant date range: [%s, %s]", start_date, cycle_date))

  # Now pull (claimant) x (incm) x (wage) data for qtr
  # This is only remotely efficient if we create temp tables now that we have
  # to build the wage report table to tally claimants' employers
  output_message("Building claimant list")
  
  sql <- sprintf("
    drop table if exists tmp.tmp_qwr_cla;
  
    create table tmp.tmp_qwr_cla as
    select bf.*, c.last_emplr_id
    from (
      select distinct on(claimt_id) claimt_id, claim_start_dt
      from nrd.aggr_claimt_benf
      where cert_perd_end_dt >= date '%1$s'
      and   cert_perd_end_dt <= date '%2$s'
      order by claimt_id, cert_perd_end_dt desc) bf
    join nrd.claim c
    on  bf.claimt_id = c.claimt_id
    and bf.claim_start_dt = c.claim_start_dt;", start_date, cycle_date)
  
  res <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error = apm_error_handler)
  if ("error" %in% class(res)) {
    return(32)
  }
  
  output_message("Building stored values")
  
  sql <- sprintf("
    drop table if exists tmp.tmp_qwr_sv;
  
    create table tmp.tmp_qwr_sv as 
    select claim.claimt_id, sum(incm.reptd_incm_amt) as stored_value
    from nrd.cert
    join nrd.incm
      on cert.cert_id = incm.cert_id
    join nrd.claim 
      on cert.claim_id = claim.claim_id
    where to_char(cert.cert_perd_end_dt, 'YYYYQ')::int = %1$d
    and   incm.reptd_incm_amt is not null
    and   claim.claimt_id in (select claimt_id from tmp.tmp_qwr_cla)
    group by claim.claimt_id;", qtr$qtr[[1]])
  
  res <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error = apm_error_handler)
  if ("error" %in% class(res)) {
    return(33)
  }
  
  output_message("Building wage table")
  
  sql <- sprintf("
    drop table if exists tmp.tmp_qwr_wg;
  
    create table tmp.tmp_qwr_wg as
    select
      claimt_id,
      sum(wage.wage_amt) as tot_wage_amt,
      array_agg(emplr.emplr_id) as emplr_id_array
    from nrd.wage_rept
    join nrd.emplr
      on wage_rept.emplr_id = emplr.emplr_id
    join nrd.wage
      on wage_rept.wage_rept_id = wage.wage_rept_id
    join nrd.empl
      on wage.empl_id = empl.empl_id
    join nrd.claimt
      on empl.empl_ref_num = claimt.claimt_ref_num
    where to_char(wage_rept.wage_dt, 'YYYYQ')::int = %1$d
    and claimt_id in (select claimt_id from tmp.tmp_qwr_cla)
    group by claimt_id;", qtr$qtr[[1]])
  
  res <- tryCatch(RJDBC::dbSendUpdate(db$conn, sql), error = apm_error_handler)
  if ("error" %in% class(res)) {
    return(34)
  }
  
  output_message("Pulling data set")
  
  sql <- "
    select cla.claimt_id, sv.stored_value, wg.tot_wage_amt,
      cla.last_emplr_id = any(wg.emplr_id_array) as last_emplr_flag
    from tmp.tmp_qwr_cla cla
    join tmp.tmp_qwr_sv sv
      on cla.claimt_id = sv.claimt_id
    join tmp.tmp_qwr_wg wg
      on cla.claimt_id = wg.claimt_id;"
  
  benf <- tryCatch(RJDBC::dbGetQuery(db$conn, sql), error = apm_error_handler)
  if ("error" %in% class(benf)) {
    return(35)
  }

  output_message(sprintf("Number of QWR scores to generate: %d", nrow(benf)))

  
  # Implement the business rule in a dumb but vectorized way:
  # Progressivly overwrite NAs, acting only on NA records to preserve the
  # case_when() character of the rule.
  # ----------------------------------------------------------------------------
  benf$score <- NA_real_

  # score = 0 if no wages
  benf$score <- replace(benf$score,
                        is.na(benf$score) &
                          ((benf$tot_wage_amt <= 0) | is.na(benf$tot_wage_amt)),
                        0)

  # score = 0 if stored_value >= wages
  benf$score <- replace(benf$score,
                        is.na(benf$score) &
                          benf$stored_value >= benf$tot_wage_amt,
                        0)

  # score = 20 if stored_value = a * wages, a in [0.8, 1.0)
  benf$score <- replace(benf$score,
                        is.na(benf$score) &
                          benf$stored_value/benf$tot_wage_amt  < 1.0 &
                          benf$stored_value/benf$tot_wage_amt >= 0.8,
                        20)

  # score = 40 if stored_value = a * wages, a in [0.6, 0.8)
  benf$score <- replace(benf$score,
                        is.na(benf$score) &
                          benf$stored_value/benf$tot_wage_amt  < 0.8 &
                          benf$stored_value/benf$tot_wage_amt >= 0.6,
                        40)

  # score = 60 if stored_value = a * wages, a in [0.4, 0.6)
  benf$score <- replace(benf$score,
                        is.na(benf$score) &
                          benf$stored_value/benf$tot_wage_amt  < 0.6 &
                          benf$stored_value/benf$tot_wage_amt >= 0.4,
                        80)

  # score = 100 if stored_value = a * wages, a in (-Inf, 0.4)
  benf$score <- replace(benf$score,
                        is.na(benf$score) &
                          benf$stored_value/benf$tot_wage_amt  < 0.4,
                        100)


  # Every case should be covered at this point, so any NAs mean we failed
  # and should bail out before writing (wrong) scores
  if (any(is.na(benf$score))) {
    return(4)
  }
  
  
  # Set scores to 30 depending on last employer contrib to wages
  benf[(benf$score > 0 &
        !is.na(benf$last_emplr_flag) &
        benf$last_emplr_flag == "t"), "score"] <- 30

  
  ##############################################################################
  ## Store/return the scores
  ##############################################################################

  if (is.null(score_table_name)) {
    output_message("Returning data frame")

    close_db(db)

    # stringsAsFactors=F to keep strings strings! (or numerics, post-coercion)
    return(data.frame(
      claimt_id        = as.numeric(benf$claimt_id),
      score            = benf$score,
      cycle_date       = cycle_date,
      score_cd         = model_code,
      timestamp        = Sys.time(),
      stringsAsFactors = FALSE))
  } else {
    output_message("Begin Transaction")

    s <- db_begin(db$conn)

    if ("error" %in% class(s)) {
      return(5)
    }

    output_message("Delete Previous Scores")

    s <- delete_scores(conn = db$conn,
                       score_cd = model_code,
                       cycle_date = cycle_date,
                       scoreTableName = score_table_name)

    if ("error" %in% class(s)) {
      return(6)
    }

    output_message("Output Scores")

    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

    s <- tryCatch(write_scores(claimt_ids = benf$claimt_id,
                               scores = benf$score,
                               conn = db$conn,
                               score_cd = model_code,
                               cycle_date = cycle_date,
                               scoreTableName = score_table_name,
                               timestamp = timestamp),
                  error = apm_error_handler)

    if ("error" %in% class(s)) {
      db_rollback(db$conn)
      return(7)
    }

    s <- tryCatch(db_commit(db$conn), error = apm_error_handler)

    if ("error" %in% class(s)) {
      return(8)
    }

    close_db(db)

    return(0)
}}


#' NYSDOL Statistical Model Wrapper
#'
#' @description
#' A wrapper function that applies a by-hand Bayesian Rule List model
#' to identify likely non-willful non-recoverable (NWNR) overpayments. This
#' corrects an issue with the NRDB's targets whereby the modules pinpoint
#' overpayments that NY cannot recover.
#' 
#' The scores produced by the statistical models are downweighted proportionally
#' to the BRL model's posterior probability of being NWNR.
#' 
#' This function depends on the "Suggested" package `sbrl` and an internal
#' not-documented data set `brl.storage`.
#'
#' @param model DAPM module code as a character vector (`"KNN_KNN"`, `"RF_RF"`,
#'   or `"SVM_SVM"`)
#' @param effect Scale the usual risk score by `(1 - effect * nwnr_prob)`
#' @param cycle_date APM cycle date as a character vector
#' @param prevDays Claimants who have certified within `prevDays` days will
#'   be assigned a score
#' @param inputDF A data frame of predictors, if a pull from the NRDB is not
#'   desired
#' @param scoreTableName NRDB score table name or `NULL`
#'   If `NULL`, return a data frame instead of writing to the NRDB.
#' @param k Passed to \link{k_nearest_neighbors_score} if `model` = "KNN_KNN"
#' @return An integer return code (or a data frame containing the scores if
#'   `scoreTableName` is `NULL`).
#' @export
ny_statistical_model <- function(model, effect = 0.75, cycle_date = NULL,
                                 prevDays = 90, inputDF = NULL,
                                 scoreTableName = "nrd.enty_score",
                                 k = NULL) {
  
  
  # brk is a named list; each name is a column in dt, and its value is a
  # numeric vector of cuts to use in binning the data
  set_breaks_dt <- function(dt, brk) {
    for (ftr in names(brk)) {
      dt[, c(ftr) := ordered(findInterval(dt[[ftr]], brk[[ftr]]))]
    }
    invisible(NULL)
  }
  
  
  # Start up the APM package as if we were running the RF model to access
  # the RF feature transformations.
  if(initialize_apm("NY Model Scoring Wrapper") != 0) {
    return(91)
  }
  
  
  # Note that model has to be a DAPM module code
  model <- trimws(toupper(model))
  
  tryCatch({
    if (!(model %in% c("KNN_KNN", "RF_RF", "SVM_SVM"))) {
      stop(paste0("Invalid DAPM statistical model code specified: ", model))
    }
  }, error = apm_error_handler) -> s
  
  if ("error" %in% class(s)) {
    return(92)
  }

  
  tryCatch({
    features        <- rf.storage$features
    transform.funcs <- rf.storage$transform.funcs
    impute.vals     <- rf.storage$impute.vals
    
    # These should not be NULL, which seems to be what happens if not loaded
    if (any(vapply(rf.storage, is.null, TRUE))) {
      stop("One or more model variables were not loaded.")
    }
  }, error = apm_error_handler) -> s
  
  if ("error" %in% class(s)) {
    return(93)
  }
  
  
  # Two critical requirements:
  # 1. brl.storage (INTERNAL, undocumented data)
  # 2. sbrl package (suggested, not imported)
  tryCatch({
    brl_model  <- brl.storage$model
    brl_breaks <- brl.storage$breaks
  }, error = apm_error_handler) -> s
  
  if ("error" %in% class(s)) {
    return(98)
  }
  
  # http://r-pkgs.had.co.nz/description.html
  tryCatch({
    if (!requireNamespace("sbrl", quietly = TRUE)) {
      stop("sbrl is required for this function.", call. = FALSE)
    }
  }, error = apm_error_handler) -> s
  
  if ("error" %in% class(s)) {
    return(99)
  }
  
  
  db_conn <- initialize_db()
  
  if("error" %in% class(db_conn)) {
    return(94)
  }
  
  if(is.null(cycle_date)) {
    cycle_date <- APM_CYCL_DT
  }
  
  
  # Load all the data here to pass as a data frame to the model later
  if (is.null(inputDF)) {
    earliestDate <- as.character(as.Date(cycle_date, "%Y-%m-%d") - prevDays)
    
    output_message(sprintf(
      "Loading certifications from SQL; date range = [%s, %s]",
      earliestDate, cycle_date))
    
    benx <- tryCatch(RJDBC::dbGetQuery(db_conn$conn,
              sprintf("SELECT DISTINCT ON(claimt_id) *
                       FROM nrd.aggr_claimt_benf
                       WHERE cert_perd_end_dt >= '%s'
                       AND   cert_perd_end_dt <= '%s'
                       ORDER BY claimt_id, cert_perd_end_dt DESC",
                       earliestDate, cycle_date)),
              error = apm_error_handler)
    
    if("error" %in% class(benx)) {
      return(95)
    }
  } else {
    output_message("Scoring certifications passed as a data.frame")
    benx <- inputDF
  }
  
  benx <- data.table::data.table(benx)
  
  
  # Now we need to copy the data -- the original benx goes unmodified to
  # the model code. But we need a transformed copy to apply the NWNR submodel.
  nwnr <- data.table::copy(benx)
  
  # Keep only the claimant ID plus the relevant features
  nwnr[, setdiff(names(nwnr), c("claimt_id", features)) := NULL]
  
  # Imputations
  if (length(impute.vals) > 0) {
    err <- impute_values_dt(nwnr, impute.vals)
    
    if ("error" %in% class(err)) {
      return(96)
    }
  }
  
  # Transformations
  if (length(transform.funcs) > 0) {
    err <- transform_inputs_dt(nwnr, transform.funcs)
    
    if ("error" %in% class(err)) {
      return(97)
    }
  }
  
  # Drop NAs
  nwnr <- APM:::na_check_and_omit(nwnr)
  
  
  # Predict NWNR
  set_breaks_dt(nwnr, brl_breaks)
  
  predictions <- sbrl::predict.sbrl(brl_model, as.data.frame(nwnr))
  nwnr[, p := predictions$V2]
  nwnr[, setdiff(names(nwnr), c("claimt_id", "p")) := NULL]

  
  # Dispatch to a model
  if (model == "KNN_KNN") {
    result <- k_nearest_neighbors_score(
      cycle_date = cycle_date, prevDays = prevDays,
      inputDF = as.data.frame(benx), scoreTableName = NULL, k = k)
  } else if (model == "RF_RF") {
    result <- random_forest_score(
      cycle_date = cycle_date, prevDays = prevDays,
      inputDF = as.data.frame(benx), scoreTableName = NULL)
  } else if (model == "SVM_SVM") {
    result <- support_vector_machine_score(
      cycle_date = cycle_date, prevDays = prevDays,
      inputDF = as.data.frame(benx), scoreTableName = NULL)
  }
  
  
  # Bind and downweight
  # Downweight the NWNR probability: A 100% NWNR should be reduced by N%
  result <- merge(data.table::as.data.table(result), nwnr, by = "claimt_id")
  result[, adj.score := round(score * (1 - effect * p))]
  result
  
  # Write to the database
  if (is.null(scoreTableName)) {
    output_message("Returning data frame")
    close_db(db_conn)
    
    df.score <- data.frame(claimt_id = as.numeric(result$claimt_id),
                           score = result$adj.score,
                           cycle_date = cycle_date,
                           timestamp = Sys.time(),
                           stringsAsFactors = FALSE)
    
    return(df.score)
  } else {
    output_message("Begin Transaction")
    
    s <- db_begin(db_conn$conn)
    
    if("error" %in% class(s)) {
      return(101)
    }
    
    
    output_message("Delete Previous Scores")
    
    s <- delete_scores(db_conn$conn, model, cycle_date, scoreTableName)
    
    if("error" %in% class(s)) {
      return(102)
    }
    
    
    output_message("Output Scores")
    
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    
    s <- tryCatch(write_scores(result$claimt_id, result$adj.score, db_conn$conn,
                               model, cycle_date, scoreTableName, timestamp),
                  error = apm_error_handler)
    
    if("error" %in% class(s)) {
      db_rollback(db_conn$conn)
      return(103)
    }
    
    s <- tryCatch(db_commit(db_conn$conn), error = apm_error_handler)
    
    if("error" %in% class(s)) {
      return(44)
    }
    
    close_db(db_conn)
  }
  
  output_message("End")
  return(0)

}
