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
ny_qwr_score <- function(cycle_date       = NULL,
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

  # Select the necessary information
  # inner join - only taking ppl w/benefits and a qwr
  benf <- tryCatch(RJDBC::dbGetQuery(db$conn,
            "select
               a.claimt_id,
               cert_perd_end_qtr_num,
               tot_wage_amt,
               sum(tot_reglr_reptd_incm_amt) as stored_value
             from nrd.aggr_claimt_benf a
             join nrd.aggr_claimt_wage_rept b
             on  a.claimt_id = b.claimt_id
             and a.cert_perd_end_qtr_num = b.wage_qtr_num
             group by a.claimt_id, cert_perd_end_qtr_num, tot_wage_amt"),
            error = apm_error_handler)

  if ("error" %in% class(benf)) {
    return(3)
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


  ##############################################################################
  ## Store/return the scores
  ##############################################################################

  if (is.null(score_table_name)) {
    output_message("Returning data frame")

    close_db(db)

    # stringsAsFactors=F to keep strings strings! (or numerics, post-coercion)
    return(data.frame(
      claimt_id = as.numeric(benf$claimt_id),
      score = benf$score,
      cycle_date = cycle_date,
      score_cd = model_code,
      timestamp = Sys.time(),
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
