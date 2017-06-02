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
            "select to_char(cert_perd_end_dt, 'YYYYQ')::int as qtr
            from cert
            inner join incm
              on cert.cert_id = incm.cert_id
            where incm.reglr_incm_flag is TRUE
            and incm.reptd_incm_amt is not NULL
            order by cert_perd_end_dt desc
            limit 1"),
            error = apm_error_handler)

  if ("error" %in% class(qtr)) {
    return(31)
  }

  output_message(sprintf("Using quarter: %d", qtr$qtr[[1]]))

  # Set the date window for the pull
  start_date <- as.character(as.Date(cycle_date, "%Y-%m-%d") - prev_days)
  output_message(sprintf("Claimant date range: [%s, %s]", start_date, cycle_date))

  # Now pull (claimant) x (incm) x (wage) data for qtr
  benf <- tryCatch(RJDBC::dbGetQuery(db$conn,
              sprintf(
                "-- Claimants certifying in the window
                with cla as (
                  select distinct claimt_id
                  from nrd.aggr_claimt_benf
                  where cert_perd_end_dt >= '%1$s'
                  and   cert_perd_end_dt <= '%2$s'
                ),
                -- STORED_VALUEs
                sv as (
                  select
                    claim.claimt_id,
                    cert.cert_qtr,
                    sum(incm.reptd_incm_amt) as incm
                  from (
                    select *, to_char(cert_perd_end_dt, 'YYYYQ')::int cert_qtr
                    from nrd.cert) cert
                  -- all inner joins to pull only claimants with income
                  inner join nrd.claim
                    on claim.claim_id = cert.claim_id
                  inner join nrd.claimt
                    on claimt.claimt_id = claim.claimt_id
                  inner join nrd.incm
                    on incm.cert_id = cert.cert_id
                  where cert.cert_qtr = '%3$d'
                  and incm.reglr_incm_flag is TRUE
                  group by claim.claimt_id, cert.cert_qtr
                ),
                -- WAGEs
                wg as (
                  select
                    claimt.claimt_id,
                    wage_rept.wage_qtr,
                    sum(wage.wage_amt) as wages
                  from (
                    select *, to_char(wage_dt, 'YYYYQ')::int as wage_qtr
                    from nrd.wage_rept) as wage_rept
                  inner join nrd.wage
                    on wage.wage_rept_id = wage_rept.wage_rept_id
                  inner join nrd.empl
                    on empl.empl_id = wage.empl_id
                  -- inner join: wage reports must belong to claimants
                  inner join nrd.claimt
                    on claimt.claimt_ref_num = empl.empl_ref_num
                  where wage_rept.wage_qtr = '%3$d'
                  group by claimt.claimt_id, wage_rept.wage_qtr
                )
                select
                  cla.claimt_id,
                  sv.cert_qtr as qtr,
                  sv.incm as stored_value,
                  wg.wages as tot_wage_amt
                from cla
                inner join sv
                  on cla.claimt_id = sv.claimt_id
                inner join wg
                  on cla.claimt_id = wg.claimt_id",
                start_date, cycle_date, qtr$qtr[[1]])),
              error = apm_error_handler)

  if ("error" %in% class(benf)) {
    return(32)
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
