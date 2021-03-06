#' Distance Risk Module
#'
#' @description
#' This module is intended to replace the Location Risk Module used in Kansas
#' and Idaho. New York did not find the Location Risk Module so helpful because,
#' on one hand, many claimants file legally from nearby states and Canada
#' while, on the other hand, some places within the state may be high risk if
#' they are far from a claimant's home address.
#'
#' The location of the home address is pulled from the zip code given in
#' \code{nrd.addr}, which is then matched with a database containing zip codes
#' and corresponding latitude and longitude coordinates. The home address
#' location is then compared to the lat/long coordinates for the current
#' certification's IP address to get the distance in miles. The zip code
#' database containing lat/lon coordinates is from federalgovernmentzipcodes.us
#' (version updated 1/22/2012). The current certification's IP address is
#' determined from \code{nrd.aggr_claimt_sessn}.
#'
#' The base unit is a single certificaiton (one week) for a claimant, but some
#' cumulative information is collected for the entire claim. The final score
#' reported uses the average between two numbers:
#'   1. Distance from home as described above
#'   2. Average distance between all certifications in the claim
#' and is scaled from 0 to 100.
#'
#' @param cycle_date The cycle date being scored
#' @param scoreTableName The fully-qualified table into which to place the
#'   scores. If \code{scoreTableName=NULL}, return a data frame instead of
#'   writing to the database.
#' @param inputDF A data frame of certifications to score. If
#'   \code{inputDF=NULL} (the default), this function scores certifications from
#'   the database. This function  assumed the data was taken from an SQL query
#'   like that in the source.
#' @param exclude_cellular Whether or not the module should exclude records
#'   identified as originating from cellular IPs.
#' @param addr_type_cd NRDB type code for the address to use in the query (e.g.,
#'   `RESD`, `PHYS`, etc.)
#'
#' @return Returns 0 on success, >0 on failure, look at the \code{return}
#'   statements to identify where the failure occurred.
#' @author Daniel Brannock
#' @export
distance_score <- function(cycle_date = NULL, scoreTableName = "nrd.enty_score",
                           inputDF = NULL, exclude_cellular = NULL,
                           addr_type_cd = NULL) {

  # Basic APM init stuff
  if (initialize_apm("Distance IP Scoring") != 0)
    return(1)

  output_message("Start", doMemory=TRUE, clear=TRUE)

  if (is.null(cycle_date))
    cycle_date <- APM_CYCL_DT

  db_conn <- initialize_db()

  if ("error" %in% class(db_conn))
    return(2)


  # Parameter setting
  exclude_cellular <- set_param_lgl(param = "EXCLUDE_CELLULAR",
                                    con = db_conn$conn,
                                    input = exclude_cellular)

  if ("error" %in% class(exclude_cellular)) {
    return(21)
  }
  
  addr_type_cd <- set_param_chr(param = "TRAVEL_ADDR_TYPE_CD",
                                con = db_conn$conn, input = addr_type_cd)
  
  if ("error" %in% class(addr_type_cd)) {
    return(22)
  }


  if (is.null(inputDF)) {
    output_message("Load Data from NRDB")
    
    sql <- sprintf(
      "-- Claimants certifying in the last 90 days
      with t_claimt as (
        select claimt_id, max(claim_start_dt) as claim_start_dt
        from nrd.aggr_claimt_benf
        where cert_perd_end_dt >= ('%1$s'::date - 90)
        and cert_perd_end_dt <= ('%1$s'::date)
        group by claimt_id
      ),
      -- All session info for 2 years, excluding CEL if requested
      t_sessn as (
        select
          claimt_id, claimt_sessn_tmstmp, sessn_enty_type_cd,
          ip_lat_val, ip_long_val
        from nrd.aggr_claimt_sessn
        where claimt_sessn_tmstmp >= ('%1$s'::date - 730)
        -- Optional CEL exclusion
        %2$s
      ),
      -- All address data for two years
      t_addr as (
        select enty_id as claimt_id, eff_dt, exp_dt, zip_cd
        from nrd.addr_idfr
        join nrd.addr
          on addr_idfr.addr_id = addr.addr_id
        where enty_type_cd = 'CLAIMT'
        -- Claimant address type: PHYS, RESD, MAIL, ...
        and addr_type_cd = '%3$s'
        and (exp_dt is null or exp_dt >= ('%1$s'::date - 730))
      )
      select
        t_claimt.claimt_id, claim_start_dt,
        claimt_sessn_tmstmp, sessn_enty_type_cd, ip_lat_val, ip_long_val,
        eff_dt, exp_dt, zip_cd
      from t_claimt
      left join t_sessn
        on t_claimt.claimt_id = t_sessn.claimt_id
        and claimt_sessn_tmstmp >= (claim_start_dt - 7)
      left join t_addr
        on t_claimt.claimt_id = t_addr.claimt_id
        and eff_dt <= claimt_sessn_tmstmp
        and (exp_dt >= claimt_sessn_tmstmp or exp_dt is null)",
      cycle_date,
      ifelse(exclude_cellular,
             "and (ip_conn_type_cd is null or ip_conn_type_cd != 'CEL')",
             ""),
      addr_type_cd)
    
    ov <- tryCatch(data.table::data.table(RJDBC::dbGetQuery(db_conn$conn, sql)),
                   error = apm_error_handler)
    
    if ("error" %in% class(ov))
      return(23)
    
  } else {
    output_message(paste("Using Input data.frame"))
    ov <- data.table::data.table(inputDF)
  }

  ## Filter out people with no certification IP addresses
  temp <- ov[!is.na(claimt_sessn_tmstmp)]

  if ("error" %in% class(temp))
    return(3)


  output_message("Match ZIP Codes to Home Address")

  ## Merge lat/long from zip for home address
  temp <- merge(temp, zipcodes, by = "zip_cd", all.x = TRUE)
  data.table::setnames(temp, c("Lat","Long"), c("home_lat","home_long"))

  # Drop  out those people whose zip code did not have a corresponding coordinate
  temp <- temp[!is.na(home_lat)]


  output_message("Calculate Distances")

  ## We want two distances:
  ##   1. Distance from home address (from claim zip)
  ##   2. Distance from previous certifications (either sum or average)

  ## 1. Distance from home address (convert from meters to miles)
  home.mat <- matrix(c(temp$home_long, temp$home_lat), ncol = 2, nrow = nrow(temp))
  sess.mat <- matrix(c(temp$ip_long_val, temp$ip_lat_val), ncol = 2, nrow = nrow(temp))
  home.dist <- geosphere::distGeo(home.mat, sess.mat)/1609.34
  temp[, IPA_CTR_HOM := home.dist]

  if ("error" %in% class(temp))
    return(4)


  ## 2. Distance from previous certifications
  temp <- temp[order(claimt_id, claimt_sessn_tmstmp)]
  temp[, prev.long := data.table::shift(ip_long_val, 1), by = "claimt_id"]
  temp[, prev.lat  := data.table::shift(ip_lat_val, 1), by = "claimt_id"]
  curr.mat <- matrix(c(temp$ip_long_val, temp$ip_lat_val), ncol = 2, nrow = nrow(temp))
  prev.mat <- matrix(c(temp$prev.long, temp$prev.lat), ncol = 2, nrow = nrow(temp))
  trav.dist <- geosphere::distGeo(curr.mat, prev.mat)/1609.34
  temp[, trav.dist := trav.dist]

  ## Calculate average and total distance travelled (only report out average for now)
  temp[, IPA_CTR_AVG := mean(trav.dist, na.rm = TRUE), by = "claimt_id"]
  #temp[, sum.dist := sum(trav.dist, na.rm = TRUE), by = "claimt_id"]

  ## Keep only the most recent session for each claimt
  temp <- unique(temp, by = c("claimt_id"), fromLast = TRUE)

  ## Find average between the distance travelled and home distance then standardize from 0 - 100
  temp[, score := mean(c(IPA_CTR_HOM, IPA_CTR_AVG), na.rm = TRUE), by = "claimt_id"]
  temp[, l_score := log(score + 1)]
  temp[, IPA_CTR := (l_score - min(l_score, na.rm = TRUE)) *
            (100/(max(l_score, na.rm = TRUE) - min(l_score, na.rm = TRUE)))]

  ## Make ave.dist and home.dist separate score types and convert to "long" format
  temp2 <- data.table::melt(temp, c("claimt_id"), c("IPA_CTR_HOM", "IPA_CTR_AVG", "IPA_CTR"))
  temp2 <- temp2[!is.nan(value) & !is.na(value)]  # Remove NAs
  temp.home <- temp2[variable == "IPA_CTR_HOM"]
  temp.ave  <- temp2[variable == "IPA_CTR_AVG"]
  temp.ctr  <- temp2[variable == "IPA_CTR"]

  if ("error" %in% class(temp2))
    return(5)


  if (is.null(scoreTableName)) {
    output_message("Returning Scores as data.frame")
    df.score <- data.frame(claimt_id = temp2$claimt_id,
                           score_cd  = temp2$variable,
                           score_val = temp2$value,
                           cycle_date = cycle_date,
                           timestamp = Sys.time(),
                           stringsAsFactors = F)
    return(df.score)
  }


  output_message("Begin Transaction")

  s <- db_begin(db_conn$conn)

  if ("error" %in% class(s))
    return(6)


  output_message("Delete Previous Scores")

  s <- delete_scores(db_conn$conn, "IPA_CTR_HOM", cycle_date, scoreTableName)
  s <- delete_scores(db_conn$conn, "IPA_CTR_AVG", cycle_date, scoreTableName)
  s <- delete_scores(db_conn$conn, "IPA_CTR",     cycle_date, scoreTableName)

  if ("error" %in% class(s))
    return(7)


  output_message("Output Scores")

  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  s <- tryCatch(write_scores(temp.home$claimt_id, temp.home$value, db_conn$conn, "IPA_CTR_HOM",
                             cycle_date, scoreTableName, timestamp),
                error=apm_error_handler)

  s <- tryCatch(write_scores(temp.ave$claimt_id,  temp.ave$value,  db_conn$conn, "IPA_CTR_AVG",
                             cycle_date, scoreTableName, timestamp),
                error=apm_error_handler)

  s <- tryCatch(write_scores(temp.ctr$claimt_id,  temp.ctr$value,  db_conn$conn, "IPA_CTR",
                             cycle_date, scoreTableName, timestamp),
                error=apm_error_handler)

    if  ("error" %in% class(s)) {
    db_rollback(db_conn$conn)   # Don't check because can't do anything
    return(8)
  }

  s <- tryCatch(db_commit(db_conn$conn), error=apm_error_handler)

  if ("error" %in% class(s))
    return(9)

  close_db(db_conn)


  output_message("End")
  return(0)
}
