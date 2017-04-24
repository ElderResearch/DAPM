
#' dt op
#' @name :=
#' @importFrom data.table :=
#' @export
NULL


###################### Wayne Code ##############################


#' Error handler for tryCatch calls.
#'
#' @return side effects
#'
#' @param e Error
#'
#' @author Wayne Folta
#' @note Needs global `APM_MODULE_NAME`.
#' @export

apm_error_handler <- function (e)
{
  cat (">>> ERROR:", APM_MODULE_NAME, "::", e$message, "\n")
  e
}


#' Output (log) messages
#'
#' \code{output_message} outputs a (log) message to stdout, optionally including
#' the amount of allocated RAM and optionally clearing the screen.
#'
#' @param mess string message to output
#' @param doMemory add the amount of RAM available to R
#' @param clear clear the screen (output ^L) before message
#'
#' @author Wayne Folta
#' @export

output_message <- function (mess, doMemory=FALSE, clear=FALSE)
{

  cat (sprintf ("====================  %s @ %s -- %s", APM_MODULE_NAME, date (), mess))

  if (doMemory)
    cat (sprintf (" (memory: %d)", memory.limit ()))

  cat ("\n")
}


#' Convert fields
#'
#' \code{convTF012} converts several varieties of input flag fields
#' to several kinds of output flag fields.
#'
#' Inputs can be:
#' boolean (TRUE, FALSE, NA), boolean strings ("t", "T", "f", "F", NA),
#' boolean numbers (0, 1, 2, where 0 is FALSE, 1 is TRUE, and 2 is NA).
#'
#' Outputs can be boolean (TRUE, FALSE, NA), numeric (0, 0.5, 1, where
#' 0 if FALSE, 1 is TRUE and 0.5 is NA), or factor ("T", "F", "NA").
#'
#' For example the SOM prefers numeric outputs, while the SVM prefers
#' factor. (Factor output also does not have NA's, which can cause
#' rows to be omitted in many models. What would be an NA is a factor
#' level "NA".)
#'
#' Currently there is an input_type and an output_type, but input_type
#' is ignored since the currently accepted types work without special
#' case handling. Of course output_type is critical, since each type
#' has special case handling.
#'
#' @return converted thingie
#'
#' @param varTF012 The input
#' @param input_type Input type (ignored for now)
#' @param output_type Output type ("numeric", "factor1", "boolean")
#'
#' @author Wayne Folta
#' @export

convTF012 <- function (varTF012, input_type, output_type)
{

  if (output_type == "numeric") {
    TF012_NA2 <- 0.5
    TF012_T1  <- 1.0
    TF012_F0  <- 0.0

  } else if (output_type == "factor1") {
    TF012_NA2 <- "NA"
    TF012_T1  <- "T"
    TF012_F0  <- "F"
    TF012_factors <- c("F", "T", "NA")
  } else if (output_type == "boolean") {
    TF012_NA2 <- NA
    TF012_T1  <- TRUE
    TF012_F0  <- FALSE
  } # else ERROR, BUT NOT A SEVERE ONE!!


  retTF012 <- rep (TF012_NA2, length (varTF012))  # Includes NA

  retTF012[varTF012 %in% c("t", "T")]  <- TF012_T1
  retTF012[varTF012 == TRUE]           <- TF012_T1  # Actually TRUE == 1 too, but...
  retTF012[varTF012 == 1]              <- TF012_T1


  retTF012[varTF012 %in% c("f", "F")]  <- TF012_F0
  retTF012[varTF012 == FALSE]          <- TF012_F0 # Actually FALSE == 0 too, but...
  retTF012[varTF012 == 0]              <- TF012_F0

  if (output_type == "factor1") retTF012 <- factor (retTF012, TF012_factors)

  retTF012
}


#' Full Factors
#'
#' \code{fullFactors} takes a vector of values/factors and turns
#   Them into a factor with a fixed set of levels, and a catch-all level.
#'
#'   This function takes a vector of values/factors and turns
#'   Them into a factor with a fixed set of levels, and a catch-all level.
#'   This is important for separate training and scoring scripts, for
#'   two reasons:
#'
#'   1. The data for scoring is often smaller than the training data
#'      and therefore may be missing levels comipared to the training
#'      data, so if you made a factor from the training data and then
#'      made dummy variables, you would end up missing some dummy
#'      variables, causing errors at scoring time.
#'
#'   2. We may find that the Top 10 most common values matter the
#'      most, so we create levels for them, and then a catch-all
#'      level to handle the rest. This is useful in its own right,
#'      but it's also possible that the data for scoring might
#'      have a rare level not seen in the training data, so throwing
#'      it into the catchall helps it to be seen by the model.
#'
#' @return converted thingie
#'
#' @param factorVar Vector of values to turn into factors
#' @param choices Vector of factor levels to be assigned
#' @param NAlevel Should the routine assign an "NA" level to missing values?
#'
#' @author Wayne Folta
#' @export

fullFactors <- function (factorVar, choices, NAlevel=TRUE) {
  factorVar <- as.character(factorVar)
  choices   <- as.character(choices)

  if (NAlevel) {
    choices <- c("NA", choices)

    if (any(is.na(factorVar))) {
      factorVar[is.na(factorVar)] <- "NA"
    }
  }

  factorVar[!(factorVar %in% choices)] <- tail(choices, 1)
  factor(factorVar, choices)
}


#' Convert fields to dummies
#'
#' \code{convMulti} turn a column into a set of dummy variables. The `model.matrix` function
# does this in a basic way, but we need something more sophisticated. See also
# `fullFactors`, which does some of the magic.
#'
#' @return converted thingie
#'
#' @param dTable the \code{data.table} object.
#' @param colName the column in the \code{data.table} to be converted to dummies
#' @param targetLevels the levels that the input factors (and therefore dummy columns)
#' are expected to have, with the final one being a catch-all.
#'
#' @author Wayne Folta
#' @export

convMulti <- function (dTable, colName, targetLevels) {

  a <- fullFactors (dTable[, get (colName)], targetLevels)
  targetLevels <- as.character (levels (a))

  b <- data.frame (a)
  colnames (b) <- colName

  m <- model.matrix (formula (paste ("~", colName, "-1")), b)
  colnames (m) <- paste (colName, targetLevels, sep="_")

  # Return a data.table, since it's needed to insert multiple columns in a
  # a data.table at once...

  data.table::data.table (m)
}


#' Input the Customer ID List
#'
#' \code{get_custIdList} reads in the customer ID list from \code{nrd.aggr_claimt_benf}.
#'
#' @return converted thingie
#'
#' @param conn Connection returned from \code{initialize_db}.
#' @param train.start the earliest year/quarter (numeric YYYYQ) to be read in.
#'
#' @author Wayne Folta
#' @export
#'
get_custIdList<- function (conn, train.start)
{

  custIdList <- tryCatch (RJDBC::dbGetQuery (conn, sprintf ("SELECT DISTINCT claimt_id FROM aggr_claimt_benf WHERE cert_perd_end_qtr_num >= %d", train.start)),
                          error=apm_error_handler)

  if ("error" %in% class (custIdList))
    return (custIdList)

  as.character (unlist (custIdList, use.names=FALSE))
}


#' Initialize the APM utilities
#'
#' \code{initialize_apm} initializes the utilities, including checking for the
#' existence of \code{APM_} variables that indicate we're called from Kettle, and
#' setting the \code{APM_MODULE_NAME} that is used for errors and logging.
#'
#' @return converted thingie
#'
#' @param module_name the module name to be set (globally) for error and logging
#'
#' @note If the global variable \code{APM_DB_NAME} exists, we've been called from
#' Kettle, which sets it (and other \code{APM_} variables). If it does not exist,
#' get the Windows System Environment variable \code{APM_WORKING} and use that to
#' find \code{nrdbPassword.R} and source it which will set the variables.
#'
#' @author Wayne Folta
#' @export

initialize_apm <- function (module_name)
{
  APM_MODULE_NAME <<- module_name

  # If run via Kettle, various APM_ variables will be pre-defined.
  # If run via RStudio, etc, we'll need to set these.

  if (!exists ("APM_DB_NAME"))
  {
    working <- Sys.getenv ("APM_WORKING")  # Must be defined in System Environment

    if (nchar (working) == 0)
    {
      cat (">>> ERROR: can't read env variable APM_WORKING in", APM_MODULE_NAME, "\n")
      return (-1)
    }

    output_message ("nrdbPassword.R Running")

    sys.source (paste (normalizePath (working, winslash = "/"), "nrdbPassword.R", sep="/"),
                envir=globalenv ())

    # Also set variable in case we are scoring at some point
    APM_WHOAMI <<- substr(as.character(Sys.info()["user"]), 1, 20)

  } else {
    if (!exists("APM_WHOAMI")) {
      APM_WHOAMI <<- "ETL"
    }
  }

  0
}

#' Initialize the DB
#'
#' \code{initialize_db} initializes the connection to the NRDB.
#'
#' @return conn the \code{dbConnect} returned value
#' @return DRV the \code{JDBC} returned value
#'
#' @author Wayne Folta
#' @export

initialize_db <- function ()
{
  options (java.parameters="-Xmx6g")   # Must be before anything tries to load Java!
  #require (RJDBC)

  DRV <- RJDBC::JDBC("org.postgresql.Driver", APM_DB_DRIVER)

  conn <- tryCatch (RJDBC::dbConnect (DRV,
                               sprintf ("jdbc:postgresql://%s:%s/%s", APM_SERVER_URL, APM_DB_PORT, APM_DB_NAME),
                               dbname=APM_DB_NAME, host=APM_SERVER_URL, port=APM_DB_PORT,
                               user=APM_DB_USERNAME, password=APM_DB_PASSWORD),
                    error=apm_error_handler)

  if ("error" %in% class (conn))
    return (conn)

  list (conn=conn, DRV=DRV)
}


#' Close the DB
#'
#' \code{close_db} closes the connection to the NRDB.
#'
#' @param db_conn the \code{db_conn} value returned from \code{initialize_db}
#'
#' @author Wayne Folta
#' @export

close_db <- function (db_conn)
{
  RJDBC::dbDisconnect (db_conn$conn)
  RJDBC::dbUnloadDriver (db_conn$DRV)
}


#' Read Benefits
#'
#' \code{read_overpayments} reads in the entire overpayments table (\code{nrd.aggr_claimt_ovpaymt}).
#'
#' @param conn the \code{conn} part of the \code{db_conn} value returned from \code{initialize_db}
#' @param custIdChosen a list of strings of customer IDs to be retrieved
#' @param custIdSize number of customer IDs in custIdChosen (I think)
#' @param train.start the earliest year/quarter (numeric YYYYQ) to retrieve
#'
#' @author Wayne Folta
#' @export

read_benefits <- function (conn, custIdChosen, custIdSize, train.start)
{
  benx <- data.table::data.table ()

  # Need to loop and read the benefits of 500 claimant ID's at a time,
  # otherwise the SQL query string is too long (with thousands of id's)

  for (i in seq (1, custIdSize, 500))
  {
    s <- custIdChosen[i:(i+499)]

    cat ("Querying ", i, " through ", i + 499, " now...\n")
    beny <- tryCatch (RJDBC::dbGetQuery (conn, sprintf ("SELECT * FROM aggr_claimt_benf WHERE claimt_id in (%s) AND cert_perd_end_qtr_num >= %d",
                                                 paste (sprintf ("%s", s), collapse=", "), train.start)),
                      error=apm_error_handler)

    if ("error" %in% class (beny))
      return (beny)

    benx <- data.table::rbindlist (list (benx, data.table::data.table (beny)))
  }

  benx[, claimt_id := as.character (claimt_id)] # Important for data.table!
  benx[, fending   := as.numeric (as.Date (cert_perd_end_dt) - as.Date ("2000-01-01"))]
  benx[, fstarting := fending - 6]  # six days, so it's a week

  data.table::setkey (benx, claimt_id, fstarting, fending)

  benx
}


#' Read Overpayments
#'
#' \code{read_overpayments} reads in the entire overpayments table (\code{nrd.aggr_claimt_ovpaymt}).
#'
#' @param conn the \code{conn} part of the \code{db_conn} value returned from \code{initialize_db}.
#'
#' @author Wayne Folta
#' @export

read_overpayments <- function (conn)
{
  op <- tryCatch (RJDBC::dbGetQuery (conn, "select * from aggr_claimt_ovpaymt"),
                  error=apm_error_handler)

  if ("error" %in% class (op))
    return (op)

  op <- data.table::data.table (op)

  op[, claimt_id  := as.character (claimt_id)] # Important for data.table!
  op[, fstarting  := as.numeric (as.Date (ovpaymt_start_dt)  - as.Date ("2000-01-01"))]
  op[, fending    := as.numeric (as.Date (ovpaymt_end_dt)    - as.Date ("2000-01-01"))]

  op <- op[fstarting <= fending]  # No bad records in my NRDB tests, but

  data.table::setkey (op, claimt_id, fstarting, fending)

  op
}


#' Set up Score Writing Transaction
#'
#' \code{db_begin} starts a score transaction that can be committed or rolled back.
#'
#' @param conn the \code{conn} part of the \code{db_conn} value returned from \code{initialize_db}.
#'
#' @author Wayne Folta
#' @export

db_begin <- function (conn) {

  s <- tryCatch (RJDBC::dbSendUpdate (conn, "BEGIN;"), error=apm_error_handler)

  if ("error" %in% class (s))
    return (s)

  0
}


#' Commit of Delete and Write Scores
#'
#' \code{db_commit} commits score writing.
#'
#' @param conn the \code{conn} part of the \code{db_conn} value returned from \code{initialize_db}.
#'
#' @author Wayne Folta
#' @export

db_commit <- function (conn) {

  s <- tryCatch (RJDBC::dbSendUpdate (conn, "COMMIT;"), error=apm_error_handler)

  if ("error" %in% class (s))
    return (s)

  0
}


#' Rollback of Delete and Write Scores
#'
#' \code{db_rollback} rolls back the score-writing transaction. Used in case of error.
#'
#' @param conn the \code{conn} part of the \code{db_conn} value returned from \code{initialize_db}.
#'
#' @author Wayne Folta
#' @export

db_rollback <- function (conn) {

  s <- tryCatch (RJDBC::dbSendUpdate (conn, "ROLLBACK;"), error=apm_error_handler)

  if ("error" %in% class (s))
    return (s)

  0
}


#' Determine if a table exists in a schema
#'
#' \code{table_exists} Determine if a table exists in a schema.
#'
#' @param conn the \code{conn} part of the \code{db_conn} value returned from \code{initialize_db}.
#' @param scoreTableName fully-qualified table to determine if it exists.
#'
#' @author Wayne Folta
#' @export

table_exists <- function (conn, scoreTableName) {

  pieces <- unlist (strsplit (scoreTableName, ".", fixed=TRUE))

  s <- tryCatch (RJDBC::dbGetQuery (conn,
                               sprintf ("SELECT EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema = '%s' AND table_name = '%s') ;",
                                        pieces[1], pieces[2])),
                 error=apm_error_handler)

  if ("error" %in% class (s)) {
    return (s)
  }

  s$exists == "t"
}


#' Delete (Old) Scores
#'
#' \code{delete_scores} deletes old scores from this Cycle Date.
#'
#' This code was modified so it's not an error to delete scores from a
#' non-existent score table. However, \code{write_scores} doesn't create
#' a table, so this turns out to be less than useful sometimes.
#'
#' @param conn the \code{conn} part of the \code{db_conn} value returned from \code{initialize_db}.
#' @param score_cd the the character string code of this score type.
#' @param cycle_date the cycle date (numeric YYYYQ) of the current cycle.
#' @param scoreTableName fully-qualified table from which to delete scores. Could be redirected
#'
#' @author Wayne Folta
#' @export

delete_scores <- function (conn, score_cd, cycle_date, scoreTableName) {

  do_delete <- table_exists (conn, scoreTableName)

  if ("error" %in% class (do_delete))
    return (do_delete)

  if (do_delete) {
  s <- tryCatch (RJDBC::dbSendUpdate (conn,
                               sprintf ("DELETE FROM %s WHERE score_cd = '%s' AND cycl_dt = '%s'",
                                        scoreTableName, score_cd, as.character (cycle_date))),
                 error=apm_error_handler)

  if ("error" %in% class (s))
    return (s)
  }

  0
}


#' Write Scores
#'
#' This function writes function scores out to a score table. The
#' \code{scoreTableName} defines where, and this table must already exist.
#'
#' \code{write_scores} writes out scores to a score table.
#'
#' @param claimt_ids vector of character claimant ids.
#' @param scores vector of numerical scores.
#' @param conn the \code{conn} part of the \code{db_conn} value returned from \code{initialize_db}.
#' @param score_cd the the character string code of this score type.
#' @param cycle_date the cycle date (numeric YYYYQ) of the current cycle.
#' @param scoreTableName fully-qualified table to which to write scores. This table must already exist.
#' @param timestamp character string timestamp of when this ran (more granular than Cycle Date).
#'
#' @author Wayne Folta
#' @export
write_scores <- function(claimt_ids, scores, conn, score_cd, cycle_date,
                         scoreTableName, timestamp) {

  for (i in 1:length (scores)) {
    sql <- paste("INSERT INTO", scoreTableName,
              "(cycl_dt, enty_type_cd, enty_id, score_cd, score_val, crt_tmstmp)",
              "VALUES (",
              "to_date ('", as.character (cycle_date), "', 'YYYY-MM-DD'),",  # cycl_dt
              "'CLAIMT',",                                                   # enty_type_cd   (is claimant score)
              claimt_ids[i], ",",                                            # enty_id
              sprintf ("'%s',", score_cd),                                   # score_cd   (SOM score)
              scores[i], ",",                                                # score_val
              "to_timestamp ('", timestamp, "', 'YYYY-MM-DD HH24:MI:SS')",   # crt_tmstmp
              ")")

    s <- tryCatch (RJDBC::dbSendUpdate(conn, sql), error=apm_error_handler)

    if ("error" %in% class (s)) {
      # RJDBC defaults to auto-commit, so this doesn't work and
      # I don't know how to fix it. Would also need a dbCommit (conn),
      # if no errors were encountered.
      #
      # dbRollback (conn)
      return (s)
    }
  }

  0
}


# Kenny code ###################################################################

#' Read in claimant data to create an edge list
#'
#' @return side effects
#' @param db_conn Database connection
#'
#' @author Micah Darrell
#' @export

get_pii_edgelist <- function (db_conn) {
  sql_query <- "select from_claimt_id, to_claimt_id, email_link_wgt_val, phon_link_wgt_val, passwd_link_wgt_val, orig_ip_link_wgt_val from aggr_claimt_shared_attr_edges"
  table <- data.table::data.table(RJDBC::dbGetQuery(db_conn$conn, sql_query))
}


#' Read in employer data to create an edge list
#'
#' @return side effects
#' @param db_conn Database connection
#' @param sql_quarter Quarter to anchor on for the edgelist
#'
#' @author Micah Darrell
#' @export

get_emp_edgelist <-  function (db_conn, sql_quarter) {
  sql_query <- paste("select claimt_id, emplr_id, empl_start_qtr_num, empl_end_qtr_num from aggr_claimt_emplr_hist where empl_start_qtr_num <= '", sql_quarter, "' and empl_end_qtr_num >= '", sql_quarter, "'", sep = "")
  emp_edgelist <- data.table::as.data.table(RJDBC::dbGetQuery(db_conn$conn, sql_query))

  employers <- data.table::as.data.table(table(emp_edgelist$emplr_id))
  names(employers) <- c("emplr_id", "emplr_cnt")
  employers$emplr_id <- as.numeric(employers$emplr_id)

  emp_edgelist <- merge(emp_edgelist, employers, by = "emplr_id")
  emp_edgelist2 <- emp_edgelist

  temp <- merge(emp_edgelist, emp_edgelist2, by = "emplr_id", allow.cartesian = TRUE)

  data.table::data.table(from_claimt_id = temp$claimt_id.x, to_claimt_id = temp$claimt_id.y, emp_weight = temp$emplr_cnt.x/(length(emp_edgelist2$claimt_id)))
}


#' Dummy function for testing demonstration
#'
#' @param a An arbitrary numeric input
#' @return A numeric value
#' @author Wayne Folta
fcn_test <- function(a) {
  a + 10
}


# Tom new code #################################################################
# The following are a few helper functions for (1) obtaining the current
# version of the model code using Git and (2) saving version information
# as an R data object for inclusion in, e.g., the binary we send to NTELX.
################################################################################

#' Return a string with the current git status.
#'
#' @return String, e.g., "1.01-90-g8c319fe".
#' @author Tom Shafer
find_git_version <- function() {
  vvv <- tryCatch(system2("git", c("describe", "--tags", "--dirty"),
                          stdout=TRUE, stderr=FALSE),
                  error=function(e) NA)

  # If not in a Git repo this returns a status, not an error :(
  if ("status" %in% names(attributes(vvv))) {
    vvv <- NA
  }

  return(vvv)
}

#' Return the model version stored in the APM package
#'
#' @return Character vector
#' @author Tom Shafer
#' @export
get_model_version <- function() {
  # model.version is a ./data/ object
  tryCatch(model.version, error=function(x) "NO_VERSION")
}

#' Store the model version as a data object
#'
#' @param model.version Character vector describing the models bundled
#' with the package.
#'
#' @author Tom Shafer
store_model_version <- function(model.version) {
  devtools::use_data(model.version, overwrite=T)
}

## Train/Test/Holdout table creation ###########################################
################################################################################

#' Count rows in any table
#'
#' @param conn Database connection handle.
#' @param tbl  Database table name.
#'
#' @return An integer number of rows in the table.
#' @author Tom Shafer
count_rows <- function(conn, tbl) {
  return(as.integer(RJDBC::dbGetQuery(conn, sprintf("select count(*) from %s", tbl))))
}

#' Form an SQL string excluding tables of claimants from a query
#'
#' This returns something like:
#'    and claimt_id not in (select distinct claimt_id from A)
#'    and claimt_id not in (select distinct claimt_id from B)
#'    ...
#'
#' @param the_list List of tables to exclude.
#'
#' @return SQL string of exclusion clauses.
#' @author Tom Shafer
excl_tables <- function(the_list=NULL) {
  excl_list_str <- ""
  if (!is.null(the_list)) {
    excl_list_str <- paste(
      sapply(the_list,
        function(x) {
          sprintf("and claimt_id not in (select distinct claimt_id from %s)", x)
        }), collapse=" ")
  }
  return(excl_list_str)
}

#' Merge a list to a comma-separated string, prepending a table name
#'
#' E.g. c("x", "y") -> "TBL.x, TBL.y"
#'
#' @param vec Character vector of column names.
#' @param tbl Database table name.
#'
#' @return String containing comma-joined column names.
#' @author Tom Shafer
vec_to_tbl_cols <- function(vec, tbl) {
  return(paste(tbl, ".", vec, sep="", collapse=", "))
}

#' Draw a random sample from nrd.aggr_claimt_benf
#'
#' Sample nrd.aggr_claimt_benf, possibly excluding claimants from a list of
#' tables, and left join with the benefits table to form an ABT.
#'
#' @param conn            Database connection handle.
#' @param start_qtr       Start quarter number.
#' @param end_qtr         End quarter number (inclusive).
#' @param n_records       Number of records to sample.
#' @param tbl_name        Name (with schema) of the table to create.
#' @param exclude_claimts Character vector of tables whose claimants should not
#'                        be included.
#' @param target_cols     Character vector of columns to join from the
#'                        overpayment table.
#'
#' @return Integer with the number of rows in the specified table.
#' @author Tom Shafer
make_sample_table_with_n_records <- function(conn, start_qtr, end_qtr,
                                             n_records, tbl_name,
                                             exclude_claimts=NULL,
                                             target_cols=c("ovpaymt_amt")) {
  # exclude_claimts should be a character vector or string
  excl_list_str <- excl_tables(exclude_claimts)

  # Join on the overpayments table for targets
  targets <- vec_to_tbl_cols(target_cols, tbl="b")

  # Run the script...
  sql <- trimws(
    sprintf("
      create table %1$s as
      select a.*, %2$s
      from (
        select * from nrd.aggr_claimt_benf
        where cert_perd_end_qtr_num between %3$d and %4$d
        %5$s
      ) a
      left join nrd.aggr_claimt_ovpaymt b
      on  a.claimt_id = b.claimt_id
      and a.cert_perd_end_dt between b.ovpaymt_start_dt and b.ovpaymt_end_dt
      where b.ovpaymt_start_qtr_num >= %3$d
      or    b.ovpaymt_start_qtr_num is null
      order by random()
      limit %6$d;",
      tbl_name, targets, start_qtr, end_qtr, excl_list_str, n_records))

  cat(sql, "\n")
  RJDBC::dbSendUpdate(conn, sql)

  print(sprintf("Returned %d rows", as.integer(count_rows(conn, tbl_name))))
}

#' Draw all records from nrd.aggr_claimt_benf for a random sample of claimants
#'
#' Sample nrd.aggr_claimt_benf, possibly excluding claimants from a list of
#' tables, and left join with the benefits table to form an ABT.
#'
#' @param conn            Database connection handle.
#' @param start_qtr       Start quarter number.
#' @param end_qtr         End quarter number (inclusive).
#' @param n_claimts       Number of claimants to sample.
#' @param tbl_name        Name (with schema) of the table to create.
#' @param exclude_claimts Character vector of tables whose claimants should not
#'                        be included.
#' @param target_cols     Character vector of columns to join from the
#'                        overpayment table.
#'
#' @return Integer with the number of rows in the specified table.
#' @author Tom Shafer
make_sample_table_with_n_claimts <- function(conn, start_qtr, end_qtr,
                                             n_claimts, tbl_name,
                                             exclude_claimts=NULL,
                                             target_cols=c("ovpaymt_amt")) {
  # SQL phrase to exclude claimants appearing in specified tables
  excl_str <- excl_tables(exclude_claimts)

  # Target columns
  targets <- vec_to_tbl_cols(target_cols, tbl="b")

  # ABT
  sql <- trimws(
    sprintf("
      create table %1$s as
      -- Select all possible inputs, chosen targets
      select a.*,
             %2$s
      from (
        -- Select all benefit records from a random sample of claimants
        -- in this date window, possibly excluding the claimants that
        -- appear in other tables (e.g., for train vs. test samples)
        select *
        from nrd.aggr_claimt_benf
        where claimt_id in (
          select claimt_id
          from (
            select distinct claimt_id
            from nrd.aggr_claimt_benf
            where cert_perd_end_qtr_num between %3$d and %4$d
            %6$s
          ) c
          order by random()
          limit %5$d
        )
        and cert_perd_end_qtr_num between %3$d and %4$d
      ) a
      -- Left join the targets,
      -- We only get a non-null join for a claim if overpayment happened on
      -- that particular claim
      left join nrd.aggr_claimt_ovpaymt b
      on  a.claimt_id = b.claimt_id
      and a.cert_perd_end_dt between b.ovpaymt_start_dt and b.ovpaymt_end_dt
      where b.ovpaymt_start_qtr_num >= %3$d
      or    b.ovpaymt_start_qtr_num is null;",
      tbl_name, targets, start_qtr, end_qtr, n_claimts, excl_str))

  cat(sql, "\n")
  RJDBC::dbSendUpdate(conn, sql)

  print(sprintf("Returned %d rows", as.integer(count_rows(conn, tbl_name))))
}


## New refactoring code by Tom #################################################

#' Report and scrub rows with NA values
#'
#' @param ov Benefits + overpayments (usually a data table or data frame)
#' @return Object stripped of rows containing NA values
#' @author Wayne Folta, Stuart Price; refactored by Tom Shafer
na_check_and_omit <- function(ov) {
  # Count NAs
  ov.NA <- na.omit(ov, invert=TRUE)
  print(paste("Initial dim(ov):", paste(dim(ov), collapse=", ")))
  print(paste("NA      dim(ov):", paste(dim(ov.NA), collapse=", ")))

  # Scrub any NAs
  ov <- na.omit(ov)
  print(paste("Final   dim(ov):", paste(dim(ov), collapse=", ")))

  return(ov)
}

#' Log the number of NAs as an APM message
#'
#' @param df A data frame
#' @return 0
#' @export
na_output_message <- function(df) {
  n.na <- nrow(na.omit(df, invert=TRUE))
  if (n.na > 0) {
    output_message(sprintf("NOTICE: Found %d row%s containing NA values",
                           n.na, ifelse(n.na==1, "", "s")))
  }
}

#' Remove constant predictors
#'
#' Remove predictors which have constant values across all observations.
#'
#' @param df The input data frame.
#' @return The updated data frame.
#' @author Wayne Folta, Stuart Price; refactored by Tom Shafer
remove_flat_vars <- function(df) {
  # Must be a data frame
  df <- as.data.frame(df)
  flat <- which(apply(df, 2, function(x) max(x) == min(x)))

  if(length(flat) > 0) {
    cat("Eliminating constant training variables, columns:",
        colnames(df)[flat], flat, "\n")
    df <- df[, -flat]
  }

  return(df)
}

#' Downsample predictors and targets
#'
#' Try to obtain a more balanced training set by removing (randomly)
#' some extra observations not marked as overpayments.
#' The default ratio non-op:op is 4:1.
#'
#' @param x Data frame of predictors (aggregated benefits).
#' @param y Vector of targets (overpayments).
#' @param false_ratio Ratio of non-op to op. Default is 4.
#'
#' @return Combined, downsampled data frame with the targets appended
#'    as column 'op'.
#'
#' @author Wayne Folta, Stuart Price; refactored by Tom Shafer
downsample_and_factor <- function(x, y, false_ratio=4) {
  x <- as.data.frame(x)
  y <- as.vector(y)

  subT <- which(y == TRUE)
  subF <- which(y == FALSE)

  subT_balanced <- sample(subT, length(subT))
  subF_balanced <- sample(subF, false_ratio*length(subT))

  print(sprintf("Downsampled T count: %d", length(subT_balanced)))
  print(sprintf("Downsampled F count: %d", length(subF_balanced)))

  df    <- x[c(subT_balanced, subF_balanced), ]
  df$op <- factor(y[c(subT_balanced, subF_balanced)])

  return(df)
}

#' Impute numeric variables for statistical models
#'
#' For each column in the data frame: if it should be imputed (if it appears in
#' impute.list and is not NA there), replace all NA values with the specified
#' value from impute.list. Otherwise, just return the original column.
#'
#' @param df Data frame (or data table, we convert here)
#' @param impute.list List(col.name=impute.value) of columns and their
#'    imputation values. If impute.value=NA, this is treated as if the column
#'    were not specified at all.
#'
#' @return Data frame in which requested NAs are replaced with imputed values.
#'
#' @author Tom Shafer
#' @export
impute_model_vars <- function(df, impute.list) {
  # Also see: http://stackoverflow.com/a/24797297/656740

  n.imp.cols <- sum(!is.na(impute.list))
  print("Running impute_model_vars()")
  print(paste0("Found ", n.imp.cols, " col", ifelse(n.imp.cols == 1, "", "s"),
    " set up for imputation",
    ifelse(n.imp.cols > 0, ": ", ""),
    paste0(names(which(!is.na(impute.list))), collapse=", ")))

  # Ensure we are working with a data frame (i.e. not a data table)
  our.df <- as.data.frame(df)

  data.frame(
    lapply(names(our.df),
      function (n) {
        if (n %in% names(impute.list) && !is.na(impute.list[[n]])) {
          # Recommended in R's ifelse docs; this consistently returns a
          # data frame (ifelse can do funny vectorized things we don't want)
          tmp <- our.df[n]
          tmp[is.na(tmp[n])] <- impute.list[[n]]

          n.imp <- sum(is.na(our.df[n])) - sum(is.na(tmp[n]))
          print(paste0("Imputed column: ", n, " -> ", impute.list[[n]],
            " (", n.imp, " replacement", ifelse(n.imp == 1, "", "s"), ")"))

          tmp
        } else {
          our.df[n]
        }
      }))
}

#' Store state-specific model inputs and parameters
#'
#' Creates a list of lists. The outer list will be created for each state and
#' will include a list for each model. The list for each model has two entries:
#'   1. Vector of input variables to use for that model
#'   2. List of model parameters
#'
#' @param state_inputs A list of list objects, one for each model
#'
#' @return Saves a list object to APM/Data that will be used to train and score
#' models
#'
#' @author Daniel Brannock
#' @export
store_state_inputs <- function(state_inputs) {
  # Do some checks to make sure that it is structured correctly
  # Contains a list for RF, SVM, and kNN
  if(!identical(c("KNN", "RF", "SVM"), sort(names(state_inputs))))
    return(paste("ERROR: Need a (named) entry for each model"))

  devtools::use_data(state_inputs, overwrite = TRUE)

  return(0)
}

#' Transformation function to control max value for a variable
#'
#' Caps variables at a given maximum. Necessary because some payments are
#' duplicated without a good way to filter out the bad ones. But no person was
#' legitimately paid more than the maximum payment amount.
#'
#' @param var Input to be capped
#' @param cap Value to be capped at
#'
#' @return The capped variable
#'
#' @author Daniel Brannock
#' @export
max_cap <- function(var, cap) ifelse(var > cap, cap, var)

#' Transformation function to take a log
#'
#' This function does two things.
#'   1. Ensures that the minimum value for a variable is not above a predefined
#'      minimum. This is critical for model robustness, since new unexpected
#'      negative values would otherwise break the log function (we've seen
#'      these in NY).
#'   2. Brings all values above zero to allow them to be logged.
#'
#' @param var Input to be logged
#' @param minval Minimum allowed value
#'
#' @return The capped variable
#'
#' @author Daniel Brannock
#' @export
min_log <- function(var, minval) {
  var <- ifelse(var < minval, minval, var)
  log(var - minval + 1)
}

#' Impute values by reference within a data table
#'
#' For each key in the list, use data table for imputation. This works by
#' selecting the correct row (\code{dt[is.na(dt[[key]]), ...])} and then
#' imputing via data.table's \code{ov[, c(<chr>) := <...>]} syntax.
#'
#' @param dt A data table.
#'
#' @param impute.list A list for which keys are column names in the data table
#'        and values are their imputed values.
#'
#' @return Returns an object with "error" in \code{class(obj)} if
#'         \code{tryCatch} detects a problem, otherwise a list of messages.
#'
#' @author Tom Shafer
#'
#' @export
#'
impute_values_dt <- function(dt, impute.list) {

  tryCatch(
    lapply(names(impute.list),
      function(key) {
        if (key %in% colnames(dt)) {
          val <- impute.list[[key]]

          # Count how many replacements we will make
          n.rep <- sum(is.na(dt[[key]]))

          # Impute
          dt[is.na(dt[[key]]), c(key) := val]

          # Status message
          print(sprintf("Imputed column: %s -> %s (made %d replacement%s)",
                 as.character(key), as.character(val), n.rep,
                 ifelse(n.rep == 1, "", "s")))
        }
      }),
    error=apm_error_handler) -> err

  return(err)

}



#' Transform columns by reference within a data table
#'
#' @param dt A data table.
#'
#' @param transform.list A list for which keys are column names in the data
#'        table and values are functions to apply.
#'
#' @return Returns an object with "error" in \code{class(obj)} if
#'         \code{tryCatch} detects a problem, otherwise a list of messages.
#'
#' @author Tom Shafer
#'
#' @export
#'
transform_inputs_dt <- function(dt, transform.list) {

  tryCatch(
    lapply(names(transform.list),
      function(col) {
        if (col %in% colnames(dt)) {
          func <- transform.list[[col]]

          # This is a bit hacky, but now we can use not-bare column names
          dt[, c(col) := func(dt[[col]])]

          # Print a status message
          print(sprintf("Transforming column %s", as.character(col)))
        }
      }),
    error=apm_error_handler) -> err

  return(err)

}


#' Fetch a model parameter from the NRDB
#'
#' @param con Database connection.
#' @param param Parameter name in \code{ref.score}.
#'
#' @return Parameter or \code{NULL}.
#' @export
fetch_param_from_nrdb <- function(con, param) {
  db_param <- RJDBC::dbGetQuery(con,
                sprintf("select parm_val from ref.parm
                         where parm_cd = '%s' limit 1", param))

  if (nrow(db_param) > 0) {
    return(db_param[[1, 1]])
  }

  # If no entry in the database, return NULL
  NULL
}


#' Assign a parameter or optionally fill from NRDB
#'
#' @param param Parameter name in the NRDB.
#' @param con NRDB connection object.
#' @param input User-assigned input. Non-\code{NULL} inputs short circuit
#'   NRDB assignment.
#' @return A parameter.
#' @export
set_param <- function(param, con, input = NULL) {
  if (!is.null(input)) {
    output_message(sprintf("PARAMETER: %s = %s", param, input))
    return(input)
  }

  # Assign a value to return_value from the NRDB.
  # If a value cannot be found, return an error.
  return_value <- NULL

  tryCatch({
    return_value <- fetch_param_from_nrdb(con = con, param = param)
    if (is.null(return_value)) {
      stop(sprintf(
        "The parameter '%s' could not be read from ref.parm", param))
    }
  }, error = apm_error_handler) -> s

  if ("error" %in% class(s)) {
    return(s)
  }

  output_message(sprintf(
    "PARAMETER: %s = %s from ref.parm", param, return_value))

  return(return_value)
}

#' Assign a parameter or optionally fill from NRDB
#'
#' @param ... Arguments to pass to \code{set_param}.
#' @return A numeric.
#' @export
#' @examples
#' \dontrun{
#' i <- set_param_num(param = "K", con = db_conn$conn, input = k)
#' }
set_param_num <- function(...) {
  return_value <- set_param(...)

  if ("error" %in% class(return_value))
    return(return_value)

  return(as.numeric(return_value))
}

#' Assign a parameter or optionally fill from NRDB
#'
#' @param ... Arguments to pass to \code{set_param}.
#' @return A boolean.
#' @export
#' @examples
#' \dontrun{
#' j <- set_param_lgl(param = "EXCLUDE_CELLULAR", con = db_conn$conn)
#' }
set_param_lgl <- function(...) {
  return_value <- set_param(...)

  if ("error" %in% class(return_value))
    return(return_value)

  return(as.logical(return_value))
}
