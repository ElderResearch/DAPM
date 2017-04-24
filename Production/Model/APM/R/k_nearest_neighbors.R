#' Train a DAPM k-Nearest-Neighbors Model
#'
#' This is a simpler, more opinionated training function than has been included
#' in DAPM previously, but this routine will extend more easily to multiple
#' states with different data.
#'
#' Assumptions:
#'
#' 1. The input data \code{df} is already balanced with respect to the target.
#' 2. The input data \code{df} contains *only* the (a) target and (b) features
#'    used to predict the target. These columns will be the only ones used in
#'    the final model.
#'
#' @param df A not-transformed data frame of predictors *plus* an additional
#'        boolean column named \code{target}.
#'
#' @param transform.funcs A list of APM-available functions that will be applied
#'        to the input data both for training and for scoring. No transformation
#'        will be applied to columns that do not have corresponding keys
#'        in this list.
#'
#' @param impute.vals A list of values that will be imputed to missing
#'        observations in both the training and scoring data. No imputation will
#'        be performed for columns that do not have corresponding keys.
#'
#' @return Returns 0 on success and an integer > 0 if an error occured. These
#'         integer codes should be unique.
#'
#' @author Tom Shafer
#'
#' @export
#'
k_nearest_neighbors_train <- function(df,
                                      transform.funcs = list(),
                                      impute.vals     = list()) {

  ##############################################################################
  ## Initialize
  ##############################################################################

  # Random seed for reproducible models, defined in vars.R
  set.seed(random_seed)


  if(initialize_apm("KNN Model Training") != 0) {
    return(10)
  }

  output_message("Start", doMemory=TRUE, clear=TRUE)


  # Use data table for in-memory processing
  ov <- data.table::data.table(df)



  ##############################################################################
  ## Clean the training data
  ##############################################################################

  output_message("Pre-processing data")

  # Basic tests of the data frame
  # ----------------------------------------------------------------------------

  # There should be a single column in the data named "target"
  if (!("target" %in% names(ov))) {
    return(21)
  }


  output_message(sprintf("Training with columns: %s",
                    paste(colnames(df), collapse=", ")))


  # Carry out imputations
  # ----------------------------------------------------------------------------

  # Check on whether imputation is even required
  if (length(impute.vals) > 0) {
    err <- impute_values_dt(ov, impute.vals)

    if ("error" %in% class(err)) {
      return(22)
    }
  }


  # Do transformations
  # ----------------------------------------------------------------------------

  if (length(transform.funcs) > 0) {
    err <- transform_inputs_dt(ov, transform.funcs)

    if ("error" %in% class(err)) {
      return(23)
    }
  }


  # There should not be NAs now
  # ----------------------------------------------------------------------------
  if (any(colSums(is.na(ov)) > 0)) {
    print("Some columns have NAs!")
    print(paste(names(which(colSums(is.na(ov)) > 0))), collapse=", ")

    return(24)

  } else {
    print("No NAs found; continuing.")
  }


  # The target must be a factor
  ov[, target := factor(target)]



  ##############################################################################
  ## Train the model
  ##############################################################################

  output_message("Train the model")

  tryCatch({

    features <- setdiff(colnames(ov), "target")

    y.train <- ov$target
    x.train <- data.frame(ov[, features, with=FALSE])

    x.scale <- caret::preProcess(x.train)
    x.train <- caret::predict.preProcess(x.scale, x.train)

  }, error=apm_error_handler) -> err

  if("error" %in% class(err)) {
    return(41)
  }



  ##############################################################################
  ## Store the model object
  ##############################################################################

  # The following objects are stored:
  #
  # - x.train
  # - y.train
  # - x.scale
  # - features
  # - transform.funcs
  # - impute.values

  s <- tryCatch({

    knn.storage <- list(x.train = x.train,
                        y.train = y.train,
                        x.scale = x.scale,
                        features = features,
                        transform.funcs = transform.funcs,
                        impute.vals = impute.vals)

    devtools::use_data(knn.storage, overwrite=TRUE)

  }, error=apm_error_handler)

  if ("error" %in% class(s)) {
    return(51)
  }


  output_message("End")
  return(0)

}



#' Score Certifications with a DAPM k-Nearest-Neighbors Model
#'
#' This is a simpler, more opinionated scoring function than has been included
#' in DAPM previously, but this routine will extend more easily to multiple
#' states with different data.
#'
#' Assumptions:
#'
#' 1. The input data (either from \code{aggr_claimt_benf} or \code{inputDF}) is
#'    raw (not transformed).
#' 2. A k-Nearest-Neighbors model object already exists and can be lazy
#'    loaded. This object will contain:
#'     - A list of functions to transform the raw data
#'     - A list of values to be imputed in the case of missing data
#'     - A list of features to be included in the scoring
#'
#' Given these assumptions, this function will:
#'
#' 1. Take in raw data
#' 2. Impute missing values
#' 3. Subset down to ID vars and predictors
#' 4. Transform the predictors
#' 5. Score with the model
#' 6. Return scores
#'
#' The source of the raw data and the final destination of the scores are
#' controlled by input parameters.
#'
#' @param cycle_date The current cycle date for scoring.
#' @param prevDays Number of days to look backwards for a certification filed by
#'   a claimant. Claimants who do not have a certification within this range
#'   will not be scored, but claimants who do have a certification within this
#'   range will have the most recent filing scored.
#' @param inputDF A data frame of certifications to score. If
#'   \code{inputDF=NULL} (the default), this function scores certifications from
#'   the database. This function  assumed the data was taken from an SQL query
#'   like that in the source.
#' @param scoreTableName The fully-qualified table into which to place the
#'   scores. If \code{scoreTableName=NULL}, return a data frame instead of
#'   writing to the database.
#' @param k The number of neighbors to take into consideration. If \code{NULL},
#'   the value is taken from \code{ref.score.K}.
#'
#' @return Returns 0 on success and an integer > 0 if an error occured. These
#'   integer codes should be unique.
#' @author Tom Shafer
#' @export
#'
k_nearest_neighbors_score <- function(cycle_date     = NULL,
                                      prevDays       = 90,
                                      inputDF        = NULL,
                                      scoreTableName = "nrd.enty_score",
                                      k              = NULL) {

  ##############################################################################
  ## Initialize
  ##############################################################################

  # Random seed for reproducible models, defined in vars.R
  set.seed(random_seed)


  if(initialize_apm("KNN Model Scoring") != 0) {
    return(11)
  }

  output_message("Start", doMemory=TRUE, clear=TRUE)


  # The cycle date controls the scoring; this is set via environmental variable
  # in Kettle for a normal weekend job, but can be reset manually
  if(is.null(cycle_date)) {
    cycle_date <- APM_CYCL_DT
  }


  # Load the model object (cf. training routine that stores the object)
  tryCatch({

    x.train         <- knn.storage$x.train
    y.train         <- knn.storage$y.train
    x.scale         <- knn.storage$x.scale
    features        <- knn.storage$features
    transform.funcs <- knn.storage$transform.funcs
    impute.vals     <- knn.storage$impute.vals

    # These should not be NULL, which seems to be what happens if not loaded
    if (any(vapply(knn.storage, is.null, TRUE))) {
      stop("One or more model variables were not loaded.")
    }

  }, error=apm_error_handler) -> s

  if ("error" %in% class(s)) {
    return(12)
  }


  # DB connectivity
  db_conn <- initialize_db()

  if("error" %in% class(db_conn)) {
    return(13)
  }


  # Parameter setting:
  k <- set_param_num(param = "K", con = db_conn$conn, input = k)

  if ("error" %in% class(k)) {
    return(14)
  }


  ##############################################################################
  ## Load and process the data for scoring
  ##############################################################################

  # If inputDF is NULL (default), fetch the most recent cert for all claimants
  # who have certified in the last prevDays days before the cycle date
  if (is.null(inputDF)) {

    earliestDate <- as.character(as.Date(cycle_date, "%Y-%m-%d") - prevDays)

    output_message(sprintf("Loading certifications from SQL; date range = [%s, %s]", earliestDate, cycle_date))

    benx <- tryCatch(RJDBC::dbGetQuery(db_conn$conn,
              sprintf("SELECT DISTINCT ON(claimt_id) *
                       FROM nrd.aggr_claimt_benf
                       WHERE cert_perd_end_dt >= '%s'
                       AND   cert_perd_end_dt <= '%s'
                       ORDER BY claimt_id, cert_perd_end_dt DESC",
                       earliestDate, cycle_date)),
              error=apm_error_handler)

    if("error" %in% class(benx)) {
      return(21)
    }

  } else {

    output_message("Scoring certifications passed as a data.frame")

    benx <- inputDF

  }

  benx <- data.table::data.table(benx)


  # Subset the data
  # ----------------------------------------------------------------------------

  # Keep the claimant ID plus the relevant features -- remove everything else
  benx[, setdiff(names(benx), c("claimt_id", features)) := NULL]


  # Carry out imputations
  # ----------------------------------------------------------------------------

  if (length(impute.vals) > 0) {
    err <- impute_values_dt(benx, impute.vals)

    if ("error" %in% class(err)) {
      return(22)
    }
  }


  # Do transformations
  # ----------------------------------------------------------------------------

  if (length(transform.funcs) > 0) {
    err <- transform_inputs_dt(benx, transform.funcs)

    if ("error" %in% class(err)) {
      return(23)
    }
  }


  # Set up for scoring
  # ----------------------------------------------------------------------------

  # Omit NAs to keep the models from crashing
  benx <- na_check_and_omit(benx)

  # Detach claimant ID from the data table and convert to data frame for scoring
  claimt_id <- as.character(benx$claimt_id)

  benx[, claimt_id := NULL]

  # Reset the var order to be cautious
  x.train <- x.train[, features]
  benx <- as.data.frame(benx[, features, with=FALSE])

  benx <- caret::predict.preProcess(x.scale, benx)



  ##############################################################################
  ## Run the model and extract scores
  ##############################################################################

  output_message("Scoring certifications")


  knn.p <- tryCatch(FNN::knn(train=x.train, test=benx, cl=y.train, k=k,
                      prob=TRUE, algorithm="cover_tree"), error=apm_error_handler)

  if("error" %in% class(knn.p)) {
    return(31)
  }


  knn.pred <- ifelse(knn.p == "TRUE", attr(knn.p, "prob"), 1-attr(knn.p, "prob"))
  knn.pred <- round(100*knn.pred)


  # We've previously encountered a bug where the length of claimt_id is
  # different from the length of the score predictions
  if (length(claimt_id) != length(knn.pred)) {
    tryCatch(stop("length(claimt_id) != length(scores)"), error=apm_error_handler)
    return(32)
  }



  ##############################################################################
  ## Store/return the scores
  ##############################################################################

  # Set scoreTableName=NULL to return a data frame instead of an integer.
  # In this case, no scores are written to the database.
  if (is.null(scoreTableName)) {

    output_message("Returning data frame")

    close_db(db_conn)

    # stringsAsFactors=F to keep strings strings! (or numerics, post-coercion)
    df.score <- data.frame(claimt_id=as.numeric(claimt_id), score=knn.pred,
                  cycle_date=cycle_date, timestamp=Sys.time(), stringsAsFactors=FALSE)

    return(df.score)

  # Otherwise, send the scores to the SQL databasse
  } else {

    output_message("Begin Transaction")

    s <- db_begin(db_conn$conn)

    if("error" %in% class(s)) {
      return(41)
    }


    output_message("Delete Previous Scores")

    s <- delete_scores(db_conn$conn, "KNN_KNN", cycle_date, scoreTableName)

    if("error" %in% class(s)) {
      return(42)
    }


    output_message("Output Scores")

    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

    s <- tryCatch(write_scores(claimt_id, knn.pred, db_conn$conn, "KNN_KNN",
                    cycle_date, scoreTableName, timestamp),
                  error=apm_error_handler)

    if("error" %in% class(s)) {
      db_rollback(db_conn$conn)   # Don't check because can't do anything
      return(43)
    }


    s <- tryCatch(db_commit(db_conn$conn), error=apm_error_handler)

    if("error" %in% class(s)) {
      return(44)
    }


    close_db(db_conn)

  }


  output_message("End")
  return(0)

}

