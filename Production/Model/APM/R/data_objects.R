# Data objects for the APM package

#' Random forest model object
#' 
#' \strong{This model object should never be put into version control.}
#' 
#' @format See training/scoring functions.
"rf.storage"

#' K nearest neighbors model object
#' 
#' \strong{This model object should never be put into version control.}
#' 
#' @format See training/scoring functions.
"knn.storage"

#' Support vector machines model object
#' 
#' \strong{This model object should never be put into version control.}
#' 
#' @format See training/scoring functions.
"svm.storage"

#' Model version character vector
#' 
#' @format A character vector naming the particular model object.
"model.version"

#' ZIP Codes database
#' 
#' @format A data.table with three variables:
#'    1. zip_code: 5-digit ZIP code
#'    2. Lat:      Latitude coordinate
#'    3. Long:     Longitude coordinate
"zipcodes"
