% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{remove_flat_vars}
\alias{remove_flat_vars}
\title{Remove constant predictors}
\usage{
remove_flat_vars(df)
}
\arguments{
\item{df}{The input data frame.}
}
\value{
The updated data frame.
}
\description{
Remove predictors which have constant values across all observations.
}
\author{
Wayne Folta, Stuart Price; refactored by Tom Shafer
}

