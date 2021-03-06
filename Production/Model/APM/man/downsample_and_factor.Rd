% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{downsample_and_factor}
\alias{downsample_and_factor}
\title{Downsample predictors and targets}
\usage{
downsample_and_factor(x, y, false_ratio = 4)
}
\arguments{
\item{x}{Data frame of predictors (aggregated benefits).}

\item{y}{Vector of targets (overpayments).}

\item{false_ratio}{Ratio of non-op to op. Default is 4.}
}
\value{
Combined, downsampled data frame with the targets appended
   as column 'op'.
}
\description{
Try to obtain a more balanced training set by removing (randomly)
some extra observations not marked as overpayments.
The default ratio non-op:op is 4:1.
}
\author{
Wayne Folta, Stuart Price; refactored by Tom Shafer
}

