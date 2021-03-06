% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{convMulti}
\alias{convMulti}
\title{Convert fields to dummies}
\usage{
convMulti(dTable, colName, targetLevels)
}
\arguments{
\item{dTable}{the \code{data.table} object.}

\item{colName}{the column in the \code{data.table} to be converted to dummies}

\item{targetLevels}{the levels that the input factors (and therefore dummy columns)
are expected to have, with the final one being a catch-all.}
}
\value{
converted thingie
}
\description{
\code{convMulti} turn a column into a set of dummy variables. The `model.matrix` function
}
\author{
Wayne Folta
}

