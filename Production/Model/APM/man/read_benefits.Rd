% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{read_benefits}
\alias{read_benefits}
\title{Read Benefits}
\usage{
read_benefits(conn, custIdChosen, custIdSize, train.start)
}
\arguments{
\item{conn}{the \code{conn} part of the \code{db_conn} value returned from \code{initialize_db}}

\item{custIdChosen}{a list of strings of customer IDs to be retrieved}

\item{custIdSize}{number of customer IDs in custIdChosen (I think)}

\item{train.start}{the earliest year/quarter (numeric YYYYQ) to retrieve}
}
\description{
\code{read_overpayments} reads in the entire overpayments table (\code{nrd.aggr_claimt_ovpaymt}).
}
\author{
Wayne Folta
}

