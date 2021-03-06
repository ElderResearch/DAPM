% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sql_scoring.R
\name{changing_ip_score}
\alias{changing_ip_score}
\title{Changing IP Module}
\usage{
changing_ip_score(cycle_date = NULL, db_schema = "nrd",
  exclude_cellular = NULL)
}
\arguments{
\item{cycle_date}{The APM cycle date.}

\item{db_schema}{The database schema to write scores and related tables to.
Default: "nrd".}

\item{exclude_cellular}{Whether or not the module should exclude
records identified as originating from cellular IPs.}
}
\value{
An integer status code, like other models.
}
\description{
This module reads in the IP base table, determines which users have changed
IPs significantly over a certain time period, and returns a table with risk
scores. The final risk score is represented by the percentile to which
claimant belongs in terms of a score that weights
  (1) The number of unique 24-bit IPs a claimant has had in the time period
      of interest
  (2) The percentage of time a claimant has used a unique 24-bit IP address
      during the time period of interest
}
\examples{
\dontrun{
rc <- changing_ip_score()
}

}

