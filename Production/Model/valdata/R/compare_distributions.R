#' @title compare_distributions
#' @description Compares distribution for each variable within a single state to its historical 
#' distribution. In particular, it pulls the most recent week's data and determines how different
#' the density distribution for each variable is from a density distribution that represents the 
#' historical distribution of that variable. The difference is measured using both the 
#' Bhattacharyya coefficient as well as the shared area of each distribution.
#' #@export

compare_distributions <- function(state, old_df, new_df) {
  
  # Pull historic density. Where would a density be stored?? For now I can store them
  # in a table in the tmp schema.
  APM::initialize_apm ("")
  db_conn <- APM::initialize_db ()
  
  
  # Pull out the x and y values for the densities we want to compare
  x1 <- dplyr::filter(df, st_init == state)$x
  y1 <- dplyr::filter(df, st_init == state)$y
  x2 <- dplyr::filter(df, st_init == state)$x
  y2 <- dplyr::filter(df, st_init == state)$y
  
  
}


compute_historic_distributions <- function(state="ID") {
  
  # valdata::pull_data() # Won't work until we can load valdata again. For now import manually.
  # Need to ask Will about why a lot of this is in pull_data. Seems to be doing much more
  # than we need it to do. Also figure out if it is worth using all data points from aggr_claimt_benf
  # for this one time density calculation.
  
  # Keep only numeric columns
  aggr_claimt_benf                   <- aggr_claimt_benf[sapply(aggr_claimt_benf, is.numeric)]
  aggr_claimt_ovpaymt                <- aggr_claimt_ovpaymt[sapply(aggr_claimt_ovpaymt, is.numeric)]
  sql_changing_ip                    <- sql_changing_ip[sapply(sql_changing_ip, is.numeric)]
  sql_clmt_ip_location               <- sql_clmt_ip_location[sapply(sql_clmt_ip_location, is.numeric)]
  sql_clmt_sharing_ips_with_employer <- sql_clmt_sharing_ips_with_employer[sapply(sql_clmt_sharing_ips_with_employer, is.numeric)]
  sql_new_hire                       <- sql_new_hire[sapply(sql_new_hire, is.numeric)]
  sql_QWR                            <- sql_QWR[sapply(sql_QWR, is.numeric)]
  sql_sharing_ip                     <- sql_sharing_ip[sapply(sql_sharing_ip, is.numeric)]
}