#' @title generate_new_stats
#' @description Generates new statistics by running the functions
#'  create_char_tbl, create_density_tbl, create_num_tbl across all variables
#' @export

generate_new_stats <- function(st_initials = "AA", min_date = "1900-01-01",
                               max_date = Sys.Date(), save = FALSE, pull = TRUE){

  options(scipen = 10)  #meant to prevent storing stats in scientific notation

  st_initials <- toupper(st_initials)

  #pulls all data necessary to generate statistics
  if(pull == TRUE) pull_data(min_date, max_date)

  #check to see if the user is in valdata directory, this is important when writing to file
  path <- getwd()
  path <- substr(path, nchar(path) - 6, nchar(path))
  if(path != "valdata") cat("You cannot save because you are not in the valdata directory.")


  #generate stats for each field
  cat("\n \n Begin calculating numeric summary statistics------------")
  claimt_benf_stats <- create_num_tbl(aggr_claimt_benf, st_initials, min_date, max_date)
  claimt_ovpaymt_stats <- create_num_tbl(aggr_claimt_ovpaymt, st_initials, min_date, max_date)
  sql_new_hire_stats <- create_num_tbl(sql_new_hire, st_initials, min_date, max_date)
  sql_QWR_stats <- create_num_tbl(sql_QWR, st_initials, min_date, max_date)
  sql_changing_ip_stats <- create_num_tbl(sql_changing_ip, st_initials, min_date, max_date)
  sql_clmt_sharing_ips_with_employer_stats <- create_num_tbl(sql_clmt_sharing_ips_with_employer, st_initials, min_date, max_date)
  sql_sharing_ip_stats <- create_num_tbl(sql_sharing_ip, st_initials, min_date, max_date)

  #If stats from the current state already exist, filter them out so new ones can be recorded
  old_tbl <- num_stats_tbl %>%
    filter(st_init != st_initials)

  #save a new num_stats tbl to the global environment
  assign("num_stats_tbl", rbind(old_tbl, claimt_benf_stats,claimt_ovpaymt_stats, sql_new_hire_stats,
                         sql_QWR_stats, sql_changing_ip_stats, sql_clmt_sharing_ips_with_employer_stats,
                         sql_sharing_ip_stats), envir = globalenv())


  #overwrite the current file
  if(save == TRUE & path == "valdata"){
  eval(parse(text = "devtools::use_data(num_stats_tbl, overwrite = TRUE)"))
  }

  cat("Summary statistics calculated \n \n")
  ####################################


  #generate information for density plots for each field
  cat("Begin calculating density functions------------")
  claimt_benf_density <- create_density_tbl(aggr_claimt_benf, st_initials, min_date, max_date)
  claimt_ovpaymt_density <- create_density_tbl(aggr_claimt_ovpaymt, st_initials, min_date, max_date)
  sql_new_hire_density <- create_density_tbl(sql_new_hire, st_initials, min_date, max_date)
  sql_QWR_density <- create_density_tbl(sql_QWR, st_initials, min_date, max_date)
  sql_changing_ip_density <- create_density_tbl(sql_changing_ip, st_initials, min_date, max_date)
  sql_clmt_sharing_ips_with_employer_density <-create_density_tbl(sql_clmt_sharing_ips_with_employer, st_initials, min_date, max_date)
  sql_sharing_ip_density <- create_density_tbl(sql_sharing_ip, st_initials, min_date, max_date)

  old_tbl <- density_tbl %>%
    filter(st_init != st_initials)

 assign("density_tbl", rbind(old_tbl, claimt_benf_density, claimt_ovpaymt_density, sql_new_hire_density, sql_QWR_density,
                      sql_changing_ip_density, sql_clmt_sharing_ips_with_employer_density,
                      sql_sharing_ip_density), envir = globalenv())

  if(save == TRUE & path == "valdata"){

  eval(parse(text = "devtools::use_data(density_tbl, overwrite = TRUE)"))

  }

  cat("Density functions calculated \n \n")
  ####################################


  #save the character fields as a list, record pct of each field
  cat("Begin calculating categorical variable statistics------------")
  claimt_benf_char_stats <- create_char_tbl(aggr_claimt_benf, st_initials, min_date, max_date)
  claimt_ovpaymt_char_stats <- create_char_tbl(aggr_claimt_ovpaymt, st_initials, min_date, max_date)
  sql_clmt_ip_location_char_stats <- create_char_tbl(sql_clmt_ip_location, st_initials, min_date, max_date)

  old_tbl <- char_stats_tbl %>%
    filter(st_init != st_initials)

  assign("char_stats_tbl", rbind(old_tbl, claimt_benf_char_stats, claimt_ovpaymt_char_stats,
                       sql_clmt_ip_location_char_stats), envir = globalenv())


  if(save == TRUE & path == "valdata"){

    eval(parse(text = "devtools::use_data(char_stats_tbl, overwrite = TRUE)"))

  }

  cat("Categorical variable statistics calculated  \n \n ")
  #################################
}






