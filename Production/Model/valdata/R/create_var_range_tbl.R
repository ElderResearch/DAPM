
#' @title create_var_range_tbl
#' @description Create a table to set an acceptable range for each variable. Used in the form_data_dictionary function
#' @export



#aggr_claimt_benf table numeric variables
create_var_range_tbl <- function() {
  current_date <- Sys.Date()
  year <- substr(current_date,1,4)
  month <- as.numeric(substr(current_date,6,7))
  qtr <- ifelse(month <= 3, 1, ifelse(month <= 6,2,ifelse(month <= 9,3, ifelse(month <=12,4,0))))
  max.date <- as.numeric(paste(year,qtr,sep = ''))

  #  any time a 0 is in the max, it is actually NA
  #                       1     2       3    4    5   6     7     8    9      10     11    12      13     14  15  16  17   18    19  20  21   22
  acceptable.min <- c(    1,    19371,  15,  1,   0,  0,    0,    0,  0,      0,     0,    0,      0,     0,  1,  0,  0,  0,      0,  0,  0,  0 )
  acceptable.max <- c(29200, max.date, 100, 50,   0,  0, 1000, 1000, 26,  52000, 52000, 10000, 13000, 26000, 52, 52, 52, 52,  52000,  0,  0,  0)
  var_name <- c("last_emplr_days_emplyd_cnt", #1  The min number of days a person can
                #   be employed by there last employer is 1.
                #   The max is set to 80 years (29200/365) as it is unreasonable
                #   for a person to work at one company longer than that

                "cert_perd_end_qtr_num",      #2  The min is set to 19371 as the social security was not enacted
                #   until then The max is set to the current time (future dates are wrong)

                "claimt_age",                 #3  The min is set to 15 since a person can nt legally work until 14
                #   and the base period is typically a year. The max is set to 100,
                #   as it is not probable for someone to collect who is that old

                "claim_year_seq",             #4  The min is set to 1 since it is not possible to be less than 1
                #   The max is set to 50 since it is not probable that a person to claim more
                #   Than 50 years (even close to 50 is improbable)

                "perds_to_claim_start_cnt",  #5   The min is set to 0 since it is not possible to be less than 0
                #    The max is not clearly defined at the moment

                "perds_to_cert_start_cnt",   #6   The min is set to 0 since it is not possible to be less than 0
                #    The max is not clearly defined at the moment

                "base_perd_paymt_amt",       #7   The min is set to 0 since it is not possible to be less than 0
                #    The max is not easy to define as the max changes for every state
                #    The max value across all states is 698 (and this will rise with inflation)
                #    So a value of 1000 will be used. This will atleast flag when an obviously incorrect value occurs

                "st_base_perd_paymt_amt",    #8   The min is set to 0 since it is not possible to be less than 0
                #    The max is not easy to define as the max changes for every state
                #    The max value across all states is 698 (and this will rise with inflation)
                #    So a value of 1000 will be used. This will atleast flag when an obviously incorrect value occurs

                "base_paymt_perds_cnt",      #9   The min value is set to 0 since it is not possible to be less than 0
                #    The max is typically 26, since it is a standard across states.. but there are
                #    Exceptions when we are in an economic downturn so we will set the max at 52 since
                #    it is improbable to be that high

                "max_paymt_amt",             #10   The min value is set to 0 since it is not possible to be less than 0
                #    The max value is set to 52,000.  This is because this variable is the max number
                #    of weeks collecting * max benefit amount. we set the max weeks to 52 and max benefit amt to 1000
                #    This may not be the actual max, (since it would vary by state) but it would still allow us to
                #    catch errors in ETL or the raw data

                "st_max_paymt_amt",          #11  Same thing as 9 (atleast in Idaho's data)

                "tot_reglr_reptd_incm_amt",  #12  The min value is set to 0 since it is not possible to be less than 0
                #    The max is set to $10,000.  One cannot collect unemployment insurance if
                #    They are earning a certain amount (which changes per state), and $10,000 far exceeds
                #    The max one can earn and still collect. Again we are using this to find errors in data
                #    So this value works.  You may find values higher than $10,000, but they should be collecting
                #    $0 from unemployment insurance


                "tot_reglr_paymt_amt",       #13  The min value is set to 0 since it is not possible to be less than 0
                #    The max is set to 13,000 (500*26) because it is not possible for a value to exceed that amount.
                #    500 -- typical unemployment insurance benefit amount, 26-- typical max number of weeks allowed to collect

                "cuml_paymt_amt",            #14  The min value is set to 0 since it is not possible to be less than 0
                #    The max is set to 26,000 (1000*26) because it is not possible for a value to exceed that amount.
                #    1000-- high unemployment insurance amount (higher than any state), 26 -- typical max number of weeks

                "cuml_paymt_perds_cnt",      #15  The min value is set to 0 since it is not possible to be less than 0
                #    The max is set to 52 because it is not possible for a value to exceed that amount.

                "cuml_zero_incm_cert_perd_cnt", #16  The min value is set to 0 since it is not possible to be less than 0
                #    The max is set to 52 because it is not possible for a value to exceed that amount.

                "cuml_lt_min_incm_cert_perd_cnt", #17  The min value is set to 0 since it is not possible to be less than 0
                #    The max is set to 52 because it is not possible for a value to exceed that amount.

                "cuml_gt_max_incm_cert_perd_cnt", #18  The min value is set to 0 since it is not possible to be less than 0
                #    The max is set to 52 because it is not possible for a value to exceed that amount.

                "cuml_reptd_incm_amt",        #19  The min value is set to 0 since it is not possible to be less than 0
                #    The max value is set to $52,000 (26 weeks * $2000) we assumed earlier that it is
                #    Highle unlikely a person report they made more than $2,000 in a week

                "paymt_to_base_paymt_ratio",  #20  The min value is set to 0 since it is not possible to be less than 0
                #    attribute definition not in the data dictionary so hard to determine

                "incm_to_base_paymt_ratio",   #21  The min value is set to 0 since it is not possible to be less than 0
                #    attribute definition not in the data dictionary so hard to determine

                "paymt_to_base_paymt_perds_ratio") #22  The min value is set to 0 since it is not possible to be less than 0
                 #    attribute definition not in the data dictionary so hard to determine

  tbl1 <- data.frame(var_name = var_name, acceptable_min = acceptable.min, acceptable_max = acceptable.max)


  ################################################################################


  #sql_QWR variables
  #                        1       2       3    4  5
  acceptable.min <- c(     0,      0,      0,   0, 0)
  acceptable.max <- c(100000, 100000, 100000, 500, 400)
  var_name <- c("total_earnings",             #1  The min value is set to 0 since it is not possible to be less than 0
                #    The max value is set to $100,000. It is highly unlikely a value will appear that
                #    is higher than $100,000. If it is over 100,000 than it is likely inserted in after the fact

                "total_benefit" ,             #2  The min value is set to 0 since it is not possible to be less than 0
                #    The max value is set to 100,000. It is highly unlikely a value will appear that high
                #    and if such a value does appear, it is likely wrong.

                "total_wba"  ,                #3  The min is set to 0 since it is not possible to be less than 0
                #    The max value is set to 100,000. It is highly unlikely a value will appear that high
                #    and if such a value does appear, it is likely wrong.

                "benefit_weeks",              #4  The min is set to 0 since it is not possible to be less than 0
                #   The max value is set to 500. It is highly unlikely a value will appear that high
                #    and if such a value does appear, it is likely wrong.

                "zero_earnings_weeks")        #5   The min is set to 0 since it is not possible to be less than 0
  #    The max value is set to 400. It is highly unlikely a value will appear that high
  #    and if such a value does appear, it is likely wrong. (This should be less than benefit_weeks)


  tbl2 <- data.frame(var_name = var_name, acceptable_min = acceptable.min, acceptable_max = acceptable.max)

  ################################################################################


  #aggr_claimt_ovpaymt table numeric variables
  acceptable.min <- 0
  acceptable.max <- 100000

  var_name <- "ovpaymt_amt" #  The min value is set to 0 since it is not possible to be less than 0
  #   The max value is set to 100000. It is highly unlikely a value will appear that high
  #    and if such a value does appear, it is likely wrong.

  tbl3 <- data.frame(var_name = var_name, acceptable_min = acceptable.min, acceptable_max = acceptable.max)

  ################################################################################


  # aggr_claimt_new_hire
  acceptable.min <- c(0,0)
  acceptable.max <- c(0,0)

  var_name <-  c("max_reptd_incm_amt",  #  The min value is set to 0 since it is not possible to be less than 0
                 #  The max value is not set as we don't have a clear definition in the dictionary

                 "min_incm_threshld_amt")  #  The min value is set to 0 since it is not possible to be less than 0
  #   The max value is not set as we don't have a clear definition in the dictionary

  tbl4 <- data.frame(var_name = var_name, acceptable_min = acceptable.min, acceptable_max = acceptable.max)

  ################################################################################


  #sql_changing_ip
  acceptable.min <- c(   0,    0, 0)
  acceptable.max <- c(1000, 1000, 100)
  var_name <-  c("num_unique_ips",  #  The min value is set to 0 since it is not possible to be less than 0
                 #  The max value is set to 1000. It is highle unlikely a person would have a value
                 #  this extreme, and if it occurs, the value is probably wrong

                 "num_of_filings",  #  The min value is set to 0 since it is not possible to be less than 0
                 #  The max value is set to 1000. It is highle unlikely a person would have a value
                 #  this extreme, and if it occurs, the value is probably wrong

                 "perc_unique")     #  The percent must be between 0 and 1

  tbl5 <- data.frame(var_name = var_name, acceptable_min = acceptable.min, acceptable_max = acceptable.max)

  ################################################################################

  #sql_clmt_sharing_ips
  acceptable.min <- c(0,0,0,0,0)
  acceptable.max <- c(6000,6000,100,100,100)
  var_name <- c("total_emp_shared_ct", #  The min value is set to 0 since it is not possible to be less than 0
                #  The max value is set to 1000. It is highly unlikely a person would have a value
                #  this extreme, and if it occurs, the value is probably wrong

                "tot_claims_ct" ,      #  The min value is set to 0 since it is not possible to be less than 0
                #  The max value is set to 1000. It is highly unlikely a person would have a value
                #  this extreme, and if it occurs, the value is probably wrong
                "perc_shared_w_emp", #This value is a percentage, value between 0 and 100
                "perc_emp_share_score",#This value is a percentile, value between 0 and 100
                "tot_emp_share_score") #This value is a percentile, value between 0 and 100

  tbl6 <- data.frame(var_name = var_name, acceptable_min = acceptable.min, acceptable_max = acceptable.max)





  #sql_sharing_ips
  acceptable.min <- c(0,0,0,0,0,0)
  acceptable.max <- c(604800,5000,5000,100,100,100)
  var_name <- c("diffseconds",    # The min value is set to 0 since it is not possible to be less than 0
                                  # The max is set to 604800 because that is the number of seconds in a week
                                  # and this variable is only looking at times viewed within the same week

                "prev_unique_users", # The min value is set to 0 since it is not possible to be less than 0
                                     # The max value is set to 5,000. It is highly unlikely an IP would have a value
                                     # This extreme, and if it occurs, the value is probably wrong

                "unique_users_in_week",# The min value is set to 0 since it is not possible to be less than 0
                                       # The max value is set to 5,000. It is highly unlikely an IP would have a value
                                       # This extreme, and if it occurs, the value is probably wrong

                "weekly_users_score", #This value is a percentile, value between 0 and 100

                "all_time_users_score", #This value is a percentile, value between 0 and 100

                "time_between_submissions_score")#This value is a percentile, value between 0 and 100

  tbl7 <- data.frame(var_name = var_name, acceptable_min = acceptable.min, acceptable_max = acceptable.max)




tbl <- rbind(tbl1, tbl2, tbl3, tbl4, tbl5, tbl6, tbl7)
return(tbl)
}














