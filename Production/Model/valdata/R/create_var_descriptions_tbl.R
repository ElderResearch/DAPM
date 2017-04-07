
#' @title create_data_descriptions
#' @description Make a table containing a description of each variable. The table will contain the majority of information
#'  in the data dictionary. It is applied in the form_data_dictionary function.
#'
#' @export

create_data_descriptions <- function(){

var_names <- c("last_emplr_naics2_cd", "seprtn_reas_cd", "educ_lvl_cd", "workrs_comp_cd",#4
               "pensn_cd" , "subst_workr_flag",  "school_empl_flag" , "filg_outsd_st_flag" ,#8
               "union_flag" , "corp_flag" ,  "sevrnc_flag" , "commtr_flag"  , #12
               "last_emplr_days_emplyd_cnt","cert_perd_end_qtr_num", "claimt_age", "claim_year_seq",#16
               "perds_to_claim_start_cnt", "perds_to_cert_start_cnt", "base_perd_paymt_amt", #19
               "st_base_perd_paymt_amt", "base_paymt_perds_cnt", "max_paymt_amt", #22
               "st_max_paymt_amt", "tot_reglr_reptd_incm_amt", "tot_reglr_paymt_amt", "cuml_paymt_amt", #26
               "cuml_paymt_perds_cnt", "cuml_zero_incm_cert_perd_cnt", "cuml_lt_min_incm_cert_perd_cnt",#29
               "cuml_gt_max_incm_cert_perd_cnt", "cuml_reptd_incm_amt", "paymt_to_base_paymt_ratio",#32
               "incm_to_base_paymt_ratio", "paymt_to_base_paymt_perds_ratio",  "ovpaymt_amt",#35
               "fraud_flag", "reas_cd", "num_unique_ips", "num_of_filings",#39
               "perc_unique" , "ip_outsd_cntry_and_st", "ip_outsd_st_and_listed_st",#42
               "ip_anon_proxy",  "ip_outsd_listed_st", "no_st_on_record","total_emp_shared_ct",#46
               "tot_claims_ct", "perc_shared_w_emp", "perc_emp_share_score", "tot_emp_share_score",#50
               "max_reptd_incm_amt", "min_incm_threshld_amt","total_earnings", "total_benefit",#54
               "total_wba", "benefit_weeks", "zero_earnings_weeks",  "diffseconds", "prev_unique_users",#59
               "unique_users_in_week", "weekly_users_score", "all_time_users_score", "time_between_submissions_score")#63

var_definitions <- c("last_emplr_naics2_cd: 2-digit employer NAICS code. ",#1
                     "seprtn_reas_cd:  Reason for a claimant's separation from their most recent employer, e.g., Quite, Fired, Leave of Absence, Labor Dispute. Certain codes make a Claimant illegible for UI.",#2
                     "educ_lvl_cd:  Self-reported variable on the UI initial claim on which the claimant lists highest education level obtained.",#3
                     "workrs_comp_cd:  Self-reported information about whether or not Claimant was receiving Worker's Comp payments at the time claim is filed.",#4
                     "pensn_cd:  Self-reported information about whether or not Claimant was receiving Pension payments at the time claim is filed." ,#5
                     "subst_workr_flag: Is claimant on a substitute worker list for any district within the state? ",#6
                     "school_empl_flag:  Did the Claimant work for a school or a company that provided services to a school (e.g. as a contractor) within the last 18 months?" ,#7
                     "filg_outsd_st_flag:  Was claimant employed in one state but lived in another? Claim can be filed in the state of employment. Information extracted from IP filing address." ,#8
                     "union_flag:  Is Claimant a member of a placement union or did employment come through the union's hiring?" ,#9
                     "corp_flag:  Claimant was a corporate officer." ,#10
                     "sevrnc_flag:  Self-reported information about whether or not Claimant was receiving Severance payments at the time claim is filed." ,#11
                     "commtr_flag: Claimant's ability or need to have transportation to work. Claimant may not be working due to lack of available transportation. "  ,#12
                     "last_emplr_days_emplyd_cnt:  Number of days employed at the last employer.",#13
                     "cert_perd_end_qtr_num Quarter:  containing the end date of this certification period.",#14
                     "claimt_age:  Age of claimant.",#15
                     "claim_year_seq:  Counter for the applicable claim year. 1 = 1st year in which claimant made a claim, 2 = 2nd year in which claimant made a claim.  Possible to have multiple claims in each year.",#16
                     "perds_to_claim_start_cnt:  Number of periods, typically weeks, between leaving the last employer and filing an initial claim.",#17
                     "perds_to_cert_start_cnt:  Number of periods, typically weeks, between leaving the last employer and this certification period start date.",#18
                     "base_perd_paymt_amt:  Amount the DOL will pay claimant per certification period.",#19
                     "st_base_perd_paymt_amt:  State component of the amount the DOL will pay claimant per certification period?",#20
                     "base_paymt_perds_cnt:  The base, or initial, number of periods for which the period payment amount will be made.",#21
                     "max_paymt_amt:  Maximum amount the DOL will pay claimant under this claim/period of unemployment.",#22
                     "st_max_paymt_amt:  WBA (Weekly Benefits Amount), which is the amount per week that the claimant is entitled to based on prior income, etc. State Maximum Payment Amount is the total amount of weekly benefits they are entitled to during this benefit year. In general, this is a multiple of a number of weeks they are entitled to times the WBA, and is also known as the TBA (Total Benefits Amount).",#23
                     "tot_reglr_reptd_incm_amt:  Total regular income reported by this claimant in this certification period.",#24
                     "tot_reglr_paymt_amt:  Total of regular payments for this claimant in this certification period.",#25
                     "cuml_paymt_amt:  Cumulative total of regular payments for this claimant up to and including this certification period.",#26
                     "cuml_paymt_perds_cnt:  Cumulative number of certification periods for which this claimant has been receiving payments under the current claim.",#27
                     "cuml_zero_incm_cert_perd_cnt:  Cumulative number of certification periods for which this claimant reported zero income.",#28
                     "cuml_lt_min_incm_cert_perd_cnt:  Cumulative number of certification periods for which this claimant reported income between zero and the state's minimum percentage threshold.  For Kansas = 25%, for Idaho = 50%",#29
                     "cuml_gt_max_incm_cert_perd_cnt:  Cumulative number of certification periods for which this claimant reported income greater than the state's maximum percentage threshold.  For Kansas = 125%",#30
                     "cuml_reptd_incm_amt:  Cumulative Reported Income Amount",#31
                     "paymt_to_base_paymt_ratio:  NA",#32
                     "incm_to_base_paymt_ratio:  NA",#33
                     "paymt_to_base_paymt_perds_ratio:  NA",#34
                     "ovpaymt_amt: Amount of overpayment",#35
                     "fraud_flag:  NA",#36
                     "reas_cd: NA",#37
                     "num_unique_ips: Number of unique IP addresses a claimant has used when filing for a claim",#38
                     "num_of_filings: Total number of claim filings",#39
                     "perc_unique: The percent of filings that came from a unique address" ,#40
                     "ip_outsd_cntry_and_st:  whether a matched transaction occurred outside the US ",#41
                     "ip_outsd_st_and_listed_st: whether a matched transaction occurred outside local states ",#42
                     "ip_anon_proxy:  whether an IP belongs to a know anonymous proxy server",#43
                     "ip_outsd_listed_st: whether a matched transaction occurred in a state not listed as regular/resident state/province ",#44
                     "no_st_on_record:  whether a claimant has any state listed on record",#45
                     "total_emp_shared_ct: Total number of claims shared with emps in the 3-month period",#46
                     "tot_claims_ct:  Total times shared in last 3 months???",#47
                     "perc_shared_w_emp: Total times shared with employer in last 3 months???",#48
                     "perc_emp_share_score: Percentile of tot_emp_shared_ct",#49
                     "tot_emp_share_score:  Percentile of the percent shared with employer",#50
                     "max_reptd_incm_amt:  NA",#51
                     "min_incm_threshld_amt:  NA",#52
                     "total_earnings:  Sum of regular income reported by this claimant in this certification period -- sum (tot_reglr_reptd_incm_amt)",#53
                     "total_benefit:  Sum of regular payments for this claimant in this certification period.-- sum (tot_reglr_paymt_amt)",#54
                     "total_wba:  Total amount the DOL will pay claimant per certification period -- sum(base_perd_paymt_amt)",#55
                     "benefit_weeks:  Number of weeks a claimant has recieved benefits",#56
                     "zero_earnings_weeks:  Number of weeks a claimant has reported making zero dollars",#57
                     "diffseconds: Out of people filing within the same week form the same IP address, the number of seconds between submissions",#58
                     "prev_unique_users:  The number of unique users for each ip address within a given week",#59
                     "unique_users_in_week: The number of unique users with each ip in the week ",#60
                     "weekly_users_score: quantile scoring based on unique users from one IP address in a week ",#61
                     "all_time_users_score:  quantile scoring based on previous users from one IP address in a week",#62
                     "time_between_submissions_score: decay scoring, start with a score of 100, after the time between users goes past x minutes the score begins to decay ")#63

  var_classes <- c("character", "character", "character", "character", "character", "character", "character", "character", "character",#9
                    "character", "character", "character", "numeric", "numeric",  "numeric", "numeric", "numeric",  "numeric",#18
                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",#27
                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character",#36
                    "character", "numeric", "numeric", "numeric", "character", "character", "character", "character", "character",#45
                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",#54
                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")#63


  var_parent_tbl <- c("aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf",#5
                      "aggr_claimt_benf","aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf",#10
                      "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf",#15
                      "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf",#20
                      "aggr_claimt_benf","aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf",#25
                      "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf",#30
                      "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_ovpaymt", #35
                      "aggr_claimt_ovpaymt", "aggr_claimt_ovpaymt","created.from.aggr_claimt_sessn", "created.from.aggr_claimt_sessn", #39
                      "created.from.aggr_claimt_sessn", "created.from.aggr_claimt_sessn", "created.from.aggr_claimt_sessn", #42
                      "created.from.aggr_claimt_sessn", "created.from.aggr_claimt_sessn", "created.from.aggr_claimt_sessn", #45
                      "created.from.aggr_claimt_sessn", "created.from.aggr_claimt_sessn", "created.from.aggr_claimt_sessn", #48
                      "created.from.aggr_claimt_sessn", "created.from.aggr_claimt_sessn","aggr_claimt_new_hire", "aggr_claimt_new_hire",#52
                      "created.from.aggr_claimt_benf", "created.from.aggr_claimt_benf", "created.from.aggr_claimt_benf", #55
                      "created.from.aggr_claimt_benf", "created.from.aggr_claimt_benf", "created.from.aggr_claimt_sessn", #58
                      "created.from.aggr_claimt_sessn", "created.from.aggr_claimt_sessn", "created.from.aggr_claimt_sessn", #61
                      "created.from.aggr_claimt_sessn", "created.from.aggr_claimt_sessn") #63


  obj_stored_in  <-c("aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf",#5
                      "aggr_claimt_benf","aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf",#10
                      "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf",#15
                      "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf",#20
                      "aggr_claimt_benf","aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf",#25
                      "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf",#30
                      "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_benf", "aggr_claimt_ovpaymt", #35
                      "aggr_claimt_ovpaymt", "aggr_claimt_ovpaymt","sql_changing_ip", "sql_changing_ip", "sql_changing_ip", #40
                      "sql_clmt_ip_location", "sql_clmt_ip_location", "sql_clmt_ip_location", "sql_clmt_ip_location",  #44
                      "sql_clmt_ip_location","sql_clmt_sharing_ips_with_employer", "sql_clmt_sharing_ips_with_employer", #47
                      "sql_clmt_sharing_ips_with_employer", "sql_clmt_sharing_ips_with_employer", "sql_clmt_sharing_ips_with_employer", #50
                      "sql_new_hire", "sql_new_hire", "sql_QWR", "sql_QWR", "sql_QWR", "sql_QWR", "sql_QWR", #57
                      "sql_sharing_ip", "sql_sharing_ip", "sql_sharing_ip", "sql_sharing_ip", "sql_sharing_ip", "sql_sharing_ip") #63


   check <- paste(var_names, var_classes, sep = ".")

   tbl <- data.frame(var_name = var_names, var_class = var_classes, check = check,
                     var_parent_tbl = var_parent_tbl, obj_stored_in = obj_stored_in, var_definition = var_definitions)
   return(tbl)
}

