
#' Percent overpayment in each score bin
#' @export
bins_table <- function(df) {
  library(dplyr)

  df %>% tbl_df %>%
    mutate(bin=cut(score, seq(0, 110, 10), right=F)) %>%
    group_by(bin) %>%
    summarize(n               = n(),
              n.ov            = sum(ov==TRUE),
              pct.ov.in.bin   = 100*n.ov/n,
              pct.ov.95.lower = 100*prop.test(n.ov, n, conf.level=0.95)$conf[1],
              pct.ov.95.upper = 100*prop.test(n.ov, n, conf.level=0.95)$conf[2]) %>%
    mutate(cml.pct.ov=cumsum(n.ov)/sum(n.ov)*100) %>%
    mutate_each(funs(round(., 4)), starts_with("pct"))
}


#' Percent overpayment moving average vs. cumulative percent claimants
#' Ordered in the same way as cumulative percent response
#' @export
moving_avg_table <- function(df, n.pts=1001) {
  library(dplyr)
  library(zoo)

  df %>% tbl_df %>%
    # Hack to remove front loading of TRUEs
    group_by(score) %>% sample_frac() %>% ungroup %>%
    arrange(desc(score)) %>%
    mutate(cml.pct.clmts = row_number()/n()*100,
            x            = ifelse(ov=="TRUE", 1, 0),
           avg.pct.ov    = 100*rollmean(x, n.pts, fill=NA)) %>%
    # MASS gets in the way... http://stackoverflow.com/q/24202120/656740
    dplyr::select(-x) %>%
    mutate_each(funs(round(., 4)), contains("pct"))
}


#' Percent overpayment per score
#' Probably only useful for "discrete" scores
#' @export
scores_table <- function(df) {
  library(dplyr)

  df %>% tbl_df %>%
    group_by(score) %>%
    summarize(n               = n(),
              n.ov            = sum(ov==TRUE),
              pct.ov.score    = 100*n.ov/n,
              pct.ov.95.lower = 100*prop.test(n.ov, n, conf.level=0.95)$conf[1],
              pct.ov.95.upper = 100*prop.test(n.ov, n, conf.level=0.95)$conf[2]) %>%
    mutate(cml.pct.ov=cumsum(n.ov)/sum(n.ov)*100) %>%
    mutate_each(funs(round(., 4)), contains("pct"))
}


#' Cumulate % captured response vs. culuative % claimants
#' Ordered by score, descending
#' @export
cuml_response_table <- function(df) {
  library(dplyr)

  df %>% tbl_df %>%
    # Hack to shuffle per-score so we don't front load overpayments
    group_by(score) %>% sample_frac() %>% ungroup %>%
    arrange(desc(score))  %>%
    mutate(cml.pct.clmts = row_number()/n()*100,
           cml.pct.res   = cumsum(ov==TRUE)/sum(ov==TRUE)*100) %>%
    mutate_each(funs(round(., 4)), contains("pct"))
}


#' Cumulative percent overpayment in sample as a function of score
#' Ordered by score, descending
#' @export
cuml_ovpaymt_table <- function(df) {
  library(dplyr)

  df %>% tbl_df %>%
    group_by(score) %>%
    summarize(n=n(), n.ov=sum(ov==TRUE)) %>%
    arrange(desc(score)) %>%
    mutate(cml.pct.ov=cumsum(n.ov)/sum(n.ov)*100) %>%
    mutate_each(funs(round(., 4)), contains("pct"))
}


##########################################################################################
# Testing SQL modules
#
# Obviously there is no need for a distinction between testing and training data sets
# since these are basically just business rules. Will have to change if we decide to
# optimize the cutoffs. For now we are just testing how good the scores in the nrdb are.
# 1. Pull scores for appropriate module from nrd.enty_score
# 2. Pull in target from nrd.aggr_claimt_ovpaymt
# 3. Change target to be 0 or 1 depending on if you want fraud or just overpayment
##########################################################################################

#' Pull scores for appropriate module from nrdb then
#' merge with target from aggr_claimt_ovpaymt
#' 
#' @param sql_module The module you want to pull data for
#' @param cycle_date Pretty obvious at this point
#' @param prevDays Number of days back the overpayment could have been occuring
#' @param onlyFraud if True then we only count Fraud as a 1, not just overpayment
#' 
#' @return Data frame containing all the scores for all the claims from nrd.enty_score 
#' @return for the desired module along with the target value.
#'  
#' @export
sql_module_df <- function(sql_module, cycle_date=NULL, prevDays=365, onlyFraud=FALSE) {
  if(initialize_apm("RF Statistical Model Training") != 0)
    return(1)
  
  output_message("Start", doMemory=TRUE, clear=TRUE)
  
  if(is.null(cycle_date))
    cycle_date <- APM_CYCL_DT
  earliestDate <- as.Date(cycle_date, "%Y-%m-%d") - prevDays
  
  db_conn <- initialize_db()
  
  if("error" %in% class(db_conn))
    return(2)
  
  # Pull scores and targets from nrdb
  sql <- trimws(
    sprintf("
      select a.*, round(a.score_val) as r_score_val,
             b.ovpaymt_amt, b.fraud_flag, b.ovpaymt_start_dt, b.ovpaymt_end_dt
        from nrd.enty_score a 
        left join nrd.aggr_claimt_ovpaymt b
          on a.enty_id = b.claimt_id
            and (b.ovpaymt_start_dt between '%2$s' and '%3$s'
                 or b.ovpaymt_end_dt between '%2$s' and '%3$s')
        where a.score_cd='%1$s'
        order by enty_id;"
    , sql_module, earliestDate, cycle_date))
  
  scores <- tryCatch(RJDBC::dbGetQuery(db_conn$conn, sql),
                 error=apm_error_handler)
  
  close_db(db_conn)
  
  if ("error" %in% class(scores))
    return(3)
  
  # Modify overpayments to be just 0 or 1 depending on onlyFraud value
  if(!onlyFraud)
    scores$y <- ifelse(is.na(scores$ovpaymt_amt),"FALSE","TRUE")
  
  if(onlyFraud)
    scores$y <- ifelse((scores$fraud_flag=="f"|is.na(scores$fraud_flag)),"FALSE","TRUE")
  
  output_message("End")
  return(scores)
}