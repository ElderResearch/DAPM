
#' Run SQL validation script and push tables and plots to global environment
#' 
#' By default will use the scores from the most recent cycle date to predict
#' whether claimants had overpayment over the previous 365 days. Can be changed
#' to any length of time prior to the cycle date and also whether or not only
#' fraud should be considered (instead of overpayment).
#' @export
sql_validate <- function(score_cd=c("NWC_COR","CLA_RISK","QWR_QWR","IPA_CHIP","IPA_SHIP_PERD",
                                    "IPA_SHIP_RAW","IPA_SHIP_ALL","IPA_ELR","IPA_SHIP",
                                    "IPA_SHIP_TIME","IPA_ELR_PCT","IPA_ELR_TOT","IPA_CLR",
                                    "NHI_NHR","SOM_SOM","SVM_SVM","RF_RF","KNN_KNN"), 
                         prevDays=365, onlyFraud=FALSE) {
  library(APM)
  library(ggplot2)

  # Make sure score_cd is valid
  score_cd <- match.arg(score_cd)
  
  # Pull in data from nrdb
  print("Pulling in data")
  sql_results <- sql_module_df(score_cd, cycle_date=NULL, prevDays=prevDays, onlyFraud=onlyFraud)
  df <- data.frame(score=sql_results$r_score_val, ov=sql_results$y)
  
  print("========== Score bins table")
  sql_score_bins_table <<- bins_table(df)
  print(sql_score_bins_table)

  print("========== Moving avg table")
  sql_moving_avg_table <<- moving_avg_table(df, n.pts=nrow(df)/100)
  print(sql_moving_avg_table)

  print("========== Score table")
  sql_scores_table <<- scores_table(df)
  print(sql_scores_table)

  print("========== Cumulative response table")
  sql_cuml_response_table <<- cuml_response_table(df)
  print(sql_cuml_response_table)

  print("========== Cumulative overpayment table")
  sql_cuml_ovpaymt_table <<- cuml_ovpaymt_table(df)
  print(sql_cuml_ovpaymt_table)

  # Generate plots
  sql_score_bins_plot <<- plot_pct.ov_per_score_bin(sql_score_bins_table, model=score_cd)
  print(sql_score_bins_plot)
  dmy <- readline("Press a key for the next plot")

  sql_moving_avg_plot <<- plot_moving_avg(sql_moving_avg_table, model=score_cd)
  print(sql_moving_avg_plot)
  dmy <- readline("Press a key for the next plot")

  sql_scores_plot <<- plot_pct.ov_per_score(sql_scores_table, model=score_cd)
  print(sql_scores_plot)
  dmy <- readline("Press a key for the next plot")

  sql_cuml_response_plot <<- plot_cml.res_per_cml.clmt(sql_cuml_response_table, model=score_cd)
  print(sql_cuml_response_plot)
  dmy <- readline("Press a key for the next plot")

  sql_cuml_ovpaymt_plot <<- plot_cml.pct.ov_per_score(sql_cuml_ovpaymt_table, model=score_cd)
  print(sql_cuml_ovpaymt_plot)
}