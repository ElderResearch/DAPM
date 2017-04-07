
#' Run RF validation script and push tables and plots to global environment
#' 
#' @param test.df Overpayment-enriched data frame for testing.
#'   See the \code{APM} documentation for, e.g., \code{knn_train()} and
#'   \code{make_sample_table_with_n_records()}.
#' 
#' @export
rf_validate <- function(test.df) {
  library(APM)
  library(ggplot2)

  # APM test function
  print("Running APM::rf_test()")
  rf_test_results <<- APM::rf_test(df=test.df)

  # Generate tables
  print("Making Performance Tables")
  df <- data.frame(score=rf_test_results$scores, ov=rf_test_results$y.test)

  print("========== Score bins table")
  rf_score_bins_table <<- bins_table(df)
  print(rf_score_bins_table)

  print("========== Moving avg table")
  # 1% of points in symmetric rolling average
  rf_moving_avg_table <<- moving_avg_table(df, n.pts=nrow(df)%/%100 + 1)
  print(rf_moving_avg_table)

  print("========== Score table")
  rf_scores_table <<- scores_table(df)
  print(rf_scores_table)

  print("========== Cumulative response table")
  rf_cuml_response_table <<- cuml_response_table(df)
  print(rf_cuml_response_table)

  print("========== Cumulative overpayment table")
  rf_cuml_ovpaymt_table <<- cuml_ovpaymt_table(df)
  print(rf_cuml_ovpaymt_table)

  # Generate plots
  rf_score_bins_plot <<- plot_pct.ov_per_score_bin(rf_score_bins_table, model="RF")
  print(rf_score_bins_plot)
  dmy <- readline("Press a key for the next plot")

  rf_moving_avg_plot <<- plot_moving_avg(rf_moving_avg_table, model="RF")
  print(rf_moving_avg_plot)
  dmy <- readline("Press a key for the next plot")

  rf_scores_plot <<- plot_pct.ov_per_score(rf_scores_table, model="RF")
  print(rf_scores_plot)
  dmy <- readline("Press a key for the next plot")

  rf_cuml_response_plot <<- plot_cml.res_per_cml.clmt(rf_cuml_response_table, model="RF")
  print(rf_cuml_response_plot)
  dmy <- readline("Press a key for the next plot")

  rf_cuml_ovpaymt_plot <<- plot_cml.pct.ov_per_score(rf_cuml_ovpaymt_table, model="RF")
  print(rf_cuml_ovpaymt_plot)
}
