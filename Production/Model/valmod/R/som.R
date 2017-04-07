
#' Run RF validation script and push tables and plots to global environment
#' 
#' @param test.df Overpayment-enriched data frame for testing.
#'   See the \code{APM} documentation for, e.g., \code{knn_train()} and
#'   \code{make_sample_table_with_n_records()}.
#' 
#' @export
som_validate <- function(test.df) {
  library(APM)
  library(ggplot2)

  # APM test function
  print("Running APM::som_test()")
  som_test_results <<- APM::som_test(df=test.df)

  # Generate tables
  print("Making Performance Tables")
  df <- data.frame(score=ifelse(som_test_results$scores==TRUE, 100, 0),
                   ov=som_test_results$y.test)

  print("========== Score bins table")
  som_score_bins_table <<- bins_table(df)
  print(som_score_bins_table)

  print("========== Moving avg table")
  # 1% of points in symmetric rolling average
  som_moving_avg_table <<- moving_avg_table(df, n.pts=nrow(df)%/%100 + 1)
  print(som_moving_avg_table)

  print("========== Score table")
  som_scores_table <<- scores_table(df)
  print(som_scores_table)

  print("========== Cumulative response table")
  som_cuml_response_table <<- cuml_response_table(df)
  print(som_cuml_response_table)

  print("========== Cumulative overpayment table")
  som_cuml_ovpaymt_table <<- cuml_ovpaymt_table(df)
  print(som_cuml_ovpaymt_table)

  # Generate plots
  som_score_bins_plot <<- plot_pct.ov_per_score_bin(som_score_bins_table, model="SOM")
  print(som_score_bins_plot)
  dmy <- readline("Press a key for the next plot")

  som_moving_avg_plot <<- plot_moving_avg(som_moving_avg_table, model="SOM")
  print(som_moving_avg_plot)
  dmy <- readline("Press a key for the next plot")

  som_scores_plot <<- plot_pct.ov_per_score(som_scores_table, model="SOM")
  print(som_scores_plot)
  dmy <- readline("Press a key for the next plot")

  som_cuml_response_plot <<- plot_cml.res_per_cml.clmt(som_cuml_response_table, model="SOM")
  print(som_cuml_response_plot)
  dmy <- readline("Press a key for the next plot")

  som_cuml_ovpaymt_plot <<- plot_cml.pct.ov_per_score(som_cuml_ovpaymt_table, model="SOM")
  print(som_cuml_ovpaymt_plot)
}
