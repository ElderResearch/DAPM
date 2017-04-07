
#' Run KNN validation script and push tables and plots to global environment
#' 
#' @param test.df Overpayment-enriched data frame for testing.
#'   See the \code{APM} documentation for, e.g., \code{knn_train()} and
#'   \code{make_sample_table_with_n_records()}.
#' 
#' @export
knn_validate <- function(test.df) {
  library(APM)
  library(ggplot2)

  # APM test function
  print("Running APM::knn_test()")
  knn_test_results <<- APM::knn_test(df=test.df)

  # Generate tables
  df <- data.frame(score=knn_test_results$scores, ov=knn_test_results$y.test)

  print("========== Score bins table")
  knn_score_bins_table <<- bins_table(df)
  print(knn_score_bins_table)

  print("========== Moving avg table")
  # 1% of points in symmetric rolling average
  knn_moving_avg_table <<- moving_avg_table(df, n.pts=nrow(df)%/%100 + 1)
  print(knn_moving_avg_table)

  print("========== Score table")
  knn_scores_table <<- scores_table(df)
  print(knn_scores_table)

  print("========== Cumulative response table")
  knn_cuml_response_table <<- cuml_response_table(df)
  print(knn_cuml_response_table)

  print("========== Cumulative overpayment table")
  knn_cuml_ovpaymt_table <<- cuml_ovpaymt_table(df)
  print(knn_cuml_ovpaymt_table)

  # Generate plots
  knn_score_bins_plot <<- plot_pct.ov_per_score_bin(knn_score_bins_table, model="KNN")
  print(knn_score_bins_plot)
  dmy <- readline("Press a key for the next plot")

  knn_moving_avg_plot <<- plot_moving_avg(knn_moving_avg_table, model="KNN")
  print(knn_moving_avg_plot)
  dmy <- readline("Press a key for the next plot")

  knn_scores_plot <<- plot_pct.ov_per_score(knn_scores_table, model="KNN")
  print(knn_scores_plot)
  dmy <- readline("Press a key for the next plot")

  knn_cuml_response_plot <<- plot_cml.res_per_cml.clmt(knn_cuml_response_table, model="KNN")
  print(knn_cuml_response_plot)
  dmy <- readline("Press a key for the next plot")

  knn_cuml_ovpaymt_plot <<- plot_cml.pct.ov_per_score(knn_cuml_ovpaymt_table, model="KNN")
  print(knn_cuml_ovpaymt_plot)
}
