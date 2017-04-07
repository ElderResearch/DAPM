
#' Run RF validation script and push tables and plots to global environment
#' 
#' @param test.df Overpayment-enriched data frame for testing.
#'   See the \code{APM} documentation for, e.g., \code{knn_train()} and
#'   \code{make_sample_table_with_n_records()}.
#' 
#' @export
svm_validate <- function(test.df) {
  library(APM)
  library(ggplot2)

  # APM test function
  print("Running APM::svm_test()")
  svm_test_results <<- APM::svm_test(df=test.df)

  # Generate tables
  print("Making Performance Tables")
  df <- data.frame(score=svm_test_results$scores, ov=svm_test_results$y.test)

  print("========== Score bins table")
  svm_score_bins_table <<- bins_table(df)
  print(svm_score_bins_table)

  print("========== Moving avg table")
  # 1% of points in symmetric rolling average
  svm_moving_avg_table <<- moving_avg_table(df, n.pts=nrow(df)%/%100 + 1)
  print(svm_moving_avg_table)

  print("========== Score table")
  svm_scores_table <<- scores_table(df)
  print(svm_scores_table)

  print("========== Cumulative response table")
  svm_cuml_response_table <<- cuml_response_table(df)
  print(svm_cuml_response_table)

  print("========== Cumulative overpayment table")
  svm_cuml_ovpaymt_table <<- cuml_ovpaymt_table(df)
  print(svm_cuml_ovpaymt_table)

  # Generate plots
  svm_score_bins_plot <<- plot_pct.ov_per_score_bin(svm_score_bins_table, model="SVM")
  print(svm_score_bins_plot)
  dmy <- readline("Press a key for the next plot")

  svm_moving_avg_plot <<- plot_moving_avg(svm_moving_avg_table, model="SVM")
  print(svm_moving_avg_plot)
  dmy <- readline("Press a key for the next plot")

  svm_scores_plot <<- plot_pct.ov_per_score(svm_scores_table, model="SVM")
  print(svm_scores_plot)
  dmy <- readline("Press a key for the next plot")

  svm_cuml_response_plot <<- plot_cml.res_per_cml.clmt(svm_cuml_response_table, model="SVM")
  print(svm_cuml_response_plot)
  dmy <- readline("Press a key for the next plot")

  svm_cuml_ovpaymt_plot <<- plot_cml.pct.ov_per_score(svm_cuml_ovpaymt_table, model="SVM")
  print(svm_cuml_ovpaymt_plot)
}
