
#' Plot the moving average of % overpayment per score
#' @export
plot_moving_avg <- function(df, model="") {
  library(ggplot2)

  ggplot(df) +
    aes(x=cml.pct.clmts, y=avg.pct.ov) +
    geom_line() +
    labs(x="Approx. percent claimants (w/o NA edges)",
         y="Percent overpayment",
         title=sprintf("%s Moving Average, Ordered by DESC(score)", model))
}


#' Plot the % overpayment per score
#' This is not terribly useful because we may have very few observations
#' at very high scores. On the other hand, if we have 0 overpayments at
#' high scores, that might mean something.
#' @export
plot_pct.ov_per_score <- function(df, model="") {
  library(ggplot2)

  ggplot(df) +
    aes(x=score, y=pct.ov.score, ymin=pct.ov.95.lower, ymax=pct.ov.95.upper) +
    geom_ribbon(fill="lightblue", alpha=0.6) +
    geom_point() +
    labs(x="Score value",
         y="Percent overpayment per score value",
         title=sprintf("%s Score Performance (not normalized)", model))
}


#' Plot the cumulative % response vs. cumulative % claimants
#' @export
plot_cml.res_per_cml.clmt <- function(df, model="") {
  library(ggplot2)

  ggplot(df) +
    aes(x=cml.pct.clmts, y=cml.pct.res) +
    geom_path(size=1) +
    geom_abline(aes(xmin=0, xmax=100), slope=1, color="grey") +
    labs(x="Cumulative percent claimants",
         y="Cumulative percent captured response",
         title=sprintf("%s Cumulative %% Captured Response", model))
}


#' Plot the cumulative % overpayments vas. score (descending)
#' I.e., where should our cutoff in score be placed to capture an ideal
#' amount of overpayment?
#' @export
plot_cml.pct.ov_per_score <- function(df, model="") {
  library(ggplot2)

  ggplot(df) +
    aes(x=score, y=cml.pct.ov) +
    geom_area(aes(ymin=0), fill="lightblue", alpha=0.6) +
    geom_line(aes(y=cml.pct.ov), size=1) +
    scale_x_reverse() +
    labs(x="Score value",
          y="Cumulative percent overpayment in sample",
          title=sprintf("%s Cumulative %% Overpayment vs. Score", model))
}


#' Plot the percent overpayment per score bin
#' Binned into units of 10 points; again, not especially useful, but could
#' indicate problems with models if we do very poorly at high scores
#' @export
plot_pct.ov_per_score_bin <- function(df, model="") {
  library(ggplot2)

  ggplot(df) +
    aes(x=bin, y=pct.ov.in.bin, ymin=pct.ov.95.lower, ymax=pct.ov.95.upper) +
    geom_pointrange() +
    labs(x="Score bin",
         y="Percent overpayment per bin",
         title=sprintf("%s percent overpayment in each bin (not normalized)", model))
}


#' Plot all 4 cumulative response curves
#' Not finished, so not exported
plot_all_cml.res_per_cml.clmt <- function() {
  ggplot() +
    geom_line(data=knn_cuml_response_table, aes(x=cml.pct.clmts, y=cml.pct.res, color="KNN")) +
    geom_line(data=rf_cuml_response_table,  aes(x=cml.pct.clmts, y=cml.pct.res, color="RF" )) +
    geom_line(data=svm_cuml_response_table, aes(x=cml.pct.clmts, y=cml.pct.res, color="SVM")) +
    geom_line(data=som_cuml_response_table, aes(x=cml.pct.clmts, y=cml.pct.res, color="SOM")) +
    geom_abline(slope=1)
}
