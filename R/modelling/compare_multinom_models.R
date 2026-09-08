#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fit_reduced
#' @param fit_full
#' @return
#' @author Taren Sanders
#' @export
compare_multinom_models <- function(fit_reduced, fit_full) {
  require(dplyr)

  added <- setdiff(
    names(multinom_coef_vector(fit_full$model)),
    names(multinom_coef_vector(fit_reduced$model))
  )

  # LR test is only approximate with the weights
  lr_stat <- fit_reduced$model$deviance - fit_full$model$deviance
  lr_df <- length(added)
  lr_p <- stats::pchisq(lr_stat, df = lr_df, lower.tail = FALSE)

  # Wald test using the bootstrap covariance
  b <- multinom_coef_vector(fit_full$model)[added]
  draws <- fit_full$boot_draws[, added, drop = FALSE]
  draws <- draws[stats::complete.cases(draws), , drop = FALSE]
  v <- stats::cov(draws)
  wald_stat <- as.numeric(t(b) %*% solve(v) %*% b)
  wald_p <- stats::pchisq(wald_stat, df = length(b), lower.tail = FALSE)

  tibble::tibble(
    test = c(
      "Likelihood ratio (approximate, weighted)",
      "Joint Wald (cluster-bootstrap covariance)"
    ),
    statistic = c(lr_stat, wald_stat),
    df = c(lr_df, length(b)),
    p = c(lr_p, wald_p)
  )
}
