#' Test a block of added terms across two nested three-step models
#'
#' Two complementary tests of the added block (e.g. the sex x parenting
#' interactions): (1) a likelihood-ratio test on the weighted deviances —
#' approximate only, because posterior weighting makes this a
#' pseudo-likelihood; (2) a joint Wald test using the cluster-bootstrap
#' covariance of the added coefficients, which respects the clustering.
#'
#' @param fit_reduced Output of `fit_threestep_multinom()` (nested model).
#' @param fit_full Output of `fit_threestep_multinom()` (with added block).
#' @return Tibble: one row per test (statistic, df, p).
#' @author Taren Sanders
#' @export
compare_multinom_models <- function(fit_reduced, fit_full) {
  require(dplyr)

  added <- setdiff(
    names(multinom_coef_vector(fit_full$model)),
    names(multinom_coef_vector(fit_reduced$model))
  )

  # Approximate LR test (weighted pseudo-likelihood).
  lr_stat <- fit_reduced$model$deviance - fit_full$model$deviance
  lr_df <- length(added)
  lr_p <- stats::pchisq(lr_stat, df = lr_df, lower.tail = FALSE)

  # Bootstrap joint Wald test on the added block.
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
