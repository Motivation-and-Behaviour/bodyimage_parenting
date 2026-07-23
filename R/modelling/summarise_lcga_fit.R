#' Summarise fit statistics for one LCGA model
#'
#' One row per fit: convergence, information criteria (SABIC computed
#' manually), relative entropy (manual, from posterior probabilities; NA for
#' k = 1), and class proportions.
#'
#' @param fit An `hlme` fit from `fit_lcga()`.
#' @param outcome Outcome column name (recorded in the row).
#' @param sample Label for the analysis sample (e.g. "pooled", "boys").
#' @return A one-row tibble of fit statistics.
#' @author Taren Sanders
#' @export
summarise_lcga_fit <- function(
  fit,
  outcome = "body_discrepancy",
  sample = "pooled"
) {
  require(dplyr)

  k <- fit$ng
  loglik <- fit$loglik
  npm <- length(fit$best)
  ns <- fit$ns
  sabic <- -2 * loglik + npm * log((ns + 2) / 24)

  if (k > 1) {
    pp <- as.matrix(fit$pprob[, paste0("prob", seq_len(k))])
    plogp <- ifelse(pp > 0, pp * log(pp), 0)
    entropy <- 1 - sum(-plogp) / (ns * log(k))
    props <- as.numeric(prop.table(table(
      factor(fit$pprob$class, levels = seq_len(k))
    )))
  } else {
    entropy <- NA_real_
    props <- 1
  }

  tibble::tibble(
    outcome = outcome,
    sample = sample,
    k = k,
    converged = fit$conv == 1,
    loglik = loglik,
    npm = npm,
    AIC = fit$AIC,
    BIC = fit$BIC,
    SABIC = sabic,
    entropy = entropy,
    class_props = paste(
      sprintf("%.1f", 100 * sort(props, decreasing = TRUE)),
      collapse = " / "
    ),
    smallest_class_pct = 100 * min(props)
  )
}
