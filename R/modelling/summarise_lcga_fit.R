#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fit
#' @param outcome
#' @param sample
#' @return
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
    niter = fit$niter,
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
