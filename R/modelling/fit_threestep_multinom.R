#' Flatten a multinom coefficient matrix to a named vector
#'
#' Names are `"<class>:<term>"`; kept stable across bootstrap refits because
#' the class factor levels are fixed in the analysis data.
#'
#' @param model An `nnet::multinom` fit.
#' @return Named numeric vector of coefficients.
#' @author Taren Sanders
#' @export
multinom_coef_vector <- function(model) {
  co <- coef(model)
  if (!is.matrix(co)) {
    co <- matrix(
      co,
      nrow = 1,
      dimnames = list(model$lev[2], names(co))
    )
  }
  out <- as.vector(t(co))
  names(out) <- paste(
    rep(rownames(co), each = ncol(co)),
    colnames(co),
    sep = ":"
  )
  out
}

#' Three-step class-membership model with cluster bootstrap
#'
#' Multinomial logistic regression of latent class membership on baseline
#' covariates, with classification uncertainty propagated by proportional
#' posterior weights (rows built in `prepare_threestep_data()`). Model-based
#' SEs are invalid (a child's rows are not independent), so inference uses a
#' cluster bootstrap over children: percentile 95% CIs and normal-approximation
#' p-values from the bootstrap SE. The LCGA itself is held fixed throughout
#' (stated limitation: classification error is not re-estimated, justified at
#' high entropy).
#'
#' @param threestep_data Output of `prepare_threestep_data()`.
#' @param rhs Character vector of right-hand-side terms.
#' @param n_boot Number of bootstrap resamples.
#' @param maxit Max iterations for `nnet::multinom`.
#' @return List: `model`, `boot_draws`, `tidy`, `ref_class`, `n_children`,
#'   `n_boot_failed`, `rhs`.
#' @author Taren Sanders
#' @export
fit_threestep_multinom <- function(
  threestep_data,
  rhs,
  n_boot = 500,
  maxit = 1000
) {
  require(dplyr)

  form <- stats::reformulate(rhs, response = "class")
  vars <- setdiff(all.vars(form), "class")

  df <- threestep_data |>
    dplyr::filter(stats::complete.cases(dplyr::pick(dplyr::all_of(vars))))

  # Reference class = largest class (by total posterior weight).
  class_sizes <- df |>
    dplyr::group_by(class) |>
    dplyr::summarise(size = sum(w), .groups = "drop")
  ref <- as.character(class_sizes$class[which.max(class_sizes$size)])
  df <- df |>
    dplyr::mutate(class = stats::relevel(factor(class), ref = ref))

  fit <- nnet::multinom(
    form,
    data = df,
    weights = w,
    trace = FALSE,
    maxit = maxit
  )
  est <- multinom_coef_vector(fit)

  children <- unique(df$id_num)
  boot_draws <- matrix(
    NA_real_,
    nrow = n_boot,
    ncol = length(est),
    dimnames = list(NULL, names(est))
  )
  for (b in seq_len(n_boot)) {
    resample <- tibble::tibble(
      id_num = sample(children, replace = TRUE)
    )
    boot_df <- dplyr::inner_join(
      resample,
      df,
      by = "id_num",
      relationship = "many-to-many"
    )
    boot_fit <- tryCatch(
      nnet::multinom(
        form,
        data = boot_df,
        weights = w,
        trace = FALSE,
        maxit = maxit
      ),
      error = function(e) NULL
    )
    if (is.null(boot_fit)) {
      next
    }
    draw <- multinom_coef_vector(boot_fit)
    boot_draws[b, names(draw)] <- draw
  }
  n_boot_failed <- sum(!stats::complete.cases(boot_draws))

  se <- apply(boot_draws, 2, stats::sd, na.rm = TRUE)
  ci <- apply(
    boot_draws,
    2,
    stats::quantile,
    probs = c(0.025, 0.975),
    na.rm = TRUE
  )

  tidy <- tibble::tibble(
    class = sub(":.*$", "", names(est)),
    term = sub("^[^:]+:", "", names(est)),
    log_rrr = unname(est),
    se_boot = unname(se),
    rrr = exp(unname(est)),
    rrr_lower = exp(ci[1, ]),
    rrr_upper = exp(ci[2, ]),
    p = 2 * stats::pnorm(-abs(unname(est) / unname(se)))
  )

  list(
    model = fit,
    boot_draws = boot_draws,
    tidy = tidy,
    ref_class = ref,
    n_children = length(children),
    n_boot_failed = n_boot_failed,
    rhs = rhs
  )
}
