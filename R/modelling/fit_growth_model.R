#' Fit the secondary continuous growth model
#'
#' Multilevel linear growth model of body dissatisfaction on the same
#' primary regression sample as the three-step models (complete Parent 1
#' baseline covariates). Parenting main effects estimate associations with
#' the *level* at age 8 (time = 0); parenting x time terms estimate
#' associations with *change* per year. Random intercept + slope for time by
#' child, dropped to intercept-only if the fit is singular or fails to
#' converge (only three timepoints).
#'
#' @param df_model Modelling data from `make_model_data()`.
#' @param outcome Outcome column name.
#' @param moderation Add the sex x parenting x time interaction block.
#' @return List: `model`, `tidy`, `random_structure`, `n_children`, `n_obs`.
#' @author Taren Sanders
#' @export
fit_growth_model <- function(
  df_model,
  outcome = "body_discrepancy",
  moderation = FALSE
) {
  require(dplyr)
  require(lmerTest)

  covariates <- c(
    "parenting_warm_p1_z_bl",
    "parenting_angry_p1_z_bl",
    "sex",
    "ses_z_bl",
    "bmiz_bl"
  )
  df <- df_model |>
    dplyr::filter(stats::complete.cases(
      dplyr::pick(dplyr::all_of(covariates))
    ))

  rhs <- if (moderation) {
    paste(
      "(parenting_warm_p1_z_bl + parenting_angry_p1_z_bl) * time * sex",
      "+ ses_z_bl + bmiz_bl"
    )
  } else {
    paste(
      "(parenting_warm_p1_z_bl + parenting_angry_p1_z_bl) * time",
      "+ sex + ses_z_bl + bmiz_bl"
    )
  }

  fit_with <- function(random_part) {
    formula_full <- stats::as.formula(
      paste(outcome, "~", rhs, "+", random_part)
    )
    lmerTest::lmer(formula_full, data = df, REML = TRUE)
  }

  fit <- tryCatch(fit_with("(1 + time | id_num)"), error = function(e) NULL)
  random_structure <- "random intercept + slope"
  if (is.null(fit) || lme4::isSingular(fit, tol = 1e-4)) {
    fit <- fit_with("(1 | id_num)")
    random_structure <- "random intercept only"
  }

  tidy <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE) |>
    dplyr::select(
      term,
      estimate,
      std.error,
      df,
      statistic,
      p.value,
      conf.low,
      conf.high
    )

  list(
    model = fit,
    tidy = tidy,
    random_structure = random_structure,
    outcome = outcome,
    moderation = moderation,
    n_children = dplyr::n_distinct(df$id_num),
    n_obs = nrow(df)
  )
}

#' Likelihood-ratio test for the sex-moderation block in the growth model
#'
#' `anova()` on the two lmer fits (refit with ML automatically) testing the
#' joint contribution of the sex x parenting (x time) interaction terms.
#'
#' @param growth_main Output of `fit_growth_model(moderation = FALSE)`.
#' @param growth_moderation Output of `fit_growth_model(moderation = TRUE)`.
#' @return One-row tibble: statistic, df, p.
#' @author Taren Sanders
#' @export
growth_moderation_test <- function(growth_main, growth_moderation) {
  comparison <- anova(growth_main$model, growth_moderation$model)

  tibble::tibble(
    test = "Likelihood ratio (ML refit): sex interaction block",
    statistic = comparison$Chisq[2],
    df = comparison$Df[2],
    p = comparison$`Pr(>Chisq)`[2]
  )
}
