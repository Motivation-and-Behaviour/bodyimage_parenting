#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_model
#' @param outcome
#' @param moderation logical. Add the sex interactions.
#' @return
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

  # drop the random slope if it is singular
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

growth_moderation_test <- function(growth_main, growth_moderation) {
  comparison <- anova(growth_main$model, growth_moderation$model)

  tibble::tibble(
    test = "Likelihood ratio (ML refit): sex interaction block",
    statistic = comparison$Chisq[2],
    df = comparison$Df[2],
    p = comparison$`Pr(>Chisq)`[2]
  )
}
