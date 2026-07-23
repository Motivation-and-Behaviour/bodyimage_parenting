#' Summarise the flow from the cleaned data to the analysis samples
#'
#' Counts children at each inclusion stage (any CBIS wave, the >= 2 wave
#' trajectory sample, all three waves, complete baseline covariates) and
#' tabulates baseline covariate missingness within the trajectory sample.
#'
#' @param df_clean Cleaned data from `clean_data()`.
#' @param df_model Modelling data from `make_model_data()`.
#' @return List with `flow` and `baseline_missingness` tibbles.
#' @author Taren Sanders
#' @export
summarise_sample_flow <- function(df_clean, df_model) {
  require(dplyr)

  baseline_labels <- c(
    sex_bl = "Sex",
    ses_z_bl = "Socioeconomic position (z)",
    bmiz_bl = "BMI z-score",
    parenting_warm_p1_z_bl = "Parental warmth, Parent 1 (z)",
    parenting_warm_p2_z_bl = "Parental warmth, Parent 2 (z)",
    parenting_angry_p1_z_bl = "Parental anger, Parent 1 (z)",
    parenting_angry_p2_z_bl = "Parental anger, Parent 2 (z)"
  )
  baseline_vars <- names(baseline_labels)

  cbis_waves_per_child <- df_clean |>
    dplyr::filter(!is.na(body_discrepancy)) |>
    dplyr::count(id, name = "n_waves")

  # One row per child in the trajectory sample (baselines are constant within
  # child, so distinct() collapses to one row each).
  df_baseline <- df_model |>
    dplyr::select(id, dplyr::all_of(baseline_vars)) |>
    dplyr::distinct()

  # The primary regressions use Parent 1 parenting only; the both-parent model
  # (adding Parent 2) is a sensitivity analysis on its complete cases.
  primary_vars <- setdiff(
    baseline_vars,
    c("parenting_warm_p2_z_bl", "parenting_angry_p2_z_bl")
  )

  n_total <- dplyr::n_distinct(df_clean$id)
  n_analysis <- nrow(df_baseline)
  n_regression <- sum(stats::complete.cases(df_baseline[primary_vars]))
  n_sensitivity <- sum(stats::complete.cases(df_baseline[baseline_vars]))

  flow <- tibble::tibble(
    Stage = c(
      "Children in cleaned data",
      "≥ 1 wave with a CBIS score",
      "≥ 2 waves with a CBIS score (trajectory sample)",
      "All 3 waves with a CBIS score",
      "Complete baseline covariates, Parent 1 model (regression sample)",
      "Complete baseline covariates incl. Parent 2 (sensitivity sample)"
    ),
    `N children` = c(
      n_total,
      nrow(cbis_waves_per_child),
      sum(cbis_waves_per_child$n_waves >= 2),
      sum(cbis_waves_per_child$n_waves >= 3),
      n_regression,
      n_sensitivity
    )
  ) |>
    dplyr::mutate(
      `% of total` = sprintf("%.1f", 100 * `N children` / n_total)
    )

  baseline_missingness <- df_baseline |>
    dplyr::summarise(dplyr::across(
      dplyr::all_of(baseline_vars),
      ~ sum(is.na(.x))
    )) |>
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = "variable",
      values_to = "n_missing"
    ) |>
    dplyr::mutate(
      Covariate = baseline_labels[variable],
      `N missing` = n_missing,
      `% missing` = sprintf("%.1f", 100 * n_missing / n_analysis),
      .keep = "none"
    )

  list(flow = flow, baseline_missingness = baseline_missingness)
}
