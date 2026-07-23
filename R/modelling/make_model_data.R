#' Build the long modelling dataset for the trajectory analysis
#'
#' One row per child x wave with a non-missing body discrepancy score, keeping
#' children observed at two or more such waves. Baseline (age 8) covariates are
#' joined back to every row with a `_bl` suffix; children without an age-8 row
#' keep NA baselines so they stay in the LCGA and drop only from regressions.
#'
#' @param df_clean Cleaned data from `clean_data()`.
#' @return Long tibble with `time`, `id_num`, `body_discrepancy_abs`, and
#'   `_bl`-suffixed baseline covariates.
#' @author Taren Sanders
#' @export
make_model_data <- function(df_clean) {
  require(dplyr)

  baseline_vars <- c(
    "sex",
    "ses_z",
    "bmiz",
    "parenting_warm_p1_z",
    "parenting_warm_p2_z",
    "parenting_angry_p1_z",
    "parenting_angry_p2_z"
  )

  # Baseline covariates come from the age-8 row even when the CBIS items are
  # missing there, since the covariates were still measured at that wave.
  df_baseline <- df_clean |>
    dplyr::filter(age_cat == 8) |>
    dplyr::select(id, dplyr::all_of(baseline_vars)) |>
    dplyr::rename_with(~ paste0(.x, "_bl"), -id)

  df_clean |>
    dplyr::filter(!is.na(body_discrepancy), !is.na(age_cat)) |>
    dplyr::group_by(id) |>
    dplyr::filter(dplyr::n() >= 2) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      time = age_cat - 8,
      body_discrepancy_abs = abs(body_discrepancy),
      # hlme() requires a numeric subject identifier.
      id_num = as.integer(factor(id))
    ) |>
    dplyr::left_join(df_baseline, by = "id") |>
    dplyr::arrange(id_num, time)
}
