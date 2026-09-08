#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_clean
#' @return
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

  # covariates come from the age 8 row even if PBI is missing there
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
      # hlme needs a numeric id
      id_num = as.integer(factor(id))
    ) |>
    dplyr::left_join(df_baseline, by = "id") |>
    dplyr::arrange(id_num, time)
}
