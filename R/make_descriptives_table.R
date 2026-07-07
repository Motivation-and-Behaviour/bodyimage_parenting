#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_clean
#' @return
#' @author Taren Sanders
#' @export
make_descriptives_table <- function(df_clean) {
  require(dplyr)

  table_df <- df_clean |>
    dplyr::select(
      age_cat,
      cohort,
      sex,
      age_years,
      ses,
      bmi,
      bmiz,
      body_image,
      body_image_wants,
      body_discrepancy,
      body_dissatisfaction,
      dplyr::starts_with("parenting_"),
      -dplyr::ends_with("_z")
    ) |>
    janitor::remove_empty("cols") |>
    dplyr::mutate(
      age_cat = factor(
        paste0("Age ", age_cat),
        levels = c("Age 8", "Age 10", "Age 12")
      )
    )

  vars <- setdiff(colnames(table_df), c("age_cat", "cohort"))

  make_table <- function(strata) {
    tab <- tableone::CreateTableOne(
      vars = vars,
      strata = strata,
      data = table_df,
      test = FALSE
    ) |>
      print(printToggle = FALSE, noSpaces = TRUE, varLabels = TRUE)
    tab[grepl("NA|NaN", tab)] <- "-"
    tab
  }

  list(
    by_age = make_table("age_cat"),
    by_cohort_age = make_table(c("cohort", "age_cat"))
  )
}
