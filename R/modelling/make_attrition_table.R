#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_clean
#' @return
#' @author Taren Sanders
#' @export
make_attrition_table <- function(df_clean) {
  require(dplyr)

  baseline_labels <- c(
    sex = "Sex",
    ses = "Socioeconomic position",
    bmiz = "BMI z-score",
    parenting_warm_p1 = "Parental warmth, Parent 1",
    parenting_angry_p1 = "Parental anger, Parent 1",
    parenting_warm_p2 = "Parental warmth, Parent 2",
    parenting_angry_p2 = "Parental anger, Parent 2"
  )
  vars <- names(baseline_labels)

  n_waves <- df_clean |>
    dplyr::filter(!is.na(body_discrepancy)) |>
    dplyr::count(id, name = "n_waves")

  baseline <- df_clean |>
    dplyr::filter(age_cat == 8) |>
    dplyr::select(id, dplyr::all_of(vars)) |>
    dplyr::distinct()

  # keep kids with no age 8 row (covariates stay NA)
  table_df <- n_waves |>
    dplyr::left_join(baseline, by = "id") |>
    dplyr::mutate(
      `PBI waves` = factor(
        n_waves,
        levels = 1:3,
        labels = c("1 wave", "2 waves", "3 waves")
      ),
      `Trajectory sample` = factor(
        n_waves >= 2,
        levels = c(FALSE, TRUE),
        labels = c("Excluded (<2)", "Included (>=2)")
      )
    )

  labelled::var_label(table_df) <- as.list(baseline_labels)[
    intersect(names(table_df), vars)
  ]

  make_table <- function(strata) {
    tab <- tableone::CreateTableOne(
      vars = vars,
      strata = strata,
      data = table_df,
      test = TRUE
    ) |>
      print(
        printToggle = FALSE,
        noSpaces = TRUE,
        varLabels = TRUE,
        smd = TRUE
      )
    tab[grepl("NA|NaN", tab)] <- "-"
    tab
  }

  list(
    by_inclusion = make_table("Trajectory sample"),
    by_waves = make_table("PBI waves")
  )
}
