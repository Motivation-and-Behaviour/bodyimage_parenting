#' Baseline characteristics by trajectory-sample retention (GRoLTS 3b)
#'
#' Describes *which* baseline (age 8) variables are related to attrition, by
#' comparing children on how many waves of the outcome they contributed. The
#' denominator is children with at least one PBI observation: children with no
#' PBI wave were never eligible on the outcome (never-observed, not attrited),
#' so including them would confound outcome coverage with attrition.
#'
#' Two cuts, mirroring `make_descriptives_table()`:
#' - `by_inclusion`: excluded (< 2 waves) vs included (>= 2, the trajectory
#'   sample) — the analytic boundary.
#' - `by_waves`: 1 / 2 / 3 observed PBI waves — the retention gradient.
#'
#' Differences are flagged by standardized mean difference (SMD), which is more
#' informative than a p-value at this sample size.
#'
#' @param df_clean Cleaned data from `clean_data()`.
#' @return List with `by_inclusion` and `by_waves` tableone character matrices.
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

  # Number of waves with a PBI score, per child (>= 1-wave children only).
  n_waves <- df_clean |>
    dplyr::filter(!is.na(body_discrepancy)) |>
    dplyr::count(id, name = "n_waves")

  # One age-8 covariate row per child.
  baseline <- df_clean |>
    dplyr::filter(age_cat == 8) |>
    dplyr::select(id, dplyr::all_of(vars)) |>
    dplyr::distinct()

  # Left join keeps children with a PBI wave but no age-8 record; their
  # covariates stay NA and are reported honestly as missing within their group.
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
        labels = c("Excluded (<2)", "Included (≥2)")
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
