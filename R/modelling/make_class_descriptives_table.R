#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param class_assignments
#' @param df_model
#' @return
#' @author Taren Sanders
#' @export
make_class_descriptives_table <- function(class_assignments, df_model) {
  require(dplyr)

  table_df <- df_model |>
    dplyr::filter(age_cat == 8) |>
    dplyr::inner_join(
      dplyr::select(class_assignments, id_num, class),
      by = "id_num"
    ) |>
    dplyr::mutate(class = paste("Class", class)) |>
    dplyr::select(
      class,
      sex,
      ses,
      bmi,
      bmiz,
      body_discrepancy,
      body_dissatisfaction,
      parenting_warm_p1,
      parenting_warm_p2,
      parenting_angry_p1,
      parenting_angry_p2
    )

  tab <- tableone::CreateTableOne(
    vars = setdiff(colnames(table_df), "class"),
    strata = "class",
    data = table_df,
    test = FALSE
  ) |>
    print(printToggle = FALSE, noSpaces = TRUE, varLabels = TRUE)
  tab[grepl("NA|NaN", tab)] <- "-"
  tab
}
