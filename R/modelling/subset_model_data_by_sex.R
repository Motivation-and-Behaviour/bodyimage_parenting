#' Subset the modelling data to one sex for stratified LCGA fits
#'
#' @param df_model Modelling data from `make_model_data()`.
#' @param sex_group `"boys"` or `"girls"`.
#' @return `df_model` restricted to the requested sex.
#' @author Taren Sanders
#' @export
subset_model_data_by_sex <- function(df_model, sex_group) {
  require(dplyr)

  sex_level <- switch(
    sex_group,
    boys = "Male",
    girls = "Female",
    stop("sex_group must be 'boys' or 'girls'")
  )

  dplyr::filter(df_model, sex == sex_level)
}
