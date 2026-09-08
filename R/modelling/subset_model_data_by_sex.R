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
