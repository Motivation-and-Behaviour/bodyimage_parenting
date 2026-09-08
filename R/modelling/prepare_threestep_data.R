#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param class_assignments
#' @param df_model
#' @param modal logical. Use modal assignment instead of posterior weights.
#' @return
#' @author Taren Sanders
#' @export
prepare_threestep_data <- function(class_assignments, df_model, modal = FALSE) {
  require(dplyr)

  k <- sum(grepl("^pprob_", names(class_assignments)))

  child_covariates <- df_model |>
    dplyr::select(id_num, sex, dplyr::ends_with("_bl"), -sex_bl) |>
    dplyr::distinct()

  if (modal) {
    expanded <- class_assignments |>
      dplyr::transmute(id, id_num, class, w = 1)
  } else {
    expanded <- class_assignments |>
      dplyr::select(id, id_num, dplyr::starts_with("pprob_")) |>
      tidyr::pivot_longer(
        dplyr::starts_with("pprob_"),
        names_to = "class",
        names_prefix = "pprob_",
        values_to = "w"
      ) |>
      dplyr::filter(w >= 1e-6) |>
      dplyr::mutate(class = factor(class, levels = as.character(seq_len(k))))
  }

  dplyr::left_join(expanded, child_covariates, by = "id_num")
}
