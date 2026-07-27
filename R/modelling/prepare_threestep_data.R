#' Build the child x class dataset for the three-step membership models
#'
#' Proportional posterior weighting (Vermunt, 2010): each child contributes
#' one row per latent class, weighted by their posterior probability of
#' membership (rows with negligible weight dropped). With `modal = TRUE`,
#' returns instead one row per child at their modal class with weight 1 (the
#' modal-assignment sensitivity specification). Baseline covariates and
#' (time-invariant) sex are joined per child; complete-case filtering happens
#' per model in `fit_threestep_multinom()`, since the primary and
#' both-parent models need different covariate sets.
#'
#' @param class_assignments Output of `extract_class_assignments()`.
#' @param df_model Modelling data from `make_model_data()`.
#' @param modal Use modal assignment with unit weights instead of
#'   proportional posterior weights.
#' @return Tibble: `id`, `id_num`, `class`, `w`, `sex`, `_bl` covariates.
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
