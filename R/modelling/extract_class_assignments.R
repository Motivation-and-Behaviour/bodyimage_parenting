#' Order LCGA classes by ascending class-specific intercept
#'
#' Label-switching guard: lcmm's class numbering is arbitrary across refits,
#' so downstream targets relabel classes by their predicted value at baseline
#' (time = 0). Element j of the result is the original class index that
#' becomes stable class j.
#'
#' @param fit An `hlme` fit.
#' @return Integer vector mapping stable class -> original class.
#' @author Taren Sanders
#' @export
lcga_class_order <- function(fit) {
  intercepts <- lcmm::predictY(
    fit,
    newdata = data.frame(time = 0),
    var.time = "time"
  )$pred
  order(as.numeric(intercepts))
}

#' Extract stable class assignments and posterior probabilities
#'
#' Relabels classes by ascending baseline intercept (see
#' `lcga_class_order()`), permutes the posterior-probability columns to
#' match, and returns one row per child.
#'
#' @param lcga_final The chosen `hlme` fit.
#' @param df_model Modelling data from `make_model_data()` (for the
#'   `id`/`id_num` map).
#' @return Tibble: `id`, `id_num`, `class` (stable factor), `pprob_1..K`,
#'   `modal_pprob`.
#' @author Taren Sanders
#' @export
extract_class_assignments <- function(lcga_final, df_model) {
  require(dplyr)

  k <- lcga_final$ng
  ord <- lcga_class_order(lcga_final)
  pp <- lcga_final$pprob

  probs <- as.matrix(pp[, paste0("prob", seq_len(k))])[, ord, drop = FALSE]
  colnames(probs) <- paste0("pprob_", seq_len(k))
  stable_class <- match(pp$class, ord)

  tibble::tibble(
    id_num = pp$id_num,
    class = factor(stable_class, levels = seq_len(k)),
    modal_pprob = probs[cbind(seq_len(nrow(probs)), stable_class)]
  ) |>
    dplyr::bind_cols(tibble::as_tibble(probs)) |>
    dplyr::left_join(
      dplyr::distinct(df_model, id, id_num),
      by = "id_num"
    ) |>
    dplyr::relocate(id, id_num, class, dplyr::starts_with("pprob_"))
}
