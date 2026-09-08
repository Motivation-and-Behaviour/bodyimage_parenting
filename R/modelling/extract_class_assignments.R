#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param lcga_final
#' @param df_model
#' @return
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

# lcmm numbers classes arbitrarily, so order them by intercept
lcga_class_order <- function(fit) {
  intercepts <- lcmm::predictY(
    fit,
    newdata = data.frame(time = 0),
    var.time = "time"
  )$pred
  order(as.numeric(intercepts))
}
