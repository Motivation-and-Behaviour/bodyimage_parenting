#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_model
#' @param class_assignments
#' @param outcome
#' @param xlab
#' @return
#' @author Taren Sanders
#' @export
plot_outcome_distribution <- function(
  df_model,
  class_assignments,
  outcome = "body_discrepancy",
  xlab = "Body dissatisfaction (perceived - ideal)"
) {
  require(dplyr)
  require(ggplot2)

  df_model |>
    dplyr::inner_join(
      dplyr::select(class_assignments, id_num, class),
      by = "id_num"
    ) |>
    dplyr::mutate(
      age = factor(paste0("Age ", time + 8)),
      class = factor(paste("Class", class))
    ) |>
    ggplot(aes(x = factor(.data[[outcome]]), fill = class)) +
    geom_bar() +
    facet_grid(class ~ age, scales = "free_y") +
    labs(x = xlab, y = "Children", fill = "Class") +
    guides(fill = "none") +
    theme_minimal()
}
