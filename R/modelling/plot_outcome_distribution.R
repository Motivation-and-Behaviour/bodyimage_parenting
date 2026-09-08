#' Distribution of the observed outcome by wave within latent class (GRoLTS 4)
#'
#' Bar charts of the discrete body-dissatisfaction score, faceted by latent
#' class (rows) and wave (columns). Mean/SD alone cannot show whether the
#' outcome is normally distributed within classes; because `hlme` assumes
#' conditional normality and the score is a bounded -6..+6 integer, a bar chart
#' (not a histogram) is the honest depiction.
#'
#' @param df_model Modelling data from `make_model_data()`.
#' @param class_assignments Output of `extract_class_assignments()`.
#' @param outcome Outcome column name.
#' @param xlab X-axis label.
#' @return A ggplot object.
#' @author Taren Sanders
#' @export
plot_outcome_distribution <- function(
  df_model,
  class_assignments,
  outcome = "body_discrepancy",
  xlab = "Body dissatisfaction (perceived − ideal)"
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
