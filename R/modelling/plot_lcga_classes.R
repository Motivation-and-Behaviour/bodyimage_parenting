#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param lcga_final
#' @param class_assignments
#' @param df_model
#' @param outcome
#' @param ylab
#' @return
#' @author Taren Sanders
#' @export
plot_lcga_classes <- function(
  lcga_final,
  class_assignments,
  df_model,
  outcome = "body_discrepancy",
  ylab = "Body dissatisfaction (perceived - ideal)"
) {
  require(dplyr)
  require(ggplot2)

  ord <- lcga_class_order(lcga_final)

  preds <- lcga_predictions(list(final = lcga_final)) |>
    dplyr::mutate(
      class = factor(match(as.integer(class), ord), levels = seq_along(ord))
    )

  class_pct <- class_assignments |>
    dplyr::count(class) |>
    dplyr::mutate(pct = 100 * n / sum(n))
  class_labels <- sprintf("Class %s (%.1f%%)", class_pct$class, class_pct$pct)

  obs_means <- df_model |>
    dplyr::inner_join(
      dplyr::select(class_assignments, id_num, class),
      by = "id_num"
    ) |>
    dplyr::group_by(class, time) |>
    dplyr::summarise(est = mean(.data[[outcome]]), .groups = "drop")

  ggplot(preds, aes(x = time, y = est, colour = class)) +
    geom_ribbon(
      aes(ymin = lower, ymax = upper, fill = class),
      alpha = 0.15,
      colour = NA
    ) +
    geom_line(linewidth = 0.9) +
    geom_line(
      data = obs_means,
      aes(x = time, y = est, colour = class),
      linetype = "dotted",
      linewidth = 0.7
    ) +
    geom_point(
      data = obs_means,
      aes(x = time, y = est, colour = class),
      size = 2
    ) +
    scale_colour_discrete(labels = class_labels) +
    scale_fill_discrete(labels = class_labels) +
    scale_x_continuous(
      breaks = c(0, 2, 4),
      labels = c("8", "10", "12")
    ) +
    labs(
      x = "Age (years)",
      y = ylab,
      colour = "Class",
      fill = "Class",
      caption = paste(
        "Solid: model-predicted mean trajectories (95% bands).",
        "Dotted/points: observed means by modal class."
      )
    ) +
    theme_minimal()
}
