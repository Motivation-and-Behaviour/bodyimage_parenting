#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fits list. hlme fits, one per K.
#' @param df_model
#' @param outcome
#' @param ylab
#' @return
#' @author Taren Sanders
#' @export
plot_lcga_trajectories <- function(
  fits,
  df_model,
  outcome = "body_discrepancy",
  ylab = "Body dissatisfaction (perceived - ideal)"
) {
  require(dplyr)
  require(ggplot2)

  fits <- fits[grepl(paste0("_", outcome, "_\\d+$"), names(fits))]
  preds_wide <- lcga_predictions(fits)

  obs_means <- df_model |>
    dplyr::group_by(time) |>
    dplyr::summarise(est = mean(.data[[outcome]]), .groups = "drop")
  obs_all <- tidyr::crossing(
    k_label = unique(preds_wide$k_label),
    obs_means
  )

  p <- ggplot(preds_wide, aes(x = time, y = est, colour = class)) +
    geom_line(linewidth = 0.8)

  if (all(c("lower", "upper") %in% names(preds_wide))) {
    p <- p +
      geom_ribbon(
        aes(ymin = lower, ymax = upper, fill = class),
        alpha = 0.15,
        colour = NA
      )
  }

  p +
    geom_line(
      data = obs_all,
      aes(x = time, y = est),
      inherit.aes = FALSE,
      linetype = "dashed",
      colour = "grey30"
    ) +
    geom_point(
      data = obs_all,
      aes(x = time, y = est),
      inherit.aes = FALSE,
      colour = "grey30",
      size = 1.5
    ) +
    facet_wrap(~k_label, nrow = 1) +
    scale_x_continuous(
      breaks = c(0, 2, 4),
      labels = c("8", "10", "12")
    ) +
    labs(
      x = "Age (years)",
      y = ylab,
      colour = "Class",
      fill = "Class",
      caption = "Dashed line: observed overall means."
    ) +
    theme_minimal()
}
