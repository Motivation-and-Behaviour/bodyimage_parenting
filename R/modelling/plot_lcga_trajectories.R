#' Predicted class trajectories for a list of LCGA fits
#'
#' Shared helper for the enumeration plots: `predictY()` for each fit with
#' 95% bands where the fit allows draws, parsed into a long tibble.
#'
#' @param fits Named list of `hlme` fits (one per K).
#' @return Tibble with `k`, `k_label`, `class`, `time`, `est`, `lower`,
#'   `upper`.
#' @author Taren Sanders
#' @export
lcga_predictions <- function(fits) {
  require(dplyr)

  newdata <- data.frame(time = 0:4)

  preds <- purrr::map_dfr(fits, function(fit) {
    # draws = TRUE needs an invertible variance matrix; fall back to point
    # predictions if a fit can't provide draws.
    py <- tryCatch(
      lcmm::predictY(fit, newdata = newdata, var.time = "time", draws = TRUE),
      error = function(e) {
        lcmm::predictY(fit, newdata = newdata, var.time = "time")
      }
    )
    pred <- as.data.frame(py$pred)
    pred$time <- newdata$time
    # Columns are Ypred / lower.Ypred / upper.Ypred, suffixed _classX when
    # ng > 1 (draws = TRUE gives the lower/upper columns).
    pred |>
      tidyr::pivot_longer(-time, names_to = "col", values_to = "value") |>
      dplyr::mutate(
        quantile = dplyr::case_when(
          stringr::str_starts(col, "lower") ~ "lower",
          stringr::str_starts(col, "upper") ~ "upper",
          TRUE ~ "est"
        ),
        class = dplyr::coalesce(
          stringr::str_extract(col, "(?<=class)\\d+"),
          "1"
        ),
        k = fit$ng
      )
  })

  preds |>
    tidyr::pivot_wider(
      id_cols = c(k, class, time),
      names_from = quantile,
      values_from = value
    ) |>
    dplyr::mutate(k_label = factor(paste0("K = ", k)))
}

#' Plot predicted class trajectories for each candidate K
#'
#' Predicted mean trajectories (with 95% bands where the fit allows draws)
#' per class, faceted by number of classes, with the observed overall means
#' overlaid for reference.
#'
#' @param fits Named list of `hlme` fits (one per K).
#' @param df_model Modelling data from `make_model_data()`.
#' @param outcome Outcome column name.
#' @return A ggplot object.
#' @author Taren Sanders
#' @export
plot_lcga_trajectories <- function(
  fits,
  df_model,
  outcome = "body_discrepancy"
) {
  require(dplyr)
  require(ggplot2)

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
      y = "Body dissatisfaction (perceived − ideal)",
      colour = "Class",
      fill = "Class",
      caption = "Dashed line: observed overall means."
    ) +
    theme_minimal()
}
