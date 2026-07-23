#' Compare pooled and sex-stratified LCGA trajectories across K
#'
#' Diagnostic for the class-enumeration decision: predicted class
#' trajectories faceted by sample (pooled / boys / girls) x number of
#' classes, with each sample's observed means overlaid. If the stratified
#' shapes mirror the pooled ones, sex differences are a matter of class
#' prevalence (handled by the membership model) rather than class shape.
#'
#' @param lcga_fits Named list of pooled `hlme` fits (one per K).
#' @param lcga_strat_fits Named list of stratified fits; names contain
#'   "boys"/"girls".
#' @param df_model Modelling data from `make_model_data()`.
#' @param outcome Outcome column name.
#' @return A ggplot object.
#' @author Taren Sanders
#' @export
plot_lcga_sex_comparison <- function(
  lcga_fits,
  lcga_strat_fits,
  df_model,
  outcome = "body_discrepancy"
) {
  require(dplyr)
  require(ggplot2)

  fit_sets <- list(
    Pooled = lcga_fits,
    Boys = lcga_strat_fits[grepl("boys", names(lcga_strat_fits))],
    Girls = lcga_strat_fits[grepl("girls", names(lcga_strat_fits))]
  )
  data_sets <- list(
    Pooled = df_model,
    Boys = subset_model_data_by_sex(df_model, "boys"),
    Girls = subset_model_data_by_sex(df_model, "girls")
  )

  preds <- purrr::imap_dfr(
    fit_sets,
    ~ dplyr::mutate(lcga_predictions(.x), sample = .y)
  )
  obs <- purrr::imap_dfr(data_sets, function(df, label) {
    df |>
      dplyr::group_by(time) |>
      dplyr::summarise(est = mean(.data[[outcome]]), .groups = "drop") |>
      dplyr::mutate(sample = label)
  })

  preds <- preds |>
    dplyr::mutate(sample = factor(sample, levels = names(fit_sets)))
  obs_all <- tidyr::crossing(
    k_label = unique(preds$k_label),
    obs
  ) |>
    dplyr::mutate(sample = factor(sample, levels = names(fit_sets)))

  p <- ggplot(preds, aes(x = time, y = est, colour = class)) +
    geom_line(linewidth = 0.7)

  if (all(c("lower", "upper") %in% names(preds))) {
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
      size = 1.2
    ) +
    facet_grid(sample ~ k_label) +
    scale_x_continuous(
      breaks = c(0, 2, 4),
      labels = c("8", "10", "12")
    ) +
    labs(
      x = "Age (years)",
      y = "Body dissatisfaction (perceived − ideal)",
      colour = "Class",
      fill = "Class",
      caption = "Dashed line: observed means within each sample."
    ) +
    theme_minimal()
}
