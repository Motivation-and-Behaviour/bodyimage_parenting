#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param threestep_fit
#' @param threestep_data
#' @return
#' @author Taren Sanders
#' @export
plot_class_probabilities <- function(threestep_fit, threestep_data) {
  require(dplyr)
  require(ggplot2)

  sexes <- levels(droplevels(threestep_data$sex))
  parenting_vars <- c(
    "Parental warmth (P1)" = "parenting_warm_p1_z_bl",
    "Parental anger (P1)" = "parenting_angry_p1_z_bl"
  )

  grids <- purrr::imap_dfr(parenting_vars, function(var, label) {
    newdata <- tidyr::expand_grid(
      value = seq(-2, 2, by = 0.1),
      sex = factor(sexes, levels = levels(threestep_data$sex))
    )
    newdata$parenting_warm_p1_z_bl <- 0
    newdata$parenting_angry_p1_z_bl <- 0
    newdata[[var]] <- newdata$value
    newdata$ses_z_bl <- 0
    newdata$bmiz_bl <- 0

    probs <- stats::predict(
      threestep_fit$model,
      newdata = newdata,
      type = "probs"
    )
    dplyr::bind_cols(
      newdata[, c("value", "sex")],
      tibble::as_tibble(probs)
    ) |>
      tidyr::pivot_longer(
        -c(value, sex),
        names_to = "class",
        values_to = "prob"
      ) |>
      dplyr::mutate(parenting = label)
  })

  grids |>
    dplyr::mutate(class = paste("Class", class)) |>
    ggplot(aes(x = value, y = prob, colour = sex)) +
    geom_line(linewidth = 0.8) +
    facet_grid(class ~ parenting, scales = "free_y") +
    labs(
      x = "Baseline parenting (SD units)",
      y = "Predicted class probability",
      colour = "Sex",
      caption = paste(
        "Other parenting variable, SES, and BMI z-score held at 0",
        "(sample mean)."
      )
    ) +
    theme_minimal()
}
