#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param growth_moderation
#' @return
#' @author Taren Sanders
#' @export
plot_growth_predictions <- function(growth_moderation) {
  require(dplyr)
  require(ggplot2)

  model_frame_sex <- stats::model.frame(growth_moderation$model)$sex
  sexes <- factor(
    levels(droplevels(model_frame_sex)),
    levels = levels(model_frame_sex)
  )

  parenting_vars <- c(
    "Parental warmth (P1)" = "parenting_warm_p1_z_bl",
    "Parental anger (P1)" = "parenting_angry_p1_z_bl"
  )

  newdata <- purrr::imap_dfr(parenting_vars, function(var, label) {
    grid <- tidyr::expand_grid(
      time = 0:4,
      sex = sexes,
      level = c(-1, 1)
    )
    grid$parenting_warm_p1_z_bl <- 0
    grid$parenting_angry_p1_z_bl <- 0
    grid[[var]] <- grid$level
    grid$ses_z_bl <- 0
    grid$bmiz_bl <- 0
    grid$parenting <- label
    grid
  })

  newdata$pred <- stats::predict(
    growth_moderation$model,
    newdata = newdata,
    re.form = NA
  )

  newdata |>
    dplyr::mutate(
      level = factor(
        level,
        levels = c(-1, 1),
        labels = c("-1 SD", "+1 SD")
      )
    ) |>
    ggplot(aes(
      x = time,
      y = pred,
      colour = sex,
      linetype = level
    )) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~parenting) +
    scale_x_continuous(breaks = c(0, 2, 4), labels = c("8", "10", "12")) +
    labs(
      x = "Age (years)",
      y = "Predicted body dissatisfaction",
      colour = "Sex",
      linetype = "Parenting level",
      caption = paste(
        "Fixed-effects predictions; other parenting variable, SES, and",
        "BMI z-score at 0."
      )
    ) +
    theme_minimal()
}
