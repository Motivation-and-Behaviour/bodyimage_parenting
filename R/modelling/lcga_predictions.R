# predicted trajectories for a list of hlme fits, one row per class x time
lcga_predictions <- function(fits) {
  require(dplyr)

  newdata <- data.frame(time = 0:4)

  preds <- purrr::map_dfr(fits, function(fit) {
    # draws = TRUE fails if the vcov isn't invertible
    py <- tryCatch(
      lcmm::predictY(fit, newdata = newdata, var.time = "time", draws = TRUE),
      error = function(e) {
        lcmm::predictY(fit, newdata = newdata, var.time = "time")
      }
    )
    pred <- as.data.frame(py$pred)
    pred$time <- newdata$time
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
