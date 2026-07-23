#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_clean
#' @return
#' @author Taren Sanders
#' @export
plot_body_dissatisfaction <- function(df_clean) {
  require(dplyr)
  require(ggplot2)

  plot_df <- df_clean |>
    dplyr::filter(!is.na(body_dissatisfaction), !is.na(age_cat)) |>
    dplyr::count(cohort, age_cat, body_dissatisfaction) |>
    dplyr::group_by(cohort, age_cat) |>
    dplyr::mutate(prop = n / sum(n)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      age_cat = factor(
        paste0("Age ", age_cat),
        levels = c(
          "Age 8",
          "Age 10",
          "Age 12"
        )
      )
    )

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = age_cat, y = prop, fill = body_dissatisfaction)
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::facet_wrap(ggplot2::vars(cohort)) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(
      x = "Age group",
      y = "Proportion of children",
      fill = "Body dissatisfaction",
      title = "Body dissatisfaction by age and cohort"
    ) +
    ggplot2::theme_minimal()
}
