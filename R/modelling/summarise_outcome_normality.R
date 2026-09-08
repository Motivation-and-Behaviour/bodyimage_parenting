#' Sample skewness (method-of-moments); NA if fewer than 3 observations.
#' @noRd
.skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) {
    return(NA_real_)
  }
  m <- mean(x)
  s2 <- sum((x - m)^2) / n
  (sum((x - m)^3) / n) / s2^1.5
}

#' Excess kurtosis (0 = Gaussian); NA if fewer than 4 observations.
#' @noRd
.excess_kurtosis <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 4) {
    return(NA_real_)
  }
  m <- mean(x)
  s2 <- sum((x - m)^2) / n
  (sum((x - m)^4) / n) / s2^2 - 3
}

#' Distributional summary of the observed outcome by wave and class (GRoLTS 4)
#'
#' Skewness and excess kurtosis (both computed manually in base R, so no extra
#' package dependency) alongside mean/SD, for three groupings: by wave, by
#' latent class, and by class x wave. Evidence for whether the Gaussian `hlme`
#' conditional-normality assumption holds within classes.
#'
#' @param df_model Modelling data from `make_model_data()`.
#' @param class_assignments Output of `extract_class_assignments()`.
#' @param outcome Outcome column name.
#' @return Tibble: grouping, group, n, mean, sd, skewness, excess_kurtosis.
#' @author Taren Sanders
#' @export
summarise_outcome_normality <- function(
  df_model,
  class_assignments,
  outcome = "body_discrepancy"
) {
  require(dplyr)

  joined <- df_model |>
    dplyr::inner_join(
      dplyr::select(class_assignments, id_num, class),
      by = "id_num"
    )

  one <- function(g, grouping) {
    g |>
      dplyr::summarise(
        n = dplyr::n(),
        mean = mean(.data[[outcome]]),
        sd = stats::sd(.data[[outcome]]),
        skewness = .skewness(.data[[outcome]]),
        excess_kurtosis = .excess_kurtosis(.data[[outcome]]),
        .groups = "drop"
      ) |>
      dplyr::mutate(grouping = grouping, .before = 1)
  }

  dplyr::bind_rows(
    one(dplyr::group_by(joined, group = paste0("Age ", time + 8)), "By wave"),
    one(dplyr::group_by(joined, group = paste("Class", class)), "By class"),
    one(
      dplyr::group_by(
        joined,
        group = paste0("Class ", class, ", Age ", time + 8)
      ),
      "By class × wave"
    )
  )
}
