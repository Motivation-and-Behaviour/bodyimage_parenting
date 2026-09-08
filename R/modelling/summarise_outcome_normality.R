#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_model
#' @param class_assignments
#' @param outcome
#' @return
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

  summarise_group <- function(g, grouping) {
    g |>
      dplyr::summarise(
        n = dplyr::n(),
        mean = mean(.data[[outcome]]),
        sd = stats::sd(.data[[outcome]]),
        skewness = skewness(.data[[outcome]]),
        excess_kurtosis = excess_kurtosis(.data[[outcome]]),
        .groups = "drop"
      ) |>
      dplyr::mutate(grouping = grouping, .before = 1)
  }

  dplyr::bind_rows(
    summarise_group(
      dplyr::group_by(joined, group = paste0("Age ", time + 8)),
      "By wave"
    ),
    summarise_group(
      dplyr::group_by(joined, group = paste("Class", class)),
      "By class"
    ),
    summarise_group(
      dplyr::group_by(
        joined,
        group = paste0("Class ", class, ", Age ", time + 8)
      ),
      "By class x wave"
    )
  )
}

skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) {
    return(NA_real_)
  }
  m <- mean(x)
  s2 <- sum((x - m)^2) / n
  (sum((x - m)^3) / n) / s2^1.5
}

excess_kurtosis <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 4) {
    return(NA_real_)
  }
  m <- mean(x)
  s2 <- sum((x - m)^2) / n
  (sum((x - m)^4) / n) / s2^2 - 3
}
