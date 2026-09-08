#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param waves_joined
#' @return
#' @author Taren Sanders
#' @export
tidy_data <- function(waves_joined) {
  require(dplyr)

  # LSAC missing codes
  waves_joined[waves_joined == -9] <- NA
  waves_joined[waves_joined == -99] <- NA

  waves_joined |>
    dplyr::mutate(
      dplyr::across(
        where(is.factor),
        ~ dplyr::case_when(
          .x %in% c("-1", "-2", "-3", "-4", "-9") ~ NA,
          TRUE ~ .x
        )
      ),
      dplyr::across(where(is.factor), forcats::fct_drop),
      # align cohorts by age rather than wave
      age_cat = dplyr::case_when(
        cohort == "B" & wave == 5 ~ 8,
        cohort == "B" & wave == 6 ~ 10,
        cohort == "B" & wave == 7 ~ 12,
        cohort == "K" & wave == 3 ~ 8,
        cohort == "K" & wave == 4 ~ 10,
        cohort == "K" & wave == 5 ~ 12,
        TRUE ~ NA_real_
      )
    )
}
