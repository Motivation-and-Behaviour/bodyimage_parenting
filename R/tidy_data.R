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

  # Recode the LSAC missing-data codes to NA. Continuous variables use the
  # numeric sentinels; the labelled factors keep them as their character codes.
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
      # The B and K cohorts are offset in wave number but aligned in age, so a
      # design-age band (not the raw wave) is the common time axis.
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
