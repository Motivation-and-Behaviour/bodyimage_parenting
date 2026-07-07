#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param filepath
#' @return
#' @author Taren Sanders
#' @export
read_waves_data <- function(filepath) {
  require(dplyr)

  age_num <- stringr::str_extract(filepath, "\\d{1,2}(?=\\.)")
  cohort_letter <- stringr::str_extract(filepath, "[bk](?=\\d{1,2}\\.)")

  wave_num <- dplyr::case_when(
    stringr::str_detect(filepath, "b6\\.sav") ~ 4,
    stringr::str_detect(filepath, "b8\\.sav") ~ 5,
    stringr::str_detect(filepath, "b10\\.sav") ~ 6,
    stringr::str_detect(filepath, "b12\\.sav") ~ 7,
    stringr::str_detect(filepath, "b14\\.sav") ~ 8,
    stringr::str_detect(filepath, "b16\\.sav") ~ 9.1,
    stringr::str_detect(filepath, "b17\\.sav") ~ 9.2,
    stringr::str_detect(filepath, "k10\\.sav") ~ 4,
    stringr::str_detect(filepath, "k12\\.sav") ~ 5,
    stringr::str_detect(filepath, "k14\\.sav") ~ 6,
    stringr::str_detect(filepath, "k16\\.sav") ~ 7,
    stringr::str_detect(filepath, "k18\\.sav") ~ 8,
    stringr::str_detect(filepath, "k20\\.sav") ~ 9.1,
    stringr::str_detect(filepath, "k21\\.sav") ~ 9.2,
    TRUE ~ NA_real_
  )
  wave_letter <-
    dplyr::case_when(
      age_num == 4 ~ "c",
      age_num == 6 ~ "d",
      age_num == 8 ~ "e",
      age_num == 10 ~ "f",
      age_num == 12 ~ "g",
      age_num == 14 ~ "h",
      age_num == 16 & cohort_letter == "b" ~ "i1",
      age_num == 16 & cohort_letter == "k" ~ "i",
      age_num == 17 ~ "i2",
      age_num == 18 ~ "j",
      age_num == 20 ~ "k1",
      age_num == 21 ~ "k2"
    )

  raw_data <-
    foreign::read.spss(
      filepath,
      to.data.frame = TRUE,
      use.value.labels = TRUE
    ) |>
    dplyr::as_tibble() |>
    dplyr::rename(
      id = hicid,
      wave = wave,
      cohort = cohort,
      sex = zf02m1,
      age_years = any_of(glue::glue("{wave_letter}f03m1")),
      ses = any_of(glue::glue("{wave_letter}sep2")),
      bmi = any_of(glue::glue("{wave_letter}cbmi")),
      bmiz = any_of(glue::glue("{wave_letter}bmiz")),
      body_image = any_of(glue::glue("{wave_letter}hb25c1")),
      body_image_wants = any_of(glue::glue("{wave_letter}hb25c2")),
      parenting_warm_p1 = any_of(glue::glue("{wave_letter}awarm")),
      parenting_warm_p2 = any_of(glue::glue("{wave_letter}bwarm")),
      parenting_warm_m = any_of(glue::glue("{wave_letter}mwarm")),
      parenting_warm_f = any_of(glue::glue("{wave_letter}fwarm")),
      parenting_angry_p1 = any_of(glue::glue("{wave_letter}aang")),
      parenting_angry_p2 = any_of(glue::glue("{wave_letter}bang")),
      parenting_angry_m = any_of(glue::glue("{wave_letter}mang")),
      parenting_angry_f = any_of(glue::glue("{wave_letter}fang"))
    ) |>
    dplyr::select(
      id,
      wave,
      cohort,
      sex,
      age_years,
      ses,
      bmi,
      bmiz,
      starts_with(c("body_image", "parenting_"))
    )

  raw_data
}
