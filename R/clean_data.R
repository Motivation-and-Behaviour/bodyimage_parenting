#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_tidy
#' @param remove_outliers
#' @return
#' @author Taren Sanders
#' @export
clean_data <- function(df_tidy, remove_outliers = TRUE) {
  require(dplyr)

  parenting_vars <- c(
    "parenting_warm_p1",
    "parenting_warm_p2",
    "parenting_warm_m",
    "parenting_warm_f",
    "parenting_angry_p1",
    "parenting_angry_p2",
    "parenting_angry_m",
    "parenting_angry_f"
  )
  outlier_vars <- c("ses", parenting_vars)
  scale_vars <- c("bmi", "ses", parenting_vars)

  df_clean <- df_tidy

  if (remove_outliers) {
    # Flag values more than 4 SD from the mean within each wave and cohort.
    df_clean <- df_clean |>
      dplyr::group_by(wave, cohort) |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(outlier_vars),
        ~ dplyr::if_else(abs(scale(.x)[, 1]) > 4, NA_real_, .x)
      )) |>
      dplyr::ungroup() |>
      # BMI is handled via its pre-standardised z-score; drop the raw BMI too.
      dplyr::mutate(
        bmiz = dplyr::if_else(abs(bmiz) > 4, NA_real_, bmiz),
        bmi = dplyr::if_else(is.na(bmiz), NA_real_, bmi)
      )
  }

  # Standardise continuous predictors within each wave and cohort.
  df_clean <- df_clean |>
    dplyr::group_by(wave, cohort) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(scale_vars),
      ~ scale(.x)[, 1],
      .names = "{.col}_z"
    )) |>
    dplyr::ungroup()

  # Children's Body Image Scale (Truby & Paxton, 2002): the perceived-minus-ideal
  # figure discrepancy is the standard body-dissatisfaction score. Figures run
  # from thinnest (Picture 1) to heaviest (Picture 7), so a positive score means
  # the child perceives themselves as larger than their ideal.
  df_clean <- df_clean |>
    dplyr::mutate(
      body_perceived = as.numeric(
        stringr::str_extract(as.character(body_image), "\\d+")
      ),
      body_ideal = as.numeric(
        stringr::str_extract(as.character(body_image_wants), "\\d+")
      ),
      body_discrepancy = body_perceived - body_ideal,
      body_dissatisfaction = factor(
        dplyr::case_when(
          body_discrepancy < 0 ~ "Thinner than ideal",
          body_discrepancy == 0 ~ "Ideal",
          body_discrepancy > 0 ~ "Larger than ideal"
        ),
        levels = c("Thinner than ideal", "Ideal", "Larger than ideal")
      )
    ) |>
    dplyr::select(-body_perceived, -body_ideal)

  labelled::var_label(df_clean) <- list(
    age_years = "Age (Years)",
    age_cat = "Age Group",
    sex = "Sex",
    ses = "Socioeconomic Position",
    bmi = "Body Mass Index",
    bmiz = "BMI z-score",
    body_image = "Body Image (Current)",
    body_image_wants = "Body Image (Ideal)",
    body_discrepancy = "Body Dissatisfaction (Perceived - Ideal)",
    body_dissatisfaction = "Body Dissatisfaction",
    parenting_warm_p1 = "Parental Warmth (Parent 1)",
    parenting_warm_p2 = "Parental Warmth (Parent 2)",
    parenting_warm_m = "Parental Warmth (Mother)",
    parenting_warm_f = "Parental Warmth (Father)",
    parenting_angry_p1 = "Parental Anger (Parent 1)",
    parenting_angry_p2 = "Parental Anger (Parent 2)",
    parenting_angry_m = "Parental Anger (Mother)",
    parenting_angry_f = "Parental Anger (Father)"
  )

  df_clean
}
