#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param threestep_fit
#' @return
#' @author Taren Sanders
#' @export
make_multinom_table <- function(threestep_fit) {
  require(dplyr)

  term_labels <- c(
    "(Intercept)" = "Intercept",
    parenting_warm_p1_z_bl = "Warmth P1 (per SD)",
    parenting_warm_p2_z_bl = "Warmth P2 (per SD)",
    parenting_angry_p1_z_bl = "Anger P1 (per SD)",
    parenting_angry_p2_z_bl = "Anger P2 (per SD)",
    sexFemale = "Sex: female",
    ses_z_bl = "SES (per SD)",
    bmiz_bl = "BMI z-score (per SD)",
    "parenting_warm_p1_z_bl:sexFemale" = "Warmth P1 x female",
    "parenting_angry_p1_z_bl:sexFemale" = "Anger P1 x female"
  )

  fmt_p <- function(p) {
    ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
  }

  threestep_fit$tidy |>
    dplyr::mutate(
      Term = dplyr::coalesce(unname(term_labels[term]), term),
      class_col = paste0("Class ", class, " vs ", threestep_fit$ref_class),
      cell = sprintf(
        "%.2f (%.2f, %.2f); p=%s",
        rrr,
        rrr_lower,
        rrr_upper,
        fmt_p(p)
      )
    ) |>
    dplyr::select(Term, class_col, cell) |>
    tidyr::pivot_wider(names_from = class_col, values_from = cell) |>
    dplyr::filter(Term != "Intercept")
}
