#' Format a growth model's fixed effects as a report-ready table
#'
#' @param growth_fit Output of `fit_growth_model()`.
#' @return Tibble ready for `knitr::kable()`.
#' @author Taren Sanders
#' @export
make_growth_model_table <- function(growth_fit) {
  require(dplyr)

  atom_labels <- c(
    "(Intercept)" = "Intercept",
    parenting_warm_p1_z_bl = "Warmth P1",
    parenting_angry_p1_z_bl = "Anger P1",
    time = "Time (per year)",
    sexFemale = "Female",
    ses_z_bl = "SES",
    bmiz_bl = "BMI z-score"
  )
  label_term <- function(term) {
    atoms <- strsplit(term, ":", fixed = TRUE)[[1]]
    labelled <- dplyr::coalesce(unname(atom_labels[atoms]), atoms)
    paste(labelled, collapse = " × ")
  }

  growth_fit$tidy |>
    dplyr::mutate(
      Term = vapply(term, label_term, character(1)),
      `Estimate (95% CI)` = sprintf(
        "%.3f (%.3f, %.3f)",
        estimate,
        conf.low,
        conf.high
      ),
      p = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
    ) |>
    dplyr::select(Term, `Estimate (95% CI)`, p)
}
