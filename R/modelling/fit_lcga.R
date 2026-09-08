#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_model df. The modelling data.
#' @param k integer. Number of classes.
#' @param outcome character. The outcome variable.
#' @param rep integer. Number of gridsearch starts.
#' @param maxiter_grid
#' @param nwg
#' @return
#' @author Taren Sanders
#' @export
fit_lcga <- function(
  df_model,
  k,
  outcome = "body_discrepancy",
  rep = 50,
  maxiter_grid = 30,
  nwg = FALSE
) {
  require(lcmm)

  df <- as.data.frame(df_model[, c("id_num", "time", outcome)])
  fixed <- stats::as.formula(paste(outcome, "~ time"))

  m1 <- lcmm::hlme(
    fixed,
    random = ~ -1,
    subject = "id_num",
    ng = 1,
    data = df,
    verbose = FALSE
  )
  # predictY() re-parses the call, so put the real formula back
  m1$call$fixed <- fixed

  if (k == 1) {
    return(m1)
  }

  # gridsearch() deparses the call, so this has to be the bare hlme
  m <- lcmm::gridsearch(
    hlme(
      fixed,
      mixture = ~time,
      random = ~ -1,
      subject = "id_num",
      ng = k,
      nwg = nwg,
      data = df,
      verbose = FALSE
    ),
    rep = rep,
    maxiter = maxiter_grid,
    minit = m1
  )
  m$call$fixed <- fixed
  m
}
