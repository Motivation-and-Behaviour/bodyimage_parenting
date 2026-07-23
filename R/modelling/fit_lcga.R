#' Fit a latent class growth analysis for a given number of classes
#'
#' LCGA proper: linear class-specific trajectories, no within-class random
#' effects (`random = ~ -1`), equal residual variance across classes
#' (`nwg = FALSE`). The one-class model is fit directly; multi-class models
#' start from it via `lcmm::gridsearch()` random starts, so each K branch is
#' independent and reproducible under targets' per-target seeding. No
#' `cl =` parallelism inside gridsearch — crew parallelises across K branches.
#'
#' @param df_model Modelling data from `make_model_data()`.
#' @param k Number of latent classes.
#' @param outcome Outcome column name.
#' @param rep Number of gridsearch random starts.
#' @param maxiter_grid Iterations per random start before the best is refined.
#' @return An `hlme` fit.
#' @author Taren Sanders
#' @export
fit_lcga <- function(
  df_model,
  k,
  outcome = "body_discrepancy",
  rep = 50,
  maxiter_grid = 30
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
  # The stored call holds the symbol `fixed`, but predictY() re-parses the
  # formula from the call, so substitute the actual formula in.
  m1$call$fixed <- fixed

  if (k == 1) {
    return(m1)
  }

  # gridsearch() deparses the inner call's function name, so it must be the
  # bare `hlme` (lcmm attached via require above), not `lcmm::hlme`.
  m <- lcmm::gridsearch(
    hlme(
      fixed,
      mixture = ~time,
      random = ~ -1,
      subject = "id_num",
      ng = k,
      nwg = FALSE,
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
