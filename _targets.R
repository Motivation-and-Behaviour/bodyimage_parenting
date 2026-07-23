library(targets)
library(tarchetypes)

tar_option_set(
  packages = c(
    "dplyr",
    "forcats",
    "labelled",
    "tableone",
    "janitor",
    "stringr",
    "glue",
    "ggplot2",
    "lcmm",
    "nnet",
    "tidyr",
    "purrr",
    "broom",
    "tibble"
  ),
  controller = crew::crew_controller_local(
    workers = min(parallel::detectCores() - 2, 20),
    seconds_idle = 15
  )
)

tar_source()

lsac_files <- c(
  "lsacgrb8.sav",
  "lsacgrb10.sav",
  "lsacgrb12.sav",
  "lsacgrk8.sav",
  "lsacgrk10.sav",
  "lsacgrk12.sav"
)
lsac_path <- "/data/2 LSAC 10 General Release/Survey Data/SPSS"
input_files <- file.path(lsac_path, lsac_files)

# LCGA class-enumeration grid: one branch per outcome x K, combined below.
lcga_spec <- tidyr::expand_grid(outcome = "body_discrepancy", k = 1:5)

# Chosen number of classes (decided with user 2026-07-23 after reviewing
# enumeration + sex-stratified check). The only place to change K.
lcga_k_chosen <- 3

lcga_targets <- tar_map(
  values = lcga_spec,
  names = tidyselect::all_of(c("outcome", "k")),
  tar_target(lcga_fit, fit_lcga(df_model, k, outcome), format = "qs"),
  tar_target(lcga_fit_summary, summarise_lcga_fit(lcga_fit, outcome))
)

# Sex-stratified enumeration: diagnostic for the pooled-vs-stratified
# decision, run before freezing K (see ANALYSIS_PLAN.md Step 3).
lcga_strat_spec <- tidyr::expand_grid(
  outcome = "body_discrepancy",
  sex_group = c("boys", "girls"),
  k = 1:5
)

lcga_strat_targets <- tar_map(
  values = lcga_strat_spec,
  names = tidyselect::all_of(c("outcome", "sex_group", "k")),
  tar_target(
    lcga_strat_fit,
    fit_lcga(subset_model_data_by_sex(df_model, sex_group), k, outcome),
    format = "qs"
  ),
  tar_target(
    lcga_strat_fit_summary,
    summarise_lcga_fit(lcga_strat_fit, outcome, sample = sex_group)
  )
)

list(
  tar_files_input(waves, input_files),
  tar_target(
    waves_data,
    read_waves_data(waves),
    pattern = map(waves),
    iteration = "list",
    format = "qs"
  ),
  tar_target(waves_joined, dplyr::bind_rows(waves_data), format = "qs"),
  tar_target(df_tidy, tidy_data(waves_joined), format = "qs"),
  tar_target(df_clean, clean_data(df_tidy), format = "qs"),
  tar_target(descriptives_table, make_descriptives_table(df_clean)),
  tar_target(
    body_dissatisfaction_plot,
    plot_body_dissatisfaction(df_clean)
  ),
  tar_target(df_model, make_model_data(df_clean), format = "qs"),
  tar_target(sample_flow_table, summarise_sample_flow(df_clean, df_model)),
  lcga_targets,
  tar_combine(
    lcga_fit_stats,
    lcga_targets[["lcga_fit_summary"]],
    command = dplyr::bind_rows(!!!.x)
  ),
  tar_combine(
    lcga_fits,
    lcga_targets[["lcga_fit"]],
    command = list(!!!.x),
    format = "qs"
  ),
  tar_target(
    lcga_trajectory_plot,
    plot_lcga_trajectories(lcga_fits, df_model)
  ),
  lcga_strat_targets,
  tar_combine(
    lcga_strat_fit_stats,
    lcga_strat_targets[["lcga_strat_fit_summary"]],
    command = dplyr::bind_rows(!!!.x)
  ),
  tar_combine(
    lcga_strat_fits,
    lcga_strat_targets[["lcga_strat_fit"]],
    command = list(!!!.x),
    format = "qs"
  ),
  tar_target(
    lcga_sex_comparison_plot,
    plot_lcga_sex_comparison(lcga_fits, lcga_strat_fits, df_model)
  ),
  tar_target(
    lcga_final,
    lcga_fits[[paste0("lcga_fit_body_discrepancy_", lcga_k_chosen)]],
    format = "qs"
  ),
  tar_target(
    class_assignments,
    extract_class_assignments(lcga_final, df_model),
    format = "qs"
  ),
  tar_target(
    lcga_class_plot,
    plot_lcga_classes(lcga_final, class_assignments, df_model)
  ),
  tar_quarto(report, "doc/report.qmd")
)
