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
    "tibble",
    "lme4",
    "lmerTest",
    "broom.mixed"
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

lcga_spec <- tidyr::expand_grid(
  outcome = c("body_discrepancy", "body_discrepancy_abs"),
  k = 1:5
)

# chosen after reviewing the enumeration
lcga_k_chosen <- 3
lcga_k_chosen_abs <- 3

rhs_parenting_primary <- c(
  "parenting_warm_p1_z_bl",
  "parenting_angry_p1_z_bl",
  "sex",
  "ses_z_bl",
  "bmiz_bl"
)
rhs_parenting_both <- c(
  "parenting_warm_p1_z_bl",
  "parenting_warm_p2_z_bl",
  "parenting_angry_p1_z_bl",
  "parenting_angry_p2_z_bl",
  "sex",
  "ses_z_bl",
  "bmiz_bl"
)
rhs_parenting_moderation <- c(
  "parenting_warm_p1_z_bl * sex",
  "parenting_angry_p1_z_bl * sex",
  "ses_z_bl",
  "bmiz_bl"
)

lcga_targets <- tar_map(
  values = lcga_spec,
  names = tidyselect::all_of(c("outcome", "k")),
  tar_target(lcga_fit, fit_lcga(df_model, k, outcome), format = "qs"),
  tar_target(lcga_fit_summary, summarise_lcga_fit(lcga_fit, outcome))
)

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
  tar_target(attrition_table, make_attrition_table(df_clean)),
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
  tar_target(
    outcome_distribution_plot,
    plot_outcome_distribution(df_model, class_assignments)
  ),
  tar_target(
    outcome_normality_table,
    summarise_outcome_normality(df_model, class_assignments)
  ),
  # sensitivity: class-specific residual variance
  tar_target(
    lcga_final_nwg,
    fit_lcga(df_model, lcga_k_chosen, "body_discrepancy", nwg = TRUE),
    format = "qs"
  ),
  tar_target(
    lcga_nwg_comparison,
    dplyr::bind_rows(
      summarise_lcga_fit(
        lcga_final,
        "body_discrepancy",
        sample = "Equal variance (primary)"
      ),
      summarise_lcga_fit(
        lcga_final_nwg,
        "body_discrepancy",
        sample = "Class-specific variance"
      )
    )
  ),
  # class membership models
  tar_target(
    threestep_data,
    prepare_threestep_data(class_assignments, df_model),
    format = "qs"
  ),
  tar_target(
    threestep_data_modal,
    prepare_threestep_data(class_assignments, df_model, modal = TRUE),
    format = "qs"
  ),
  tar_target(
    class_descriptives_table,
    make_class_descriptives_table(class_assignments, df_model)
  ),
  tar_target(
    parenting_model,
    fit_threestep_multinom(threestep_data, rhs_parenting_primary),
    format = "qs"
  ),
  tar_target(parenting_model_table, make_multinom_table(parenting_model)),
  tar_target(
    parenting_model_bothparents,
    fit_threestep_multinom(threestep_data, rhs_parenting_both),
    format = "qs"
  ),
  tar_target(
    parenting_model_bothparents_table,
    make_multinom_table(parenting_model_bothparents)
  ),
  tar_target(
    parenting_model_modal,
    fit_threestep_multinom(threestep_data_modal, rhs_parenting_primary),
    format = "qs"
  ),
  tar_target(
    parenting_model_modal_table,
    make_multinom_table(parenting_model_modal)
  ),
  tar_target(
    moderation_model,
    fit_threestep_multinom(threestep_data, rhs_parenting_moderation),
    format = "qs"
  ),
  tar_target(
    moderation_test,
    compare_multinom_models(parenting_model, moderation_model)
  ),
  tar_target(moderation_model_table, make_multinom_table(moderation_model)),
  tar_target(
    moderation_plot,
    plot_class_probabilities(moderation_model, threestep_data)
  ),
  # growth models
  tar_target(
    growth_model,
    fit_growth_model(df_model, "body_discrepancy"),
    format = "qs"
  ),
  tar_target(
    growth_model_abs,
    fit_growth_model(df_model, "body_discrepancy_abs"),
    format = "qs"
  ),
  tar_target(
    growth_moderation_model,
    fit_growth_model(df_model, "body_discrepancy", moderation = TRUE),
    format = "qs"
  ),
  tar_target(
    growth_moderation_lrt,
    growth_moderation_test(growth_model, growth_moderation_model)
  ),
  tar_target(growth_model_table, make_growth_model_table(growth_model)),
  tar_target(
    growth_model_abs_table,
    make_growth_model_table(growth_model_abs)
  ),
  tar_target(
    growth_moderation_table,
    make_growth_model_table(growth_moderation_model)
  ),
  tar_target(
    growth_predictions_plot,
    plot_growth_predictions(growth_moderation_model)
  ),
  # absolute discrepancy sensitivity
  tar_target(
    lcga_trajectory_plot_abs,
    plot_lcga_trajectories(
      lcga_fits,
      df_model,
      outcome = "body_discrepancy_abs",
      ylab = "Absolute body dissatisfaction |perceived - ideal|"
    )
  ),
  tar_target(
    lcga_final_abs,
    lcga_fits[[paste0("lcga_fit_body_discrepancy_abs_", lcga_k_chosen_abs)]],
    format = "qs"
  ),
  tar_target(
    class_assignments_abs,
    extract_class_assignments(lcga_final_abs, df_model),
    format = "qs"
  ),
  tar_target(
    lcga_class_plot_abs,
    plot_lcga_classes(
      lcga_final_abs,
      class_assignments_abs,
      df_model,
      outcome = "body_discrepancy_abs",
      ylab = "Absolute body dissatisfaction |perceived - ideal|"
    )
  ),
  tar_target(
    threestep_data_abs,
    prepare_threestep_data(class_assignments_abs, df_model),
    format = "qs"
  ),
  tar_target(
    parenting_model_abs,
    fit_threestep_multinom(threestep_data_abs, rhs_parenting_primary),
    format = "qs"
  ),
  tar_target(
    parenting_model_table_abs,
    make_multinom_table(parenting_model_abs)
  ),
  tar_quarto(report, "doc/report.qmd")
)
