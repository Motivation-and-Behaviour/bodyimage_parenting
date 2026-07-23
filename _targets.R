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
  tar_quarto(report, "doc/report.qmd")
)
