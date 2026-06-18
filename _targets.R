library(targets)

lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source)

tar_option_set(
  packages = character() # add packages your targets need
)

# Replace these example targets with your own analysis steps
list(
  tar_target(example_data, make_example_data()),
  tar_target(example_summary, summarise_example(example_data))
)
