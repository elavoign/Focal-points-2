library(targets)

tar_option_set(
  packages = c(
    "dplyr",
    "readxl",
    "arrow",
    "tibble",
    "stringr",
    "sf",
    "magick",
    "httr2",
    "xml2",
    "rvest",
    "purrr",
    "readr",
    "ggplot2",
    "scales",
    "grid",
    "tarchetypes",
    "openxlsx",
    "lubridate",
    "tidyr"
  )
)

tar_source("R")
tar_source("targets")

list(
  raw_to_processed(),
  raw_to_processed_int(),
  raw_to_processed_inegi(),
  analysis_to_graphs_inegi(),
  analysis_to_map(),
  processed_to_merged_panel(),
  merged_to_analysis_spreads(),
  analysis_aggregations(),
  analysis_station_price_transitions(),
  analysis_station_price_transition_graphs(),
  analysis_regular_break_2399(),
  analysis_to_graphs(),
  analysis_to_map_heatmaps(),
  outputpdf(),
  shaun_mun_month()
)