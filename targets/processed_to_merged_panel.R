# targets/processed_to_merged_panel.R

suppressPackageStartupMessages({
  library(targets)
})

processed_to_merged_panel <- function() {
  list(
    tar_target(
      panel_station_day_parquets,
      {
        # --- Dependencias upstream (raw_to_processed) ---
        c(
          retail_2017_parquet, retail_2018_parquet, retail_2019_parquet,
          retail_2020_parquet, retail_2021_parquet, retail_2022_parquet,
          retail_2023_parquet, retail_2024_parquet, retail_2025_parquet,
          terminal_2017_parquet, terminal_2018_parquet, terminal_2019_parquet,
          terminal_2020_parquet, terminal_2021_parquet, terminal_2022_parquet,
          terminal_2023_parquet, terminal_2024_parquet, terminal_2025_parquet,
          stations_parquet
        )

        years_vec <- 2017:2025

        vapply(
          X = years_vec,
          FUN = function(yy) {
            build_panel_station_day_year(
              year = as.integer(yy),
              out_dir = "data/merged/panel_station_day"
            )
          },
          FUN.VALUE = character(1)
        )
      },
      format = "file",
      iteration = "list"
    )
  )
}
