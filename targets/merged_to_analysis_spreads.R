# targets/merged_to_analysis_spreads.R

suppressPackageStartupMessages({
  library(targets)
})

merged_to_analysis_spreads <- function() {
  list(
    tar_target(
      spreads_station_day_parquets,
      {
        balanced_panel_parquets

        years_vec <- 2017:2025
        vapply(
          years_vec,
          function(yy) compute_spreads_station_day_year(
            year = as.integer(yy),
            out_dir = "data/analysis/spreads_station_day"
          ),
          FUN.VALUE = character(1)
        )
      },
      format = "file",
      iteration = "list"
    )
  )
}
