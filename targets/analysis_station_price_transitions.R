suppressPackageStartupMessages({
  library(targets)
})

analysis_station_price_transitions <- function() {
  list(
    tar_target(
      station_price_transition_files,
      {
        spreads_station_day_parquets

        build_station_price_transition_outputs(
          years = c(2024, 2025),
          reform_date = as.Date("2025-03-03"),
          out_dir = "outputs/station_price_transitions"
        )
      },
      format = "file"
    )
  )
}