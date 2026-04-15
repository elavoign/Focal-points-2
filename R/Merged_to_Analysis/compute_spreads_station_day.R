# R/Merged_to_Analysis/compute_spreads_station_day.R

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
})

read_panel_year <- function(year) {
  arrow::read_parquet(sprintf(
    "data/merged/panel_station_day/year=%d/panel_station_day.parquet", year
  ))
}

compute_spreads_station_day_year <- function(year, out_dir = "data/analysis/spreads_station_day") {
  panel <- read_panel_year(year)

  out <- panel %>%
    mutate(
      spread_retail_terminal_regular = station_regular - terminal_regular,
      spread_retail_terminal_premium = station_premium - terminal_premium,
      spread_retail_terminal_diesel  = station_diesel  - terminal_diesel,

      spread_terminal_int_regular = terminal_regular - regular_int_mxn_l,
      spread_terminal_int_diesel  = terminal_diesel  - diesel_int_mxn_l,

      spread_retail_int_regular = station_regular - regular_int_mxn_l,
      spread_retail_int_diesel  = station_diesel  - diesel_int_mxn_l
    ) %>%
    select(
        station_id, date, year,
        numero_permiso,
        terminal_id,
        estado, municipio, CVGEO, localidad,
        lat, lon,

        station_regular, station_premium, station_diesel,
        terminal_regular, terminal_premium, terminal_diesel,
        regular_int_mxn_l, diesel_int_mxn_l,

        starts_with("spread_")
    ) %>%
    arrange(station_id, date)

  yy_dir <- file.path(out_dir, paste0("year=", year))
  dir.create(yy_dir, showWarnings = FALSE, recursive = TRUE)
  out_path <- file.path(yy_dir, "spreads_station_day.parquet")
  arrow::write_parquet(out, out_path)

  out_path
}
