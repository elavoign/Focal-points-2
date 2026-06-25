suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(lubridate)
  library(tibble)
  library(stringr)
})

read_retail_year <- function(year) {
  arrow::read_parquet(sprintf("data/processed/retail/year=%d/retail.parquet", year), mmap = FALSE)
}

read_terminal_year <- function(year) {
  arrow::read_parquet(sprintf("data/processed/terminal/year=%d/terminal.parquet", year), mmap = FALSE)
}

read_international_year <- function(year) {
  arrow::read_parquet(sprintf("data/processed/international/year=%d/international.parquet", year), mmap = FALSE)
}

read_stations <- function() {
  arrow::read_parquet("data/processed/stations/stations.parquet", mmap = FALSE)
}

build_panel_station_day_year <- function(year, out_dir = "data/merged/panel_station_day") {

  retail <- read_retail_year(year) %>%
    transmute(
      station_id,
      numero_permiso,
      date,
      year,
      station_regular = regular,
      station_premium = premium,
      station_diesel  = diesel,
      flag_bad_date_station = flag_bad_date,
      flag_missing_any_price_station = flag_missing_any_price,
      flag_nonpositive_any_price_station = flag_nonpositive_any_price,
      flag_dup_station_day_station = flag_dup_station_day
    )

  stations <- read_stations() %>%
    transmute(
      station_id,
      terminal_id,
      estado,
      municipio,
      CVEGEO,
      localidad,
      lat,
      lon,
      flag_missing_terminal_id
    )

  terminal <- read_terminal_year(year) %>%
    transmute(
      terminal_id,
      date,
      year,
      terminal_regular = regular,
      terminal_premium = premium,
      terminal_diesel  = diesel,
      flag_bad_date_terminal = flag_bad_date,
      flag_missing_any_price_terminal = flag_missing_any_price,
      flag_nonpositive_any_price_terminal = flag_nonpositive_any_price
    )

  intl <- read_international_year(year) %>%
    transmute(
      date,
      year,
      regular_int_mxn_l,
      diesel_int_mxn_l,
      fx_mxn_usd
    )

  retail   <- retail   %>% distinct(station_id, date, year, .keep_all = TRUE)
  stations <- stations %>% distinct(station_id, .keep_all = TRUE)
  terminal <- terminal %>% distinct(terminal_id, date, year, .keep_all = TRUE)

  panel <- retail %>%
    left_join(stations, by = "station_id") %>%
    left_join(terminal, by = c("terminal_id", "date", "year")) %>%
    left_join(intl, by = c("date", "year")) %>%
    arrange(station_id, date)

  coverage <- mean(!is.na(panel$terminal_regular))
  if (coverage < 0.10) {
    stop(sprintf("Terminal join coverage too low: %.2f", coverage))
  }

  yy_dir <- file.path(out_dir, paste0("year=", year))
  dir.create(yy_dir, showWarnings = FALSE, recursive = TRUE)

  out_path <- file.path(yy_dir, "panel_station_day.parquet")
  arrow::write_parquet(panel, out_path)

  out_path
}
