# R/Processed_to_Merged/build_balanced_panel.R
#
# Creates a BALANCED station x day panel.
# For each station active in a given year, guarantees exactly one row per calendar
# day in that year. Missing prices are filled forward (LOCF, carry-forward) from
# the last observed price within the same year.
#
# Carry-forward cap: if a station has gone more than 60 days without reporting
# any non-NA price, the carried-forward values are set back to NA and flagged.
#
# Design assumptions (documented):
#  - "Active station in year Y" = station_id present in panel_station_day for year Y.
#  - Carry-forward does NOT cross year boundaries. A station that last reported on
#    Dec 30 will have NA prices at Jan 1 of the next year (conservative).
#  - "Reporting" = having at least one non-NA station price (regular OR premium).
#    A station-day in the retail data with all-NA prices does NOT reset the
#    staleness clock (the last valid price was before that date).
#  - The 60-day window is measured per station as the number of days elapsed
#    since the last non-NA station price observation within the year.
#  - Terminal, international, and quality-flag columns are NOT carry-forwarded;
#    they remain NA for synthetic (grid-added) rows, since they belong to
#    different data sources with their own reporting schedules.

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(lubridate)
  library(tidyr)
  library(stringr)
})

BALANCED_PANEL_MAX_CARRY_DAYS <- 60L

build_balanced_panel_year <- function(year,
                                      in_dir  = "data/merged/panel_station_day",
                                      out_dir = "data/merged/balanced_panel") {

  in_path <- file.path(in_dir, paste0("year=", year), "panel_station_day.parquet")
  panel   <- arrow::read_parquet(in_path)

  # All calendar days in the year
  year_dates <- seq.Date(
    from = as.Date(sprintf("%d-01-01", year)),
    to   = as.Date(sprintf("%d-12-31", year)),
    by   = "day"
  )
  n_days       <- length(year_dates)
  all_stations <- unique(panel$station_id)
  n_stations   <- length(all_stations)
  expected_rows <- n_stations * n_days

  message(sprintf(
    "[balanced_panel %d] %d stations x %d days = %d expected rows",
    year, n_stations, n_days, expected_rows
  ))

  # --- Station-level metadata (time-invariant per station) ---
  station_meta <- panel %>%
    select(
      station_id, numero_permiso, terminal_id,
      estado, municipio, CVGEO, localidad, lat, lon,
      flag_missing_terminal_id
    ) %>%
    distinct(station_id, .keep_all = TRUE)

  # --- Actual price observations from retail data ---
  # Mark each row as observed (is_obs = TRUE) so we can distinguish grid rows later
  obs_data <- panel %>%
    transmute(
      station_id,
      date,
      station_regular, station_premium, station_diesel,
      terminal_regular, terminal_premium, terminal_diesel,
      regular_int_mxn_l, diesel_int_mxn_l, fx_mxn_usd,
      flag_bad_date_station,
      flag_missing_any_price_station,
      flag_nonpositive_any_price_station,
      flag_dup_station_day_station,
      flag_bad_date_terminal,
      flag_missing_any_price_terminal,
      flag_nonpositive_any_price_terminal,
      is_obs = TRUE
    )

  # --- Build the full balanced grid ---
  grid <- tidyr::expand_grid(station_id = all_stations, date = year_dates)

  balanced <- grid %>%
    left_join(station_meta, by = "station_id") %>%
    left_join(obs_data,     by = c("station_id", "date")) %>%
    mutate(
      is_obs = coalesce(is_obs, FALSE),
      # A "valid report" requires at least one non-NA station price.
      # This is what resets the staleness clock.
      has_any_price = is_obs & (!is.na(station_regular) | !is.na(station_premium))
    ) %>%
    arrange(station_id, date)   # CRITICAL: fill() respects row order

  # --- Carry-forward prices and track last-price date ---
  balanced <- balanced %>%
    group_by(station_id) %>%
    mutate(
      last_price_date = if_else(has_any_price, date, as.Date(NA_character_))
    ) %>%
    tidyr::fill(
      last_price_date,
      station_regular, station_premium, station_diesel,
      .direction = "down"
    ) %>%
    mutate(
      days_since_last_report = if_else(
        !is.na(last_price_date),
        as.integer(date - last_price_date),
        NA_integer_
      ),
      flag_carry_forward  = !has_any_price & !is.na(last_price_date),
      flag_stale_over_60d = !is.na(days_since_last_report) &
                            days_since_last_report > BALANCED_PANEL_MAX_CARRY_DAYS,
      year = as.integer(year)
    ) %>%
    # Apply 60-day cap: nullify stale carry-forward prices
    mutate(
      station_regular = if_else(flag_stale_over_60d, NA_real_, station_regular),
      station_premium = if_else(flag_stale_over_60d, NA_real_, station_premium),
      station_diesel  = if_else(flag_stale_over_60d, NA_real_, station_diesel)
    ) %>%
    ungroup()

  # ==========================================================================
  # VALIDATION CHECKS
  # ==========================================================================

  # (1) Balanced: total row count
  if (nrow(balanced) != expected_rows) {
    stop(sprintf(
      "[balanced_panel %d] BALANCE CHECK FAILED: expected %d rows, got %d",
      year, expected_rows, nrow(balanced)
    ))
  }

  # (2) Balanced: every station has exactly n_days rows
  rows_bad <- balanced %>%
    count(station_id) %>%
    filter(n != n_days)
  if (nrow(rows_bad) > 0L) {
    stop(sprintf(
      "[balanced_panel %d] %d stations do not have exactly %d rows.",
      year, nrow(rows_bad), n_days
    ))
  }

  # (3) 60-day rule: no stale row should have a non-NA price
  stale_with_price <- balanced %>%
    filter(flag_stale_over_60d) %>%
    summarise(n = sum(!is.na(station_regular) | !is.na(station_premium))) %>%
    pull(n)
  if (stale_with_price > 0L) {
    stop(sprintf(
      "[balanced_panel %d] BUG: %d stale rows still have non-NA prices.",
      year, stale_with_price
    ))
  }

  # (4) Summary stats
  n_original <- sum(!balanced$is_obs)   # wait, is_obs=FALSE means it was added by grid
  # Actually: is_obs TRUE = was in retail data; FALSE = synthetic row added by grid
  n_synthetic  <- sum(!balanced$is_obs)
  n_carry      <- sum(balanced$flag_carry_forward)
  n_stale      <- sum(balanced$flag_stale_over_60d)
  pct_carry    <- round(100 * n_carry  / nrow(balanced), 1)
  pct_stale    <- round(100 * n_stale  / nrow(balanced), 1)
  pct_synth    <- round(100 * n_synthetic / nrow(balanced), 1)

  pct_na_reg  <- round(100 * mean(is.na(balanced$station_regular)),  1)
  pct_na_prem <- round(100 * mean(is.na(balanced$station_premium)), 1)

  message(sprintf(
    "[balanced_panel %d] synthetic rows (grid-added): %d (%.1f%%)",
    year, n_synthetic, pct_synth
  ))
  message(sprintf(
    "[balanced_panel %d] carry-forward used: %d rows (%.1f%%)",
    year, n_carry, pct_carry
  ))
  message(sprintf(
    "[balanced_panel %d] discarded stale >%dd: %d rows (%.1f%%)",
    year, BALANCED_PANEL_MAX_CARRY_DAYS, n_stale, pct_stale
  ))
  message(sprintf(
    "[balanced_panel %d] NA rate after imputation: regular=%.1f%%, premium=%.1f%%",
    year, pct_na_reg, pct_na_prem
  ))

  # ==========================================================================
  # WRITE
  # ==========================================================================
  yy_dir   <- file.path(out_dir, paste0("year=", year))
  dir.create(yy_dir, showWarnings = FALSE, recursive = TRUE)
  out_path <- file.path(yy_dir, "balanced_panel.parquet")
  arrow::write_parquet(balanced, out_path, compression = "zstd")

  out_path
}
