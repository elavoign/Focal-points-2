suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(lubridate)
  library(arrow)
  library(tibble)
  library(tidyr)
})

GALLON_TO_LITER <- 3.785411784

usd_per_gallon_to_mxn_per_liter <- function(price_usd_gal, fx_mxn_usd) {
  (price_usd_gal * fx_mxn_usd) / GALLON_TO_LITER
}

read_regular_usd_gal <- function(path) {
  raw <- read_excel(
    path,
    sheet = "Data 1",
    range = readxl::cell_cols("A:B"),
    col_names = c("excel_date", "regular_usd_gal"),
    col_types = c("numeric", "numeric")
  )

  raw %>%
    filter(is.finite(excel_date)) %>%
    mutate(date = as.Date(excel_date, origin = "1899-12-30")) %>%
    select(date, regular_usd_gal) %>%
    filter(!is.na(regular_usd_gal)) %>%
    arrange(date)
}

read_diesel_usd_gal <- function(path) {
  raw <- read_excel(
    path,
    sheet = "Data 1",
    range = readxl::cell_cols("A:B"),
    col_names = c("excel_date", "diesel_usd_gal"),
    col_types = c("numeric", "numeric")
  )

  raw %>%
    filter(is.finite(excel_date)) %>%
    mutate(date = as.Date(excel_date, origin = "1899-12-30")) %>%
    select(date, diesel_usd_gal) %>%
    filter(!is.na(diesel_usd_gal)) %>%
    arrange(date)
}

read_fx_mxn_usd <- function(path) {
  fx_raw <- read_excel(path, sheet = "tipoCambio", skip = 6)

  fx_raw %>%
    transmute(
      date = dmy(Fecha),
      fx_mxn_usd = `Para solventar\nobligaciones`
    ) %>%
    filter(!is.na(date), !is.na(fx_mxn_usd)) %>%
    arrange(date)
}

expand_daily_locf <- function(df, value_col) {

  df %>%
    arrange(date) %>%
    complete(date = seq(min(date), max(date), by = "day")) %>%
    fill(all_of(value_col), .direction = "down")
}

build_international_daily <- function(path_regular, path_diesel, path_fx) {
  reg  <- read_regular_usd_gal(path_regular) %>%
    expand_daily_locf("regular_usd_gal")

  dies <- read_diesel_usd_gal(path_diesel) %>%
    expand_daily_locf("diesel_usd_gal")

  fx   <- read_fx_mxn_usd(path_fx)

  reg %>%
    inner_join(dies, by = "date") %>%
    inner_join(fx,   by = "date") %>%
    mutate(
      year = year(date),
      regular_int_mxn_l = usd_per_gallon_to_mxn_per_liter(regular_usd_gal, fx_mxn_usd),
      diesel_int_mxn_l  = usd_per_gallon_to_mxn_per_liter(diesel_usd_gal,  fx_mxn_usd)
    ) %>%
    select(date, year, regular_int_mxn_l, diesel_int_mxn_l, fx_mxn_usd) %>%
    arrange(date)
}

write_international_parquets_by_year <- function(df_international, out_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  years <- sort(unique(df_international$year))
  out_paths <- setNames(rep(NA_character_, length(years)), years)

  for (yy in years) {
    yy_dir <- file.path(out_dir, paste0("year=", yy))
    dir.create(yy_dir, showWarnings = FALSE, recursive = TRUE)

    out_path <- file.path(yy_dir, "international.parquet")
    if (file.exists(out_path)) unlink(out_path)

    arrow::write_parquet(df_international %>% filter(year == yy), out_path)

    out_paths[[as.character(yy)]] <- out_path
  }

  tibble(year = years, path = unname(out_paths))
}
