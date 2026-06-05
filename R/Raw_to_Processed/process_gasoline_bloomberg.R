suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(arrow)
  library(lubridate)
})

# Reads GASOLINE.xlsx (Bloomberg spot assessments downloaded from ITAM finance lab).
# Sheets used:
#   MOIGC87P — Gulf Coast Conv Regular 87 (monthly, up to Jan 2024)
#   MOIGC93P — Gulf Coast Conv Premium 93 (monthly, up to Jan 2024)
# Bloomberg discontinued spot assessments on 2024-01-31.
# Prices are reported in US cents per gallon (USc/gal).
# Output converts to MXN per litre using monthly average FX from Banxico
# (sourced from the existing international processed parquet).

process_gasoline_bloomberg <- function(
  xlsx_path   = "data/raw_public/GASOLINE.xlsx",
  intl_dir    = "data/processed/international",
  out_parquet = "data/processed/bloomberg/gasoline_bloomberg.parquet"
) {
  dir.create(dirname(out_parquet), recursive = TRUE, showWarnings = FALSE)

  # Monthly average FX (MXN per USD) from existing international dataset
  intl_files <- list.files(intl_dir, pattern = "\\.parquet$",
                           full.names = TRUE, recursive = TRUE)
  fx_monthly <- arrow::open_dataset(intl_files) |>
    dplyr::select(date, fx_mxn_usd) |>
    dplyr::collect() |>
    dplyr::mutate(
      date  = as.Date(date),
      year  = as.integer(format(date, "%Y")),
      month = as.integer(format(date, "%m"))
    ) |>
    dplyr::group_by(year, month) |>
    dplyr::summarise(fx_mxn_usd = mean(fx_mxn_usd, na.rm = TRUE),
                     .groups = "drop")

  parse_sheet <- function(sheet) {
    df <- suppressMessages(
      readxl::read_excel(xlsx_path, sheet = sheet, skip = 6)
    ) |>
      dplyr::select(1:2) |>
      setNames(c("date_raw", "price_usc_gal")) |>
      dplyr::filter(!is.na(price_usc_gal)) |>
      dplyr::mutate(
        date          = suppressWarnings(
          as.Date(as.numeric(date_raw), origin = "1899-12-30")),
        price_usc_gal = suppressWarnings(as.numeric(price_usc_gal))
      ) |>
      dplyr::filter(!is.na(date), !is.na(price_usc_gal), price_usc_gal > 0) |>
      dplyr::mutate(
        year  = as.integer(format(date, "%Y")),
        month = as.integer(format(date, "%m"))
      ) |>
      dplyr::select(year, month, price_usc_gal)
    df
  }

  regular_87 <- parse_sheet("MOIGC87P") |>
    dplyr::rename(regular_87_usc_gal = price_usc_gal)

  premium_93 <- parse_sheet("MOIGC93P") |>
    dplyr::rename(premium_93_usc_gal = price_usc_gal)

  # 1 USc/gal → MXN/l: × 0.01 (USc→USD) ÷ 3.78541 (gal→l) × fx (USD→MXN)
  out <- dplyr::full_join(regular_87, premium_93, by = c("year", "month")) |>
    dplyr::left_join(fx_monthly, by = c("year", "month")) |>
    dplyr::mutate(
      regular_87_mxn_l = regular_87_usc_gal * 0.01 / 3.78541 * fx_mxn_usd,
      premium_93_mxn_l = premium_93_usc_gal * 0.01 / 3.78541 * fx_mxn_usd,
      date             = lubridate::make_date(year, month, 1L)
    ) |>
    dplyr::filter(!is.na(regular_87_mxn_l) | !is.na(premium_93_mxn_l)) |>
    dplyr::arrange(date) |>
    dplyr::select(date, year, month,
                  regular_87_usc_gal, premium_93_usc_gal,
                  fx_mxn_usd, regular_87_mxn_l, premium_93_mxn_l)

  arrow::write_parquet(out, out_parquet, compression = "zstd")

  n_reg  <- sum(!is.na(out$regular_87_mxn_l))
  n_prem <- sum(!is.na(out$premium_93_mxn_l))
  message(sprintf(
    "Bloomberg gasoline: %d months total | Regular 87: %d (%s–%s) | Premium 93: %d (%s–%s)",
    nrow(out),
    n_reg,
    format(min(out$date[!is.na(out$regular_87_mxn_l)])),
    format(max(out$date[!is.na(out$regular_87_mxn_l)])),
    n_prem,
    format(min(out$date[!is.na(out$premium_93_mxn_l)])),
    format(max(out$date[!is.na(out$premium_93_mxn_l)]))
  ))

  out_parquet
}
