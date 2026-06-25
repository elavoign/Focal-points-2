suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(arrow)
  library(tidyr)
  library(purrr)
  library(lubridate)
})

.parse_excel_date <- function(x) {
  n <- suppressWarnings(as.numeric(x))
  as.Date(ifelse(is.na(n), NA_real_, n), origin = "1899-12-30")
}

.read_cuotas_base <- function(xlsx_path) {
  raw <- readxl::read_excel(
    xlsx_path,
    sheet     = "CUOTAS_BASE",
    col_names = FALSE,
    col_types = "text"
  )

  df <- raw[-(1:2), 1:4]
  names(df) <- c("year", "magna_base", "prem_base", "diesel_base")
  df |>
    dplyr::filter(!is.na(year), suppressWarnings(!is.na(as.numeric(year)))) |>
    dplyr::transmute(
      year         = as.integer(year),
      magna_base   = as.numeric(magna_base),
      prem_base    = as.numeric(prem_base),
      diesel_base  = as.numeric(diesel_base)
    )
}

.read_ieps_raw <- function(xlsx_path) {

  raw <- readxl::read_excel(
    xlsx_path,
    sheet     = "DATOS",
    skip      = 1,
    col_names = FALSE,
    col_types = "text"
  )

  df     <- raw[-1L, ]
  nc     <- ncol(df)
  labels <- c(
    "fecha_dof", "url", "fecha_inicio", "fecha_fin",
    "magna_estimulo_pct", "magna_cuota",   "magna_ieps_base",
    "prem_estimulo_pct",  "prem_cuota",    "prem_ieps_base",
    "diesel_estimulo_pct","diesel_cuota",  "diesel_ieps_base",
    "aux1", "aux2"
  )
  names(df) <- labels[seq_len(nc)]
  df
}

.clean_ieps <- function(df, cuotas_base) {
  df |>
    dplyr::mutate(
      fecha_dof    = .parse_excel_date(fecha_dof),
      fecha_inicio = .parse_excel_date(fecha_inicio),
      fecha_fin    = .parse_excel_date(fecha_fin),
      across(
        c(magna_estimulo_pct, magna_cuota,
          prem_estimulo_pct,  prem_cuota,
          diesel_estimulo_pct, diesel_cuota),
        ~ suppressWarnings(as.numeric(.x))
      )
    ) |>
    dplyr::filter(!is.na(fecha_inicio), !is.na(fecha_fin)) |>
    dplyr::mutate(year = lubridate::year(fecha_inicio)) |>
    dplyr::left_join(cuotas_base, by = "year") |>
    dplyr::mutate(

      magna_estimulo_pct  = dplyr::if_else(is.na(magna_cuota),  0,    magna_estimulo_pct),
      magna_cuota         = dplyr::if_else(is.na(magna_cuota),  magna_base,   magna_cuota),
      prem_estimulo_pct   = dplyr::if_else(is.na(prem_cuota),   0,    prem_estimulo_pct),
      prem_cuota          = dplyr::if_else(is.na(prem_cuota),   prem_base,    prem_cuota),
      diesel_estimulo_pct = dplyr::if_else(is.na(diesel_cuota), 0,    diesel_estimulo_pct),
      diesel_cuota        = dplyr::if_else(is.na(diesel_cuota), diesel_base,  diesel_cuota)
    ) |>
    dplyr::select(fecha_dof, fecha_inicio, fecha_fin,
                  magna_estimulo_pct, magna_cuota,
                  prem_estimulo_pct,  prem_cuota,
                  diesel_estimulo_pct, diesel_cuota) |>
    dplyr::arrange(fecha_inicio)
}

.expand_to_daily <- function(df_clean) {
  df_clean |>
    dplyr::arrange(fecha_inicio) |>
    dplyr::mutate(

      date = purrr::map2(fecha_inicio, fecha_fin - 1L, seq, by = "day")
    ) |>
    tidyr::unnest(date) |>
    dplyr::select(date,
                  magna_estimulo_pct, magna_cuota,
                  prem_estimulo_pct,  prem_cuota,
                  diesel_estimulo_pct, diesel_cuota) |>
    dplyr::arrange(date)
}

.aggregate_to_monthly <- function(daily) {
  daily |>
    dplyr::mutate(
      year  = lubridate::year(date),
      month = lubridate::month(date)
    ) |>
    dplyr::group_by(year, month) |>
    dplyr::summarise(
      ieps_magna_cuota          = mean(magna_cuota,         na.rm = TRUE),
      ieps_magna_estimulo_pct   = mean(magna_estimulo_pct,  na.rm = TRUE),
      ieps_prem_cuota           = mean(prem_cuota,          na.rm = TRUE),
      ieps_prem_estimulo_pct    = mean(prem_estimulo_pct,   na.rm = TRUE),
      ieps_diesel_cuota         = mean(diesel_cuota,        na.rm = TRUE),
      n_days                    = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.nan(ieps_magna_cuota))
}

process_ieps_combustibles <- function(
  xlsx_path   = "data/raw_public/IEPS_Combustibles_Mexico.xlsx",
  out_daily   = "data/processed/ieps/ieps_daily.parquet",
  out_monthly = "data/processed/ieps/ieps_monthly.parquet"
) {
  dir.create(dirname(out_daily),   recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_monthly), recursive = TRUE, showWarnings = FALSE)

  cuotas_base <- .read_cuotas_base(xlsx_path)
  message(sprintf("IEPS: cuotas_base loaded for years %d-%d",
                  min(cuotas_base$year), max(cuotas_base$year)))

  raw   <- .read_ieps_raw(xlsx_path)
  clean <- .clean_ieps(raw, cuotas_base)

  n_adjusted   <- sum(clean$magna_estimulo_pct != 0, na.rm = TRUE)
  n_unadjusted <- sum(clean$magna_estimulo_pct == 0, na.rm = TRUE)
  message(sprintf(
    "IEPS: %d weeks total | %d with SHCP adjustment | %d at base rate (estimulo=0)",
    nrow(clean), n_adjusted, n_unadjusted
  ))
  message(sprintf("  Period: %s to %s",
    as.character(min(clean$fecha_inicio)),
    as.character(max(clean$fecha_fin))
  ))

  daily   <- .expand_to_daily(clean)
  monthly <- .aggregate_to_monthly(daily)

  message(sprintf("IEPS daily:   %d rows  (%s to %s)",
    nrow(daily),
    as.character(min(daily$date)),
    as.character(max(daily$date))
  ))
  message(sprintf("IEPS monthly: %d year-months  (%d-%d)",
    nrow(monthly), min(monthly$year), max(monthly$year)
  ))

  n_na <- sum(is.na(daily$magna_cuota))
  if (n_na > 0L) {
    warning(sprintf(
      "[ieps] %d daily rows have NA magna_cuota — check CUOTAS_BASE coverage", n_na
    ))
  }

  arrow::write_parquet(daily,   out_daily,   compression = "zstd")
  arrow::write_parquet(monthly, out_monthly, compression = "zstd")

  out_monthly
}
