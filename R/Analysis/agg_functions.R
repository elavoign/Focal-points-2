suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(arrow)
})

REFORM_DATE <- as.Date("2025-03-03")

vars_level_cvegeo <- c(
  "station_regular","station_premium","station_diesel",
  "terminal_regular","terminal_premium","terminal_diesel",
  "regular_int_mxn_l","diesel_int_mxn_l",
  "spread_retail_int_diesel","spread_retail_int_regular",
  "spread_terminal_int_diesel","spread_terminal_int_regular",
  "spread_retail_terminal_diesel","spread_retail_terminal_premium","spread_retail_terminal_regular"
)

vars_spreads_station <- c(
  "spread_retail_int_diesel","spread_retail_int_regular",
  "spread_terminal_int_diesel","spread_terminal_int_regular",
  "spread_retail_terminal_diesel","spread_retail_terminal_premium","spread_retail_terminal_regular"
)

vars_prices_station <- c(
  "station_regular",
  "station_premium",
  "station_diesel"
)

vars_spreads_terminal <- c(
  "spread_retail_int_diesel","spread_retail_int_regular",
  "spread_retail_terminal_diesel","spread_retail_terminal_premium","spread_retail_terminal_regular"
)

keep_existing <- function(df, cols) {
  cols[cols %in% names(df)]
}

get_prepost_window <- function(cut = REFORM_DATE, window_months = 1L) {
  list(
    pre_start  = cut %m-% months(window_months),
    pre_end    = cut - days(1),
    post_start = cut,
    post_end   = cut %m+% months(window_months)
  )
}

agg_daily_cvegeo_one_year <- function(in_parquet, year, out_dir) {
  out <- file.path(out_dir, sprintf("year=%d", year), "daily_cvegeo.parquet")
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)

  df <- arrow::read_parquet(in_parquet) |> as_tibble()

  if (!"date" %in% names(df) && "Fecha" %in% names(df)) {
    df <- df |> rename(date = Fecha)
  }

  df <- df |> mutate(date = as.Date(date))

  vars <- keep_existing(df, vars_level_cvegeo)

  df_out <- df |>
    filter(!is.na(CVEGEO), CVEGEO != "") |>
    group_by(date, CVEGEO) |>
    summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

  arrow::write_parquet(df_out, out)
  out
}

agg_prepost_cvegeo_from_daily <- function(daily_cvegeo_files, out_path, window_months = 1L) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

  win <- get_prepost_window(cut = REFORM_DATE, window_months = window_months)

  df <- arrow::open_dataset(daily_cvegeo_files) |> collect()
  df <- df |> mutate(date = as.Date(date))

  vars <- keep_existing(df, vars_level_cvegeo)

  pre <- df |>
    filter(date >= win$pre_start, date <= win$pre_end) |>
    group_by(CVEGEO) |>
    summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") |>
    mutate(period = "pre")

  post <- df |>
    filter(date >= win$post_start, date <= win$post_end) |>
    group_by(CVEGEO) |>
    summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") |>
    mutate(period = "post")

  out_df <- bind_rows(pre, post)
  arrow::write_parquet(out_df, out_path)
  out_path
}

agg_prepost_station_spreads <- function(spreads_station_day_files, out_path, window_months = 1L) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

  win <- get_prepost_window(cut = REFORM_DATE, window_months = window_months)

  ds <- arrow::open_dataset(spreads_station_day_files)

  nm <- names(ds)
  if (!"date" %in% nm && "Fecha" %in% nm) {
    ds <- ds |> dplyr::rename(date = Fecha)
  }

  vars <- keep_existing(ds, vars_spreads_station)

  pre <- ds |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::filter(date >= win$pre_start, date <= win$pre_end) |>
    dplyr::filter(!is.na(station_id), station_id != "") |>
    dplyr::group_by(station_id) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(vars), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(period = "pre") |>
    dplyr::collect()

  post <- ds |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::filter(date >= win$post_start, date <= win$post_end) |>
    dplyr::filter(!is.na(station_id), station_id != "") |>
    dplyr::group_by(station_id) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(vars), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(period = "post") |>
    dplyr::collect()

  out_df <- dplyr::bind_rows(pre, post)
  arrow::write_parquet(out_df, out_path)
  out_path
}

agg_prepost_station_prices <- function(spreads_station_day_files, out_path, window_months = 1L) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

  win <- get_prepost_window(cut = REFORM_DATE, window_months = window_months)

  ds <- arrow::open_dataset(spreads_station_day_files)

  nm <- names(ds)
  if (!"date" %in% nm && "Fecha" %in% nm) {
    ds <- ds |> dplyr::rename(date = Fecha)
  }

  vars <- keep_existing(ds, vars_prices_station)
  if (length(vars) == 0) {
    stop("The station-day dataset has no station price columns.")
  }

  pre <- ds |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::filter(date >= win$pre_start, date <= win$pre_end) |>
    dplyr::filter(!is.na(station_id), station_id != "") |>
    dplyr::group_by(station_id) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(vars), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(period = "pre") |>
    dplyr::collect()

  post <- ds |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::filter(date >= win$post_start, date <= win$post_end) |>
    dplyr::filter(!is.na(station_id), station_id != "") |>
    dplyr::group_by(station_id) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(vars), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(period = "post") |>
    dplyr::collect()

  out_df <- dplyr::bind_rows(pre, post)
  arrow::write_parquet(out_df, out_path)
  out_path
}

agg_station_price_quantiles <- function(spreads_station_day_files,
                                        price_var = "station_regular",
                                        out_path,
                                        window_months = 1L) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

  win <- get_prepost_window(cut = REFORM_DATE, window_months = window_months)

  ds <- arrow::open_dataset(spreads_station_day_files)

  nm <- names(ds)
  if (!"date" %in% nm && "Fecha" %in% nm) {
    ds <- ds |> dplyr::rename(date = Fecha)
  }

  if (!price_var %in% names(ds)) {
    stop(sprintf("La variable '%s' no existe en la base.", price_var))
  }

  ds_price <- ds |>
    dplyr::select(station_id, date, price = dplyr::all_of(price_var))

  pre <- ds_price |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::filter(date >= win$pre_start, date <= win$pre_end) |>
    dplyr::filter(!is.na(station_id), station_id != "") |>
    dplyr::group_by(station_id) |>
    dplyr::summarise(
      price_pre = mean(price, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::collect()

  post <- ds_price |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::filter(date >= win$post_start, date <= win$post_end) |>
    dplyr::filter(!is.na(station_id), station_id != "") |>
    dplyr::group_by(station_id) |>
    dplyr::summarise(
      price_post = mean(price, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::collect()

  out_df <- pre |>
    dplyr::inner_join(post, by = "station_id") |>
    dplyr::filter(
      !is.na(price_pre), is.finite(price_pre),
      !is.na(price_post), is.finite(price_post)
    ) |>
    dplyr::mutate(
      quantile_group = dplyr::ntile(price_pre, 4),
      quantile_label = dplyr::case_when(
        quantile_group == 1 ~ "0-25",
        quantile_group == 2 ~ "25-50",
        quantile_group == 3 ~ "50-75",
        quantile_group == 4 ~ "75-100",
        TRUE ~ NA_character_
      ),
      price_var = price_var,
      window_months = window_months
    )

  arrow::write_parquet(out_df, out_path)
  out_path
}
