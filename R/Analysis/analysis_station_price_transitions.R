# R/analysis_station_price_transitions.R

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(tidyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(openxlsx)
  library(lubridate)
})

normalize_estado <- function(x) {
  x |>
    as.character() |>
    stringr::str_replace_all('"', "") |>
    stringr::str_replace_all("'", "") |>
    stringr::str_squish() |>
    stringr::str_to_upper() |>
    iconv(from = "", to = "ASCII//TRANSLIT") |>
    (\(z) dplyr::case_when(
      z %in% c("AGUASCALIENTES") ~ "AGUASCALIENTES",
      z %in% c("BAJA CALIFORNIA") ~ "BAJA CALIFORNIA",
      z %in% c("BAJA CALIFORNIA SUR") ~ "BAJA CALIFORNIA SUR",
      z %in% c("CAMPECHE") ~ "CAMPECHE",
      z %in% c("COAHUILA", "COAHUILA DE ZARAGOZA") ~ "COAHUILA",
      z %in% c("COLIMA") ~ "COLIMA",
      z %in% c("CHIAPAS") ~ "CHIAPAS",
      z %in% c("CHIHUAHUA") ~ "CHIHUAHUA",
      z %in% c("CIUDAD DE MEXICO", "CDMX", "DISTRITO FEDERAL", "MEXICO CITY") ~ "CIUDAD DE MÉXICO",
      z %in% c("DURANGO") ~ "DURANGO",
      z %in% c("GUANAJUATO") ~ "GUANAJUATO",
      z %in% c("GUERRERO") ~ "GUERRERO",
      z %in% c("HIDALGO") ~ "HIDALGO",
      z %in% c("JALISCO") ~ "JALISCO",
      z %in% c("MEXICO", "ESTADO DE MEXICO", "ESTADO DE MEXIC") ~ "ESTADO DE MÉXICO",
      z %in% c("MICHOACAN", "MICHOACAN DE OCAMPO") ~ "MICHOACÁN",
      z %in% c("MORELOS") ~ "MORELOS",
      z %in% c("NAYARIT") ~ "NAYARIT",
      z %in% c("NUEVO LEON") ~ "NUEVO LEÓN",
      z %in% c("OAXACA") ~ "OAXACA",
      z %in% c("PUEBLA") ~ "PUEBLA",
      z %in% c("QUERETARO", "QUERETARO DE ARTEAGA") ~ "QUERÉTARO",
      z %in% c("QUINTANA ROO") ~ "QUINTANA ROO",
      z %in% c("SAN LUIS POTOSI") ~ "SAN LUIS POTOSÍ",
      z %in% c("SINALOA") ~ "SINALOA",
      z %in% c("SONORA") ~ "SONORA",
      z %in% c("TABASCO") ~ "TABASCO",
      z %in% c("TAMAULIPAS") ~ "TAMAULIPAS",
      z %in% c("TLAXCALA") ~ "TLAXCALA",
      z %in% c("VERACRUZ", "VERACRUZ DE IGNACIO DE LA LLAVE") ~ "VERACRUZ",
      z %in% c("YUCATAN") ~ "YUCATÁN",
      z %in% c("ZACATECAS") ~ "ZACATECAS",
      TRUE ~ z
    ))()
}

read_spreads_station_day_years <- function(
  years = c(2024, 2025),
  base_dir = "data/analysis/spreads_station_day"
) {
  files <- sprintf(
    "%s/year=%d/spreads_station_day.parquet",
    base_dir,
    as.integer(years)
  )

  existing_files <- files[file.exists(files)]

  if (length(existing_files) == 0) {
    stop("No se encontraron archivos de spreads_station_day para los años solicitados.")
  }

  bind_rows(lapply(existing_files, arrow::read_parquet))
}

build_window_dates <- function(reform_date, window_months) {
  pre_start  <- reform_date %m-% months(window_months)
  pre_end    <- reform_date - 1
  post_start <- reform_date
  post_end   <- reform_date %m+% months(window_months) - 1

  tibble(
    window_months = as.integer(window_months),
    pre_start = pre_start,
    pre_end = pre_end,
    post_start = post_start,
    post_end = post_end
  )
}

prepare_station_price_long <- function(df) {
  df |>
    transmute(
      station_id,
      date = as.Date(date),
      numero_permiso,
      estado = normalize_estado(estado),
      municipio,
      CVEGEO,
      localidad,
      lat,
      lon,
      station_regular,
      station_premium,
      station_diesel
    ) |>
    pivot_longer(
      cols = c(station_regular, station_premium, station_diesel),
      names_to = "product_var",
      values_to = "price"
    ) |>
    mutate(
      producto = case_when(
        product_var == "station_regular" ~ "regular",
        product_var == "station_premium" ~ "premium",
        product_var == "station_diesel" ~ "diesel",
        TRUE ~ NA_character_
      )
    ) |>
    select(
      station_id, date, numero_permiso, estado, municipio, CVEGEO,
      localidad, lat, lon, producto, price
    )
}

compute_station_prepost_one_window <- function(df_long, reform_date, window_months) {
  wd <- build_window_dates(reform_date, window_months)

  pre_df <- df_long |>
    filter(date >= wd$pre_start, date <= wd$pre_end) |>
    group_by(
      station_id, numero_permiso, estado, municipio, CVEGEO,
      localidad, lat, lon, producto
    ) |>
    summarise(
      price_pre = mean(price, na.rm = TRUE),
      .groups = "drop"
    )

  post_df <- df_long |>
    filter(date >= wd$post_start, date <= wd$post_end) |>
    group_by(
      station_id, numero_permiso, estado, municipio, CVEGEO,
      localidad, lat, lon, producto
    ) |>
    summarise(
      price_post = mean(price, na.rm = TRUE),
      .groups = "drop"
    )

  full_join(
    pre_df,
    post_df,
    by = c(
      "station_id", "numero_permiso", "estado", "municipio", "CVEGEO",
      "localidad", "lat", "lon", "producto"
    )
  ) |>
    mutate(window_months = as.integer(window_months)) |>
    filter(!is.na(price_pre), !is.na(price_post))
}

add_state_quantiles <- function(df) {
  df |>
    group_by(window_months, estado, producto) |>
    mutate(
      quantile_pre = dplyr::ntile(price_pre, 4),
      quantile_post = dplyr::ntile(price_post, 4)
    ) |>
    ungroup() |>
    mutate(
      quantile_label_pre = case_when(
        quantile_pre == 1 ~ "0-25",
        quantile_pre == 2 ~ "25-50",
        quantile_pre == 3 ~ "50-75",
        quantile_pre == 4 ~ "75-100",
        TRUE ~ NA_character_
      ),
      quantile_label_post = case_when(
        quantile_post == 1 ~ "0-25",
        quantile_post == 2 ~ "25-50",
        quantile_post == 3 ~ "50-75",
        quantile_post == 4 ~ "75-100",
        TRUE ~ NA_character_
      ),
      delta_price = price_post - price_pre,
      pct_change_price = dplyr::if_else(
        !is.na(price_pre) & price_pre != 0,
        100 * (price_post - price_pre) / price_pre,
        NA_real_
      )
    )
}

build_state_transition_summary <- function(df_transitions) {
  df_transitions |>
    group_by(
      window_months,
      estado,
      producto,
      quantile_label_pre,
      quantile_label_post
    ) |>
    summarise(
      n_stations = n(),
      avg_pct_change_in_cell = mean(pct_change_price, na.rm = TRUE),
      median_pct_change_in_cell = median(pct_change_price, na.rm = TRUE),
      avg_delta_in_cell = mean(delta_price, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(window_months, estado, producto, quantile_label_pre, quantile_label_post)
}

build_state_transition_pct <- function(df_summary) {
  df_summary |>
    group_by(window_months, estado, producto, quantile_label_pre) |>
    mutate(
      pct_stations = n_stations / sum(n_stations)
    ) |>
    ungroup() |>
    arrange(window_months, estado, producto, quantile_label_pre, quantile_label_post)
}

write_one_transition_excel <- function(df_transitions, out_file) {
  df_summary <- build_state_transition_summary(df_transitions)
  df_pct <- build_state_transition_pct(df_summary)

  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb, "station_transitions")
  openxlsx::writeData(
    wb,
    sheet = "station_transitions",
    x = df_transitions |>
      arrange(estado, producto, station_id)
  )

  openxlsx::addWorksheet(wb, "state_transition_summary")
  openxlsx::writeData(
    wb,
    sheet = "state_transition_summary",
    x = df_summary
  )

  openxlsx::addWorksheet(wb, "state_transition_pct")
  openxlsx::writeData(
    wb,
    sheet = "state_transition_pct",
    x = df_pct
  )

  openxlsx::saveWorkbook(wb, file = out_file, overwrite = TRUE)

  out_file
}

build_station_price_transition_outputs <- function(
  years = c(2024, 2025),
  reform_date = as.Date("2025-03-03"),
  out_dir = "outputs/station_price_transitions"
) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  raw_df <- read_spreads_station_day_years(years = years)
  df_long <- prepare_station_price_long(raw_df)
  windows <- c(1L, 3L, 6L)

  out_files <- map_chr(windows, function(w) {
    station_prepost <- compute_station_prepost_one_window(
      df_long = df_long,
      reform_date = reform_date,
      window_months = w
    )

    station_transitions <- add_state_quantiles(station_prepost)

    parquet_dir <- file.path(out_dir, sprintf("window_%sm", w))
    dir.create(parquet_dir, recursive = TRUE, showWarnings = FALSE)

    parquet_file <- file.path(parquet_dir, "station_transitions.parquet")
    arrow::write_parquet(station_transitions, parquet_file)

    excel_file <- file.path(
      out_dir,
      sprintf("station_price_transitions_window_%sm.xlsx", w)
    )

    write_one_transition_excel(
      df_transitions = station_transitions,
      out_file = excel_file
    )

    parquet_file
  })

  out_files
}