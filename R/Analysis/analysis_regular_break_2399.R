suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(purrr)
  library(stringr)
  library(openxlsx)
  library(lubridate)
  library(tidyr)
})

normalize_estado_break2399 <- function(x) {
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
      z %in% c(
        "CIUDAD DE MEXICO",
        "CDMX",
        "DISTRITO FEDERAL",
        "MEXICO CITY"
      ) ~ "CIUDAD DE MÉXICO",
      z %in% c("DURANGO") ~ "DURANGO",
      z %in% c("GUANAJUATO") ~ "GUANAJUATO",
      z %in% c("GUERRERO") ~ "GUERRERO",
      z %in% c("HIDALGO") ~ "HIDALGO",
      z %in% c("JALISCO") ~ "JALISCO",
      z %in% c(
        "MEXICO",
        "ESTADO DE MEXICO",
        "ESTADO DE MEXIC"
      ) ~ "ESTADO DE MÉXICO",
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

read_one_spreads_station_day_year_break2399 <- function(
  year = 2025,
  base_dir = "data/analysis/spreads_station_day"
) {
  file <- sprintf(
    "%s/year=%d/spreads_station_day.parquet",
    base_dir,
    as.integer(year)
  )

  if (!file.exists(file)) {
    return(NULL)
  }

  arrow::read_parquet(file, mmap = FALSE)
}

read_spreads_station_day_break2399 <- function(
  years = 2025,
  base_dir = "data/analysis/spreads_station_day"
) {
  purrr::map_dfr(
    years,
    ~ read_one_spreads_station_day_year_break2399(
      year = .x,
      base_dir = base_dir
    )
  )
}

prepare_break2399_panel <- function(
  years = 2025,
  cut = as.Date("2025-03-03"),
  end_date = as.Date("2025-08-31"),
  base_dir = "data/analysis/spreads_station_day"
) {
  df <- read_spreads_station_day_break2399(
    years = years,
    base_dir = base_dir
  )

  if (nrow(df) == 0) {
    return(tibble(
      station_id = character(),
      numero_permiso = character(),
      date = as.Date(character()),
      month = as.Date(character()),
      estado = character(),
      municipio = character(),
      CVEGEO = character(),
      station_regular = numeric()
    ))
  }

  df |>
    transmute(
      station_id = as.character(station_id),
      numero_permiso = as.character(numero_permiso),
      date = as.Date(date),
      estado = normalize_estado_break2399(estado),
      municipio = stringr::str_squish(as.character(municipio)),
      CVEGEO = as.character(CVEGEO),
      station_regular = as.numeric(station_regular)
    ) |>
    filter(
      !is.na(date),
      date > cut,
      date <= end_date
    ) |>
    mutate(
      month = as.Date(lubridate::floor_date(date, unit = "month"))
    )
}

compute_station_month_breaks <- function(
  years = 2025,
  cut = as.Date("2025-03-03"),
  end_date = as.Date("2025-08-31"),
  threshold = 23.99,
  base_dir = "data/analysis/spreads_station_day"
) {
  df <- prepare_break2399_panel(
    years = years,
    cut = cut,
    end_date = end_date,
    base_dir = base_dir
  )

  if (nrow(df) == 0) {
    return(tibble(
      estado = character(),
      month = as.Date(character()),
      station_id = character(),
      numero_permiso = character(),
      municipio = character(),
      CVEGEO = character(),
      broke_2399 = integer(),
      max_price_in_month = numeric(),
      first_break_date_in_month = as.Date(character())
    ))
  }

  df |>
    group_by(estado, month, station_id, numero_permiso, municipio, CVEGEO) |>
    summarise(
      broke_2399 = as.integer(any(!is.na(station_regular) & station_regular > threshold)),
      max_price_in_month = suppressWarnings(max(station_regular, na.rm = TRUE)),
      first_break_date_in_month = {
        dd <- date[!is.na(station_regular) & station_regular > threshold]
        if (length(dd) == 0) as.Date(NA) else min(dd)
      },
      .groups = "drop"
    ) |>
    mutate(
      max_price_in_month = if_else(is.infinite(max_price_in_month), NA_real_, max_price_in_month)
    )
}

compute_estado_month_summary <- function(
  years = 2025,
  cut = as.Date("2025-03-03"),
  end_date = as.Date("2025-08-31"),
  threshold = 23.99,
  base_dir = "data/analysis/spreads_station_day"
) {
  station_month <- compute_station_month_breaks(
    years = years,
    cut = cut,
    end_date = end_date,
    threshold = threshold,
    base_dir = base_dir
  )

  if (nrow(station_month) == 0) {
    return(tibble(
      estado = character(),
      month = as.Date(character()),
      n_stations_observed = integer(),
      n_stations_break_2399 = integer(),
      pct_stations_break_2399 = numeric(),
      permisos_break_2399 = character()
    ))
  }

  station_month |>
    group_by(estado, month) |>
    summarise(
      n_stations_observed = n_distinct(station_id),
      n_stations_break_2399 = sum(broke_2399, na.rm = TRUE),
      pct_stations_break_2399 = if_else(
        n_stations_observed > 0,
        100 * n_stations_break_2399 / n_stations_observed,
        NA_real_
      ),
      permisos_break_2399 = paste(
        sort(unique(numero_permiso[broke_2399 == 1])),
        collapse = ", "
      ),
      .groups = "drop"
    ) |>
    arrange(month, estado)
}

compute_estado_total_mar_aug <- function(
  years = 2025,
  cut = as.Date("2025-03-03"),
  end_date = as.Date("2025-08-31"),
  threshold = 23.99,
  base_dir = "data/analysis/spreads_station_day"
) {
  station_month <- compute_station_month_breaks(
    years = years,
    cut = cut,
    end_date = end_date,
    threshold = threshold,
    base_dir = base_dir
  )

  if (nrow(station_month) == 0) {
    return(tibble(
      estado = character(),
      n_stations_observed_mar_aug = integer(),
      n_stations_break_2399_mar_aug = integer(),
      pct_stations_break_2399_mar_aug = numeric()
    ))
  }

  station_any_break <- station_month |>
    group_by(estado, station_id) |>
    summarise(
      numero_permiso = dplyr::first(numero_permiso),
      broke_any_mar_aug = as.integer(any(broke_2399 == 1)),
      .groups = "drop"
    )

  station_any_break |>
    group_by(estado) |>
    summarise(
      n_stations_observed_mar_aug = n_distinct(station_id),
      n_stations_break_2399_mar_aug = sum(broke_any_mar_aug, na.rm = TRUE),
      pct_stations_break_2399_mar_aug = if_else(
        n_stations_observed_mar_aug > 0,
        100 * n_stations_break_2399_mar_aug / n_stations_observed_mar_aug,
        NA_real_
      ),
      .groups = "drop"
    ) |>
    arrange(desc(pct_stations_break_2399_mar_aug), estado)
}

build_regular_break_2399_tables <- function(
  cut = as.Date("2025-03-03"),
  end_date = as.Date("2025-08-31"),
  years = 2025,
  threshold = 23.99,
  base_dir = "data/analysis/spreads_station_day"
) {
  station_month_breaks <- compute_station_month_breaks(
    years = years,
    cut = cut,
    end_date = end_date,
    threshold = threshold,
    base_dir = base_dir
  )

  estado_month_summary <- compute_estado_month_summary(
    years = years,
    cut = cut,
    end_date = end_date,
    threshold = threshold,
    base_dir = base_dir
  )

  estado_total_mar_aug <- compute_estado_total_mar_aug(
    years = years,
    cut = cut,
    end_date = end_date,
    threshold = threshold,
    base_dir = base_dir
  )

  list(
    station_month_breaks = station_month_breaks,
    estado_month_summary = estado_month_summary,
    estado_total_mar_aug = estado_total_mar_aug
  )
}

write_regular_break_2399_excel <- function(tables, out_file) {
  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb, "station_month_breaks")
  openxlsx::writeData(wb, "station_month_breaks", tables$station_month_breaks)

  openxlsx::addWorksheet(wb, "estado_month_summary")
  openxlsx::writeData(wb, "estado_month_summary", tables$estado_month_summary)

  openxlsx::addWorksheet(wb, "estado_total_mar_aug")
  openxlsx::writeData(wb, "estado_total_mar_aug", tables$estado_total_mar_aug)

  openxlsx::saveWorkbook(wb, file = out_file, overwrite = TRUE)

  out_file
}

build_regular_break_2399_outputs <- function(
  cut = as.Date("2025-03-03"),
  end_date = as.Date("2025-08-31"),
  years = 2025,
  threshold = 23.99,
  out_dir = "outputs/regular_break_2399",
  base_dir = "data/analysis/spreads_station_day"
) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  tables <- build_regular_break_2399_tables(
    cut = cut,
    end_date = end_date,
    years = years,
    threshold = threshold,
    base_dir = base_dir
  )

  parquet_dir <- file.path(out_dir, "parquet")
  dir.create(parquet_dir, recursive = TRUE, showWarnings = FALSE)

  station_month_parquet <- file.path(parquet_dir, "station_month_breaks.parquet")
  estado_month_parquet <- file.path(parquet_dir, "estado_month_summary.parquet")
  estado_total_parquet <- file.path(parquet_dir, "estado_total_mar_aug.parquet")

  arrow::write_parquet(tables$station_month_breaks, station_month_parquet)
  arrow::write_parquet(tables$estado_month_summary, estado_month_parquet)
  arrow::write_parquet(tables$estado_total_mar_aug, estado_total_parquet)

  excel_file <- file.path(out_dir, "regular_break_2399_mar_aug.xlsx")
  write_regular_break_2399_excel(tables, excel_file)

  c(
    station_month_parquet,
    estado_month_parquet,
    estado_total_parquet,
    excel_file
  )
}
