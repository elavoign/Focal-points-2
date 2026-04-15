# R/Raw_to_Processed/process_retail_year.R

parse_date_safe <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_

  out <- rep(as.Date(NA), length(x))

  # YYYY-MM-DD (o con tiempo)
  idx1 <- !is.na(x) & grepl("^\\d{4}-\\d{2}-\\d{2}", x)
  if (any(idx1)) {
    out[idx1] <- as.Date(substr(x[idx1], 1, 10), format = "%Y-%m-%d")
  }

  # DD/MM/YYYY (o con tiempo)
  idx2 <- !is.na(x) & is.na(out) & grepl("^\\d{2}/\\d{2}/\\d{4}", x)
  if (any(idx2)) {
    out[idx2] <- as.Date(substr(x[idx2], 1, 10), format = "%d/%m/%Y")
  }

  # YYYY/MM/DD (fallback)
  idx3 <- !is.na(x) & is.na(out) & grepl("^\\d{4}/\\d{2}/\\d{2}", x)
  if (any(idx3)) {
    out[idx3] <- as.Date(substr(x[idx3], 1, 10), format = "%Y/%m/%d")
  }

  out
}

choose_diesel_column <- function(df) {
  diesel_candidates <- names(df)[grepl("^diesel", names(df), ignore.case = TRUE)]
  # prioriza automotriz si existe
  if ("diesel_automotriz" %in% names(df)) return("diesel_automotriz")
  if (length(diesel_candidates) == 0) return(NA_character_)

  # elige la primera que tenga al menos un valor numérico no-NA en una muestra
  sample_n <- min(2000, nrow(df))
  if (sample_n == 0) return(diesel_candidates[1])

  for (col in diesel_candidates) {
    v <- suppressWarnings(as.numeric(df[[col]][seq_len(sample_n)]))
    if (any(!is.na(v))) return(col)
  }
  diesel_candidates[1]
}

process_retail_year <- function(in_csv, out_parquet, year = NULL) {
  # Lee raw
  df <- readr::read_csv(in_csv, show_col_types = FALSE, progress = FALSE)

  # columnas esperadas mínimas
  if (!("numero_permiso" %in% names(df))) {
    stop("Retail: falta columna 'numero_permiso' en: ", in_csv)
  }
  if (!("fecha" %in% names(df))) {
    stop("Retail: falta columna 'fecha' en: ", in_csv)
  }
  if (!("regular" %in% names(df))) {
    stop("Retail: falta columna 'regular' en: ", in_csv)
  }
  if (!("premium" %in% names(df))) {
    stop("Retail: falta columna 'premium' en: ", in_csv)
  }

  diesel_col <- choose_diesel_column(df)
  if (is.na(diesel_col)) {
    stop("Retail: no encontré ninguna columna diesel (diesel*) en: ", in_csv)
  }

  out <- df |>
    dplyr::transmute(
      station_id = .data$numero_permiso,
      numero_permiso = .data$numero_permiso,
      date = parse_date_safe(.data$fecha),
      year = as.integer(format(.data$date, "%Y")),
      regular = suppressWarnings(as.numeric(.data$regular)),
      premium = suppressWarnings(as.numeric(.data$premium)),
      diesel = suppressWarnings(as.numeric(.data[[diesel_col]]))
    ) |>
    dplyr::mutate(
      flag_bad_date = is.na(.data$date),
      flag_missing_any_price = is.na(.data$regular) | is.na(.data$premium) | is.na(.data$diesel),
      flag_nonpositive_any_price = (!is.na(.data$regular) & .data$regular <= 0) |
        (!is.na(.data$premium) & .data$premium <= 0) |
        (!is.na(.data$diesel) & .data$diesel <= 0)
    )

  # Si year viene NULL, lo inferimos del filename Retail_YYYY.csv
  if (is.null(year)) {
    m <- regmatches(in_csv, regexpr("\\d{4}", in_csv))
    if (length(m) == 1 && !is.na(m) && nchar(m) == 4) {
      year <- as.integer(m)
    }
  }

  # Filtra al año si tenemos year
  if (!is.null(year) && !is.na(year)) {
    out <- out |> dplyr::filter(.data$year == year)
  }

  # Duplicados por station-day (después de filtrar)
  out <- out |>
    dplyr::mutate(
      flag_dup_station_day = duplicated(paste0(.data$station_id, "||", .data$date))
    )

  # Crea carpeta salida y escribe parquet
  dir.create(dirname(out_parquet), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(out, out_parquet, compression = "zstd")

  invisible(out_parquet)
}
