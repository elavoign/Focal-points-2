# R/Raw_to_Processed/process_stations.R

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(arrow)
})

clean_id <- function(x) {
  s <- as.character(x)
  s <- str_squish(s)
  s[s == ""] <- NA_character_
  s
}

process_stations <- function(in_rda, out_parquet) {

  e <- new.env(parent = emptyenv())
  obj_names <- load(in_rda, envir = e)

  if (!("stations" %in% obj_names)) {
    stop(
      "Stations.rda no contiene un objeto llamado 'stations'. Objetos: ",
      paste(obj_names, collapse = ", ")
    )
  }

  df <- e[["stations"]]

  if (!is.data.frame(df)) {
    stop("El objeto 'stations' no es data.frame/tibble.")
  }

  stations_out <- df %>%
    transmute(
      station_id     = station_id(code),
      region_wholesale_pemex = as.character(region_wholesale_pemex),
      terminal_id = terminal_id(region_wholesale_pemex),
      estado    = str_squish(as.character(state_cre0)),
      municipio = str_squish(as.character(muniname_cre0)),
      CVGEO = str_pad(as.character(municode_map), width = 5, side = "left", pad = "0"),
      localidad = str_squish(as.character(suburb_cre0)),
      lat = suppressWarnings(as.numeric(coalesce(lat_correct, latitude))),
      lon = suppressWarnings(as.numeric(coalesce(long_correct, longitude)))
    ) %>%
    mutate(
      flag_missing_region_wholesale_pemex =
        is.na(region_wholesale_pemex) | str_trim(region_wholesale_pemex) == "",
      flag_missing_terminal_id = is.na(terminal_id), 
      flag_missing_cvegeo_mun = is.na(CVGEO) | CVGEO == "000NA" | CVGEO == "00000"
    )

  if (any(is.na(stations_out$station_id) | stations_out$station_id == "")) {
    stop("Hay station_id vacíos/NA después de limpiar. Revisa columna 'code'.")
  }

  dup_n <- sum(duplicated(stations_out$station_id))
  if (dup_n > 0) {
    stop("station_id no es único. Duplicados: ", dup_n)
  }

  dir.create(dirname(out_parquet), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(stations_out, out_parquet, compression = "zstd")

  invisible(out_parquet)
}
