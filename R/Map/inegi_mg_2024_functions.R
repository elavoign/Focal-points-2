suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(sf)
  library(arrow)
})

inegi_mg_2024_zip_path <- function() {
  "data/raw_public/inegi_mg_2024/794551163061_s.zip"
}

inegi_mg_2024_unzip_dir <- function() {
  "data/map/inegi_mg_2024/unzipped"
}

inegi_mg_2024_outputs_dir <- function() {
  "data/map/inegi_mg_2024"
}

ensure_unzipped_inegi_mg_2024 <- function(zip_path,
                                         unzip_dir = inegi_mg_2024_unzip_dir()) {
  dir.create(unzip_dir, recursive = TRUE, showWarnings = FALSE)
  done_file <- file.path(unzip_dir, ".unzipped_done")
  if (file.exists(done_file)) return(done_file)

  unzip_quiet <- function(zipfile, exdir) {
    dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
    status <- suppressWarnings(
      system2(
        command = "unzip",
        args = c("-oq",
                 shQuote(normalizePath(zipfile, mustWork = TRUE)),
                 "-d",
                 shQuote(normalizePath(exdir, mustWork = FALSE)))
      )
    )
    if (!identical(status, 0L)) stop("unzip failed with status=", status, " for: ", zipfile)
    TRUE
  }

  unzip_quiet(zip_path, unzip_dir)

  integ_zip <- list.files(unzip_dir, pattern = "mg_2025_integrado\\.zip$", full.names = TRUE)
  if (length(integ_zip) != 1) stop("Could not find unique mg_2025_integrado.zip inside unzip_dir.")

  integ_dir <- file.path(unzip_dir, "mg_2025_integrado")
  unzip_quiet(integ_zip[1], integ_dir)

  src_dir <- file.path(integ_dir, "conjunto_de_datos")
  need <- c("00mun.shp","00mun.shx","00mun.dbf","00mun.prj","00mun.cpg")
  src_files <- file.path(src_dir, need)
  if (!all(file.exists(src_files))) stop("Missing required 00mun.* files after unzip integrated zip.")

  out_dir <- file.path(unzip_dir, "ONLY_MUNICIPIOS_00mun")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(src_files, out_dir, overwrite = TRUE)

  unlink(integ_dir, recursive = TRUE, force = TRUE)
  inner_zips <- list.files(unzip_dir, pattern = "\\.zip$", full.names = TRUE)
  unlink(inner_zips, force = TRUE)

  writeLines(sprintf("unzipped_from=%s\nwhen=%s", zip_path, Sys.time()), done_file)
  done_file
}

list_shapefiles_recursive <- function(root_dir) {
  list.files(root_dir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
}

score_municipios_shp <- function(shp_path) {

  base <- tolower(basename(shp_path))
  s <- 0
  if (grepl("mun\\.shp$", base)) s <- s + 25

  x <- tryCatch(sf::st_read(shp_path, quiet = TRUE), error = function(e) NULL)
  if (is.null(x)) return(-Inf)

  nms <- names(x)

  if ("CVE_ENT" %in% nms) s <- s + 3
  if ("CVE_MUN" %in% nms) s <- s + 8
  if ("NOM_ENT" %in% nms) s <- s + 2
  if ("NOM_MUN" %in% nms) s <- s + 6

  key <- NULL
  if ("CVEGEO" %in% nms) key <- "CVEGEO"

  if (!is.null(key)) {
    v <- as.character(x[[key]])
    v <- v[!is.na(v)]
    if (length(v) > 0) {
      v <- v[seq_len(min(200, length(v)))]
      lens <- nchar(v)
      is_mun <- (lens == 5) & grepl("^[0-9]{5}$", v)
      p_mun <- mean(is_mun)

      if (p_mun > 0.8) s <- s + 20
      if (p_mun > 0.5) s <- s + 10
      if (p_mun < 0.1) s <- s - 30
    }
  }

  if ("CVE_ENT" %in% nms && !("CVE_MUN" %in% nms) && !("NOM_MUN" %in% nms)) s <- s - 10

  s
}

score_estados_shp <- function(shp_path) {
  x <- tryCatch(sf::st_read(shp_path, quiet = TRUE), error = function(e) NULL)
  if (is.null(x)) return(-Inf)

  nms <- names(x)
  s <- 0

  if ("CVE_ENT" %in% nms) s <- s + 5
  if ("NOM_ENT" %in% nms) s <- s + 4

  if ("CVE_MUN" %in% nms) s <- s - 10
  if ("NOM_MUN" %in% nms) s <- s - 10
  if ("CVEGEO" %in% nms) s <- s - 2

  s
}

detect_best_shp <- function(shp_paths, scorer) {
  if (length(shp_paths) == 0) stop("No .shp files found in unzipped INEGI folder.")
  scores <- vapply(shp_paths, scorer, FUN.VALUE = numeric(1))
  best <- shp_paths[which.max(scores)]
  if (!is.finite(max(scores))) stop("Could not read or score any shapefile candidates.")
  best
}

detect_municipios_shp <- function(unzip_dir = inegi_mg_2024_unzip_dir()) {
  p <- file.path(unzip_dir, "ONLY_MUNICIPIOS_00mun", "00mun.shp")
  if (!file.exists(p)) stop("Expected municipios shapefile not found: ", p)
  p
}

detect_estados_shp <- function(unzip_dir = inegi_mg_2024_unzip_dir()) {
  shp_paths <- list_shapefiles_recursive(unzip_dir)
  detect_best_shp(shp_paths, score_estados_shp)
}

pad_left <- function(x, width) {
  x <- as.character(x)
  stringr::str_pad(x, width = width, side = "left", pad = "0")
}

standardize_cvegeo_mun <- function(df_sf) {
  nms <- names(df_sf)

  if (!("CVEGEO" %in% names(df_sf))) {

    if (!all(c("CVE_ENT", "CVE_MUN") %in% names(df_sf))) {
      stop("Municipios layer missing CVEGEO and also missing CVE_ENT/CVE_MUN to construct it.")
    }
    df_sf <- df_sf %>%
      mutate(
        CVE_ENT = pad_left(CVE_ENT, 2),
        CVE_MUN = pad_left(CVE_MUN, 3),
        CVEGEO  = paste0(CVE_ENT, CVE_MUN)
      )
  } else {

    df_sf <- df_sf %>%
      mutate(CVEGEO = substr(as.character(CVEGEO), 1, 5)) %>%
      mutate(CVEGEO = pad_left(CVEGEO, 5))
  }

  if (!("CVE_ENT" %in% names(df_sf))) df_sf <- df_sf %>% mutate(CVE_ENT = substr(CVEGEO, 1, 2))
  if (!("CVE_MUN" %in% names(df_sf))) df_sf <- df_sf %>% mutate(CVE_MUN = substr(CVEGEO, 3, 5))

  bad_share <- mean(!grepl("^[0-9]{5}$", df_sf$CVEGEO))
  if (is.na(bad_share)) bad_share <- 1
  if (bad_share > 0) {
    stop("After standardization, CVEGEO is not 5-digit numeric for some rows. Bad share = ", bad_share)
  }

  df_sf
}

standardize_keys_state <- function(df_sf) {
  if (!("CVE_ENT" %in% names(df_sf))) stop("Estados layer missing CVE_ENT.")
  df_sf %>% mutate(CVE_ENT = pad_left(CVE_ENT, 2))
}

to_wgs84 <- function(x) {
  if (is.na(sf::st_crs(x))) {
    message("Warning: municipios/estados CRS is NA; leaving geometry CRS unchanged.")
    return(x)
  }
  sf::st_transform(x, 4326)
}

safe_point_on_surface <- function(x) {
  sf::st_point_on_surface(x)
}

add_centroids <- function(sf_obj) {
  pts <- safe_point_on_surface(sf_obj)
  coords <- sf::st_coordinates(pts)
  sf_obj %>%
    mutate(
      centroid_lon = coords[, 1],
      centroid_lat = coords[, 2]
    )
}

write_geoparquet_sf <- function(sf_obj, out_path) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

  out_path <- sub("\\.geoparquet$", ".gpkg", out_path)
  out_path <- sub("\\.parquet$", ".gpkg", out_path)

  sf::st_write(sf_obj, out_path, driver = "GPKG", quiet = TRUE, append = FALSE)
  out_path
}

build_municipios_geo_mg2024 <- function(unzip_done_file,
                                       unzip_dir = inegi_mg_2024_unzip_dir(),
                                       out_dir   = inegi_mg_2024_outputs_dir()) {
  shp <- detect_municipios_shp(unzip_dir)
  message("Selected municipios shp: ", shp)

  mun <- sf::st_read(shp, quiet = TRUE) %>%
    standardize_cvegeo_mun() %>%
    to_wgs84()

  keep <- intersect(
    c("CVEGEO","CVE_ENT","CVE_MUN","NOM_ENT","NOM_MUN"),
    names(mun)
  )

  mun <- mun %>%
    select(any_of(keep), geometry) %>%
    arrange(CVEGEO)

  out_path <- file.path(out_dir, "municipios", "municipios.gpkg")
  write_geoparquet_sf(mun, out_path)
}

build_estados_geo_mg2024 <- function(unzip_done_file,
                                    unzip_dir = inegi_mg_2024_unzip_dir(),
                                    out_dir   = inegi_mg_2024_outputs_dir()) {
  shp <- detect_estados_shp(unzip_dir)
  edo <- sf::st_read(shp, quiet = TRUE) %>%
    standardize_keys_state() %>%
    to_wgs84()

  keep <- intersect(c("CVE_ENT","NOM_ENT"), names(edo))
  edo <- edo %>%
    select(any_of(keep), geometry) %>%
    arrange(CVE_ENT)

  out_path <- file.path(out_dir, "estados", "estados.gpkg")
  write_geoparquet_sf(edo, out_path)
}

build_municipios_lookup_mg2024 <- function(municipios_geo_file,
                                          out_dir = inegi_mg_2024_outputs_dir()) {
  mun <- sf::read_sf(municipios_geo_file)

  mun2 <- mun %>%
    add_centroids() %>%
    sf::st_drop_geometry()

  out_path <- file.path(out_dir, "lookups", "municipios_lookup.parquet")
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(mun2, out_path)
  out_path
}
