# R/Raw_to_Processed/process_volumes.R
#
# Reads CRE/SENER volume data (04_volumenes_venta_expendio_petroliferos.csv),
# assigns CVEGEO to each municipality via name matching against the INEGI
# Marco Geoestadístico 2024 shapefile, and aggregates to
# municipality × year × month with Regular and Premium volumes in liters.
#
# --- Matching strategy ---
#
# Step 1: State mapping (CSV entidad → CVE_ENT)
#   The CSV uses common/short state names (e.g., "Coahuila", "Estado de México"),
#   while INEGI uses official long forms ("Coahuila de Zaragoza", "Mexico").
#   A hardcoded 32-entry table maps CSV names to 2-digit CVE_ENT codes.
#   This mapping is deterministic and covers 100% of the 32 states.
#
# Step 2: Municipality name normalization
#   Both the CSV 'municipios' field and the shapefile NOMGEO are normalized:
#     - Strip accents: iconv(..., to = "ASCII//TRANSLIT")
#     - UPPERCASE
#     - Replace non-alphanumeric characters (except spaces) with space
#     - Collapse multiple spaces; trim
#   Join key: (CVE_ENT, normalized_name)
#   Since the same municipality name can appear in multiple states, the
#   state code is mandatory for uniqueness.
#
# Step 3: Hardcoded overrides for 3 known name discrepancies
#   After normalization the following CSV names do not match INEGI NOMGEO:
#   | CSV name (normalized)      | INEGI NOMGEO                              | CVEGEO |
#   |----------------------------|-------------------------------------------|--------|
#   | SAN JOSE ITURBIDE (GTO)    | San José de Iturbide                      | 11032  |
#   | JUCHITAN DE ZARAGOZA (OAX) | Heroica Ciudad de Juchitán de Zaragoza    | 20043  |
#   | SOLIDARIDAD (QRO)          | Playa del Carmen (renamed 2023 by INEGI)  | 23008  |
#
# Step 4: Multi-municipality rows
#   Some rows in the CSV list multiple municipalities in a single 'municipios'
#   cell, comma-separated (e.g., "El Llano,Cosío,San José de Gracia,Tepezalá").
#   These represent a combined volume for the listed municipalities.
#   Strategy: split by comma, assign volume equally to each municipality.
#   These rows account for ~4.4% of Regular+Premium rows.
#
# Match results (validated): ~99.5% of rows matched. Unmatched rows dropped.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(sf)
  library(arrow)
  library(purrr)
})

# ---------------------------------------------------------------------------
# Internal: text normalization
# ---------------------------------------------------------------------------

.normalize_vol_name <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  x <- gsub("[^A-Z0-9 ]", " ", x)
  x <- gsub("\\s+",       " ", x)
  trimws(x)
}

# ---------------------------------------------------------------------------
# Internal: CSV entidad → CVE_ENT (2-digit, zero-padded)
# Covers all 32 CSV state names exactly as they appear in the source file.
# ---------------------------------------------------------------------------

.build_state_map <- function() {
  c(
    "Aguascalientes"      = "01",
    "Baja California"     = "02",
    "Baja California Sur" = "03",
    "Campeche"            = "04",
    "Coahuila"            = "05",
    "Colima"              = "06",
    "Chiapas"             = "07",
    "Chihuahua"           = "08",
    "Ciudad de M\u00e9xico"    = "09",
    "Durango"             = "10",
    "Guanajuato"          = "11",
    "Guerrero"            = "12",
    "Hidalgo"             = "13",
    "Jalisco"             = "14",
    "Estado de M\u00e9xico"    = "15",
    "Michoac\u00e1n"           = "16",
    "Morelos"             = "17",
    "Nayarit"             = "18",
    "Nuevo Le\u00f3n"           = "19",
    "Oaxaca"              = "20",
    "Puebla"              = "21",
    "Quer\u00e9taro"            = "22",
    "Quintana Roo"        = "23",
    "San Luis Potos\u00ed"      = "24",
    "Sinaloa"             = "25",
    "Sonora"              = "26",
    "Tabasco"             = "27",
    "Tamaulipas"          = "28",
    "Tlaxcala"            = "29",
    "Veracruz"            = "30",
    "Yucat\u00e1n"              = "31",
    "Zacatecas"           = "32"
  )
}

# ---------------------------------------------------------------------------
# Internal: hardcoded overrides
# Key format: "<CVE_ENT>|<normalized_CSV_municipio_name>"
# Value: correct CVEGEO
# ---------------------------------------------------------------------------

.OVERRIDES_VOL <- c(
  "11|SAN JOSE ITURBIDE"    = "11032",
  # INEGI full name: "San José de Iturbide"
  # CSV drops the "de": "San José Iturbide"

  "20|JUCHITAN DE ZARAGOZA" = "20043",
  # INEGI full name: "Heroica Ciudad de Juchitán de Zaragoza"
  # CSV uses short name: "Juchitán de Zaragoza"

  "23|SOLIDARIDAD"          = "23008"
  # Municipality renamed to "Playa del Carmen" in INEGI 2024 shapefile.
  # CSV records prior to 2024 still use "Solidaridad".
)

# ---------------------------------------------------------------------------
# Internal: Spanish month name → integer
# ---------------------------------------------------------------------------

.MONTH_MAP_VOL <- c(
  "Enero" = 1L, "Febrero" = 2L, "Marzo" = 3L, "Abril" = 4L,
  "Mayo"  = 5L, "Junio"   = 6L, "Julio" = 7L, "Agosto" = 8L,
  "Septiembre" = 9L, "Octubre" = 10L, "Noviembre" = 11L, "Diciembre" = 12L
)

# ---------------------------------------------------------------------------
# Main function
# ---------------------------------------------------------------------------

process_volumes <- function(
  in_csv      = "data/raw_public/04_volumenes_venta_expendio_petroliferos.csv",
  mun_shp     = "data/map/inegi_mg_2024/unzipped/ONLY_MUNICIPIOS_00mun/00mun.shp",
  out_parquet = "data/processed/volumes/mun_month_volumes.parquet"
) {
  state_map <- .build_state_map()

  # --- Build INEGI municipality lookup (CVE_ENT, NOMGEO_norm) → CVEGEO ---
  mun_lookup <- sf::st_read(mun_shp, quiet = TRUE) |>
    sf::st_drop_geometry() |>
    dplyr::transmute(
      CVEGEO      = as.character(CVEGEO),
      CVE_ENT     = as.character(CVE_ENT),
      NOMGEO_norm = .normalize_vol_name(as.character(NOMGEO))
    )

  # --- Read and filter: Regular and Premium only ---
  df_raw <- readr::read_csv(in_csv, show_col_types = FALSE, progress = FALSE) |>
    dplyr::filter(subproducto %in% c("Regular", "Premium"))

  n_raw <- nrow(df_raw)

  # --- Parse month and year ---
  df_raw <- df_raw |>
    dplyr::mutate(
      month = .MONTH_MAP_VOL[mes],
      year  = as.integer(anio)
    )

  bad_mes <- df_raw$mes[is.na(df_raw$month)]
  if (length(bad_mes) > 0) {
    stop("Unknown 'mes' values: ", paste(unique(bad_mes), collapse = ", "))
  }

  # --- Map state → CVE_ENT ---
  df_raw <- df_raw |>
    dplyr::mutate(CVE_ENT = state_map[entidad])

  bad_states <- unique(df_raw$entidad[is.na(df_raw$CVE_ENT)])
  if (length(bad_states) > 0) {
    stop("Unmapped entidad values: ", paste(bad_states, collapse = ", "))
  }

  # --- Expand multi-municipality rows (comma-separated) ---
  # Volume is divided equally among the named municipalities.
  is_multi   <- grepl(",", df_raw$municipios)
  df_single  <- df_raw[!is_multi, ]
  df_multi   <- df_raw[is_multi, ]

  n_multi_rows    <- nrow(df_multi)
  n_expanded_rows <- 0L

  if (n_multi_rows > 0L) {
    df_multi <- df_multi |>
      dplyr::mutate(
        mun_list  = stringr::str_split(municipios, ","),
        n_parts   = purrr::map_int(mun_list, length),
        vol_split = volumen_vendido_l / n_parts
      ) |>
      tidyr::unnest(mun_list) |>
      dplyr::mutate(
        municipios        = stringr::str_squish(mun_list),
        volumen_vendido_l = vol_split
      ) |>
      dplyr::select(-mun_list, -n_parts, -vol_split)

    n_expanded_rows <- nrow(df_multi)
    df_raw <- dplyr::bind_rows(df_single, df_multi)
  } else {
    df_raw <- df_single
  }

  # --- Normalize municipality names ---
  df_raw <- df_raw |>
    dplyr::mutate(municipios_norm = .normalize_vol_name(municipios))

  # --- Primary match: (CVE_ENT, normalized name) → CVEGEO ---
  df_matched <- df_raw |>
    dplyr::left_join(
      mun_lookup,
      by = c("CVE_ENT", "municipios_norm" = "NOMGEO_norm")
    )

  # --- Apply overrides for known name discrepancies ---
  override_key    <- paste(df_matched$CVE_ENT, df_matched$municipios_norm, sep = "|")
  override_vals   <- .OVERRIDES_VOL[override_key]
  to_override     <- !is.na(override_vals)
  n_overrides     <- sum(to_override)
  df_matched$CVEGEO[to_override] <- override_vals[to_override]

  # --- Validation report ---
  n_total    <- nrow(df_matched)
  n_matched  <- sum(!is.na(df_matched$CVEGEO))
  n_unmatched <- n_total - n_matched

  unmatched_lk <- df_matched |>
    dplyr::filter(is.na(CVEGEO)) |>
    dplyr::distinct(entidad, municipios, municipios_norm) |>
    dplyr::arrange(entidad, municipios)

  message("=== process_volumes: CVEGEO matching report ===")
  message(sprintf("  Raw rows (Regular+Premium):       %d", n_raw))
  message(sprintf("  Multi-mun rows (expanded):        %d -> %d rows", n_multi_rows, n_expanded_rows))
  message(sprintf("  Total rows after expansion:       %d", n_total))
  message(sprintf("  Primary name-match:               %d (%.1f%%)", n_matched, 100 * n_matched / n_total))
  message(sprintf("  Override corrections applied:     %d", n_overrides))
  message(sprintf("  Unmatched (dropped):              %d", n_unmatched))
  if (nrow(unmatched_lk) > 0L) {
    message("  Unmatched municipalities:")
    for (i in seq_len(nrow(unmatched_lk))) {
      message(sprintf("    [%s] %s", unmatched_lk$entidad[i], unmatched_lk$municipios[i]))
    }
  }

  # Drop unmatched rows
  df_matched <- df_matched |> dplyr::filter(!is.na(CVEGEO))

  # --- Aggregate to (CVEGEO, year, month, subproducto) ---
  df_agg <- df_matched |>
    dplyr::group_by(CVEGEO, year, month, subproducto) |>
    dplyr::summarise(
      volume_l = sum(volumen_vendido_l, na.rm = TRUE),
      .groups  = "drop"
    )

  # --- Pivot wide: one row per (CVEGEO, year, month) ---
  df_wide <- df_agg |>
    tidyr::pivot_wider(
      names_from  = subproducto,
      values_from = volume_l,
      values_fill = NA_real_
    )

  # Rename and ensure both columns exist
  if ("Regular" %in% names(df_wide)) {
    df_wide <- df_wide |> dplyr::rename(regular_volume_l = Regular)
  } else {
    df_wide$regular_volume_l <- NA_real_
  }
  if ("Premium" %in% names(df_wide)) {
    df_wide <- df_wide |> dplyr::rename(premium_volume_l = Premium)
  } else {
    df_wide$premium_volume_l <- NA_real_
  }

  df_wide <- df_wide |>
    dplyr::select(CVEGEO, year, month, regular_volume_l, premium_volume_l) |>
    dplyr::arrange(CVEGEO, year, month)

  message(sprintf("  Output rows (mun x month):        %d", nrow(df_wide)))
  message(sprintf("  Unique CVEGEOs in output:          %d", dplyr::n_distinct(df_wide$CVEGEO)))
  message(sprintf("  Year range:                        %d-%d",
                  min(df_wide$year, na.rm = TRUE),
                  max(df_wide$year, na.rm = TRUE)))

  dir.create(dirname(out_parquet), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(df_wide, out_parquet, compression = "zstd")
  message(sprintf("Written: %s", out_parquet))

  out_parquet
}
