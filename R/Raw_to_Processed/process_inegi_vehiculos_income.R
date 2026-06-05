suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(stringr)
  library(purrr)
  library(readr)
})

# --------------------------------------------------------------------------
# Process one state's Viviendas CSV
# --------------------------------------------------------------------------
# AUTOPROP coding in INEGI Censo 2020 Ampliado (CA):
#   7 = sí tiene automóvil propio
#   8 = no tiene
#   9 = no especificado
# INGTRHOG: ingreso trimestral del hogar en pesos corrientes
#   999999 = no especificado (exclude)
#   0      = may be genuine zero; excluded to avoid distorting the mean

.process_one_state_income <- function(viviendas_csv) {
  d <- readr::read_csv(viviendas_csv, show_col_types = FALSE) |>
    dplyr::select(ENT, MUN, FACTOR, AUTOPROP, INGTRHOG) |>
    dplyr::mutate(
      CVEGEO   = paste0(
        stringr::str_pad(as.character(ENT),    2L, "left", "0"),
        stringr::str_pad(as.character(MUN),    3L, "left", "0")
      ),
      INGTRHOG = suppressWarnings(as.numeric(INGTRHOG)),
      AUTOPROP = suppressWarnings(as.integer(AUTOPROP)),
      FACTOR   = suppressWarnings(as.numeric(FACTOR))
    ) |>
    dplyr::filter(
      !is.na(INGTRHOG), INGTRHOG > 0, INGTRHOG < 999999,
      !is.na(FACTOR),   FACTOR   > 0
    )

  # --- Conditional mean: car-owning households only ---
  income_cond <- d |>
    dplyr::filter(AUTOPROP == 7L) |>
    dplyr::group_by(CVEGEO) |>
    dplyr::summarise(
      income_car_owners  = stats::weighted.mean(INGTRHOG, w = FACTOR, na.rm = TRUE),
      n_car_owner_hh     = dplyr::n(),
      .groups = "drop"
    )

  # --- Unconditional mean (robustness) ---
  income_uncond <- d |>
    dplyr::group_by(CVEGEO) |>
    dplyr::summarise(
      income_unconditional = stats::weighted.mean(INGTRHOG, w = FACTOR, na.rm = TRUE),
      n_hh                 = dplyr::n(),
      .groups = "drop"
    )

  dplyr::full_join(income_cond, income_uncond, by = "CVEGEO")
}

# --------------------------------------------------------------------------
# Main wrapper — loops over all 31 state folders
# --------------------------------------------------------------------------

process_inegi_vehiculos_income <- function(
  vehiculos_dir = "data/raw_public/Inegi Vehiculos",
  out_parquet   = "data/processed/inegi_vehiculos/municipal_income_car_owners.parquet"
) {
  dir.create(dirname(out_parquet), recursive = TRUE, showWarnings = FALSE)

  viviendas_files <- list.files(
    vehiculos_dir,
    pattern    = "^Viviendas\\d+\\.CSV$",
    recursive  = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  message(sprintf("Found %d Viviendas CSV files across state folders", length(viviendas_files)))

  results <- purrr::map(viviendas_files, function(f) {
    state_folder <- basename(dirname(f))
    message(sprintf("  Processing: %s", state_folder))
    tryCatch(
      .process_one_state_income(f),
      error = function(e) {
        warning(sprintf("ERROR in %s: %s", state_folder, conditionMessage(e)))
        NULL
      }
    )
  })

  out <- dplyr::bind_rows(Filter(Negate(is.null), results)) |>
    dplyr::arrange(CVEGEO)

  message(sprintf(
    "Income table: %d municipalities | car-owner income non-NA: %d | unconditional non-NA: %d",
    nrow(out),
    sum(!is.na(out$income_car_owners)),
    sum(!is.na(out$income_unconditional))
  ))

  arrow::write_parquet(out, out_parquet, compression = "zstd")
  out_parquet
}
