# R/Analysis/mun_month_functions.R
#
# Aggregates the balanced station x day panel to municipality x month prices.
#
# Aggregation method (Shaun's double-average specification):
#   Step 1 — station prices → municipality x day
#     For each (CVEGEO, date): simple mean of non-NA station prices across all
#     stations in the municipality. Each station gets equal weight for that day.
#     Only prices that are NOT stale (flag_stale_over_60d = FALSE) are included.
#
#   Step 2 — municipality x day → municipality x month
#     For each (CVEGEO, year, month): simple mean of the daily municipal prices
#     from step 1. Each day gets equal weight in the monthly average.
#
#   This two-step procedure guarantees that the monthly price is the
#   "average of daily averages", NOT a pooled average of all station-days in
#   the month. The distinction matters when the number of active stations
#   varies across days within a month.
#
# CVEGEO assignment:
#   CVEGEO comes directly from stations.parquet (field municode_map in the raw
#   CRE catalog). No text matching is performed. Stations with CVEGEO missing,
#   "000NA", or "00000" are excluded from the municipality aggregation.
#
# Municipality and state names:
#   NOMGEO (municipality name) is read from the INEGI Marco Geoestadistico
#   shapefile (00mun.shp). State names (NOM_ENT) are derived from the official
#   INEGI state codes (CVE_ENT extracted from CVEGEO).
#
# IMPORTANT — Volume data NOT available:
#   The columns premium_volume, regular_volume, and premium_share require
#   municipality-level gasoline sales volumes (in physical units, e.g. liters).
#   The files currently in data/raw_public/ do NOT provide this:
#     - SAIC_Exporta_2026318_10052423.xlsx : INEGI Censos Economicos,
#         state-level monetary values only, census years 2003/2008/2013/2018/2023.
#         No physical volumes, no municipality breakdown, no premium/regular split.
#     - Book1.xlsx : PEMEX quarterly segment financials, no product-level volumes.
#   These three columns are set to NA_real_ as explicit placeholders.

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(lubridate)
  library(stringr)
  library(sf)
})

# --------------------------------------------------------------------------
# Internal helpers
# --------------------------------------------------------------------------

# INEGI official state names keyed by CVE_ENT (2-digit zero-padded).
# Source: SAIC entity column ("01 Aguascalientes", etc.) — 32 states + CDMX.
.cve_ent_to_nom <- function() {
  tibble::tribble(
    ~CVE_ENT, ~NOM_ENT,
    "01", "Aguascalientes",
    "02", "Baja California",
    "03", "Baja California Sur",
    "04", "Campeche",
    "05", "Coahuila de Zaragoza",
    "06", "Colima",
    "07", "Chiapas",
    "08", "Chihuahua",
    "09", "Ciudad de Mexico",
    "10", "Durango",
    "11", "Guanajuato",
    "12", "Guerrero",
    "13", "Hidalgo",
    "14", "Jalisco",
    "15", "Mexico",
    "16", "Michoacan de Ocampo",
    "17", "Morelos",
    "18", "Nayarit",
    "19", "Nuevo Leon",
    "20", "Oaxaca",
    "21", "Puebla",
    "22", "Queretaro",
    "23", "Quintana Roo",
    "24", "San Luis Potosi",
    "25", "Sinaloa",
    "26", "Sonora",
    "27", "Tabasco",
    "28", "Tamaulipas",
    "29", "Tlaxcala",
    "30", "Veracruz de Ignacio de la Llave",
    "31", "Yucatan",
    "32", "Zacatecas"
  )
}

# Reads INEGI municipios shapefile and returns a tidy lookup:
# CVEGEO (5-digit) | CVE_ENT | NOM_ENT | NOM_MUN
.build_geo_lookup <- function(
  mun_shp = "data/map/inegi_mg_2024/unzipped/ONLY_MUNICIPIOS_00mun/00mun.shp"
) {
  if (!file.exists(mun_shp)) {
    warning(
      "Municipios shapefile not found: ", mun_shp,
      ". INEGI names will be NA in output."
    )
    return(tibble::tibble(CVEGEO = character(), CVE_ENT = character(),
                          NOM_ENT = character(), NOM_MUN = character()))
  }

  mun <- sf::st_read(mun_shp, quiet = TRUE) %>%
    sf::st_drop_geometry() %>%
    transmute(
      CVEGEO  = stringr::str_pad(as.character(CVEGEO), width = 5, side = "left", pad = "0"),
      CVE_ENT = stringr::str_pad(as.character(CVE_ENT), width = 2, side = "left", pad = "0"),
      NOM_MUN = as.character(NOMGEO)
    )

  state_lk <- .cve_ent_to_nom()

  mun %>% left_join(state_lk, by = "CVE_ENT")
}

# --------------------------------------------------------------------------
# Step 1: balanced panel → municipality x day
# --------------------------------------------------------------------------

.compute_mun_day_one_year <- function(panel_yr) {
  panel_yr %>%
    # Exclude invalid CVEGEOs
    filter(
      !is.na(CVEGEO),
      CVEGEO != "",
      CVEGEO != "000NA",
      CVEGEO != "00000"
    ) %>%
    mutate(date = as.Date(date)) %>%
    group_by(CVEGEO, date) %>%
    summarise(
      n_stations_total          = n(),
      n_stations_regular        = sum(!is.na(station_regular)),
      n_stations_premium        = sum(!is.na(station_premium)),
      # flag_carry_forward is TRUE when price came from LOCF (within 60-day cap).
      # Stale prices are already set to NA upstream, so conditioning on
      # !is.na(station_*) cleanly counts only non-stale imputed observations.
      n_stations_reg_imputed    = sum(!is.na(station_regular) & flag_carry_forward),
      n_stations_prem_imputed   = sum(!is.na(station_premium) & flag_carry_forward),
      mun_avg_regular    = mean(station_regular, na.rm = TRUE),
      mun_avg_premium    = mean(station_premium, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # mean(x, na.rm=TRUE) returns NaN when all inputs are NA — convert to NA
      mun_avg_regular = if_else(
        n_stations_regular == 0L | is.nan(mun_avg_regular),
        NA_real_, mun_avg_regular
      ),
      mun_avg_premium = if_else(
        n_stations_premium == 0L | is.nan(mun_avg_premium),
        NA_real_, mun_avg_premium
      )
    )
}

# --------------------------------------------------------------------------
# Step 2: municipality x day → municipality x month
# --------------------------------------------------------------------------

.compute_mun_month_from_day <- function(mun_day, year) {
  mun_day %>%
    mutate(
      year  = as.integer(year),
      month = lubridate::month(date)
    ) %>%
    group_by(CVEGEO, year, month) %>%
    summarise(
      n_days_in_month          = n(),
      n_days_with_regular      = sum(!is.na(mun_avg_regular)),
      n_days_with_premium      = sum(!is.na(mun_avg_premium)),
      regular_price_monthly    = mean(mun_avg_regular, na.rm = TRUE),
      premium_price_monthly    = mean(mun_avg_premium, na.rm = TRUE),
      # Total and imputed station-day counts summed across days in month
      .n_sd_reg_total   = sum(n_stations_regular,     na.rm = TRUE),
      .n_sd_prem_total  = sum(n_stations_premium,     na.rm = TRUE),
      .n_sd_reg_imp     = sum(n_stations_reg_imputed,  na.rm = TRUE),
      .n_sd_prem_imp    = sum(n_stations_prem_imputed, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      regular_price_monthly = if_else(
        n_days_with_regular == 0L | is.nan(regular_price_monthly),
        NA_real_, regular_price_monthly
      ),
      premium_price_monthly = if_else(
        n_days_with_premium == 0L | is.nan(premium_price_monthly),
        NA_real_, premium_price_monthly
      ),
      frac_imputed_regular = if_else(
        .n_sd_reg_total  > 0L, .n_sd_reg_imp  / .n_sd_reg_total,  NA_real_
      ),
      frac_imputed_premium = if_else(
        .n_sd_prem_total > 0L, .n_sd_prem_imp / .n_sd_prem_total, NA_real_
      )
    ) %>%
    dplyr::select(-.n_sd_reg_total, -.n_sd_prem_total,
                  -.n_sd_reg_imp,   -.n_sd_prem_imp)
}

# --------------------------------------------------------------------------
# Main function: balanced panel files → municipality x month parquet
# --------------------------------------------------------------------------

compute_mun_month_prices <- function(
  balanced_panel_files,
  out_path       = "data/analysis/mun_month_prices/mun_month_prices.parquet",
  mun_shp        = "data/map/inegi_mg_2024/unzipped/ONLY_MUNICIPIOS_00mun/00mun.shp",
  volumes_file   = NULL
) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

  # --- Load INEGI geo names ---
  geo_lookup <- .build_geo_lookup(mun_shp)

  # --- Process one year at a time (memory-safe) ---
  message(sprintf("compute_mun_month_prices: processing %d year files",
                  length(balanced_panel_files)))

  mun_month_list <- lapply(balanced_panel_files, function(f) {
    yr <- as.integer(stringr::str_extract(f, "(?<=year=)\\d+"))
    if (is.na(yr)) {
      warning("Could not extract year from path: ", f, " — skipping.")
      return(NULL)
    }
    message(sprintf("  [%d] Step 1: station -> mun x day", yr))
    panel_yr <- arrow::read_parquet(f)
    mun_day  <- .compute_mun_day_one_year(panel_yr)

    message(sprintf(
      "  [%d] Step 1 done: %d municipality-days (%.0f municipalities)",
      yr, nrow(mun_day), n_distinct(mun_day$CVEGEO)
    ))

    message(sprintf("  [%d] Step 2: mun x day -> mun x month", yr))
    mun_month_yr <- .compute_mun_month_from_day(mun_day, yr)

    message(sprintf(
      "  [%d] Step 2 done: %d municipality-months",
      yr, nrow(mun_month_yr)
    ))
    mun_month_yr
  })

  # Remove any NULLs (skipped years)
  mun_month_list <- Filter(Negate(is.null), mun_month_list)
  mun_month      <- dplyr::bind_rows(mun_month_list)

  # --- Price ratio ---
  mun_month <- mun_month %>%
    mutate(
      # premium_to_regular_price_ratio:
      # Monthly premium price divided by monthly regular price, within municipality.
      # Computed from step-2 averages (averages of daily averages).
      # NA when either monthly price is missing.
      premium_to_regular_price_ratio = premium_price_monthly / regular_price_monthly
    )

  # --- Join INEGI names ---
  mun_month <- mun_month %>%
    left_join(geo_lookup, by = "CVEGEO")

  # --- Join volumes and compute premium_share ---
  # volumes_file: path to mun_month_volumes.parquet produced by process_volumes()
  # Columns expected: CVEGEO, year, month, regular_volume_l, premium_volume_l
  #
  # premium_share = premium_volume_l / (premium_volume_l + regular_volume_l)
  # Interpretation: share of premium gasoline in total (premium + regular) sales
  # by volume (liters) within the municipality-month.
  # NA when either volume is missing.
  if (!is.null(volumes_file) && file.exists(volumes_file)) {
    vols <- arrow::read_parquet(volumes_file) |>
      dplyr::select(CVEGEO, year, month, regular_volume_l, premium_volume_l)
    mun_month <- mun_month |>
      dplyr::left_join(vols, by = c("CVEGEO", "year", "month")) |>
      dplyr::mutate(
        premium_volume = premium_volume_l,
        regular_volume = regular_volume_l,
        # premium_share: premium liters / (premium + regular liters)
        # NA if either volume is missing or their sum is zero/NA
        premium_share = dplyr::if_else(
          !is.na(premium_volume_l) & !is.na(regular_volume_l) &
            (premium_volume_l + regular_volume_l) > 0,
          premium_volume_l / (premium_volume_l + regular_volume_l),
          NA_real_
        )
      ) |>
      dplyr::select(-regular_volume_l, -premium_volume_l)

    pct_vol_matched <- round(100 * mean(!is.na(mun_month$premium_volume)), 1)
    message(sprintf(
      "  Volume join: %.1f%% of mun-months matched (premium_volume non-NA)",
      pct_vol_matched
    ))

    # Coverage by year — flag years with < 50% match
    cov_yr <- mun_month |>
      dplyr::group_by(year) |>
      dplyr::summarise(
        pct_vol = round(100 * mean(!is.na(premium_volume)), 1),
        .groups = "drop"
      )
    message("  Volume coverage by year:")
    for (i in seq_len(nrow(cov_yr))) {
      flag <- if (cov_yr$pct_vol[i] < 50) " <<< LOW" else ""
      message(sprintf("    %d: %.1f%%%s", cov_yr$year[i], cov_yr$pct_vol[i], flag))
    }
    if (any(cov_yr$pct_vol < 50)) {
      warning("[mun_month] Volume coverage below 50% in at least one year. ",
              "Check whether the volume CSV covers that year.")
    }
  } else {
    if (is.null(volumes_file)) {
      message("  volumes_file not provided — premium_volume, regular_volume, premium_share set to NA")
    } else {
      warning("  volumes_file not found: ", volumes_file,
              " — volume columns set to NA")
    }
    mun_month <- mun_month |>
      dplyr::mutate(
        premium_volume = NA_real_,
        regular_volume = NA_real_,
        premium_share  = NA_real_
      )
  }

  # --- Final column order (matches Shaun's requested table) ---
  mun_month <- mun_month %>%
    select(
      year,
      month,
      CVEGEO,
      CVE_ENT,
      NOM_ENT,
      NOM_MUN,
      premium_price_monthly,
      regular_price_monthly,
      premium_to_regular_price_ratio,
      premium_volume,
      regular_volume,
      premium_share,
      # Diagnostic columns
      n_days_in_month,
      n_days_with_regular,
      n_days_with_premium,
      # Carry-forward diagnostics: fraction of station-day price observations
      # that came from LOCF imputation (within the 60-day cap).
      # 0 = all prices observed fresh that day; 1 = all prices carried forward.
      frac_imputed_regular,
      frac_imputed_premium
    ) %>%
    arrange(CVEGEO, year, month)

  # ==========================================================================
  # VALIDATION CHECKS
  # ==========================================================================

  n_rows    <- nrow(mun_month)
  n_mun     <- n_distinct(mun_month$CVEGEO)
  n_years   <- n_distinct(mun_month$year)

  # Check 1: expected months per mun-year
  expected_mun_year_months <- n_mun * n_years * 12L
  actual_mun_year_months   <- nrow(mun_month)
  # (Not a hard fail: municipalities may not appear in all year-months)

  # Check 2: missing rates
  pct_miss_reg   <- round(100 * mean(is.na(mun_month$regular_price_monthly)), 1)
  pct_miss_prem  <- round(100 * mean(is.na(mun_month$premium_price_monthly)), 1)
  pct_miss_ratio <- round(100 * mean(is.na(mun_month$premium_to_regular_price_ratio)), 1)

  # Check 3: CVEGEO not matched to INEGI names
  n_no_inegi_name <- sum(is.na(mun_month$NOM_MUN))
  n_cvgeos_in_output <- n_distinct(mun_month$CVEGEO)
  n_cvgeos_in_lookup <- n_distinct(geo_lookup$CVEGEO)

  # Check 4: double-aggregation integrity
  # For each mun-year-month, n_days_with_regular should be <= n_days_in_month
  bad_days <- mun_month %>%
    filter(n_days_with_regular > n_days_in_month | n_days_with_premium > n_days_in_month)
  if (nrow(bad_days) > 0L) {
    warning(sprintf(
      "[mun_month] %d rows have n_days_with_price > n_days_in_month (unexpected)",
      nrow(bad_days)
    ))
  }

  # Check 5: carry-forward fraction summary
  pct_imp_reg  <- round(100 * mean(mun_month$frac_imputed_regular,  na.rm = TRUE), 1)
  pct_imp_prem <- round(100 * mean(mun_month$frac_imputed_premium, na.rm = TRUE), 1)

  message("=== Municipality-month output: validation summary ===")
  message(sprintf("  Total rows:                     %d", n_rows))
  message(sprintf("  Unique municipalities (CVEGEO): %d", n_mun))
  message(sprintf("  Years covered:                  %d", n_years))
  message(sprintf("  Missing regular_price_monthly:  %.1f%%", pct_miss_reg))
  message(sprintf("  Missing premium_price_monthly:  %.1f%%", pct_miss_prem))
  message(sprintf("  Missing price_ratio:            %.1f%%", pct_miss_ratio))
  message(sprintf("  CVEGEOs without INEGI name:     %d (of %d in shapefile)",
                  n_no_inegi_name, n_cvgeos_in_lookup))
  message(sprintf(
    "  Carry-forward share in monthly avg: regular=%.1f%%, premium=%.1f%%",
    pct_imp_reg, pct_imp_prem
  ))
  message(sprintf("  premium_volume / regular_volume / premium_share: ALL NA"))
  message(sprintf("  -> Provide municipality-level sales volume data to populate."))

  arrow::write_parquet(mun_month, out_path, compression = "zstd")
  message(sprintf("Written: %s", out_path))
  out_path
}

# --------------------------------------------------------------------------
# Merge CONEVAL municipal poverty index into municipality x month panel
# --------------------------------------------------------------------------
#
# Left-joins poverty_final (and companion columns) from the CONEVAL 2020
# municipal poverty parquet into the municipality x month gasoline panel.
#
# The join key is CVEGEO (5-digit, zero-padded character). All 1,539
# CVEGEOs present in the gasoline panel have a matching row in the poverty
# file; 930 poverty CVEGEOs with no gas stations are dropped (left join).
#
# Poverty data is cross-sectional (2020 only). The same poverty value is
# attached to every year-month row for a given municipality.
#
# Output columns added:
#   poverty_final            — weighted mean of sex/age/geo partition estimates
#   poverty_sex              — sex-partition estimate
#   poverty_age              — age-partition estimate
#   poverty_geo              — geo-partition estimate
#   n_poverty_estimates      — number of non-NA partition estimates (0-3)
#   flag_partition_divergence — TRUE if any two estimates differ by > 5pp
# --------------------------------------------------------------------------

merge_poverty_into_mun_month <- function(
  mun_month_parquet,
  poverty_parquet = "data/processed/coneval/municipal_poverty_2020.parquet",
  out_path        = "data/analysis/mun_month_prices/mun_month_prices_with_poverty.parquet"
) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

  mun_month <- arrow::read_parquet(mun_month_parquet)
  poverty   <- arrow::read_parquet(poverty_parquet) |>
    dplyr::select(
      CVEGEO,
      poverty_final,
      poverty_sex,
      poverty_age,
      poverty_geo,
      n_estimates,
      flag_partition_divergence
    ) |>
    dplyr::rename(n_poverty_estimates = n_estimates)

  out <- mun_month |>
    dplyr::left_join(poverty, by = "CVEGEO")

  # --- Validation ---
  n_rows        <- nrow(out)
  n_mun         <- dplyr::n_distinct(out$CVEGEO)
  n_pov_nonNA   <- sum(!is.na(out$poverty_final))
  n_pov_NA      <- sum(is.na(out$poverty_final))
  pct_matched   <- round(100 * n_pov_nonNA / n_rows, 1)

  message("=== merge_poverty_into_mun_month ===")
  message(sprintf("  Gasoline panel rows:              %d", n_rows))
  message(sprintf("  Unique CVEGEOs in panel:          %d", n_mun))
  message(sprintf("  Rows with poverty_final non-NA:   %d (%.1f%%)", n_pov_nonNA, pct_matched))
  message(sprintf("  Rows with poverty_final NA:       %d", n_pov_NA))

  if (n_pov_NA > 0L) {
    unmatched_cvegeos <- out |>
      dplyr::filter(is.na(poverty_final)) |>
      dplyr::distinct(CVEGEO, NOM_MUN, NOM_ENT)
    message(sprintf("  CVEGEOs without poverty data:     %d",
                    nrow(unmatched_cvegeos)))
    message("  (first 10):")
    unmatched_cvegeos |>
      head(10) |>
      (function(d) message(paste(capture.output(print(as.data.frame(d))),
                                 collapse = "\n")))()
  }

  # --- Volume coverage by poverty quartile ---
  # Detects whether missing volumes are systematically concentrated in poorer
  # municipalities, which would introduce selection bias in the regression sample.
  if ("premium_volume" %in% names(out) && "poverty_final" %in% names(out)) {
    pov_cov <- out |>
      dplyr::filter(!is.na(poverty_final)) |>
      dplyr::mutate(
        pov_quartile = dplyr::ntile(poverty_final, 4)
      ) |>
      dplyr::group_by(pov_quartile) |>
      dplyr::summarise(
        pct_volume  = round(100 * mean(!is.na(premium_volume)), 1),
        mean_poverty = round(mean(poverty_final), 1),
        .groups = "drop"
      )
    message("  Volume coverage by poverty quartile (Q1=richest, Q4=poorest):")
    for (i in seq_len(nrow(pov_cov))) {
      message(sprintf("    Q%d (mean pov=%.1f%%): volume match=%.1f%%",
                      pov_cov$pov_quartile[i],
                      pov_cov$mean_poverty[i],
                      pov_cov$pct_volume[i]))
    }
    pov_range <- diff(range(pov_cov$pct_volume, na.rm = TRUE))
    if (pov_range > 15) {
      warning(sprintf(
        "[mun_month_poverty] Volume coverage varies %.1fpp across poverty quartiles. ",
        pov_range
      ), "This may indicate selection bias in the regression sample.")
    }
  }

  arrow::write_parquet(out, out_path, compression = "zstd")
  message(sprintf("Written: %s", out_path))
  out_path
}
