suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(fixest)
  library(ggplot2)
  library(scales)
  library(lubridate)
  library(tidyr)
  library(grid)
  library(gridExtra)
  library(tibble)
})

.meth_data <- function(n_obs, n_mun, yr_min, yr_max, sample_label) {
  lines <- c(
    "DATA CONSTRUCTION",
    "",
    "DEPENDENT VARIABLE — LOGIT PREMIUM SHARE",
    "  Source: CRE/SENER retail volume reports (litres sold by fuel type,",
    "  municipality, month). 2017-2025.",
    "  - Municipalities reporting across comma-separated locations (~4.6% of",
    "    rows) are dropped: cannot be allocated to a single CVEGEO.",
    "  - Remaining records matched to INEGI CVEGEO via string normalisation.",
    "  - Premium and Magna litres summed by CVEGEO x calendar month.",
    "  - logit_share = log(premium_lt / magna_lt)",
    "                = log(premium_share / (1 - premium_share))",
    "  - Municipality-months where premium_share = 0 or 1 are dropped.",
    sprintf("  Panel: %s obs | %s municipalities | %d-%d  [%s]",
            scales::comma(n_obs), scales::comma(n_mun),
            yr_min, yr_max, sample_label),
    "",
    "KEY REGRESSOR — LOG PRICE RATIO",
    "  Source: CRE station-level daily price reports (Magna and Premium).",
    "  - Station prices averaged within each CVEGEO x day (equal-weight mean).",
    "  - LOCF carry-forward applied for up to 60 consecutive missing days;",
    "    beyond 60 days the value is set to NA.",
    "  - Municipal daily averages averaged within each CVEGEO x calendar month.",
    "  - log_price_ratio = log(premium_price_monthly / regular_price_monthly).",
    "",
    "INSTRUMENT 1 — NATIONAL WHOLESALE PRICE RATIO",
    "  Source: PEMEX terminal gate prices, 76 terminals, daily, 2017-2025.",
    "  - National average wholesale premium/regular ratio computed per month",
    "    by averaging across all terminals.",
    "  - log_national_wholesale_ratio = log(national_avg_prem / national_avg_reg).",
    "  - National average used to avoid terminal-level simultaneity bias.",
    "  - Joined to panel on year + month only (no municipality mapping).",
    "",
    "INSTRUMENT 2 — IEPS PREMIUM / MAGNA CUOTA RATIO",
    "  Source: DOF weekly decrees, manually entered (IEPS_Combustibles.xlsx).",
    "  - IEPS federal excise tax (MXN/litre) set independently for Premium",
    "    and Magna via weekly SHCP stimulus adjustments.",
    "  - Weekly rows expanded to daily using fecha_inicio / fecha_fin,",
    "    then averaged to calendar month.",
    "  - log_ieps_ratio = log(ieps_prem_cuota / ieps_magna_cuota).",
    "  - Full decree history used (2017 onwards).",
    "  - 7 erroneous entries corrected after systematic audit vs. DOF.",
    "",
    "INCOME — MUNICIPAL INCOME, CAR-OWNING HOUSEHOLDS",
    "  Source: INEGI Censo 2020, Cuestionario Ampliado.",
    "  - Households with AUTOPROP=7 (car ownership), INGTRHOG in (0, 999999).",
    "  - Weighted mean of quarterly household income by CVEGEO (FACTOR weight).",
    "  - Standardised: income_m_std = (income_m - mean) / sd.",
    "  - Time-invariant cross-section, joined on CVEGEO only.",
    "  - Conditioning on car ownership focuses on households choosing between",
    "    fuel grades."
  )
  .text_page(lines, title_line = 1L)
}

.meth_specs <- function() {
  lines <- c(
    "ECONOMETRIC SPECIFICATIONS",
    "",
    "FIXED EFFECTS AND CLUSTERING",
    "  IV specs use year FE + month FE (separate). Both instruments vary only",
    "  nationally per month, so year-month FE would absorb them entirely.",
    "  Separate year + month FE preserves within-year month-to-month variation.",
    "  Spec 1 uses year-month FE (strictest time control).",
    "  Clustering: CVEGEO + year_month (two-way) for Specs 2-6.",
    "              CVEGEO only for Spec 1 (year_month absorbed by FE).",
    "",
    "SPEC 1 — OLS BASELINE  [Mun FE + Year-month FE, cluster: CVEGEO]",
    "  logit_share ~ log(p_prem/p_reg) | CVEGEO + year_month",
    "  OLS benchmark. May be biased if local shocks drive price and mix together.",
    "",
    "SPEC 2 — IV: NATIONAL WHOLESALE  [Mun FE + Year FE + Month FE]",
    "  log(p_prem/p_reg) instrumented by log_national_wholesale_ratio.",
    "  Full sample 2017-2025.",
    "",
    "SPEC 3 — IV: IEPS RATIO  [Mun FE + Year FE + Month FE]",
    "  log(p_prem/p_reg) instrumented by log_ieps_ratio.",
    "  Full IEPS history (2017 onwards).",
    "",
    "SPEC 4 — IV: BOTH INSTRUMENTS  [Mun FE + Year FE + Month FE]",
    "  Over-identified: 2 instruments for 1 endogenous variable.",
    "  Sargan-Hansen test: H0 = both instruments orthogonal to structural error.",
    "  Rejection means at least one instrument is invalid (test cannot say which).",
    "",
    "SPEC 5 — IV x INCOME  [Mun FE + Year FE + Month FE]",
    "  Two endogenous variables: log(p_prem/p_reg) and its interaction with",
    "  income_m_std. Instruments: log_national_wholesale_ratio and its",
    "  interaction with income_m_std.",
    "  beta(income) = beta1 + beta2 * income_m_std",
    "    beta1 = elasticity at mean income.",
    "    beta2 = income gradient of elasticity.",
    "",
    "SPEC 6 — IV: UNRESTRICTED  [Mun FE + Year FE + Month FE]",
    "  Two endogenous: log(p_prem/p_reg) and log(p_reg).",
    "  Instruments: log_national_wholesale_ratio and log_terminal_regular.",
    "  Specs 1-5 impose beta_prem = -beta_reg (ratio restriction).",
    "  Spec 6 tests this: H0 = coeff on log(p_reg) = 0 (no income effect).",
    "",
    "NATIONAL TIME-SERIES COLLAPSE",
    "  Panel collapsed to national monthly averages (~96 year-month obs).",
    "  OLS (raw and year FE) and IV run on collapsed series.",
    "  Tests whether aggregate time-series variation drives the result."
  )
  .text_page(lines, title_line = 1L)
}

build_results_pdf <- function(
  base_parquet         = "data/analysis/mun_month_prices/mun_month_prices_with_poverty.parquet",
  ieps_monthly_parquet = "data/processed/ieps/ieps_monthly.parquet",
  income_parquet       = "data/processed/inegi_vehiculos/municipal_income_car_owners.parquet",
  terminal_dir         = "data/processed/terminal",
  bloomberg_parquet    = "data/processed/bloomberg/gasoline_bloomberg.parquet",
  out_path             = "outputs/shaun/results_updated.pdf",
  restricted_states    = NULL,
  precomputed_dir      = NULL
) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

  sample_label <- if (is.null(restricted_states)) "Full sample" else {
    paste0("Restricted (excl. ", paste(restricted_states, collapse = ", "), ")")
  }

  message("=== Building regression panel ===")
  panel <- .build_regression_panel(
    base_parquet, ieps_monthly_parquet, income_parquet, terminal_dir,
    restricted_states = restricted_states
  )
  message(sprintf("  %d obs | %d mun | %d year-months",
    nrow(panel), dplyr::n_distinct(panel$CVEGEO),
    dplyr::n_distinct(panel$year_month)))

  models_rds   <- if (!is.null(precomputed_dir)) file.path(precomputed_dir, "models.rds")          else NULL
  nat_rds      <- if (!is.null(precomputed_dir)) file.path(precomputed_dir, "national_models.rds") else NULL
  bloom_rds    <- if (!is.null(precomputed_dir)) file.path(precomputed_dir, "bloomberg_models.rds") else NULL

  if (!is.null(models_rds) && file.exists(models_rds)) {
    message("=== Loading pre-computed models ===")
    models           <- readRDS(models_rds)
    national_models  <- if (!is.null(nat_rds)   && file.exists(nat_rds))   readRDS(nat_rds)   else NULL
    bloomberg_models <- if (!is.null(bloom_rds)  && file.exists(bloom_rds)) readRDS(bloom_rds) else list()
  } else {
    message("=== Running regressions ===")
    models           <- .run_regressions(panel)
    national_models  <- .run_national_collapse(panel)
    bloomberg_models <- .run_bloomberg_specs(panel)
  }

  message("=== Building spread data ===")
  retail_spread    <- if (file.exists(base_parquet)) {
    tryCatch(.read_retail_national_monthly_spread(base_parquet),
             error = function(e) NULL)
  } else NULL
  terminal_spread  <- .read_terminal_national_monthly_spread(terminal_dir)
  bloomberg_spread <- if (file.exists(bloomberg_parquet)) {
    tryCatch(.read_bloomberg_monthly_spread(bloomberg_parquet),
             error = function(e) NULL)
  } else NULL

  ieps_daily_parquet <- sub("ieps_monthly\\.parquet$", "ieps_daily.parquet",
                            ieps_monthly_parquet)

  ieps_x_end <- tryCatch({
    tmp <- arrow::read_parquet(ieps_monthly_parquet, mmap = FALSE)
    lubridate::make_date(max(tmp$year), max(tmp$month[tmp$year == max(tmp$year)]), 1L)
  }, error = function(e) as.Date("2026-04-01"))

  message(sprintf("=== Writing PDF: %s ===", out_path))
  grDevices::pdf(out_path, width = 11, height = 8.5, onefile = TRUE)

  grid::grid.newpage()
  grid::grid.draw(.make_regression_table_grob(models[setdiff(names(models), "(4) IV-Both")]))

  if (!is.null(national_models) && length(national_models) > 0L) {
    grid::grid.newpage()
    grid::grid.draw(.make_regression_table_grob(national_models))
  }

  if (length(bloomberg_models) > 0L) {
    grid::grid.newpage()
    grid::grid.draw(.make_regression_table_grob(bloomberg_models))
  }

  reg5 <- models[["(5) IV×Income"]]
  if (!is.null(reg5)) {
    pg5 <- tryCatch(.plot_income_interaction(reg5, income_data = panel),
                    error = function(e) NULL)
    if (!is.null(pg5)) print(pg5)
  }

  ieps_graph <- tryCatch(.plot_ieps_rates(ieps_daily_parquet), error = function(e) NULL)
  if (!is.null(ieps_graph)) print(ieps_graph)

  ieps_spread_graph <- tryCatch(.plot_ieps_spread(ieps_daily_parquet), error = function(e) NULL)
  if (!is.null(ieps_spread_graph)) print(ieps_spread_graph)

  price_ratio_graph <- tryCatch(
    .plot_national_price_ratio(base_parquet), error = function(e) NULL
  )
  if (!is.null(price_ratio_graph)) print(price_ratio_graph)

  vol_graph <- tryCatch(
    .plot_national_volumes(base_parquet), error = function(e) NULL
  )
  if (!is.null(vol_graph)) print(vol_graph)

  combined_ratios_graph <- tryCatch(
    .plot_combined_ratios(base_parquet), error = function(e) NULL
  )
  if (!is.null(combined_ratios_graph)) print(combined_ratios_graph)

  bloom_graph <- tryCatch(
    .plot_bloomberg_prices(bloomberg_parquet, x_end = ieps_x_end),
    error = function(e) NULL
  )
  if (!is.null(bloom_graph)) print(bloom_graph)

  grDevices::dev.off()
  message(sprintf("Done: %s", out_path))
  out_path
}
