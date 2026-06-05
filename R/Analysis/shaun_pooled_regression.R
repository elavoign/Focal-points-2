suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(fixest)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(tibble)
  library(purrr)
  library(tidyr)
})

# --------------------------------------------------------------------------
# Internal: text-page helper (shared by both methodology pages)
# --------------------------------------------------------------------------

.text_page <- function(lines, title_line = 1L) {
  df <- tibble::tibble(
    x    = 0,
    y    = rev(seq_along(lines)),
    lab  = lines,
    bold = seq_along(lines) == title_line |
           (nchar(lines) > 0 & lines == toupper(lines) & nchar(lines) >= 4)
  )

  ggplot2::ggplot(df, ggplot2::aes(
      x = .data$x, y = .data$y, label = .data$lab
    )) +
    ggplot2::geom_text(
      ggplot2::aes(fontface = ifelse(.data$bold, "bold", "plain")),
      hjust  = 0, size = 2.85, family = "mono",
      colour = ifelse(df$bold, "#111111", "#333333")
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = 0.8)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin     = ggplot2::margin(20, 20, 20, 20),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}

# --------------------------------------------------------------------------
# Methodology page 1 — Data & panel construction
# --------------------------------------------------------------------------

.make_methodology_page_1 <- function(n_obs, n_mun, year_min, year_max,
                                     sample_label = "Full sample") {
  lines <- c(
    "PAGE 1 OF 2: DATA CONSTRUCTION",
    "",
    "1. DEPENDENT VARIABLE — LOGIT PREMIUM SHARE",
    "   Source: CRE/SENER retail volume reports (liters sold by fuel type,",
    "   municipality, month).",
    "   a) I drop municipalities that report volume across a comma-separated list",
    "      of locations (~4.6% of rows) — these cannot be allocated to a single",
    "      CVEGEO without arbitrary assumptions.",
    "   b) I match remaining records to INEGI CVEGEO codes via string",
    "      normalization (accents removed, uppercase, punctuation stripped).",
    "   c) I sum Premium and Magna liters by CVEGEO x calendar month.",
    "   d) Dependent variable: logit_share = log(premium_lt / magna_lt)",
    "                                      = log(premium_share / (1-premium_share)).",
    "      I drop municipality-months where premium_share is 0 or 1.",
    "      Beta is the grade substitution elasticity: a 1-unit increase in",
    "      log(P_prem/P_mag) changes log(premium_lt/magna_lt) by beta units.",
    sprintf("   Panel: %s obs | %s municipalities | %d-%d  [%s]",
            scales::comma(n_obs), scales::comma(n_mun), year_min, year_max,
            sample_label),
    "",
    "2. KEY REGRESSOR — LOG PRICE RATIO",
    "   Source: CRE station-level daily price reports (Magna and Premium).",
    "   a) I average all station prices within each CVEGEO x day (equal-weight",
    "      mean across stations with non-NA prices). I carry-forward up to 60",
    "      consecutive missing station-days; beyond 60 days I set the value to NA.",
    "   b) I average municipal daily prices within each CVEGEO x calendar month.",
    "   c) log_price_ratio = log(premium_price_monthly / regular_price_monthly).",
    "",
    "3. INSTRUMENT 1 — NATIONAL WHOLESALE PRICE RATIO",
    "   Source: PEMEX terminal gate prices, 76 terminals, daily, 2017-2025.",
    "   I compute the national average wholesale premium/regular ratio by",
    "   averaging across all terminals within each calendar month:",
    "   log_national_wholesale_ratio = log(national_avg_prem / national_avg_reg).",
    "   Using the national average avoids terminal-specific deviations that",
    "   could be driven by local demand (simultaneity concern for terminal-level IV).",
    "   Joined to panel on year + month only — no municipality mapping needed.",
    "",
    "4. INSTRUMENT 2 — IEPS PREMIUM / MAGNA CUOTA RATIO",
    "   Source: DOF weekly decrees, manually entered in IEPS_Combustibles.xlsx.",
    "   The IEPS federal excise tax (pesos/litre) applies separately to Premium",
    "   and Magna, with independent stimulus adjustments published weekly by SHCP.",
    "   a) When SHCP publishes a decree, I use the effective cuota for both",
    "      Premium and Magna directly.",
    "   b) For weeks with no decree, I apply the statutory base rates (estimulo=0%)",
    "      from the CUOTAS_BASE sheet, set annually by Ley del IEPS.",
    "   c) I expand each weekly row to one row per calendar day using",
    "      fecha_inicio / fecha_fin, then average to calendar month.",
    "   d) Instrument: log_ieps_ratio = log(ieps_prem_cuota / ieps_magna_cuota).",
    "      This captures the differential tax burden on Premium vs Magna,",
    "      shifting the retail price ratio through a policy channel.",
    "   Note: I currently use decree data from 2022 onwards only.",
    "",
    "5. INCOME — MUNICIPAL INCOME AMONG CAR-OWNING HOUSEHOLDS",
    "   Source: INEGI Censo 2020, Cuestionario Ampliado (Viviendas_CA_XX.csv).",
    "   Variables: INGTRHOG (quarterly household income, nominal pesos),",
    "   AUTOPROP (car ownership: 7=yes), FACTOR (expansion weight), ENT + MUN.",
    "   a) I keep households where AUTOPROP = 7, INGTRHOG in (0, 999999), FACTOR > 0.",
    "   b) I build CVEGEO = zero-padded ENT (2 digits) + MUN (3 digits).",
    "   c) I compute a weighted mean of INGTRHOG by CVEGEO, using FACTOR as weight.",
    "   d) I standardize: income_m_std = (income_m - mean) / sd.",
    "   Conditioning on car ownership focuses the measure on households actually",
    "   choosing between fuel grades, not the full municipal population.",
    "   I use it as a time-invariant proxy (joined on CVEGEO only)."
  )
  .text_page(lines, title_line = 1L)
}

# --------------------------------------------------------------------------
# Methodology page 2 — Econometric model & identification
# --------------------------------------------------------------------------

.make_methodology_page_2 <- function() {
  lines <- c(
    "PAGE 2 OF 2: ECONOMETRIC SPECIFICATIONS",
    "",
    "FIXED EFFECTS",
    "  All IV specifications use year FE + month FE (separate, not interacted).",
    "  Both instruments take a single national value per calendar month, so",
    "  they would be perfectly collinear with year-month dummies and absorbed",
    "  entirely — no variation left for the first stage. Separate year and",
    "  month FE preserve within-year, month-to-month variation for identification.",
    "  Spec 1 (OLS baseline) uses year-month FE as the strictest time control.",
    "  Standard errors clustered at the CVEGEO level in all specifications.",
    "",
    "SPECIFICATION 1 — POOLED FE OLS (BASELINE)  [Year-month FE]",
    "  logit_share_mt = alpha_m + alpha_t + beta*log(P_prem/P_mag)_mt + e_mt",
    "  I estimate this by OLS after within-transformation. Beta may be biased",
    "  if unobserved local shocks drive both the price ratio and the fuel mix",
    "  simultaneously (simultaneity bias). Used as the strict OLS benchmark.",
    "",
    "SPECIFICATION 2 — IV: NATIONAL WHOLESALE RATIO  [Year + Month FE]",
    "  Endogenous:  log(P_prem/P_mag)_mt",
    "  Instrument:  log_national_wholesale_ratio_t",
    "  I estimate this by 2SLS. The national wholesale ratio shifts retail",
    "  prices through upstream supply costs (crude, refining, logistics).",
    "  Using the national average removes terminal-level endogeneity concerns.",
    "",
    "SPECIFICATION 3 — IV: IEPS RATIO ONLY  [Year + Month FE]",
    "  Endogenous:  log(P_prem/P_mag)_mt",
    "  Instrument:  log_ieps_ratio_t = log(ieps_prem_cuota / ieps_magna_cuota)",
    "  I estimate this by 2SLS. Identification uses the differential federal",
    "  tax burden on Premium vs Magna: when the IEPS ratio changes, it shifts",
    "  the premium/regular retail price ratio nationally.",
    "  Sample restricted to 2022-2025 (years with actual decree data).",
    "",
    "SPECIFICATION 4 — IV: BOTH INSTRUMENTS  [Year + Month FE]",
    "  Endogenous:  log(P_prem/P_mag)_mt",
    "  Instruments: log_national_wholesale_ratio_t  AND  log_ieps_ratio_t",
    "  I estimate this by 2SLS. With 2 instruments for 1 endogenous variable",
    "  the model is over-identified (one more instrument than strictly needed).",
    "",
    "  SARGAN-HANSEN OVERIDENTIFICATION TEST",
    "  With a just-identified model (1 instrument, 1 endogenous variable),",
    "  there is no way to test instrument validity from the data alone.",
    "  Over-identification makes this possible: each instrument used alone",
    "  should give the same structural estimate of beta. If one is invalid",
    "  — correlated with the error — the two IV estimates will diverge.",
    "  I run the Sargan-Hansen test to formalise this:",
    "    H0: all instruments are orthogonal to the structural error.",
    "    I estimate 2SLS, recover residuals, regress them on all instruments",
    "    and controls. Test stat = n * R^2, chi-squared with df = 2-1 = 1.",
    "    Failure to reject: the two instruments are mutually consistent.",
    "    Rejection: at least one is invalid (test does not identify which).",
    "  Passing does NOT prove validity — both instruments could share the",
    "  same violation and the test would still not reject.",
    "",
    "SPECIFICATION 5 — IV x INCOME INTERACTION  [Year + Month FE]",
    "  Endogenous:  log(P_prem/P_mag)_mt  AND",
    "               log(P_prem/P_mag)_mt x income_m_std",
    "  Instruments: log_national_wholesale_ratio_t  AND",
    "               log_national_wholesale_ratio_t x income_m_std",
    "  income_m is time-invariant — its level is absorbed by the mun FE.",
    "  Its interaction with the price ratio is identified separately.",
    "  I estimate this by 2SLS with two endogenous variables.",
    "  Interpretation: beta(income) = beta1 + beta2 * income_m_std",
    "    beta1 = grade substitution elasticity at mean income.",
    "    beta2 = income gradient of that elasticity.",
    "    beta2 > 0: richer municipalities less elastic (premium inelastic",
    "               where incomes are high) -> IEPS subsidy on Magna",
    "               is relatively progressive.",
    "    beta2 < 0: richer municipalities more elastic -> regressive.",
    "",
    "PANEL JOIN SEQUENCE",
    "  Base panel          : logit_share + log_price_ratio (CVEGEO x yr x mo)",
    "  + national wholesale: joined on year + month  (national series)",
    "  + IEPS ratio        : joined on year + month  (national, 2022+ only)",
    "  + income            : joined on CVEGEO  (2020 cross-section)",
    "  I exclude rows with NA on a required instrument per specification."
  )
  .text_page(lines, title_line = 1L)
}

# --------------------------------------------------------------------------
# Income interaction graph — the core incidence result
# --------------------------------------------------------------------------

.plot_income_interaction <- function(reg4, income_data = NULL) {
  cf  <- fixest::coeftable(reg4)
  vcv <- stats::vcov(reg4)

  # Coefficient names for IV model
  nm_b1 <- intersect(rownames(cf), c("fit_log_price_ratio", "log_price_ratio"))
  nm_b2 <- intersect(rownames(cf),
    c("fit_log_price_ratio:income_m_std", "log_price_ratio:income_m_std"))

  if (length(nm_b1) == 0 || length(nm_b2) == 0) {
    message("  Income interaction graph: could not find expected coefficients — skipping")
    return(NULL)
  }

  beta1 <- cf[nm_b1, "Estimate"]
  beta2 <- cf[nm_b2, "Estimate"]
  v11   <- vcv[nm_b1, nm_b1]
  v22   <- vcv[nm_b2, nm_b2]
  v12   <- vcv[nm_b1, nm_b2]

  # Prediction grid: -2.5 SD to +2.5 SD of income
  x_seq   <- seq(-2.5, 2.5, length.out = 300)
  pred_df <- tibble::tibble(
    income_m_std = x_seq,
    beta_hat     = beta1 + beta2 * x_seq,
    se_hat       = sqrt(pmax(0, v11 + x_seq^2 * v22 + 2 * x_seq * v12)),
    ci_lo        = beta_hat - 1.96 * se_hat,
    ci_hi        = beta_hat + 1.96 * se_hat
  )

  # Income distribution rug (from processed data if provided)
  rug_df <- if (!is.null(income_data) && "income_m_std" %in% names(income_data)) {
    dplyr::distinct(income_data, CVEGEO, income_m_std) |>
      dplyr::filter(!is.na(income_m_std), income_m_std >= -2.5, income_m_std <= 2.5)
  } else NULL

  subtitle_txt <- sprintf(
    paste0(
      "β1 = %.4f (elasticity at mean income)   ",
      "β2 = %.4f (income gradient)   ",
      "SE clustered by municipality"
    ),
    beta1, beta2
  )

  incidence_direction <- if (beta2 > 0) {
    "beta2 > 0: richer municipalities less elastic → subsidy relatively more progressive"
  } else {
    "beta2 < 0: richer municipalities more elastic → subsidy relatively more regressive"
  }

  gg <- ggplot2::ggplot(pred_df, ggplot2::aes(x = income_m_std, y = beta_hat)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        colour = "grey60", linewidth = 0.4) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lo, ymax = ci_hi),
                         fill = "#d7191c", alpha = 0.18) +
    ggplot2::geom_line(colour = "#d7191c", linewidth = 1.2)

  if (!is.null(rug_df)) {
    gg <- gg + ggplot2::geom_rug(
      data  = rug_df,
      ggplot2::aes(x = income_m_std, y = NULL),
      sides = "b", alpha = 0.25, linewidth = 0.3, colour = "#2c7bb6"
    )
  }

  gg +
    ggplot2::scale_x_continuous(
      name   = "Municipal income (standardised; car-owning households, INEGI Censo 2020)",
      breaks = -2:2,
      labels = c("-2 SD\n(very poor)", "-1 SD", "Mean", "+1 SD", "+2 SD\n(very rich)")
    ) +
    ggplot2::scale_y_continuous(
      name   = expression(
        hat(beta)(income[m]) == hat(beta)[1] + hat(beta)[2] %.% income[m]
      ),
      labels = scales::label_number(accuracy = 0.001)
    ) +
    ggplot2::labs(
      title    = "Grade substitution elasticity by municipal income (logit premium share)",
      subtitle = subtitle_txt,
      caption  = paste0(
        incidence_direction, "\n",
        "Red line: predicted elasticity = β1 + β2 × income_m_std. ",
        "Band: 95% CI (delta method). Rug: municipality income distribution."
      )
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = "grey30", size = 8.5),
      plot.caption  = ggplot2::element_text(colour = "grey50", size = 7.5),
      panel.grid.minor = ggplot2::element_blank()
    )
}

# --------------------------------------------------------------------------
# IEPS time-series figures — shared helper
# --------------------------------------------------------------------------

.ieps_ts_theme <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold"),
      plot.subtitle    = ggplot2::element_text(colour = "grey30", size = 8),
      plot.caption     = ggplot2::element_text(colour = "grey50", size = 7),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(face = "bold", size = 9)
    )
}

# Figure 1 — Both IEPS rates (magna + premium) on the same y-scale.
# Panel order fixed so regular is always on top.
.plot_ieps_rates <- function(ieps_monthly_parquet) {
  if (!file.exists(ieps_monthly_parquet)) {
    message("  IEPS rates plot: parquet not found — skipping")
    return(NULL)
  }

  ieps <- arrow::read_parquet(ieps_monthly_parquet) |>
    dplyr::select(year, month, ieps_magna_cuota, ieps_prem_cuota) |>
    dplyr::mutate(date = lubridate::make_date(as.integer(year), as.integer(month), 1L)) |>
    tidyr::pivot_longer(
      cols      = c(ieps_magna_cuota, ieps_prem_cuota),
      names_to  = "series",
      values_to = "value"
    ) |>
    dplyr::mutate(
      series = factor(
        dplyr::recode(series,
          "ieps_magna_cuota" = "IEPS Magna (Regular) cuota",
          "ieps_prem_cuota"  = "IEPS Premium cuota"
        ),
        levels = c("IEPS Magna (Regular) cuota", "IEPS Premium cuota")
      )
    )

  cols <- c(
    "IEPS Magna (Regular) cuota" = "#2c7bb6",
    "IEPS Premium cuota"         = "#d7191c"
  )

  ggplot2::ggplot(ieps, ggplot2::aes(x = date, y = value, colour = series)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::facet_wrap(~ series, ncol = 1L, scales = "fixed") +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y", name = NULL) +
    ggplot2::scale_y_continuous(
      name   = "Effective cuota (MXN / litre)",
      labels = scales::label_number(accuracy = 0.01)
    ) +
    ggplot2::scale_colour_manual(values = cols, guide = "none") +
    ggplot2::labs(
      title    = "IEPS federal excise tax: effective cuota by grade",
      subtitle = paste0(
        "Monthly average of weekly effective cuota (base rate minus SHCP stimulus). ",
        "Source: DOF weekly IEPS decrees, manually entered from official gazette."
      ),
      caption  = "Drops reflect SHCP stimulus periods (cuota = base rate − stimulus)."
    ) +
    .ieps_ts_theme()
}

# Figure 2 — Bloomberg Gulf Coast spot prices: Regular 87 vs Premium 93,
# both in MXN/l, on the same y-scale. X-axis forced to match the IEPS series
# (Jan 2017 – last IEPS date) even if Bloomberg data ends earlier (Jan 2024).
.plot_bloomberg_prices <- function(bloomberg_parquet,
                                   x_end = as.Date("2026-04-01")) {
  if (is.null(bloomberg_parquet) || !file.exists(bloomberg_parquet)) {
    message("  Bloomberg price plot: parquet not found — skipping")
    return(NULL)
  }

  x_start <- as.Date("2017-01-01")

  bloom <- tryCatch(
    arrow::read_parquet(bloomberg_parquet) |>
      dplyr::select(year, month, regular_87_mxn_l, premium_93_mxn_l) |>
      dplyr::filter(!is.na(regular_87_mxn_l) | !is.na(premium_93_mxn_l)) |>
      dplyr::mutate(date = lubridate::make_date(year, month, 1L)) |>
      dplyr::filter(date >= x_start) |>
      tidyr::pivot_longer(
        cols      = c(regular_87_mxn_l, premium_93_mxn_l),
        names_to  = "series",
        values_to = "value"
      ) |>
      dplyr::mutate(
        series = factor(
          dplyr::recode(series,
            "regular_87_mxn_l"  = "Gulf Coast Regular 87 (MOIGC87P)",
            "premium_93_mxn_l"  = "Gulf Coast Premium 93 (MOIGC93P)"
          ),
          levels = c("Gulf Coast Regular 87 (MOIGC87P)",
                     "Gulf Coast Premium 93 (MOIGC93P)")
        )
      ),
    error = function(e) {
      message("  Bloomberg price plot: error — ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(bloom)) return(NULL)

  cols <- c(
    "Gulf Coast Regular 87 (MOIGC87P)"  = "#2c7bb6",
    "Gulf Coast Premium 93 (MOIGC93P)"  = "#d7191c"
  )

  ggplot2::ggplot(bloom, ggplot2::aes(x = date, y = value, colour = series)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::facet_wrap(~ series, ncol = 1L, scales = "fixed") +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      limits      = c(x_start, x_end),
      name        = NULL
    ) +
    ggplot2::scale_y_continuous(
      name   = "Spot price (MXN / litre)",
      labels = scales::label_number(accuracy = 0.01)
    ) +
    ggplot2::scale_colour_manual(values = cols, guide = "none") +
    ggplot2::labs(
      title    = "Gulf Coast wholesale spot prices by grade (Bloomberg, MXN/litre)",
      subtitle = paste0(
        "Monthly close. Bloomberg assessments discontinued Jan 2024 (series ends there). ",
        "Converted at monthly average FX (Banxico fix rate)."
      ),
      caption  = "Sources: Bloomberg MOIGC87P, MOIGC93P; Banco de México FX fix series."
    ) +
    .ieps_ts_theme()
}

# --------------------------------------------------------------------------
# Regression table — gridExtra::tableGrob (no browser dependency)
# --------------------------------------------------------------------------

.make_regression_table_grob <- function(models) {
  coef_map <- c(
    "log_price_ratio"                  = "log(P_prem / P_mag)",
    "fit_log_price_ratio"              = "log(P_prem / P_mag)",
    "log_price_ratio:income_m_std"     = "log(P/P) x Income (std)",
    "fit_log_price_ratio:income_m_std" = "log(P/P) x Income (std)"
  )

  stars_fn <- function(pv) {
    dplyr::case_when(pv < 0.01 ~ "***", pv < 0.05 ~ "**", pv < 0.1 ~ "*",
                     TRUE ~ "")
  }

  all_raw  <- unique(unlist(lapply(models, function(m) rownames(fixest::coeftable(m)))))
  keep     <- intersect(names(coef_map), all_raw)
  n_models <- length(models)

  rows <- list()
  for (nm in keep) {
    est_row <- coef_map[nm]
    se_row  <- ""
    for (m in models) {
      ct <- fixest::coeftable(m)
      if (nm %in% rownames(ct)) {
        est_row <- c(est_row,
          sprintf("%.4f%s", ct[nm, "Estimate"], stars_fn(ct[nm, "Pr(>|t|)"])))
        se_row  <- c(se_row, sprintf("(%.4f)", ct[nm, "Std. Error"]))
      } else {
        est_row <- c(est_row, "")
        se_row  <- c(se_row,  "")
      }
    }
    rows[[length(rows) + 1]] <- est_row
    rows[[length(rows) + 1]] <- se_row
  }

  nobs_row <- c("Observations", sapply(models, function(m) {
    n <- tryCatch(as.integer(fixest::fitstat(m, "n")[[1]]), error = function(e) NA)
    if (is.na(n)) "" else scales::comma(n)
  }))
  r2_row <- c("R2", sapply(models, function(m) {
    r2 <- tryCatch(fixest::fitstat(m, "r2")[[1]], error = function(e) NA_real_)
    if (is.na(r2)) "" else sprintf("%.3f", r2)
  }))
  fe_mun_row  <- c("Municipality FE", rep("Yes", n_models))
  fe_ym_row   <- c("Year-month FE",   sapply(names(models), function(nm)
                    if (nm == "(1) FE") "Yes" else ""))
  fe_sep_row  <- c("Year + Month FE", sapply(names(models), function(nm)
                    if (nm != "(1) FE") "Yes" else ""))
  ivf_row     <- c("First-stage F", sapply(models, function(m)
                    tryCatch({
                      fs <- fixest::fitstat(m, "ivf")[[1]]
                      if (!is.null(fs$stat)) sprintf("%.1f", fs$stat) else ""
                    }, error = function(e) "")))
  sargan_row  <- c("Sargan p", sapply(models, function(m)
                    tryCatch({
                      ss <- fixest::fitstat(m, "sargan")[[1]]
                      if (!is.null(ss$p)) sprintf("%.3f", ss$p) else ""
                    }, error = function(e) "")))

  rows[[length(rows) + 1]] <- rep("", n_models + 1L)
  rows[[length(rows) + 1]] <- nobs_row
  rows[[length(rows) + 1]] <- r2_row
  rows[[length(rows) + 1]] <- fe_mun_row
  rows[[length(rows) + 1]] <- fe_ym_row
  rows[[length(rows) + 1]] <- fe_sep_row
  rows[[length(rows) + 1]] <- ivf_row
  rows[[length(rows) + 1]] <- sargan_row

  df        <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(df) <- c("", names(models))

  gridExtra::tableGrob(
    df,
    rows  = NULL,
    theme = gridExtra::ttheme_minimal(
      base_size = 9,
      core    = list(
        fg_params = list(hjust = c(0, rep(0.5, n_models)),
                         x     = c(0.02, rep(0.5, n_models))),
        bg_params = list(fill  = rep(c("white", "grey96"), length.out = nrow(df)))
      ),
      colhead = list(
        fg_params = list(fontface = "bold", hjust = 0.5),
        bg_params = list(fill = "grey85")
      )
    )
  )
}

# --------------------------------------------------------------------------
# Build national wholesale price ratio
# --------------------------------------------------------------------------

# Returns national average wholesale premium/regular ratio per year-month,
# averaging across all PEMEX terminals. Using the national average avoids
# terminal-level endogeneity: deviations at a specific terminal could
# correlate with local demand (the simultaneity we are trying to remove).
# Joined to panel on year + month — no municipality mapping needed.
.build_national_wholesale_ratio <- function(terminal_dir) {
  arrow::open_dataset(terminal_dir) |>
    dplyr::filter(!is.na(regular), !is.na(premium), regular > 0, premium > 0) |>
    dplyr::mutate(month = lubridate::month(date)) |>
    dplyr::group_by(year, month) |>
    dplyr::summarise(
      mean_premium = mean(premium, na.rm = TRUE),
      mean_regular = mean(regular, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      log_national_wholesale_ratio = log(mean_premium / mean_regular),
      year  = as.integer(year),
      month = as.integer(month)
    ) |>
    dplyr::select(year, month, log_national_wholesale_ratio)
}

# --------------------------------------------------------------------------
# Assemble regression panel
# --------------------------------------------------------------------------

.build_regression_panel <- function(base_parquet, ieps_monthly_parquet,
                                    income_parquet, terminal_dir,
                                    restricted_states = NULL) {
  panel <- arrow::read_parquet(base_parquet) |>
    dplyr::filter(
      !is.na(premium_share),
      premium_share > 0, premium_share < 1,
      !is.na(premium_to_regular_price_ratio),
      premium_to_regular_price_ratio > 0
    ) |>
    dplyr::mutate(
      logit_share     = log(premium_share / (1 - premium_share)),
      log_price_ratio = log(premium_to_regular_price_ratio),
      year_month      = paste0(year, "-", sprintf("%02d", month))
    )

  # State-level sample restriction (drop states with large informal markets)
  if (!is.null(restricted_states)) {
    panel <- dplyr::filter(panel, !substr(CVEGEO, 1L, 2L) %in% restricted_states)
  }

  # National wholesale ratio (joined on year + month — no mun mapping needed)
  national_wholesale <- .build_national_wholesale_ratio(terminal_dir)
  panel <- dplyr::left_join(panel, national_wholesale, by = c("year", "month"))

  # IEPS prem/magna ratio — only years with actual decree data (2022+)
  if (file.exists(ieps_monthly_parquet)) {
    ieps <- arrow::read_parquet(ieps_monthly_parquet) |>
      dplyr::filter(year >= 2022) |>
      dplyr::select(year, month, ieps_magna_cuota, ieps_prem_cuota) |>
      dplyr::mutate(log_ieps_ratio = log(ieps_prem_cuota / ieps_magna_cuota))
    panel <- dplyr::left_join(panel, ieps, by = c("year", "month"))
  } else {
    panel <- dplyr::mutate(panel,
      ieps_magna_cuota = NA_real_,
      ieps_prem_cuota  = NA_real_,
      log_ieps_ratio   = NA_real_
    )
  }

  # Bloomberg Gulf Coast wholesale spread — joined on year + month
  # log_bloomberg_ratio = log(premium_93_mxn_l / regular_87_mxn_l)
  # Available May 2016 – Jan 2024 (Bloomberg discontinued thereafter)
  bloomberg_parquet_path <- "data/processed/bloomberg/gasoline_bloomberg.parquet"
  if (file.exists(bloomberg_parquet_path)) {
    bloom <- arrow::read_parquet(bloomberg_parquet_path) |>
      dplyr::filter(!is.na(regular_87_mxn_l), !is.na(premium_93_mxn_l),
                    regular_87_mxn_l > 0, premium_93_mxn_l > 0) |>
      dplyr::select(year, month, regular_87_mxn_l, premium_93_mxn_l) |>
      dplyr::mutate(log_bloomberg_ratio = log(premium_93_mxn_l / regular_87_mxn_l))
    panel <- dplyr::left_join(panel, bloom, by = c("year", "month"))
  } else {
    panel <- dplyr::mutate(panel, log_bloomberg_ratio = NA_real_)
  }

  if (file.exists(income_parquet)) {
    # Standardise on unique municipalities so mean/SD are not distorted by
    # unbalanced panel observation counts.
    income <- arrow::read_parquet(income_parquet) |>
      dplyr::select(CVEGEO, income_car_owners, income_unconditional) |>
      dplyr::mutate(
        income_m     = income_car_owners,
        income_m_std = (income_m - mean(income_m, na.rm = TRUE)) /
                        stats::sd(income_m, na.rm = TRUE)
      )
    stopifnot(abs(mean(income$income_m_std, na.rm = TRUE)) < 1e-9)
    panel <- dplyr::left_join(panel, income, by = "CVEGEO")
  } else {
    panel <- dplyr::mutate(panel,
      income_car_owners    = NA_real_,
      income_unconditional = NA_real_,
      income_m             = NA_real_,
      income_m_std         = NA_real_
    )
  }

  panel
}

# --------------------------------------------------------------------------
# Run the four regressions
# --------------------------------------------------------------------------

.run_regressions <- function(panel) {
  models <- list()

  # --- Spec 1: OLS baseline (year-month FE — strictest time control) ---
  message("  Spec 1: Pooled FE (OLS)")
  models[["(1) FE"]] <- fixest::feols(
    logit_share ~ log_price_ratio | CVEGEO + year_month,
    data = panel, cluster = ~CVEGEO
  )

  # --- Spec 2: IV — national wholesale ratio ---
  # National instrument is collinear with year-month FE → must use Variant B.
  message("  Spec 2: IV — national wholesale ratio")
  panel2 <- dplyr::filter(panel, !is.na(log_national_wholesale_ratio))
  models[["(2) IV-Wholesale"]] <- fixest::feols(
    logit_share ~ 1 | CVEGEO + year + month |
      log_price_ratio ~ log_national_wholesale_ratio,
    data = panel2, cluster = ~CVEGEO
  )

  # --- Spec 3: IV — IEPS ratio only (2022+ data, year + month FE) ---
  panel3 <- dplyr::filter(panel, !is.na(log_ieps_ratio))
  if (nrow(panel3) >= 100L) {
    message(sprintf("  Spec 3: IV — IEPS ratio  (%d obs)", nrow(panel3)))
    models[["(3) IV-IEPS"]] <- fixest::feols(
      logit_share ~ 1 | CVEGEO + year + month |
        log_price_ratio ~ log_ieps_ratio,
      data = panel3, cluster = ~CVEGEO
    )
  } else {
    message(sprintf("  Spec 3: SKIPPED — only %d obs", nrow(panel3)))
  }

  # --- Spec 4: IV — both instruments (year + month FE, Sargan test) ---
  panel4 <- dplyr::filter(
    panel, !is.na(log_national_wholesale_ratio), !is.na(log_ieps_ratio)
  )
  if (nrow(panel4) >= 100L) {
    message(sprintf("  Spec 4: IV — both  (%d obs)", nrow(panel4)))
    m4 <- fixest::feols(
      logit_share ~ 1 | CVEGEO + year + month |
        log_price_ratio ~ log_national_wholesale_ratio + log_ieps_ratio,
      data = panel4, cluster = ~CVEGEO
    )
    models[["(4) IV-Both"]] <- m4
    sargan <- tryCatch(fixest::fitstat(m4, "sargan"), error = function(e) NULL)
    if (!is.null(sargan)) {
      message(sprintf(
        "  Spec 4 Sargan-Hansen: stat=%.4f  p=%.4f  (df=%d)",
        sargan[[1]]$stat, sargan[[1]]$p, sargan[[1]]$df
      ))
    }
  } else {
    message(sprintf("  Spec 4: SKIPPED — only %d obs", nrow(panel4)))
  }

  # --- Spec 5: IV x income (national wholesale, year + month FE) ---
  n_income <- sum(!is.na(panel$income_m_std))
  if (n_income >= 100L) {
    message(sprintf("  Spec 5: IV x income  (%d obs)", n_income))
    panel5 <- dplyr::filter(
      panel, !is.na(log_national_wholesale_ratio), !is.na(income_m_std)
    )
    models[["(5) IV×Income"]] <- fixest::feols(
      logit_share ~ 1 | CVEGEO + year + month |
        log_price_ratio + log_price_ratio:income_m_std ~
        log_national_wholesale_ratio +
        log_national_wholesale_ratio:income_m_std,
      data = panel5, cluster = ~CVEGEO
    )
  } else {
    message(sprintf("  Spec 5: SKIPPED (%d obs with income)", n_income))
  }

  models
}

# --------------------------------------------------------------------------
# Bloomberg IV specifications (separate from the main 5 specs)
# Instrument: log(Gulf Coast premium 93 / regular 87) — wholesale spread
# --------------------------------------------------------------------------

.run_bloomberg_specs <- function(panel) {
  models <- list()

  panel_b <- dplyr::filter(panel, !is.na(log_bloomberg_ratio))
  if (nrow(panel_b) < 100L) {
    message(sprintf("  Bloomberg specs: only %d obs — skipping", nrow(panel_b)))
    return(models)
  }

  # Spec A: IV — Bloomberg wholesale spread only
  message(sprintf("  Bloomberg Spec A: IV-Bloomberg  (%d obs)", nrow(panel_b)))
  models[["(A) IV-Bloomberg"]] <- fixest::feols(
    logit_share ~ 1 | CVEGEO + year + month |
      log_price_ratio ~ log_bloomberg_ratio,
    data = panel_b, cluster = ~CVEGEO
  )

  # Spec B: IV — Bloomberg + PEMEX wholesale (over-identified, Sargan test)
  panel_b2 <- dplyr::filter(panel_b, !is.na(log_national_wholesale_ratio))
  if (nrow(panel_b2) >= 100L) {
    message(sprintf("  Bloomberg Spec B: IV-Bloomberg+Wholesale  (%d obs)", nrow(panel_b2)))
    m_b2 <- fixest::feols(
      logit_share ~ 1 | CVEGEO + year + month |
        log_price_ratio ~ log_bloomberg_ratio + log_national_wholesale_ratio,
      data = panel_b2, cluster = ~CVEGEO
    )
    models[["(B) IV-Bloom+Wholesale"]] <- m_b2
    sargan <- tryCatch(fixest::fitstat(m_b2, "sargan"), error = function(e) NULL)
    if (!is.null(sargan)) {
      message(sprintf(
        "  Spec B Sargan: stat=%.4f  p=%.4f  (df=%d)",
        sargan[[1]]$stat, sargan[[1]]$p, sargan[[1]]$df
      ))
    }
  }

  # Spec C: IV — Bloomberg x income interaction (incidence with Bloomberg IV)
  panel_b3 <- dplyr::filter(panel_b, !is.na(income_m_std),
                             !is.na(log_national_wholesale_ratio))
  if (nrow(panel_b3) >= 100L) {
    message(sprintf("  Bloomberg Spec C: IV-Bloomberg×Income  (%d obs)", nrow(panel_b3)))
    models[["(C) IV-Bloom×Income"]] <- fixest::feols(
      logit_share ~ 1 | CVEGEO + year + month |
        log_price_ratio + log_price_ratio:income_m_std ~
        log_bloomberg_ratio + log_bloomberg_ratio:income_m_std,
      data = panel_b3, cluster = ~CVEGEO
    )
  }

  models
}

# --------------------------------------------------------------------------
# Save all outputs: PDF (methodology + table + graph) + LaTeX table
# --------------------------------------------------------------------------

.save_outputs <- function(models, panel, out_dir, sample_label = "Full sample",
                         ieps_monthly_parquet = NULL,
                         bloomberg_parquet    = NULL) {

  # --- 1. Build pages ---
  n_obs  <- nrow(panel)
  n_mun  <- dplyr::n_distinct(panel$CVEGEO)
  yr_min <- min(panel$year, na.rm = TRUE)
  yr_max <- max(panel$year, na.rm = TRUE)

  page_meth1 <- .make_methodology_page_1(n_obs, n_mun, yr_min, yr_max, sample_label)
  page_meth2 <- .make_methodology_page_2()
  table_grob <- .make_regression_table_grob(models)

  # Income interaction graph (only if Spec 5 estimated)
  reg5       <- models[["(5) IV×Income"]]
  page_graph <- if (!is.null(reg5)) {
    .plot_income_interaction(reg5, income_data = panel)
  } else NULL

  # Bloomberg IV specs — separate table
  bloomberg_models <- .run_bloomberg_specs(panel)
  bloomberg_grob   <- if (length(bloomberg_models) > 0L) {
    .make_regression_table_grob(bloomberg_models)
  } else NULL

  # IEPS time series graphs (both grades) + Bloomberg reference prices
  ieps_rates_graph <- if (!is.null(ieps_monthly_parquet)) {
    .plot_ieps_rates(ieps_monthly_parquet)
  } else NULL

  # X-axis end = last month in IEPS series (so Bloomberg aligns with IEPS figure)
  ieps_x_end <- if (!is.null(ieps_monthly_parquet) && file.exists(ieps_monthly_parquet)) {
    tryCatch({
      ieps_tmp <- arrow::read_parquet(ieps_monthly_parquet)
      lubridate::make_date(
        as.integer(max(ieps_tmp$year)),
        as.integer(max(ieps_tmp$month[ieps_tmp$year == max(ieps_tmp$year)])),
        1L
      )
    }, error = function(e) as.Date("2026-04-01"))
  } else as.Date("2026-04-01")

  bloomberg_graph <- .plot_bloomberg_prices(bloomberg_parquet, x_end = ieps_x_end)

  # --- 2. Combined PDF ---
  pdf_name <- if (grepl("Restricted", sample_label)) {
    "results_restricted_sample.pdf"
  } else {
    "results_full_sample.pdf"
  }
  pdf_path <- file.path(out_dir, pdf_name)
  grDevices::pdf(pdf_path, width = 11, height = 8.5, onefile = TRUE)
  print(page_meth1)
  print(page_meth2)
  grid::grid.newpage()
  grid::grid.draw(table_grob)
  if (!is.null(page_graph))       print(page_graph)
  if (!is.null(bloomberg_grob)) {
    grid::grid.newpage()
    grid::grid.draw(bloomberg_grob)
  }
  if (!is.null(ieps_rates_graph)) print(ieps_rates_graph)
  if (!is.null(bloomberg_graph))  print(bloomberg_graph)
  grDevices::dev.off()
  message(sprintf("  PDF saved: %s", pdf_path))

  # --- 3. LaTeX regression tables ---
  tex_path <- file.path(out_dir, "regression_table.tex")
  sink(tex_path)
  fixest::etable(models, se.below = TRUE, digits = 4,
                 fitstat = c("n", "r2", "ivf", "sargan"), tex = TRUE)
  sink()
  message(sprintf("  LaTeX:     %s", tex_path))

  if (length(bloomberg_models) > 0L) {
    tex_bloom <- file.path(out_dir, "regression_table_bloomberg.tex")
    sink(tex_bloom)
    fixest::etable(bloomberg_models, se.below = TRUE, digits = 4,
                   fitstat = c("n", "r2", "ivf", "sargan"), tex = TRUE)
    sink()
    message(sprintf("  LaTeX Bloomberg: %s", tex_bloom))
  }

  # --- 4. Regression panel parquet ---
  panel_path <- file.path(out_dir, "regression_panel.parquet")
  arrow::write_parquet(panel, panel_path, compression = "zstd")
  message(sprintf("  Panel:     %s", panel_path))

  pdf_path
}

# --------------------------------------------------------------------------
# Main wrapper
# --------------------------------------------------------------------------

run_shaun_pooled_regression <- function(
  base_parquet         = "data/analysis/mun_month_prices/mun_month_prices_with_poverty.parquet",
  ieps_monthly_parquet = "data/processed/ieps/ieps_monthly.parquet",
  income_parquet       = "data/processed/inegi_vehiculos/municipal_income_car_owners.parquet",
  terminal_dir         = "data/processed/terminal",
  bloomberg_parquet    = "data/processed/bloomberg/gasoline_bloomberg.parquet",
  out_dir              = "outputs/shaun/pooled_regression",
  restricted_states    = NULL
) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  sample_label <- if (is.null(restricted_states)) {
    "Full sample"
  } else {
    paste0("Restricted (excl. states: ", paste(restricted_states, collapse = ", "), ")")
  }
  message(sprintf("=== Sample: %s ===", sample_label))

  message("=== Building regression panel ===")
  panel <- .build_regression_panel(
    base_parquet, ieps_monthly_parquet, income_parquet, terminal_dir,
    restricted_states = restricted_states
  )
  message(sprintf(
    "  %d obs | %d municipalities | %d year-months",
    nrow(panel), dplyr::n_distinct(panel$CVEGEO),
    dplyr::n_distinct(panel$year_month)
  ))

  message("=== Running regressions ===")
  models <- .run_regressions(panel)

  message(sprintf("=== %d model(s) — saving outputs ===", length(models)))
  .save_outputs(models, panel, out_dir, sample_label,
                ieps_monthly_parquet = ieps_monthly_parquet,
                bloomberg_parquet    = bloomberg_parquet)
}
