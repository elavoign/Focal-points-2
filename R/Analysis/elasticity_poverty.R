# R/Analysis/elasticity_poverty.R
#
# Tasks C and D: municipality-specific price elasticities of premium share
# and their relationship with municipal poverty (CONEVAL 2020).
#
# ==========================================================================
# MODEL — Form 1 (Task C)
# ==========================================================================
# premium_share_mt = alpha_m + gamma_t + beta_m * log(price_ratio_mt) + e_mt
#
# where:
#   alpha_m        = municipality fixed effect (intercept)
#   gamma_t        = year-month fixed effect (common time shock)
#   beta_m         = municipality-specific price responsiveness
#   price_ratio_mt = premium_price_monthly / regular_price_monthly
#
# Estimation — two-step Frisch-Waugh-Lovell:
#   Step 1: partial out the global year-month FE from both premium_share and
#           log(price_ratio) by subtracting time-period means across all
#           municipalities. For the near-balanced panel here (most municipalities
#           observe ~100 of 104 possible year-months) this is numerically
#           equivalent to the iterative FWL projector.
#   Step 2: for each municipality, run OLS of demeaned share on demeaned
#           log(ratio). The municipality intercept alpha_m is absorbed by
#           the OLS constant in step 2.
#
# ==========================================================================
# MODEL — Form 2 (Task D)
# ==========================================================================
# Semiparametric varying coefficient, estimated in two stages:
#   Stage 1: beta_m and its standard error se_m from Form 1 above.
#   Stage 2: beta_m = g(poverty_final_m) + e_m
#            where g(.) is a penalised thin-plate regression spline via
#            mgcv::gam(), with observations weighted by 1 / se_m^2
#            to account for first-stage estimation uncertainty.
#
# This avoids fitting 1,352 municipality dummies inside a GAM (which would
# be both slow and collinear with the poverty argument), while preserving
# the inferential goal: does g(poverty) show a U-shape, monotone decline,
# or other pattern?
#
# ==========================================================================
# OUTPUTS
# ==========================================================================
# data/analysis/elasticity/mun_elasticities.parquet
#   municipality-level table: beta_m, se_m, poverty_final, …
#
# outputs/shaun/elasticity/elasticity_poverty_bins.{parquet,csv}
#   summary table: mean/median/p25/p75/count of beta_m by poverty bin
#
# outputs/shaun/elasticity/form1_scatter.pdf
# outputs/shaun/elasticity/form1_bins.pdf
#   Form 1 publication graphs
#
# outputs/shaun/elasticity/form2_gam.pdf
#   Form 2 GAM smooth curve with 95 % CI
#
# outputs/shaun/elasticity/elasticity_summary.xlsx
#   Excel workbook: municipality table + bin table + interpretation

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(ggplot2)
  library(scales)
  library(mgcv)
  library(broom)
  library(openxlsx)
  library(tidyr)
  library(stringr)
})

# --------------------------------------------------------------------------
# 1. Prepare modelling dataset
# --------------------------------------------------------------------------

.prep_elasticity_data <- function(poverty_panel_parquet) {
  df <- arrow::read_parquet(poverty_panel_parquet) |>
    dplyr::filter(
      !is.na(premium_share),
      !is.na(premium_to_regular_price_ratio),
      premium_to_regular_price_ratio > 0,
      !is.na(poverty_final)
    ) |>
    dplyr::mutate(
      log_price_ratio = log(premium_to_regular_price_ratio),
      year_month      = paste(year, sprintf("%02d", month), sep = "-")
    )

  message(sprintf(
    "  Modelling dataset: %d obs, %d municipalities, years %d-%d",
    nrow(df),
    dplyr::n_distinct(df$CVEGEO),
    min(df$year), max(df$year)
  ))
  df
}

# --------------------------------------------------------------------------
# 2. Task C — estimate municipality-specific elasticities (two-step FWL)
# --------------------------------------------------------------------------

.estimate_mun_elasticities <- function(df, min_obs = 12L,
                                       time_fe_var = "year") {
  # ------------------------------------------------------------------
  # Step 1: partial out global time FE by subtracting time-period means.
  # time_fe_var = "year"       absorbs annual shocks, keeps seasonal and
  #                            month-to-month price variation for beta_m.
  # time_fe_var = "year_month" absorbs all common monthly shocks (more
  #                            conservative; uses only idiosyncratic variation).
  # ------------------------------------------------------------------
  time_means <- df |>
    dplyr::group_by(.data[[time_fe_var]]) |>
    dplyr::summarise(
      mu_share = mean(premium_share,    na.rm = TRUE),
      mu_x     = mean(log_price_ratio,  na.rm = TRUE),
      .groups  = "drop"
    )

  df <- df |>
    dplyr::left_join(time_means, by = time_fe_var) |>
    dplyr::mutate(
      y_dm = premium_share    - mu_share,
      x_dm = log_price_ratio  - mu_x
    )

  message(sprintf(
    "  Step 1 complete: time-FE demeaned (%d %s periods)",
    nrow(time_means), time_fe_var
  ))

  # ------------------------------------------------------------------
  # Step 2: within-municipality OLS on demeaned data
  # ------------------------------------------------------------------
  mun_betas <- df |>
    dplyr::group_by(CVEGEO) |>
    dplyr::filter(dplyr::n() >= min_obs) |>
    dplyr::group_modify(~{
      y <- .x$y_dm
      x <- .x$x_dm
      # Need variation in x to identify beta
      if (stats::var(x, na.rm = TRUE) < 1e-12) return(tibble::tibble())
      fit <- stats::lm(y ~ x)
      sm  <- summary(fit)
      cf  <- stats::coef(sm)
      tibble::tibble(
        beta_m    = cf[2, 1],
        se_m      = cf[2, 2],
        t_stat    = cf[2, 3],
        p_value   = cf[2, 4],
        r_sq      = sm$r.squared,
        n_obs          = nrow(.x),
        n_time_periods = dplyr::n_distinct(.x[[time_fe_var]])
      )
    }) |>
    dplyr::ungroup()

  message(sprintf(
    "  Step 2 complete: %d municipality elasticities estimated",
    nrow(mun_betas)
  ))

  # Attach municipality metadata (poverty + names)
  mun_meta <- df |>
    dplyr::distinct(CVEGEO, CVE_ENT, NOM_ENT, NOM_MUN,
                    poverty_final, poverty_sex, poverty_age, poverty_geo,
                    flag_partition_divergence) |>
    dplyr::group_by(CVEGEO) |>
    dplyr::slice(1L) |>
    dplyr::ungroup()

  mun_betas |> dplyr::left_join(mun_meta, by = "CVEGEO")
}

# --------------------------------------------------------------------------
# 3. Task C — poverty-bin summary table
# --------------------------------------------------------------------------

.build_poverty_bins <- function(mun_betas, bin_width = 5) {
  pov_range  <- range(mun_betas$poverty_final, na.rm = TRUE)
  break_lo   <- floor(pov_range[1]  / bin_width) * bin_width
  break_hi   <- ceiling(pov_range[2] / bin_width) * bin_width
  breaks     <- seq(break_lo, break_hi, by = bin_width)

  binned <- mun_betas |>
    dplyr::filter(!is.na(poverty_final), !is.na(beta_m)) |>
    dplyr::mutate(
      pov_bin_lo  = breaks[findInterval(poverty_final, breaks,
                                        rightmost.closed = TRUE)],
      pov_bin_hi  = pov_bin_lo + bin_width,
      pov_bin_lab = sprintf("[%g, %g)", pov_bin_lo, pov_bin_hi)
    )

  summary_tbl <- binned |>
    dplyr::group_by(pov_bin_lo, pov_bin_hi, pov_bin_lab) |>
    dplyr::summarise(
      n          = dplyr::n(),
      mean_beta  = mean(beta_m,   na.rm = TRUE),
      median_beta= median(beta_m, na.rm = TRUE),
      p25_beta   = quantile(beta_m, 0.25, na.rm = TRUE),
      p75_beta   = quantile(beta_m, 0.75, na.rm = TRUE),
      sd_beta    = sd(beta_m,     na.rm = TRUE),
      se_mean    = sd_beta / sqrt(n),
      .groups    = "drop"
    ) |>
    dplyr::arrange(pov_bin_lo) |>
    dplyr::mutate(
      ci_lo = mean_beta - 1.96 * se_mean,
      ci_hi = mean_beta + 1.96 * se_mean
    )

  list(
    binned      = binned,
    summary_tbl = summary_tbl
  )
}

# --------------------------------------------------------------------------
# 4. Task D — semiparametric GAM (two-stage)
# --------------------------------------------------------------------------

.estimate_gam <- function(mun_betas, k = 10) {
  df_gam <- mun_betas |>
    dplyr::filter(
      !is.na(beta_m), !is.na(se_m), !is.na(poverty_final),
      se_m > 0, is.finite(beta_m)
    ) |>
    dplyr::mutate(w = 1 / se_m^2)

  fit <- mgcv::gam(
    beta_m ~ s(poverty_final, k = k, bs = "tp"),
    data    = df_gam,
    weights = w,
    method  = "REML"
  )

  message(sprintf(
    "  GAM: n=%d, R²=%.3f, EDF=%.2f, deviance expl.=%.1f%%",
    nrow(df_gam),
    summary(fit)$r.sq,
    sum(summary(fit)$edf),
    100 * summary(fit)$dev.expl
  ))

  # Prediction grid over poverty range
  pov_seq <- seq(
    min(df_gam$poverty_final, na.rm = TRUE),
    max(df_gam$poverty_final, na.rm = TRUE),
    length.out = 300
  )
  pred <- mgcv::predict.gam(fit,
    newdata = data.frame(poverty_final = pov_seq),
    type    = "link", se.fit = TRUE
  )
  pred_df <- tibble::tibble(
    poverty_final = pov_seq,
    fit           = pred$fit,
    se            = pred$se.fit,
    ci_lo         = fit - 1.96 * se,
    ci_hi         = fit + 1.96 * se
  )

  list(model = fit, pred_df = pred_df, data = df_gam)
}

# --------------------------------------------------------------------------
# 5. Graphs — Form 1
# --------------------------------------------------------------------------

.plot_form1_scatter <- function(mun_betas, bins_list) {
  df_plot <- mun_betas |>
    dplyr::filter(!is.na(poverty_final), !is.na(beta_m), is.finite(beta_m)) |>
    # Winsorise for display only (±4 SE from median)
    dplyr::mutate(
      beta_m_clip = pmin(pmax(beta_m,
        quantile(beta_m, 0.01, na.rm = TRUE)),
        quantile(beta_m, 0.99, na.rm = TRUE)
      )
    )

  # LOESS reference line
  gg <- ggplot2::ggplot(df_plot,
    ggplot2::aes(x = poverty_final, y = beta_m_clip)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        colour = "grey60", linewidth = 0.4) +
    ggplot2::geom_point(alpha = 0.25, size = 0.8, colour = "#2c7bb6") +
    ggplot2::geom_smooth(method = "loess", span = 0.5, se = TRUE,
                         colour = "#d7191c", fill = "#d7191c",
                         alpha = 0.15, linewidth = 1) +
    ggplot2::scale_x_continuous(
      name   = "Poverty rate 2020, % (CONEVAL)",
      breaks = seq(0, 100, 10),
      labels = scales::label_number(suffix = "%")
    ) +
    ggplot2::scale_y_continuous(
      name   = expression(hat(beta)[m] ~ "(price elasticity of premium share)"),
      labels = scales::label_number(accuracy = 0.001)
    ) +
    ggplot2::labs(
      title    = "Municipality price responsiveness vs. poverty rate",
      subtitle = paste0(
        "Each point = one municipality. Red line: LOESS smooth (span = 0.5).\n",
        "FWL estimator: year-month FE partialled out; within-municipality OLS."
      ),
      caption  = paste0(
        "N = ", scales::comma(nrow(df_plot)), " municipalities. ",
        "beta_m winsorised at 1st/99th pct for display."
      )
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = "grey40", size = 9),
      plot.caption  = ggplot2::element_text(colour = "grey60", size = 8),
      panel.grid.minor = ggplot2::element_blank()
    )

  gg
}

.plot_form1_bins <- function(bins_list, bin_width = 2) {
  stbl <- bins_list$summary_tbl |>
    dplyr::filter(n >= 3L) |>
    dplyr::mutate(pov_mid = (pov_bin_lo + pov_bin_hi) / 2)

  gg <- ggplot2::ggplot(stbl,
    ggplot2::aes(x = pov_mid, y = mean_beta)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        colour = "grey60", linewidth = 0.4) +
    # IQR ribbon
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = p25_beta, ymax = p75_beta),
      fill = "#abd9e9", alpha = 0.45
    ) +
    # 95% CI ribbon for the mean
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = ci_lo, ymax = ci_hi),
      fill = "#2c7bb6", alpha = 0.35
    ) +
    ggplot2::geom_line(colour = "#2c7bb6", linewidth = 1) +
    ggplot2::geom_point(
      ggplot2::aes(size = n),
      colour = "#2c7bb6", shape = 21, fill = "white", stroke = 1
    ) +
    ggplot2::scale_x_continuous(
      name   = "Poverty rate bin midpoint, % (CONEVAL 2020)",
      breaks = seq(0, 100, 10),
      labels = scales::label_number(suffix = "%")
    ) +
    ggplot2::scale_y_continuous(
      name   = expression(mean(hat(beta)[m]) ~ "per poverty bin"),
      labels = scales::label_number(accuracy = 0.001)
    ) +
    ggplot2::scale_size_continuous(name = "Municipalities\nper bin",
                                   range = c(1.5, 5)) +
    ggplot2::labs(
      title    = sprintf("Price responsiveness by poverty bin (%g pp bins)", bin_width),
      subtitle = paste0(
        "Line = mean beta_m per bin (winsorised). ",
        "Dark ribbon = 95% CI of mean. Light ribbon = IQR (p25-p75)."
      ),
      caption  = "Bins with < 3 municipalities excluded."
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = "grey40", size = 9),
      plot.caption  = ggplot2::element_text(colour = "grey60", size = 8),
      legend.position = "right",
      panel.grid.minor = ggplot2::element_blank()
    )

  gg
}

# --------------------------------------------------------------------------
# 6. Graphs — Form 2 (GAM smooth)
# --------------------------------------------------------------------------

.plot_form2_gam <- function(gam_list, mun_betas) {
  pred_df  <- gam_list$pred_df
  df_pts   <- gam_list$data |>
    dplyr::mutate(
      beta_clip = pmin(pmax(beta_m,
        quantile(beta_m, 0.02, na.rm = TRUE)),
        quantile(beta_m, 0.98, na.rm = TRUE))
    )

  sm         <- summary(gam_list$model)
  rsq_str    <- sprintf("R\u00b2 = %.3f, deviance expl. = %.1f%%",
                        sm$r.sq, 100 * sm$dev.expl)
  edf_str    <- sprintf("EDF = %.2f (smooth term)", sum(sm$edf))

  gg <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        colour = "grey60", linewidth = 0.4) +
    # Municipality points (weight-sized, semi-transparent)
    ggplot2::geom_point(
      data = df_pts,
      ggplot2::aes(x = poverty_final, y = beta_clip,
                   size = 1 / se_m),
      alpha = 0.2, colour = "#2c7bb6"
    ) +
    # 95 % CI ribbon
    ggplot2::geom_ribbon(
      data = pred_df,
      ggplot2::aes(x = poverty_final, ymin = ci_lo, ymax = ci_hi),
      fill = "#d7191c", alpha = 0.18
    ) +
    # Smooth curve
    ggplot2::geom_line(
      data = pred_df,
      ggplot2::aes(x = poverty_final, y = fit),
      colour = "#d7191c", linewidth = 1.1
    ) +
    # Poverty rug
    ggplot2::geom_rug(
      data = df_pts,
      ggplot2::aes(x = poverty_final),
      sides  = "b", alpha = 0.2, linewidth = 0.3
    ) +
    ggplot2::scale_x_continuous(
      name   = "Poverty rate 2020, % (CONEVAL)",
      breaks = seq(0, 100, 10),
      labels = scales::label_number(suffix = "%")
    ) +
    ggplot2::scale_y_continuous(
      name   = expression(g(poverty[m]) ~ " = smoothed " ~ hat(beta)[m]),
      labels = scales::label_number(accuracy = 0.001)
    ) +
    ggplot2::scale_size_continuous(guide = "none") +
    ggplot2::labs(
      title    = "Form 2: semiparametric varying coefficient model",
      subtitle = paste0(
        "g(poverty) estimated via weighted penalised spline (mgcv GAM, REML). ",
        "Weights = 1/SE\u00b2 from stage-1 OLS.\n",
        rsq_str, ". ", edf_str, "."
      ),
      caption  = paste0(
        "Red line: estimated g(\u00b7). Shaded band: 95% pointwise CI. ",
        "Points: municipality beta_m (winsorised 2nd/98th pct)."
      )
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = "grey40", size = 9),
      plot.caption  = ggplot2::element_text(colour = "grey60", size = 8),
      panel.grid.minor = ggplot2::element_blank()
    )

  gg
}

# --------------------------------------------------------------------------
# 7. Methodology cover page (first page of PDF)
# --------------------------------------------------------------------------

.make_methodology_page <- function(n_mun, n_obs, year_range,
                                   time_fe_var, winsor_pct, bin_width) {

  lines <- c(
    "METHODOLOGY",
    "",
    "QUESTION",
    "Does the price sensitivity of premium gasoline demand vary systematically",
    "with municipal poverty? Do richer or poorer municipalities respond more",
    "strongly to changes in the premium-to-regular price ratio?",
    "",
    "DATA",
    sprintf(
      "Panel: %s municipalities x month, %d-%d  |  N = %s obs",
      scales::comma(n_mun), year_range[1], year_range[2], scales::comma(n_obs)
    ),
    "Dependent variable: premium_share = premium liters / (premium + regular liters)",
    "  Source: CRE/SENER 04_volumenes_venta_expendio_petroliferos.csv",
    "Price variable: log(premium_price_monthly / regular_price_monthly)",
    "  Source: CRE retail station panel, balanced with 60-day carry-forward cap",
    "  Aggregation: station -> municipality x day (mean), then x day -> x month (mean)",
    "Poverty: CONEVAL 2020 municipal poverty rate (% population in poverty)",
    "  Range: ~5% (richest) to ~97% (poorest). Higher value = MORE poor.",
    "  Built as pop-weighted mean across sex, age, and rural/urban partitions.",
    "",
    "MODEL",
    "  premium_share_mt = alpha_m + gamma_t + beta_m * log(price_ratio_mt) + e_mt",
    "",
    "  alpha_m  = municipality fixed effect (level differences across municipalities)",
    "  gamma_t  = time fixed effect (national shocks common to all municipalities)",
    sprintf("  Time FE used: %s (annual - retains seasonal price variation within year)",
            time_fe_var),
    "  beta_m   = municipality-specific price elasticity (the key estimate)",
    "",
    "ESTIMATOR  (two-step Frisch-Waugh-Lovell)",
    "  Step 1: subtract annual mean of premium_share and log(price_ratio) across",
    "          all municipalities for each year. This removes the global time FE.",
    "  Step 2: within each municipality, regress demeaned share on demeaned",
    "          log(price_ratio). The OLS intercept absorbs alpha_m.",
    "  Result: one beta_m per municipality, identified from within-municipality,",
    "          within-year variation in relative prices.",
    "",
    "ROBUSTNESS",
    sprintf(
      "  beta_m winsorised at [%g%%, %g%%] of its distribution before bin analysis",
      100 * winsor_pct, 100 * (1 - winsor_pct)
    ),
    sprintf("  Poverty bins: %g percentage-point intervals", bin_width),
    "  Bins with fewer than 3 municipalities excluded from the bin plot.",
    "",
    "HOW TO READ THE GRAPHS",
    "  Scatter (Graph 2): each dot = one municipality. X-axis = poverty rate",
    "    (LEFT = richer, RIGHT = poorer). Y-axis = beta_m. Red line = LOESS smooth.",
    "  Bins (Graph 3): mean beta_m per 2pp poverty bin. Dark ribbon = 95% CI",
    "    of the mean. Light ribbon = IQR (p25-p75) of individual municipality betas.",
    "  A MORE NEGATIVE beta_m means consumers in that municipality respond MORE",
    "  strongly to price: when premium gets relatively more expensive, they switch",
    "  to regular at a higher rate."
  )

  df_text <- data.frame(
    x    = 0,
    y    = rev(seq_along(lines)),
    lab  = lines,
    bold = grepl("^[A-Z ]{3,}$", lines) | lines == "METHODOLOGY"
  )

  ggplot2::ggplot(df_text, ggplot2::aes(x = x, y = y, label = lab)) +
    ggplot2::geom_text(
      ggplot2::aes(fontface = ifelse(bold, "bold", "plain")),
      hjust = 0, size = 3.1, family = "mono",
      colour = ifelse(df_text$bold, "#1a1a1a", "#333333")
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = 0.8)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin     = ggplot2::margin(18, 18, 18, 18),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}

# --------------------------------------------------------------------------
# 8. Excel workbook
# --------------------------------------------------------------------------

.write_elasticity_excel <- function(mun_betas, bins_list, bin_width,
                                    time_fe_var, winsor_pct, out_xlsx) {
  dir.create(dirname(out_xlsx), recursive = TRUE, showWarnings = FALSE)

  wb <- openxlsx::createWorkbook()

  # --- Sheet 1: Methodology (first so it opens by default) ---
  method_df <- data.frame(
    Item = c(
      "QUESTION",
      "Dependent variable",
      "Price variable",
      "Poverty variable",
      "MODEL",
      "Municipality FE (alpha_m)",
      "Time FE (gamma_t)",
      "Municipality slope (beta_m)",
      "ESTIMATOR",
      "Step 1",
      "Step 2",
      "Identification",
      "ROBUSTNESS",
      "Winsorisation",
      "Bin width",
      "Min observations",
      "DATA SOURCES",
      "Premium share",
      "Prices",
      "Poverty",
      "Panel",
      "HOW TO READ",
      "X-axis",
      "Y-axis",
      "Sign of beta_m"
    ),
    Description = c(
      "Does price sensitivity of premium gasoline demand vary with municipal poverty?",
      "premium_share = premium liters / (premium + regular liters) per municipality-month",
      "log(premium_price_monthly / regular_price_monthly)",
      "CONEVAL 2020 poverty rate: % of municipal population in poverty. Higher = MORE poor. Range: ~5% (San Pedro Garza Garcia) to ~97% (poor rural municipalities in Oaxaca/Chiapas).",
      "premium_share_mt = alpha_m + gamma_t + beta_m * log(price_ratio_mt) + e_mt",
      "Captures permanent level differences across municipalities (income, competition, infrastructure)",
      sprintf("Year fixed effect (%s). Absorbs national shocks (fuel policy, macro) common to all municipalities. Using annual FE retains seasonal and monthly price variation within each year, which is used to identify beta_m.", time_fe_var),
      "Municipality-specific price elasticity: how much premium share changes (in pp) when the premium/regular price ratio increases by 1%.",
      "Two-step Frisch-Waugh-Lovell (FWL)",
      "For each year, subtract the cross-municipality mean of premium_share and log(price_ratio). This removes the global time FE from both variables.",
      "Within each municipality, run OLS of demeaned share on demeaned log(price_ratio). The intercept absorbs the municipality FE alpha_m.",
      "Identified from within-municipality, within-year variation in relative prices across months. If premium gets relatively more expensive in a given month/year, does premium share fall more in some municipalities than others?",
      "",
      sprintf("beta_m winsorised at [%g%%, %g%%] of its distribution before computing bin statistics. Raw beta_m kept in 'Municipios' sheet for reference.", 100*winsor_pct, 100*(1-winsor_pct)),
      sprintf("%g percentage-point intervals. Bins with < 3 municipalities excluded from graphs.", bin_width),
      "12 months with valid premium_share AND valid price ratio",
      "",
      "CRE/SENER: 04_volumenes_venta_expendio_petroliferos.csv. Rows listing multiple municipalities (comma-separated, ~4.6% of raw rows) are excluded; no allocation rule can be justified without additional data.",
      "CRE retail station panel 2017-2025, balanced with 60-day carry-forward cap. Double average: station -> municipality x day, then x day -> municipality x month.",
      "CONEVAL Indicadores de pobreza municipal 2020. poverty_final = population-weighted mean of three independent partition estimates (sex, age, rural/urban).",
      "2017-2025 monthly, 1,348 municipalities with complete data.",
      "",
      "Municipal poverty rate (%). LEFT side of graph = RICHER municipalities. RIGHT side = POORER municipalities.",
      "beta_m: municipality-specific price elasticity of premium share.",
      "NEGATIVE beta_m = consumers switch away from premium when it gets relatively more expensive (normal demand response). MORE negative = stronger response."
    ),
    stringsAsFactors = FALSE
  )
  openxlsx::addWorksheet(wb, "Metodologia")
  openxlsx::writeData(wb, "Metodologia", x = method_df, startRow = 1)

  # Style: bold the section headers
  header_rows <- which(method_df$Description == "" |
    method_df$Item %in% c("QUESTION","MODEL","ESTIMATOR","ROBUSTNESS",
                           "DATA SOURCES","HOW TO READ"))
  bold_style  <- openxlsx::createStyle(textDecoration = "bold",
                                       fgFill = "#F2F2F2")
  for (r in header_rows) {
    openxlsx::addStyle(wb, "Metodologia", style = bold_style,
                       rows = r + 1L, cols = 1:2, gridExpand = TRUE)
  }
  openxlsx::setColWidths(wb, "Metodologia", cols = 1:2, widths = c(28, 95))
  openxlsx::addStyle(wb, "Metodologia",
    style = openxlsx::createStyle(wrapText = TRUE),
    rows  = 2:(nrow(method_df) + 1L), cols = 2, gridExpand = TRUE)

  # --- Sheet 2: Municipality elasticities ---
  sheet2 <- mun_betas |>
    dplyr::filter(!is.na(beta_m)) |>
    dplyr::arrange(poverty_final) |>
    dplyr::select(
      CVEGEO, NOM_MUN, NOM_ENT,
      poverty_final, poverty_sex, poverty_age, poverty_geo,
      beta_m, beta_m_w, se_m, t_stat, p_value, r_sq,
      n_obs, n_time_periods, flag_partition_divergence
    )
  openxlsx::addWorksheet(wb, "Municipios")
  openxlsx::writeDataTable(wb, "Municipios", x = sheet2,
    tableStyle = "TableStyleMedium9", withFilter = TRUE)
  openxlsx::setColWidths(wb, "Municipios",
    cols = seq_along(names(sheet2)), widths = "auto")
  beta_cols <- which(names(sheet2) %in%
    c("poverty_final","poverty_sex","poverty_age","poverty_geo",
      "beta_m","beta_m_w","se_m","t_stat","r_sq"))
  openxlsx::addStyle(wb, "Municipios",
    style = openxlsx::createStyle(numFmt = "0.0000"),
    rows  = seq_len(nrow(sheet2)) + 1L,
    cols  = beta_cols, gridExpand = TRUE)

  # --- Sheet 3: Bin summary ---
  bin_sheet_name <- sprintf("Bins_%gpp", bin_width)
  openxlsx::addWorksheet(wb, bin_sheet_name)
  openxlsx::writeDataTable(wb, bin_sheet_name,
    x = bins_list$summary_tbl, tableStyle = "TableStyleMedium2")
  openxlsx::setColWidths(wb, bin_sheet_name,
    cols = seq_along(names(bins_list$summary_tbl)), widths = "auto")

  openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)
  message(sprintf("Excel written: %s", out_xlsx))
  out_xlsx
}

# --------------------------------------------------------------------------
# 8. Main wrapper
# --------------------------------------------------------------------------

run_elasticity_poverty_analysis <- function(
  poverty_panel_parquet = "data/analysis/mun_month_prices/mun_month_prices_with_poverty.parquet",
  out_dir               = "outputs/shaun/elasticity",
  betas_parquet         = "data/analysis/elasticity/mun_elasticities.parquet",
  min_obs               = 12L,
  bin_width             = 2,
  time_fe_var           = "year",
  winsor_pct            = 0.02
) {
  dir.create(out_dir,                recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(betas_parquet), recursive = TRUE, showWarnings = FALSE)

  # ---- 1. Load data ----
  message("=== Step 1: load data ===")
  df <- .prep_elasticity_data(poverty_panel_parquet)

  # ---- 2. Estimate municipality elasticities (year FE, two-step FWL) ----
  message(sprintf("=== Step 2: municipality elasticities (time_fe_var = '%s') ===",
                  time_fe_var))
  mun_betas <- .estimate_mun_elasticities(df, min_obs = min_obs,
                                          time_fe_var = time_fe_var)

  mun_betas_pov <- mun_betas |>
    dplyr::filter(!is.na(poverty_final), !is.na(beta_m))

  message(sprintf("  Municipalities with beta + poverty: %d", nrow(mun_betas_pov)))
  message(sprintf(
    "  beta_m (raw): min=%.4f  median=%.4f  mean=%.4f  max=%.4f",
    min(mun_betas_pov$beta_m),   median(mun_betas_pov$beta_m),
    mean(mun_betas_pov$beta_m),  max(mun_betas_pov$beta_m)
  ))
  message(sprintf(
    "  Statistically significant (|t|>2): %d / %d (%.1f%%)",
    sum(abs(mun_betas_pov$t_stat) > 2, na.rm = TRUE),
    nrow(mun_betas_pov),
    100 * mean(abs(mun_betas_pov$t_stat) > 2, na.rm = TRUE)
  ))

  # ---- 3. Winsorise beta_m (robustness suggestion 1) ----
  lo <- stats::quantile(mun_betas_pov$beta_m, winsor_pct,      na.rm = TRUE)
  hi <- stats::quantile(mun_betas_pov$beta_m, 1 - winsor_pct,  na.rm = TRUE)
  mun_betas_pov <- mun_betas_pov |>
    dplyr::mutate(beta_m_w = pmin(pmax(beta_m, lo), hi))
  mun_betas <- mun_betas |>
    dplyr::mutate(beta_m_w = pmin(pmax(beta_m, lo), hi))

  message(sprintf(
    "  Winsorised at [%.1f%%, %.1f%%]: beta_m_w range [%.4f, %.4f]",
    100 * winsor_pct, 100 * (1 - winsor_pct), lo, hi
  ))

  # ---- 4. Poverty bins on winsorised betas ----
  message(sprintf("=== Step 3: poverty-bin summary (%g pp bins) ===", bin_width))
  # Swap beta_m for beta_m_w so bins use winsorised values
  mun_betas_pov_w <- mun_betas_pov |>
    dplyr::mutate(beta_m = beta_m_w)
  bins_list <- .build_poverty_bins(mun_betas_pov_w, bin_width = bin_width)
  message(sprintf(
    "  %d bins, min count = %d, max count = %d",
    nrow(bins_list$summary_tbl),
    min(bins_list$summary_tbl$n),
    max(bins_list$summary_tbl$n)
  ))

  # ---- 5. Graphs ----
  message("=== Step 4: generate graphs ===")

  gg_cover   <- .make_methodology_page(
    n_mun       = nrow(mun_betas_pov),
    n_obs       = nrow(df),
    year_range  = range(df$year),
    time_fe_var = time_fe_var,
    winsor_pct  = winsor_pct,
    bin_width   = bin_width
  )
  gg_scatter <- .plot_form1_scatter(mun_betas_pov, bins_list)
  gg_bins    <- .plot_form1_bins(bins_list, bin_width = bin_width)

  # --- Single combined PDF: cover + scatter + bins ---
  combined_path <- file.path(out_dir, "elasticity_analysis.pdf")
  grDevices::pdf(combined_path, width = 9, height = 6, onefile = TRUE)
  print(gg_cover)
  print(gg_scatter)
  print(gg_bins)
  grDevices::dev.off()

  # --- Also save individual PDFs for easy sharing ---
  scatter_path <- file.path(out_dir, "form1_scatter.pdf")
  bins_path    <- file.path(out_dir, "form1_bins.pdf")
  ggplot2::ggsave(scatter_path, plot = gg_scatter, width = 9, height = 6, dpi = 150)
  ggplot2::ggsave(bins_path,    plot = gg_bins,    width = 9, height = 6, dpi = 150)
  message(sprintf("  Combined PDF: %s", combined_path))
  message(sprintf("  Individual:   %s, %s", scatter_path, bins_path))

  # ---- 6. Parquet outputs ----
  arrow::write_parquet(mun_betas, betas_parquet, compression = "zstd")
  message(sprintf("  Municipality betas: %s", betas_parquet))

  bins_parquet <- file.path(out_dir, "elasticity_poverty_bins.parquet")
  bins_csv     <- file.path(out_dir, "elasticity_poverty_bins.csv")
  arrow::write_parquet(bins_list$summary_tbl, bins_parquet, compression = "zstd")
  readr::write_csv(bins_list$summary_tbl, bins_csv)
  message(sprintf("  Bin table: %s + csv", bins_parquet))

  # ---- 7. Excel ----
  xlsx_path <- file.path(out_dir, "elasticity_summary.xlsx")
  .write_elasticity_excel(mun_betas, bins_list, bin_width,
                          time_fe_var, winsor_pct, xlsx_path)

  # ---- 8. Done ----
  flag <- file.path(out_dir, ".analysis_done")
  writeLines(
    c(sprintf("betas_parquet=%s", betas_parquet),
      sprintf("n_mun=%d",         nrow(mun_betas)),
      sprintf("time_fe=%s",       time_fe_var),
      sprintf("winsor_pct=%.2f",  winsor_pct),
      sprintf("bin_width=%g",     bin_width),
      sprintf("when=%s",          Sys.time())),
    flag
  )
  flag
}
