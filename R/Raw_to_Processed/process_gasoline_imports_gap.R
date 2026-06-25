suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(stringr)
  library(tidyr)
  library(arrow)
  library(ggplot2)
  library(scales)
  library(patchwork)
})

.SPANISH_MONTHS <- c(
  "Ene" = 1L, "Feb" = 2L, "Mar" = 3L, "Abr" = 4L,
  "May" = 5L, "Jun" = 6L, "Jul" = 7L, "Ago" = 8L,
  "Sep" = 9L, "Oct" = 10L, "Nov" = 11L, "Dic" = 12L
)

.read_eia_exports <- function(path) {

  df <- readr::read_csv(path, skip = 4, show_col_types = FALSE,
                        col_names = c("month_str", "us_exports_kb_month"))
  df |>
    dplyr::mutate(
      date                = suppressWarnings(lubridate::my(month_str)),
      us_exports_kb_month = suppressWarnings(as.numeric(us_exports_kb_month))
    ) |>
    dplyr::filter(!is.na(date), !is.na(us_exports_kb_month)) |>
    dplyr::select(date, us_exports_kb_month) |>
    dplyr::arrange(date)
}

.read_pemex_imports <- function(path) {
  lines <- readr::read_lines(path, locale = readr::locale(encoding = "latin1"))

  month_row_idx <- which(stringr::str_detect(lines, "Ene/2016"))[1]
  gas_row_idx   <- which(stringr::str_detect(lines, "Gasolinas"))[1]

  if (is.na(month_row_idx) || is.na(gas_row_idx)) {
    stop("Could not locate month headers or 'Gasolinas' row in PEMEX file: ", path)
  }

  parse_csv_line <- function(line) {
    trimws(gsub('"', "", unlist(strsplit(line, ","))))
  }

  months <- parse_csv_line(lines[month_row_idx])
  vals   <- parse_csv_line(lines[gas_row_idx])

  month_labels <- months[-1]
  val_labels   <- vals[-1]

  n <- min(length(month_labels), length(val_labels))
  month_labels <- month_labels[seq_len(n)]
  val_labels   <- val_labels[seq_len(n)]

  df <- tibble::tibble(
    month_str = month_labels,
    raw_val   = val_labels
  ) |>
    dplyr::filter(nchar(month_str) > 0) |>
    dplyr::mutate(
      mo_abbr = substr(month_str, 1, 3),
      yr_str  = substr(month_str, 5, 8),
      month   = .SPANISH_MONTHS[mo_abbr],
      year    = suppressWarnings(as.integer(yr_str)),
      date    = lubridate::make_date(year, month, 1L),

      mbd     = suppressWarnings(as.numeric(
        dplyr::if_else(raw_val %in% c("N/D", ""), NA_character_, raw_val)
      )),

      mbd     = dplyr::if_else(mbd == 0 & !is.na(mbd), NA_real_, mbd),

      days_in_month          = lubridate::days_in_month(date),
      pemex_gasolinas_kb_month = mbd * days_in_month
    ) |>
    dplyr::filter(!is.na(date)) |>
    dplyr::select(date, mbd, days_in_month, pemex_gasolinas_kb_month)

  df
}

.read_country_shares <- function(path) {
  df <- readr::read_csv(path, show_col_types = FALSE)

  usa_row <- df |>
    dplyr::filter(
      tolower(`Country ID`) == "usa" |
      stringr::str_detect(tolower(Country), "estados unidos")
    )

  if (nrow(usa_row) == 0) stop("Could not find USA row in country share file.")

  usa_value   <- usa_row$`Trade Value`[1]
  total_value <- sum(df$`Trade Value`, na.rm = TRUE)
  computed_share_decimal <- usa_value / total_value

  raw_share <- usa_row$Share[1]
  if (raw_share > 1) {

    usa_share <- raw_share / 100
  } else {
    usa_share <- raw_share
  }

  if (abs(usa_share - computed_share_decimal) > 0.005) {
    warning(sprintf(
      "Reported USA share (%.4f) differs from computed (%.4f); using computed.",
      usa_share, computed_share_decimal
    ))
    usa_share <- computed_share_decimal
  }

  message(sprintf(
    "  USA share of Mexico gasoline imports (2024, by trade value): %.4f (%.2f%%)",
    usa_share, 100 * usa_share
  ))
  message(sprintf(
    "  NOTE: share is by USD trade value, not by volume. Fixed for all months."))
  message(sprintf(
    "  Top 5 suppliers: %s",
    paste(head(df |> dplyr::arrange(dplyr::desc(`Trade Value`)) |>
               dplyr::pull(Country), 5), collapse = ", ")
  ))

  list(
    usa_share_2024 = usa_share,
    top_countries  = df |>
      dplyr::arrange(dplyr::desc(`Trade Value`)) |>
      dplyr::select(Country, `Trade Value`, Share) |>
      head(10)
  )
}

.print_diagnostics <- function(eia, pemex, shares) {
  message("\n=== SOURCE DIAGNOSTICS ===")
  message(sprintf("  EIA:   %d months, %s to %s",
    nrow(eia),
    format(min(eia$date), "%b %Y"),
    format(max(eia$date), "%b %Y")))
  message(sprintf("  EIA units:  Thousand Barrels/month (monthly total)"))
  message(sprintf("  EIA range:  %.0f - %.0f kb/month",
    min(eia$us_exports_kb_month), max(eia$us_exports_kb_month)))

  pemex_clean <- pemex |> dplyr::filter(!is.na(pemex_gasolinas_kb_month))
  message(sprintf("  PEMEX: %d months with data (%d total parsed, %d NA/provisional)",
    nrow(pemex_clean), nrow(pemex),
    nrow(pemex) - nrow(pemex_clean)))
  message(sprintf("  PEMEX dates: %s to %s",
    format(min(pemex$date), "%b %Y"),
    format(max(pemex_clean$date), "%b %Y")))
  message(sprintf("  PEMEX original units: Mbd (thousand barrels/day)"))
  message(sprintf("  PEMEX converted units: Thousand Barrels/month"))
  message(sprintf("  PEMEX Mbd range: %.1f - %.1f Mbd",
    min(pemex_clean$mbd), max(pemex_clean$mbd)))
  message(sprintf("  PEMEX kb/month range: %.0f - %.0f",
    min(pemex_clean$pemex_gasolinas_kb_month),
    max(pemex_clean$pemex_gasolinas_kb_month)))
  message(sprintf("  PEMEX product note: 'Gasolinas b' includes MTBE and Enermex additive"))

  message(sprintf("  USA_share_2024: %.4f (by trade value, 2024 only)",
    shares$usa_share_2024))
  message("=========================\n")
}

.build_gap <- function(eia, pemex, usa_share) {

  gap <- eia |>
    dplyr::inner_join(pemex |> dplyr::select(date, pemex_gasolinas_kb_month),
                      by = "date") |>
    dplyr::filter(!is.na(us_exports_kb_month), !is.na(pemex_gasolinas_kb_month)) |>
    dplyr::mutate(
      usa_share_2024 = usa_share,

      implied_us_source_kb_month = pemex_gasolinas_kb_month * usa_share,

      raw_gap_kb_month = us_exports_kb_month - pemex_gasolinas_kb_month,

      adjusted_gap_kb_month = us_exports_kb_month - implied_us_source_kb_month,

      raw_gap_pct      = 100 * raw_gap_kb_month      / us_exports_kb_month,
      adjusted_gap_pct = 100 * adjusted_gap_kb_month / us_exports_kb_month
    ) |>
    dplyr::arrange(date)

  message(sprintf("  Overlap period: %s to %s (%d months)",
    format(min(gap$date), "%b %Y"),
    format(max(gap$date), "%b %Y"),
    nrow(gap)))
  message(sprintf("  raw_gap:      mean = %+.0f, median = %+.0f kb/month",
    mean(gap$raw_gap_kb_month), median(gap$raw_gap_kb_month)))
  message(sprintf("  adjusted_gap: mean = %+.0f, median = %+.0f kb/month",
    mean(gap$adjusted_gap_kb_month), median(gap$adjusted_gap_kb_month)))
  message(sprintf("  Months with positive adjusted_gap: %d / %d",
    sum(gap$adjusted_gap_kb_month > 0), nrow(gap)))

  gap
}

.make_gap_cover_page <- function(gap, usa_share) {
  n   <- nrow(gap)
  dt1 <- format(min(gap$date), "%b %Y")
  dt2 <- format(max(gap$date), "%b %Y")
  mean_adj <- mean(gap$adjusted_gap_kb_month)
  pos_pct  <- round(100 * mean(gap$adjusted_gap_kb_month > 0))

  lines <- c(
    "METHODOLOGY - EXPLORATORY U.S.-MEXICO GASOLINE IMPORT GAP",
    "",
    "QUESTION",
    "Does the volume of finished motor gasoline that the United States officially",
    "records as exported to Mexico match the volume that Mexico officially records",
    "as imported? A persistent discrepancy - where U.S. exports exceed Mexico's",
    "recorded receipts - is consistent with under-reporting of imports on the",
    "Mexican side, though it is not by itself conclusive evidence of illicit trade.",
    "",
    "DATA SOURCES",
    "1. EIA (U.S. Energy Information Administration)",
    "   Series: U.S. Exports to Mexico of Finished Motor Gasoline (MGFEXMX1)",
    "   Units: Thousand Barrels per month (monthly totals, NOT daily averages)",
    "   Range: Jan 1993 to Jan 2026",
    "   Product: Finished Motor Gasoline ONLY. Does not include MTBE or",
    "            blending components shipped separately.",
    "",
    "2. PEMEX / Direccion de Planeacion",
    "   Series: 'Gasolinas b' row from 'Volumen de importaciones de productos",
    "            petroliferos' (Table: OBSERVADO-MENSUAL)",
    "   Units in source: Miles de barriles DIARIOS (Mbd = thousand barrels/day)",
    "   Conversion: Mbd x days_in_month = Thousand Barrels/month",
    "   Range: Jan 2016 to Feb 2026 (Mar 2026 = 0.0, excluded as provisional)",
    "   IMPORTANT: Footnote b states 'Gasolinas' INCLUDES MTBE and Enermex",
    "   (a gasoline additive). This makes the PEMEX series BROADER than EIA.",
    "   The PEMEX figure tends to be slightly larger, compressing the gap.",
    "",
    "3. Mexico Imports by Country (2024 annual, by trade value in USD)",
    sprintf("   USA share of Mexico gasoline imports: %.2f%% (by value, 2024 only)",
            100 * usa_share),
    "   This share is held CONSTANT for all months - a simplifying assumption.",
    "   Share is by USD value, not by volume; volume share may differ slightly.",
    "",
    sprintf("OVERLAP PERIOD: %s to %s (%d months)", dt1, dt2, n),
    "",
    "UNIT CONVERSION SUMMARY",
    "   EIA:   Thousand Barrels/month    (no conversion needed)",
    "   PEMEX: Mbd x days_in_month  -->  Thousand Barrels/month",
    "   Both series are now directly comparable in the same unit.",
    "",
    "GAP MEASURES (defined on page 3 and 4)",
    "   raw_gap_t      = US_exports_t - PEMEX_gasolinas_t",
    "   adjusted_gap_t = US_exports_t - (PEMEX_gasolinas_t x USA_share_2024)",
    sprintf("   Adjusted gap: mean = %+.0f kb/month, positive in %d%% of months.",
            mean_adj, pos_pct),
    "",
    "KEY LIMITATIONS",
    "   a) Product mismatch: PEMEX includes MTBE/Enermex; EIA does not.",
    "      This inflates PEMEX, making the gap appear SMALLER than it truly is.",
    "   b) USA_share is from 2024 only; the U.S. share varied across years.",
    "   c) Share is by trade value (USD), not by volume.",
    "   d) Timing: U.S. records exports at departure; Mexico at arrival.",
    "      Cross-month lags are normal and can flip the sign of a single month.",
    "   e) Statistical methodology differs between EIA and PEMEX.",
    "",
    "CAUTION",
    "   This is an EXPLORATORY DISCREPANCY MEASURE, not proof of illicit trade.",
    "   Additional corroborating evidence (CRE retail volumes, tax data, customs",
    "   microdata) is required for stronger claims."
  )

  df <- data.frame(
    x    = 0,
    y    = rev(seq_along(lines)),
    lab  = lines,
    bold = grepl("^[A-Z ]{3,}$", lines)
  )

  ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, label = lab)) +
    ggplot2::geom_text(
      ggplot2::aes(fontface = ifelse(bold, "bold", "plain")),
      hjust = 0, size = 3.0, family = "mono",
      colour = ifelse(df$bold, "#1a1a1a", "#333333")
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = 0.8)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin     = ggplot2::margin(16, 16, 16, 16),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}

.plot_import_gap <- function(gap, usa_share, out_pdf) {
  dir.create(dirname(out_pdf), recursive = TRUE, showWarnings = FALSE)

  clr_eia   <- "#1f77b4"
  clr_pemex <- "#d62728"
  clr_gap   <- "#2ca02c"

  base_theme <- ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(size = 9.5, colour = "#333333",
                                            lineheight = 1.4),
      plot.caption  = ggplot2::element_text(size = 8, colour = "grey55",
                                            lineheight = 1.3),
      legend.position   = "top",
      panel.grid.minor  = ggplot2::element_blank(),
      plot.margin       = ggplot2::margin(14, 18, 14, 14)
    )

  p1 <- ggplot2::ggplot(gap, ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = us_exports_kb_month,
                                    colour = "U.S. exports (EIA)"),
                       linewidth = 0.9) +
    ggplot2::geom_line(ggplot2::aes(y = pemex_gasolinas_kb_month,
                                    colour = "Mexico total imports - all origins (PEMEX)"),
                       linewidth = 0.9) +
    ggplot2::scale_colour_manual(
      values = c("U.S. exports (EIA)" = clr_eia,
                 "Mexico total imports - all origins (PEMEX)" = clr_pemex)
    ) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                          expand = ggplot2::expansion(add = 20)) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(suffix = "k bbl"),
                                limits = c(0, NA)) +
    ggplot2::labs(
      title    = "Graph 1 - The two raw series",
      subtitle = paste0(
        "BLUE: What the U.S. says it exported to Mexico each month (EIA, finished motor gasoline).\n",
        "RED: What Mexico officially records as total gasoline imported from ALL countries combined (PEMEX).\n\n",
        "Key observation: The two lines track each other closely, which is expected because\n",
        "the U.S. supplies ~93% of Mexico's gasoline imports. However, the PEMEX red line\n",
        "is almost always equal to or ABOVE the blue EIA line - meaning U.S. exports alone\n",
        "are almost sufficient to account for all of Mexico's recorded imports.\n",
        "This leaves little room for the ~7% that supposedly comes from other countries.\n\n",
        "Note the sharp 2020 COVID drop in both series and the subsequent recovery.\n",
        "The gap between lines is the key quantity examined in Graphs 3 and 4."
      ),
      y       = "Thousand barrels/month",
      x       = NULL,
      colour  = NULL,
      caption = paste0(
        "EIA series: Finished Motor Gasoline only (excludes MTBE/blending components).\n",
        "PEMEX 'Gasolinas b': includes MTBE and Enermex additive - slightly broader definition."
      )
    ) +
    base_theme

  p2 <- ggplot2::ggplot(gap, ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = us_exports_kb_month,
                                    colour = "U.S. exports (EIA)"),
                       linewidth = 0.9) +
    ggplot2::geom_line(ggplot2::aes(y = implied_us_source_kb_month,
                                    colour = "Mexico implied U.S.-source imports\n(PEMEX total x 92.8%)"),
                       linewidth = 0.9, linetype = "dashed") +
    ggplot2::scale_colour_manual(
      values = c(
        "U.S. exports (EIA)" = clr_eia,
        "Mexico implied U.S.-source imports\n(PEMEX total x 92.8%)" = clr_pemex
      )
    ) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                          expand = ggplot2::expansion(add = 20)) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(suffix = "k bbl"),
                                limits = c(0, NA)) +
    ggplot2::labs(
      title    = "Graph 2 - Bilateral comparison: U.S. supply vs. Mexico's recorded U.S. receipts",
      subtitle = paste0(
        "BLUE (solid): What the U.S. says it exported to Mexico (EIA).\n",
        "RED (dashed): What Mexico officially implies it received FROM the U.S. specifically.\n",
        "             This is estimated as: PEMEX total imports x 92.8% (U.S. value share, 2024).\n\n",
        "This is the apples-to-apples bilateral comparison. Both lines should represent\n",
        "the same physical flow - gasoline moving from the U.S. to Mexico - but measured\n",
        "by two different national statistical agencies using different methods.\n\n",
        "When the solid blue line is ABOVE the dashed red line, the U.S. says it sent\n",
        "more than Mexico officially recorded as arriving from the U.S. - the adjusted gap.\n",
        "When the lines cross or the red is above, Mexico recorded more than the U.S. exported\n",
        "(can happen due to timing lags, inventory drawdowns, or transit reclassification)."
      ),
      y       = "Thousand barrels/month",
      x       = NULL,
      colour  = NULL,
      caption = paste0(
        "Implied U.S.-source = PEMEX total gasolinas x 0.9277 (USA share, 2024 trade value, fixed).\n",
        "Dashed line is an approximation: the actual U.S. share varies by year and is measured by value not volume."
      )
    ) +
    base_theme

  raw_mean <- mean(gap$raw_gap_kb_month)
  p3 <- ggplot2::ggplot(gap, ggplot2::aes(x = date, y = raw_gap_kb_month)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.6,
                        linetype = "dashed") +
    ggplot2::geom_hline(yintercept = raw_mean, colour = "#ff7f0e",
                        linewidth = 0.7, linetype = "solid") +
    ggplot2::geom_col(ggplot2::aes(fill = raw_gap_kb_month > 0),
                      width = 25, show.legend = FALSE) +
    ggplot2::annotate("text", x = max(gap$date), y = raw_mean,
                      label = sprintf("mean = %+.0f k bbl", raw_mean),
                      hjust = 1, vjust = -0.5, size = 3.2, colour = "#ff7f0e") +
    ggplot2::scale_fill_manual(values = c("TRUE" = clr_gap, "FALSE" = clr_pemex)) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                          expand = ggplot2::expansion(add = 20)) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(suffix = "k bbl")) +
    ggplot2::labs(
      title    = "Graph 3 - Raw gap: U.S. exports minus Mexico's TOTAL recorded imports",
      subtitle = paste0(
        "Formula: raw_gap_t = US_exports_t - PEMEX_gasolinas_t\n\n",
        "GREEN bar = positive: U.S. exported MORE than Mexico recorded from ALL sources combined.\n",
        "RED bar   = negative: U.S. exports were less than Mexico's total recorded imports.\n\n",
        "Interpretation: Because the U.S. supplies ~93% of Mexico's gasoline, we would EXPECT\n",
        "this gap to hover around -7% of total imports (i.e., mildly negative, reflecting other\n",
        "suppliers). A gap near zero means U.S. exports alone can explain all of Mexico's recorded\n",
        "imports. A strongly positive gap means U.S. exports EXCEED Mexico's total from all origins.\n\n",
        "What we see: The raw gap oscillates around zero, meaning U.S. exports alone are almost\n",
        "sufficient to account for 100% of Mexico's officially recorded gasoline imports.\n",
        "That is remarkable given that non-U.S. suppliers should contribute ~7%."
      ),
      y       = "Thousand barrels/month",
      x       = NULL,
      caption = paste0(
        "Orange line = average over the full period. Bars show month-by-month values.\n",
        "Note: product mismatch (MTBE included in PEMEX but not EIA) biases this gap downward (more negative)."
      )
    ) +
    base_theme +
    ggplot2::theme(legend.position = "none")

  adj_mean <- mean(gap$adjusted_gap_kb_month)
  pos_n    <- sum(gap$adjusted_gap_kb_month > 0)
  p4 <- ggplot2::ggplot(gap, ggplot2::aes(x = date, y = adjusted_gap_kb_month)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.6,
                        linetype = "dashed") +
    ggplot2::geom_hline(yintercept = adj_mean, colour = "#ff7f0e",
                        linewidth = 0.7, linetype = "solid") +
    ggplot2::geom_col(ggplot2::aes(fill = adjusted_gap_kb_month > 0),
                      width = 25, show.legend = FALSE) +
    ggplot2::annotate("text", x = max(gap$date), y = adj_mean,
                      label = sprintf("mean = %+.0f k bbl", adj_mean),
                      hjust = 1, vjust = -0.5, size = 3.2, colour = "#ff7f0e") +
    ggplot2::scale_fill_manual(values = c("TRUE" = clr_gap, "FALSE" = clr_pemex)) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                          expand = ggplot2::expansion(add = 20)) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(suffix = "k bbl")) +
    ggplot2::labs(
      title    = "Graph 4 - Adjusted gap: the key bilateral discrepancy measure",
      subtitle = paste0(
        "Formula: adjusted_gap_t = US_exports_t - (PEMEX_gasolinas_t x 0.9277)\n\n",
        "GREEN bar = positive: U.S. recorded MORE exports to Mexico than Mexico officially implies\n",
        "            it received from the U.S. This is the core discrepancy of interest.\n",
        "RED bar   = negative: Mexico's official records imply more U.S.-sourced imports than\n",
        "            the U.S. says it exported (timing lags, methodological differences).\n\n",
        sprintf("What we see: The gap is positive in %d of %d months (%.0f%%).\n",
                pos_n, nrow(gap), 100 * pos_n / nrow(gap)),
        sprintf("Mean adjusted gap: %+.0f thousand barrels/month.\n", adj_mean),
        "This means that on average, the U.S. reports exporting roughly that many thousand\n",
        "barrels per month MORE to Mexico than Mexico's official statistics account for.\n\n",
        "CAUTION: This is consistent with under-reporting in Mexico's official import\n",
        "statistics, but many legitimate explanations exist (see cover page).\n",
        "This is an EXPLORATORY MEASURE, not evidence of illicit trade."
      ),
      y       = "Thousand barrels/month",
      x       = NULL,
      caption = paste0(
        "Adjusted gap isolates the bilateral U.S.-Mexico discrepancy by scaling PEMEX by the U.S. share.\n",
        "Key assumption: USA_share = 0.9277 is fixed across all months (2024 trade-value share)."
      )
    ) +
    base_theme +
    ggplot2::theme(legend.position = "none")

  grDevices::pdf(out_pdf, width = 11, height = 8.5, onefile = TRUE)
  print(.make_gap_cover_page(gap, usa_share))
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  grDevices::dev.off()

  message(sprintf("  Multi-page PDF (5 pages): %s", out_pdf))
  out_pdf
}

.write_interpretation <- function(gap, shares, out_txt) {
  dir.create(dirname(out_txt), recursive = TRUE, showWarnings = FALSE)

  pos_months  <- sum(gap$adjusted_gap_kb_month > 0)
  n_months    <- nrow(gap)
  mean_adj    <- mean(gap$adjusted_gap_kb_month)
  med_adj     <- median(gap$adjusted_gap_kb_month)
  mean_raw    <- mean(gap$raw_gap_kb_month)

  recent <- gap |> dplyr::filter(lubridate::year(date) >= 2020)
  early  <- gap |> dplyr::filter(lubridate::year(date) < 2020)

  lines <- c(
    "EXPLORATORY U.S.-MEXICO GASOLINE IMPORT GAP - INTERPRETATION NOTE",
    "==================================================================",
    "",
    "1. WHAT EACH SERIES MEASURES",
    "",
    "   US_exports_t (EIA, Thousand Barrels/month):",
    "     Volume of finished motor gasoline that U.S. customs records as exported",
    "     to Mexico each month. Source: EIA series MGFEXMX1.",
    "     Does NOT include MTBE or blending components shipped separately.",
    "",
    "   Official_Mexico_imports_t (PEMEX, Thousand Barrels/month):",
    "     Total monthly gasoline import volume recorded in official Mexican statistics,",
    "     all supplier countries combined. Converted from Mbd (daily average) to",
    "     monthly total by multiplying by days in each month.",
    "     INCLUDES MTBE and Enermex additive - broader than the EIA definition.",
    "",
    "   USA_share_2024 = 0.9277 (92.77%):",
    "     United States share of Mexico gasoline import value in 2024 (by USD).",
    "     Held constant for all months - a strong simplifying assumption.",
    "",
    "   implied_us_source_t = Official_Mexico_imports_t * 0.9277:",
    "     Estimate of what Mexico officially recorded as arriving from the U.S.,",
    "     derived by applying the fixed 2024 share to total monthly imports.",
    "",
    "2. THE GAP MEASURES",
    "",
    "   raw_gap_t = US_exports_t - Official_Mexico_imports_t",
    sprintf("     Average: %+.0f kb/month over %d months (%s to %s).",
            mean_raw, n_months,
            format(min(gap$date), "%b %Y"),
            format(max(gap$date), "%b %Y")),
    "     Expected to be negative: the U.S. supplies ~93% of Mexico imports, so",
    "     U.S. exports alone are somewhat less than total recorded imports.",
    "     A strongly positive raw_gap would indicate an extraordinary discrepancy.",
    "",
    "   adjusted_gap_t = US_exports_t - implied_us_source_t",
    sprintf("     Average: %+.0f kb/month.  Median: %+.0f kb/month.",
            mean_adj, med_adj),
    sprintf("     Positive in %d of %d months (%.0f%%).",
            pos_months, n_months, 100 * pos_months / n_months),
    "     When positive: U.S. EIA records more exports to Mexico than Mexico's",
    "     official statistics imply were received from the U.S.",
    "     When negative: Mexico recorded more U.S.-sourced imports than the U.S.",
    "     says it exported (e.g., timing lags, different recording dates).",
    sprintf("     Pre-2020 average: %+.0f kb/month.",
            mean(early$adjusted_gap_kb_month, na.rm = TRUE)),
    sprintf("     2020+ average:    %+.0f kb/month.",
            mean(recent$adjusted_gap_kb_month, na.rm = TRUE)),
    "",
    "3. WHY THIS IS EXPLORATORY, NOT PROOF",
    "",
    "   a) Product mismatch: PEMEX 'Gasolinas' includes MTBE and Enermex;",
    "      EIA counts 'Finished Motor Gasoline' only. The PEMEX figure is",
    "      systematically larger, compressing (making less positive) the gap.",
    "",
    "   b) Timing: U.S. records exports at the port of departure; Mexico records",
    "      imports at the port of entry. Cross-month timing differences are normal.",
    "",
    "   c) Fixed share: using 2024 USA_share for all years introduces error.",
    "      The U.S. share has varied over time (e.g., Russia/Europe supplied more",
    "      before 2022 sanctions pressure).",
    "",
    "   d) Trade value vs. volume: the 2024 share is by USD, not barrels.",
    "      Premium-priced U.S. product could overstate volume share.",
    "",
    "   e) Statistical methodology: EIA and PEMEX use different survey methods,",
    "      reporting lags, and revision schedules.",
    "",
    "   A persistent, large, and positive adjusted_gap is CONSISTENT with",
    "   under-reporting of gasoline imports in Mexican official statistics, but",
    "   it is not by itself conclusive evidence of illicit or irregular trade.",
    "   Additional corroborating evidence (e.g., CRE retail volumes, tax revenue",
    "   data, customs microdata) would be needed for stronger claims.",
    "",
    "4. DATA FILES PRODUCED",
    "   data/processed/imports_gap/gasoline_imports_gap.parquet",
    "   data/processed/imports_gap/gasoline_imports_gap.csv",
    "   outputs/imports_gap/import_gap_plots.pdf",
    "   outputs/imports_gap/interpretation_note.txt  (this file)"
  )

  writeLines(lines, out_txt)
  message(sprintf("  Interpretation note: %s", out_txt))
  out_txt
}

process_gasoline_imports_gap <- function(
  eia_csv     = "data/raw_public/U.S._Exports_to_Mexico_of_Finished_Motor_Gasoline.csv",
  pemex_csv   = NULL,
  country_csv = NULL,
  out_parquet = "data/processed/imports_gap/gasoline_imports_gap.parquet",
  out_csv     = "data/processed/imports_gap/gasoline_imports_gap.csv",
  out_pdf     = "outputs/imports_gap/import_gap_plots.pdf",
  out_txt     = "outputs/imports_gap/interpretation_note.txt"
) {

  raw_dir <- "data/raw_public"

  if (is.null(pemex_csv)) {
    candidates <- list.files(raw_dir, pattern = "^kjojxagtbl.*\\.csv$",
                             full.names = TRUE)
    if (length(candidates) == 0)
      stop("No PEMEX CSV matching 'kjojxagtbl*.csv' found in ", raw_dir)
    pemex_csv <- candidates[1]
    message("  Auto-detected PEMEX file: ", pemex_csv)
  }

  if (is.null(country_csv)) {
    candidates <- list.files(raw_dir, pattern = "Importaciones-por-pais.*\\.csv$",
                             full.names = TRUE)
    if (length(candidates) == 0)
      stop("No country CSV matching 'Importaciones-por-pais*.csv' found in ", raw_dir)
    country_csv <- candidates[1]
    message("  Auto-detected country share file: ", country_csv)
  }

  dir.create(dirname(out_parquet), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_pdf),     recursive = TRUE, showWarnings = FALSE)

  message("=== Step 1: read sources ===")
  eia    <- .read_eia_exports(eia_csv)
  pemex  <- .read_pemex_imports(pemex_csv)
  shares <- .read_country_shares(country_csv)

  message("=== Step 2: source diagnostics ===")
  .print_diagnostics(eia, pemex, shares)

  message("=== Step 3: build gap measures ===")
  gap <- .build_gap(eia, pemex, shares$usa_share_2024)

  message("=== Step 4: write outputs ===")
  arrow::write_parquet(gap, out_parquet, compression = "zstd")
  readr::write_csv(gap, out_csv)
  message(sprintf("  Parquet: %s", out_parquet))
  message(sprintf("  CSV:     %s", out_csv))

  message("=== Step 5: plots ===")
  .plot_import_gap(gap, shares$usa_share_2024, out_pdf)

  message("=== Step 6: interpretation note ===")
  .write_interpretation(gap, shares, out_txt)

  message("=== Done ===")
  invisible(out_parquet)
}
