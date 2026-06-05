suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(ggplot2)
  library(lubridate)
  library(scales)
})

.read_terminal_national_monthly_spread <- function(terminal_dir) {
  arrow::open_dataset(terminal_dir) |>
    dplyr::filter(!is.na(regular), !is.na(premium), regular > 0, premium > 0) |>
    dplyr::mutate(month_num = lubridate::month(date)) |>
    dplyr::group_by(year, month = month_num) |>
    dplyr::summarise(
      mean_regular = mean(regular, na.rm = TRUE),
      mean_premium = mean(premium, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      year       = as.integer(year),
      month      = as.integer(month),
      date       = lubridate::make_date(year, month, 1L),
      abs_spread = mean_premium - mean_regular,
      rel_spread = mean_premium / mean_regular
    )
}

.read_bloomberg_monthly_spread <- function(bloomberg_parquet) {
  arrow::read_parquet(bloomberg_parquet) |>
    dplyr::filter(
      !is.na(regular_87_mxn_l), !is.na(premium_93_mxn_l),
      regular_87_mxn_l > 0, premium_93_mxn_l > 0
    ) |>
    dplyr::mutate(
      date       = lubridate::make_date(year, month, 1L),
      abs_spread = premium_93_mxn_l - regular_87_mxn_l,
      rel_spread = premium_93_mxn_l / regular_87_mxn_l
    )
}

.spread_scatter <- function(df, x_col, y_col, x_label, y_label, title, subtitle,
                             year_breaks = c(2017, 2019, 2021, 2023, 2025)) {
  valid_breaks <- as.numeric(
    lubridate::ymd(paste0(intersect(year_breaks, df$year), "-01-01"))
  )
  valid_labels <- as.character(intersect(year_breaks, df$year))

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]],
                 colour = as.numeric(date))
  ) +
    ggplot2::geom_point(size = 2.2, alpha = 0.85) +
    ggplot2::geom_smooth(
      method    = "lm", formula = y ~ x, se = TRUE,
      colour    = "black", fill = "grey70",
      linewidth = 0.6, linetype = "dashed", alpha = 0.20
    ) +
    ggplot2::scale_colour_viridis_c(
      name   = NULL,
      breaks = valid_breaks,
      labels = valid_labels,
      guide  = ggplot2::guide_colourbar(
        barwidth = 10, barheight = 0.5, title.position = "top"
      )
    ) +
    ggplot2::scale_x_continuous(
      name   = x_label,
      labels = scales::label_number(accuracy = 0.1)
    ) +
    ggplot2::scale_y_continuous(
      name   = y_label,
      labels = scales::label_number(accuracy = 0.001)
    ) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold"),
      plot.subtitle    = ggplot2::element_text(colour = "grey30", size = 8.5),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "bottom"
    )
}

build_spread_diagnostic_plots <- function(
  bloomberg_parquet = "data/processed/bloomberg/gasoline_bloomberg.parquet",
  terminal_dir      = "data/processed/terminal",
  out_dir           = "outputs/shaun/spread_diagnostics"
) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  terminal  <- .read_terminal_national_monthly_spread(terminal_dir)
  bloomberg <- if (file.exists(bloomberg_parquet)) {
    tryCatch(.read_bloomberg_monthly_spread(bloomberg_parquet), error = function(e) {
      message("  Bloomberg spread plots: error reading parquet — skipping")
      NULL
    })
  } else {
    message("  Bloomberg parquet not found — skipping Bloomberg spread plots")
    NULL
  }

  pdf_path <- file.path(out_dir, "spread_diagnostic_plots.pdf")
  grDevices::pdf(pdf_path, width = 10, height = 7, onefile = TRUE)

  print(.spread_scatter(
    terminal,
    x_col    = "mean_regular",
    y_col    = "abs_spread",
    x_label  = "National avg PEMEX terminal regular price (MXN/L)",
    y_label  = "Absolute spread: premium − regular (MXN/L)",
    title    = "PEMEX terminal prices: absolute spread vs. regular price level",
    subtitle = paste0(
      "Each dot = 1 national monthly average (2017–2025). ",
      "Dashed line: OLS trend. Colour = time."
    )
  ))

  print(.spread_scatter(
    terminal,
    x_col    = "mean_regular",
    y_col    = "rel_spread",
    x_label  = "National avg PEMEX terminal regular price (MXN/L)",
    y_label  = "Relative spread: premium / regular",
    title    = "PEMEX terminal prices: relative spread vs. regular price level",
    subtitle = paste0(
      "If ratio is stable across price levels, the log-ratio regression specification is appropriate. ",
      "A positive slope indicates an income effect: premium rises more than proportionally when level rises."
    )
  ))

  if (!is.null(bloomberg)) {
    print(.spread_scatter(
      bloomberg,
      x_col    = "regular_87_mxn_l",
      y_col    = "abs_spread",
      x_label  = "Gulf Coast Regular 87 spot price (MXN/L)",
      y_label  = "Absolute spread: Premium 93 − Regular 87 (MXN/L)",
      title    = "Bloomberg Gulf Coast: absolute spread vs. regular price level",
      subtitle = "Monthly data, 2017 – Jan 2024 (Bloomberg discontinued thereafter).",
      year_breaks = c(2017, 2019, 2021, 2023)
    ))

    print(.spread_scatter(
      bloomberg,
      x_col    = "regular_87_mxn_l",
      y_col    = "rel_spread",
      x_label  = "Gulf Coast Regular 87 spot price (MXN/L)",
      y_label  = "Relative spread: Premium 93 / Regular 87",
      title    = "Bloomberg Gulf Coast: relative spread vs. regular price level",
      subtitle = paste0(
        "Key test (Shaun Point 2): if ratio varies with price level, ",
        "the log-ratio instrument may be misspecified."
      ),
      year_breaks = c(2017, 2019, 2021, 2023)
    ))
  }

  grDevices::dev.off()
  message(sprintf("  Spread diagnostic plots: %s", pdf_path))
  pdf_path
}
