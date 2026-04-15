# R/Analysis/shaun_outputs.R
#
# Produces Shaun's deliverable outputs from the municipality-month parquet:
#
#   write_shaun_excel()
#     Excel workbook with two sheets:
#       "Datos"   — full municipality-month table
#       "Metodos" — methodology notes
#
#   write_shaun_graphs()
#     One graph per municipality: dual time-series of
#       (1) premium_to_regular_price_ratio  (left axis, solid line)
#       (2) premium_share                   (right axis, dashed line, % scale)
#     All graphs written to a single multi-page PDF.
#     One PNG per municipality also written to a subfolder.

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(openxlsx)
  library(ggplot2)
  library(scales)
  library(tidyr)
  library(lubridate)
})

# ---------------------------------------------------------------------------
# Excel output
# ---------------------------------------------------------------------------

write_shaun_excel <- function(
  mun_month_parquet,
  out_xlsx = "outputs/shaun/mun_month_ratios.xlsx"
) {
  dir.create(dirname(out_xlsx), recursive = TRUE, showWarnings = FALSE)

  df <- arrow::read_parquet(mun_month_parquet)

  # ---- Sheet 1: data table ----
  data_sheet <- df |>
    dplyr::select(
      CVEGEO,
      NOM_MUN,
      NOM_ENT,
      year,
      month,
      premium_price_monthly,
      regular_price_monthly,
      premium_to_regular_price_ratio,
      premium_volume,
      regular_volume,
      premium_share,
      n_days_in_month,
      n_days_with_regular,
      n_days_with_premium
    ) |>
    dplyr::arrange(CVEGEO, year, month)

  # ---- Sheet 2: methodology notes ----
  method_rows <- data.frame(
    Variable  = c(
      "premium_to_regular_price_ratio",
      "premium_share",
      "premium_price_monthly",
      "regular_price_monthly",
      "premium_volume",
      "regular_volume"
    ),
    Definicion = c(
      "precio mensual premium / precio mensual regular (doble promedio Shaun)",
      "premium_volume / (premium_volume + regular_volume) [en litros]",
      "promedio de promedios diarios entre estaciones dentro del municipio (gasolina premium, MXN/litro)",
      "promedio de promedios diarios entre estaciones dentro del municipio (gasolina regular, MXN/litro)",
      "sum de litros vendidos de gasolina premium por municipio-mes (CRE/SENER)",
      "sum de litros vendidos de gasolina regular por municipio-mes (CRE/SENER)"
    ),
    Fuente = c(
      "Panel balanceado CRE retail (2017-2025), 60-day carry-forward cap",
      "04_volumenes_venta_expendio_petroliferos.csv",
      "Panel balanceado CRE retail (2017-2025)",
      "Panel balanceado CRE retail (2017-2025)",
      "04_volumenes_venta_expendio_petroliferos.csv",
      "04_volumenes_venta_expendio_petroliferos.csv"
    ),
    Notas = c(
      "NA si falta precio premium o regular en ese municipio-mes",
      "NA si falta volumen premium o regular; no disponible para todos los municipios",
      "Paso 1: promedio entre estaciones por municipio-dia. Paso 2: promedio de dias dentro del mes",
      "Mismo metodo que premium_price_monthly",
      "Multi-municipio rows del CSV se dividieron en partes iguales",
      "Multi-municipio rows del CSV se dividieron en partes iguales"
    ),
    stringsAsFactors = FALSE
  )

  # ---- Build workbook ----
  wb <- openxlsx::createWorkbook()

  # --- Sheet: Datos ---
  openxlsx::addWorksheet(wb, "Datos")
  openxlsx::writeDataTable(
    wb, "Datos",
    x          = data_sheet,
    startRow   = 1, startCol = 1,
    tableStyle = "TableStyleMedium9",
    withFilter = TRUE
  )
  # Format price columns as 2 decimal places
  price_cols <- which(names(data_sheet) %in%
    c("premium_price_monthly", "regular_price_monthly",
      "premium_to_regular_price_ratio", "premium_share"))
  vol_cols   <- which(names(data_sheet) %in%
    c("premium_volume", "regular_volume"))

  num_style_2d <- openxlsx::createStyle(numFmt = "0.0000")
  num_style_0d <- openxlsx::createStyle(numFmt = "#,##0")

  if (length(price_cols) > 0) {
    openxlsx::addStyle(
      wb, "Datos",
      style = num_style_2d,
      rows  = seq_len(nrow(data_sheet)) + 1L,
      cols  = price_cols,
      gridExpand = TRUE
    )
  }
  if (length(vol_cols) > 0) {
    openxlsx::addStyle(
      wb, "Datos",
      style = num_style_0d,
      rows  = seq_len(nrow(data_sheet)) + 1L,
      cols  = vol_cols,
      gridExpand = TRUE
    )
  }
  openxlsx::setColWidths(wb, "Datos",
    cols   = seq_along(names(data_sheet)),
    widths = "auto"
  )

  # --- Sheet: Metodos ---
  openxlsx::addWorksheet(wb, "Metodos")
  openxlsx::writeDataTable(
    wb, "Metodos",
    x          = method_rows,
    tableStyle = "TableStyleLight1"
  )
  openxlsx::setColWidths(wb, "Metodos",
    cols   = seq_along(names(method_rows)),
    widths = c(35, 65, 45, 65)
  )

  openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)
  message(sprintf("Excel written: %s  (%d rows, %d municipalities)",
                  out_xlsx,
                  nrow(data_sheet),
                  dplyr::n_distinct(data_sheet$CVEGEO)))
  out_xlsx
}

# ---------------------------------------------------------------------------
# Graphs: one page per municipality
# ---------------------------------------------------------------------------

.make_mun_graph <- function(df_mun, cvegeo, nom_mun, nom_ent) {
  # Create a date column for the x-axis
  df_mun <- df_mun |>
    dplyr::mutate(date = lubridate::make_date(year, month, 1L))

  # ---- Build long format for faceting ----
  has_share <- any(!is.na(df_mun$premium_share))
  has_ratio <- any(!is.na(df_mun$premium_to_regular_price_ratio))

  if (!has_ratio && !has_share) return(NULL)

  # We plot both series in a two-panel faceted graph
  df_long <- df_mun |>
    dplyr::select(date,
      `Precio premium/regular` = premium_to_regular_price_ratio,
      `Premium share (volumen)` = premium_share
    ) |>
    tidyr::pivot_longer(
      cols      = -date,
      names_to  = "variable",
      values_to = "value"
    ) |>
    dplyr::filter(!is.na(value))

  # Custom labeller: show % for share, raw ratio for price ratio
  facet_label_fn <- function(x) {
    dplyr::case_when(
      x == "Premium share (volumen)" ~ "Premium share (litros)",
      x == "Precio premium/regular"  ~ "Precio premium / regular (MXN/L)",
      TRUE ~ x
    )
  }

  # Per-facet y-axis formatting
  df_long <- df_long |>
    dplyr::mutate(
      y_label = dplyr::if_else(
        variable == "Premium share (volumen)",
        scales::percent(value, accuracy = 0.1),
        sprintf("%.4f", value)
      )
    )

  title_str <- if (!is.na(nom_mun) && !is.na(nom_ent)) {
    sprintf("%s — %s", nom_mun, nom_ent)
  } else {
    cvegeo
  }

  gg <- ggplot2::ggplot(
    df_long,
    ggplot2::aes(x = date, y = value, group = variable)
  ) +
    ggplot2::geom_line(linewidth = 0.6, color = "#1f77b4") +
    ggplot2::geom_point(size = 0.9, color = "#1f77b4", alpha = 0.6) +
    ggplot2::facet_wrap(
      ~variable,
      ncol   = 1,
      scales = "free_y",
      labeller = ggplot2::as_labeller(facet_label_fn)
    ) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand      = ggplot2::expansion(add = 30)
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x, var) {
        # applied per facet via after_stat — using scales::label_auto
        scales::label_auto()(x)
      }
    ) +
    ggplot2::labs(
      title    = title_str,
      subtitle = sprintf("CVEGEO: %s", cvegeo),
      x        = NULL,
      y        = NULL,
      caption  = paste0(
        "Precio: doble promedio Shaun (estaci\u00f3n\u2192mun\u00d7d\u00eda\u2192mun\u00d7mes). ",
        "Share: vol premium / (vol premium + vol regular)."
      )
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 11),
      plot.subtitle = ggplot2::element_text(color = "grey50", size = 8),
      strip.text    = ggplot2::element_text(face = "bold", size = 9),
      plot.caption  = ggplot2::element_text(color = "grey60", size = 7),
      panel.grid.minor = ggplot2::element_blank()
    )

  gg
}

write_shaun_graphs <- function(
  mun_month_parquet,
  out_pdf  = "outputs/shaun/mun_month_graphs.pdf",
  out_dir  = "outputs/shaun/graphs_png"
) {
  dir.create(dirname(out_pdf),  recursive = TRUE, showWarnings = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  df <- arrow::read_parquet(mun_month_parquet) |>
    dplyr::arrange(CVEGEO, year, month)

  # Only municipalities with at least one non-NA ratio
  df <- df |>
    dplyr::group_by(CVEGEO) |>
    dplyr::filter(
      any(!is.na(premium_to_regular_price_ratio)) |
      any(!is.na(premium_share))
    ) |>
    dplyr::ungroup()

  muns <- df |>
    dplyr::distinct(CVEGEO, NOM_MUN, NOM_ENT) |>
    dplyr::arrange(CVEGEO)

  n_muns <- nrow(muns)
  message(sprintf("write_shaun_graphs: generating %d municipality graphs", n_muns))

  # ---- Multi-page PDF ----
  grDevices::pdf(out_pdf, width = 8, height = 6, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)

  png_paths <- character(0)

  for (i in seq_len(n_muns)) {
    cvegeo  <- muns$CVEGEO[i]
    nom_mun <- muns$NOM_MUN[i]
    nom_ent <- muns$NOM_ENT[i]

    df_mun <- df |> dplyr::filter(CVEGEO == cvegeo)
    gg     <- .make_mun_graph(df_mun, cvegeo, nom_mun, nom_ent)
    if (is.null(gg)) next

    # Add to PDF
    print(gg)

    # Write individual PNG
    safe_name <- gsub("[^A-Za-z0-9_]", "_",
                      paste0(cvegeo, "_", nom_mun %||% cvegeo))
    png_path  <- file.path(out_dir, paste0(safe_name, ".png"))
    ggplot2::ggsave(png_path, plot = gg, width = 8, height = 6, dpi = 150)
    png_paths <- c(png_paths, png_path)

    if (i %% 100 == 0L) {
      message(sprintf("  ... %d / %d municipalities done", i, n_muns))
    }
  }

  message(sprintf("PDF written:  %s", out_pdf))
  message(sprintf("PNGs written: %d files in %s", length(png_paths), out_dir))

  # Return a small flag-file so targets can track this as format="file"
  flag <- file.path(dirname(out_pdf), ".graphs_done")
  writeLines(
    c(sprintf("pdf=%s", out_pdf),
      sprintf("n_graphs=%d", length(png_paths)),
      sprintf("when=%s", Sys.time())),
    flag
  )
  flag
}

# ---------------------------------------------------------------------------
# Internal: null coalesce operator (avoids rlang dependency)
# ---------------------------------------------------------------------------

`%||%` <- function(x, y) if (!is.null(x) && !all(is.na(x))) x else y
