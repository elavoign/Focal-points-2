suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(ggplot2)
  library(purrr)
  library(stringr)
  library(scales)
  library(tidyr)
})

build_transition_plot_data <- function(df_transitions) {
  quantile_levels <- c("0-25", "25-50", "50-75", "75-100")

  df_transitions |>
    group_by(
      estado,
      producto,
      quantile_label_pre,
      quantile_label_post
    ) |>
    summarise(
      n_stations = n(),
      avg_pct_change_in_cell = mean(pct_change_price, na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(estado, producto, quantile_label_pre) |>
    mutate(
      pct_stations = n_stations / sum(n_stations)
    ) |>
    ungroup() |>
    tidyr::complete(
      estado,
      producto,
      quantile_label_pre = quantile_levels,
      quantile_label_post = quantile_levels,
      fill = list(
        n_stations = 0,
        pct_stations = 0,
        avg_pct_change_in_cell = NA_real_
      )
    ) |>
    mutate(
      quantile_label_pre = factor(quantile_label_pre, levels = rev(quantile_levels)),
      quantile_label_post = factor(quantile_label_post, levels = quantile_levels),
      cell_label = case_when(
        n_stations == 0 ~ "0.0%\nΔ%: —",
        TRUE ~ paste0(
          scales::percent(pct_stations, accuracy = 0.1),
          "\nΔ%: ",
          ifelse(
            is.na(avg_pct_change_in_cell),
            "—",
            paste0(format(round(avg_pct_change_in_cell, 1), nsmall = 1), "%")
          )
        )
      )
    )
}

plot_transition_matrix_one <- function(df_plot, estado_i, producto_i, window_months_i, out_file) {
  plot_df <- df_plot |>
    filter(estado == estado_i, producto == producto_i)

  estado_label <- estado_i |>
    stringr::str_replace_all('"', "") |>
    stringr::str_squish()

  producto_label <- dplyr::case_when(
    producto_i == "regular" ~ "regular",
    producto_i == "premium" ~ "premium",
    producto_i == "diesel" ~ "diesel",
    TRUE ~ producto_i
  )

  p <- ggplot(
    plot_df,
    aes(x = quantile_label_post, y = quantile_label_pre, fill = pct_stations)
  ) +
    geom_tile() +
    geom_text(aes(label = cell_label), size = 3, lineheight = 0.95) +
    scale_fill_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = paste("Transition matrix -", estado_label),
      subtitle = paste("Product:", producto_label, "| Window:", window_months_i, "month(s)"),
      x = "Post quartile",
      y = "Pre quartile",
      fill = "% within pre quartile",
      caption = "% = share of the cell within the pre quartile | Δ% = average percentage price change in the cell"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold")
    )

  ggsave(
    filename = out_file,
    plot = p,
    width = 7.6,
    height = 5.8,
    dpi = 300
  )

  out_file
}

build_transition_matrix_graphs_for_window <- function(
  parquet_file,
  out_dir_base = "outputs/station_price_transition_graphs"
) {
  df_transitions <- arrow::read_parquet(parquet_file, mmap = FALSE)

  window_months_i <- unique(df_transitions$window_months)
  if (length(window_months_i) != 1) {
    stop("The parquet file must contain a single window.")
  }

  window_months_i <- as.integer(window_months_i[[1]])

  out_dir <- file.path(out_dir_base, sprintf("window_%sm", window_months_i))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  df_plot <- build_transition_plot_data(df_transitions)

  combos <- df_plot |>
    distinct(estado, producto) |>
    arrange(producto, estado)

  purrr::map2_chr(
    combos$estado,
    combos$producto,
    function(estado_i, producto_i) {
      safe_estado <- estado_i |>
        iconv(from = "", to = "ASCII//TRANSLIT") |>
        stringr::str_replace_all("[^[:alnum:]]+", "_") |>
        stringr::str_squish()

      safe_producto <- stringr::str_replace_all(producto_i, "[^[:alnum:]]+", "_")

      out_file <- file.path(
        out_dir,
        sprintf("transition_matrix_%sm_%s_%s.png", window_months_i, safe_producto, safe_estado)
      )

      plot_transition_matrix_one(
        df_plot = df_plot,
        estado_i = estado_i,
        producto_i = producto_i,
        window_months_i = window_months_i,
        out_file = out_file
      )
    }
  )
}

build_all_transition_matrix_graphs <- function(
  parquet_files,
  out_dir_base = "outputs/station_price_transition_graphs"
) {
  purrr::map(
    parquet_files,
    build_transition_matrix_graphs_for_window,
    out_dir_base = out_dir_base
  ) |>
    unlist(use.names = FALSE)
}
