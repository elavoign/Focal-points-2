# R/Graphs/inegi_ebitda_graphs.R

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(arrow)
  library(scales)
  library(stringr)
})

plot_inegi_ebitda_panels <- function(
  in_parquet,
  out_dir = "outputs/graphs/inegi_ebitda"
) {

  # =========================
  # 1. Read data
  # =========================
  df <- arrow::read_parquet(in_parquet)

  # =========================
  # 2. Clean sample
  # =========================
  df <- df |>
    filter(
      !is.na(year),
      !is.na(entidad),
      !is.na(ebitda),
      !is.na(ebitda_revenue)
    ) |>
    mutate(
      entidad = str_squish(as.character(entidad))
    ) |>
    arrange(entidad, year)

  # =========================
  # 3. Manual north-to-south order
  # =========================
  state_order <- c(
    "00 Total nacional",
    "02 Baja California",
    "03 Baja California Sur",
    "26 Sonora",
    "08 Chihuahua",
    "05 Coahuila de Zaragoza",
    "19 Nuevo León",
    "28 Tamaulipas",
    "25 Sinaloa",
    "10 Durango",
    "32 Zacatecas",
    "24 San Luis Potosí",
    "18 Nayarit",
    "01 Aguascalientes",
    "14 Jalisco",
    "06 Colima",
    "11 Guanajuato",
    "22 Querétaro",
    "13 Hidalgo",
    "16 Michoacán de Ocampo",
    "15 México",
    "09 Ciudad de México",
    "29 Tlaxcala",
    "17 Morelos",
    "21 Puebla",
    "30 Veracruz de Ignacio de la Llave",
    "12 Guerrero",
    "20 Oaxaca",
    "07 Chiapas",
    "27 Tabasco",
    "04 Campeche",
    "31 Yucatán",
    "23 Quintana Roo"
  )

  matched_states <- state_order[state_order %in% unique(df$entidad)]

  print(sort(unique(df$entidad)))
  print(matched_states)

  df <- df |>
    filter(entidad %in% matched_states) |>
    mutate(
      entidad = factor(entidad, levels = matched_states),
      line_color = if_else(
        as.character(entidad) == "00 Total nacional",
        "Total nacional",
        "States"
      )
    ) |>
    arrange(entidad, year)

  if (nrow(df) == 0) {
    stop("No rows left after matching entidad names with state_order.")
  }

  # =========================
  # 4. Fixed y-axis limits
  # =========================
  ebitda_min <- 0
  ebitda_max <- 150000

  ratio_min <- 0
  ratio_max <- 0.7

  # =========================
  # 5. Create output folder
  # =========================
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # =========================
  # 6. EBITDA plot
  # =========================
  p_ebitda <- ggplot(
    df,
    aes(x = year, y = ebitda, group = 1, color = line_color)
  ) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 1.2) +
    facet_wrap(~ entidad) +
    scale_color_manual(
      values = c("Total nacional" = "red", "States" = "black"),
      guide = "none"
    ) +
    scale_x_continuous(
      breaks = sort(unique(df$year))
    ) +
    scale_y_continuous(
      limits = c(ebitda_min, ebitda_max),
      labels = label_number(big.mark = ",", decimal.mark = ".")
    ) +
    labs(
      title = "EBITDA proxy by state",
      subtitle = "EBITDA proxy = gross census value added - compensation",
      x = "Year",
      y = "Million pesos"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.text = element_text(size = 8, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )

  file_ebitda <- file.path(out_dir, "ebitda_by_state_facets.png")

  ggsave(
    filename = file_ebitda,
    plot = p_ebitda,
    width = 16,
    height = 12,
    dpi = 300
  )

  # =========================
  # 7. EBITDA / Revenue plot
  # =========================
  p_ratio <- ggplot(
    df,
    aes(x = year, y = ebitda_revenue, group = 1, color = line_color)
  ) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 1.2) +
    facet_wrap(~ entidad) +
    scale_color_manual(
      values = c("Total nacional" = "red", "States" = "black"),
      guide = "none"
    ) +
    scale_x_continuous(
      breaks = sort(unique(df$year))
    ) +
    scale_y_continuous(
      limits = c(ratio_min, ratio_max),
      labels = label_number(accuracy = 0.01)
    ) +
    labs(
      title = "EBITDA / Revenue by state",
      subtitle = "EBITDA proxy over revenue from goods and services",
      x = "Year",
      y = "Ratio"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.text = element_text(size = 8, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )

  file_ratio <- file.path(out_dir, "ebitda_revenue_by_state_facets.png")

  ggsave(
    filename = file_ratio,
    plot = p_ratio,
    width = 16,
    height = 12,
    dpi = 300
  )

  return(c(file_ebitda, file_ratio))
}