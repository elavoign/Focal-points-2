# R/Graphs/inegi_graphs.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(arrow)
  library(scales)
})

plot_inegi_bars <- function(
  in_parquet = "data/processed/inegi_censo/inegi_censo.parquet",
  out_path = "outputs/graphs/inegi/inegi_income_expenses.png"
) {

  df <- arrow::read_parquet(in_parquet)

  # =========================
  # INCOME
  # =========================
  income <- df |>
    transmute(
      year,
      Resale = m010a,
      Services = m020a,
      Products = m030a,
      Rent = m050a,
      Maquila = m700a,
      Other = m090a
    ) |>
    pivot_longer(-year, names_to = "category", values_to = "value") |>
    mutate(type = "Income")

  # =========================
  # EXPENSES
  # =========================
  expenses <- df |>
    transmute(
      year,
      Wages = j000a,
      Merchandise = k010a,
      Inputs = k020a,
      Raw_materials = k030a,
      Fuels = k042a,
      Electricity = k412a,
      Rent = k050a,
      Professional_services = k060a,
      Maquila = k070a,
      Freight = k096a,
      Maintenance = k950a,
      Other = k090a
    ) |>
    pivot_longer(-year, names_to = "category", values_to = "value") |>
    mutate(type = "Expenses")

  df_plot <- bind_rows(income, expenses)

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

  p <- ggplot(df_plot, aes(x = factor(year), y = value, fill = category)) +
    geom_col() +
    facet_wrap(~type) +
    scale_y_continuous(
      limits = c(0, 100000000),
      labels = scales::label_number(big.mark = ",", accuracy = 1)
    ) +
    labs(
      title = "Income and Expenses by Year (INEGI)",
      x = "Year",
      y = "Million pesos",
      fill = "Category",
      caption = "Note: Both panels use the same y-axis scale. Values are reported in current million pesos."
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0),
      legend.position = "right",
      plot.caption = element_text(hjust = 0, size = 9)
    )

  ggsave(out_path, p, width = 10, height = 6)

  return(out_path)
}