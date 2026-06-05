# targets/shaun_mun_month.R
#
# Targets factory for Shaun's municipality x month price + volume analysis.
# Adds five new layers to the pipeline (non-destructive — does not touch
# any existing targets):
#
#   balanced_panel_parquets
#     A balanced station x day panel for each year 2017-2025.
#     One row per (station_id, date) for EVERY calendar day in the year.
#     Missing prices filled forward (carry-forward) from the last observed
#     price; carry-forward values older than 60 days are set to NA.
#     Output: data/merged/balanced_panel/year=YYYY/balanced_panel.parquet
#
#   mun_month_prices_parquet
#     Municipality x month prices computed via the Shaun double-average:
#       1. average station prices -> municipality x day
#       2. average daily municipal prices -> municipality x month
#     Joins volumes to populate premium_volume, regular_volume, premium_share.
#     Output: data/analysis/mun_month_prices/mun_month_prices.parquet
#
#   volumes_mun_month_parquet
#     CRE/SENER gasoline volumes (Regular + Premium) by municipality x month,
#     with CVEGEO assigned via INEGI name matching.
#     Output: data/processed/volumes/mun_month_volumes.parquet
#
#   mun_month_excel
#     Excel workbook with both ratios per municipality-month.
#     Output: outputs/shaun/mun_month_ratios.xlsx
#
#   mun_month_graphs
#     One graph per municipality (PDF + individual PNGs).
#     Output: outputs/shaun/mun_month_graphs.pdf + outputs/shaun/graphs_png/

suppressPackageStartupMessages({
  library(targets)
})

shaun_mun_month <- function() {
  list(

    # ------------------------------------------------------------------
    # Layer 1: Balanced panel (station x day, carry-forward, 60-day cap)
    # ------------------------------------------------------------------
    tar_target(
      balanced_panel_parquets,
      {
        # Explicit dependency on the upstream merged panel
        panel_station_day_parquets

        years_vec <- 2017:2025
        vapply(
          years_vec,
          function(yy) build_balanced_panel_year(
            year    = as.integer(yy),
            in_dir  = "data/merged/panel_station_day",
            out_dir = "data/merged/balanced_panel"
          ),
          FUN.VALUE = character(1)
        )
      },
      format    = "file",
      iteration = "list"
    ),

    # ------------------------------------------------------------------
    # Layer 2: Municipality volumes (CRE/SENER, Regular + Premium)
    # ------------------------------------------------------------------
    tar_target(
      volumes_mun_month_parquet,
      {
        process_volumes(
          in_csv      = "data/raw_public/04_volumenes_venta_expendio_petroliferos.csv",
          mun_shp     = "data/map/inegi_mg_2024/unzipped/ONLY_MUNICIPIOS_00mun/00mun.shp",
          out_parquet = "data/processed/volumes/mun_month_volumes.parquet"
        )
      },
      format = "file"
    ),

    # ------------------------------------------------------------------
    # Layer 3: Municipality x month prices + volumes (double aggregation)
    # ------------------------------------------------------------------
    tar_target(
      mun_month_prices_parquet,
      {
        balanced_panel_parquets    # explicit dependency on balanced panel
        volumes_mun_month_parquet  # explicit dependency on volumes
        compute_mun_month_prices(
          balanced_panel_files = balanced_panel_parquets,
          out_path             = "data/analysis/mun_month_prices/mun_month_prices.parquet",
          volumes_file         = volumes_mun_month_parquet
        )
      },
      format = "file"
    ),

    # ------------------------------------------------------------------
    # Layer 4: Excel output
    # ------------------------------------------------------------------
    tar_target(
      mun_month_excel,
      {
        mun_month_prices_parquet  # explicit dependency
        write_shaun_excel(
          mun_month_parquet = mun_month_prices_parquet,
          out_xlsx          = "outputs/shaun/mun_month_ratios.xlsx"
        )
      },
      format = "file"
    ),

    # ------------------------------------------------------------------
    # Layer 5: Graphs (one per municipality, multi-page PDF + PNGs)
    # ------------------------------------------------------------------
    tar_target(
      mun_month_graphs,
      {
        mun_month_prices_parquet  # explicit dependency
        write_shaun_graphs(
          mun_month_parquet = mun_month_prices_parquet,
          out_pdf           = "outputs/shaun/mun_month_graphs.pdf",
          out_dir           = "outputs/shaun/graphs_png"
        )
      },
      format = "file"
    ),

    # ------------------------------------------------------------------
    # Layer 6: Municipality x month panel enriched with CONEVAL poverty
    # ------------------------------------------------------------------
    tar_target(
      mun_month_poverty_parquet,
      {
        mun_month_prices_parquet  # explicit dependency
        merge_poverty_into_mun_month(
          mun_month_parquet = mun_month_prices_parquet,
          poverty_parquet   = "data/processed/coneval/municipal_poverty_2020.parquet",
          out_path          = "data/analysis/mun_month_prices/mun_month_prices_with_poverty.parquet"
        )
      },
      format = "file"
    )

  )
}
