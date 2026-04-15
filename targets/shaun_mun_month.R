# targets/shaun_mun_month.R
#
# Targets factory for Shaun's municipality x month price analysis.
# Adds two new layers to the pipeline (non-destructive — does not touch
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
#     Also contains: premium_to_regular_price_ratio.
#     Volume columns (premium_volume, regular_volume, premium_share) are NA
#     pending external sales-volume data.
#     Output: data/analysis/mun_month_prices/mun_month_prices.parquet

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
    # Layer 2: Municipality x month prices (double aggregation)
    # ------------------------------------------------------------------
    tar_target(
      mun_month_prices_parquet,
      {
        balanced_panel_parquets  # explicit dependency
        compute_mun_month_prices(
          balanced_panel_files = balanced_panel_parquets,
          out_path = "data/analysis/mun_month_prices/mun_month_prices.parquet"
        )
      },
      format = "file"
    )

  )
}
