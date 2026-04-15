suppressPackageStartupMessages({
  library(targets)
})

analysis_to_graphs <- function() {
  list(
    tar_target(
      script_graph_functions,
      "R/Graphs/graphs_outputs_functions.R",
      format = "file"
    ),

    # (1) Series nacionales (Regular y Diesel) - no dependen de la ventana
    tar_target(
      national_price_graphs_png,
      {
        source(script_graph_functions)

        daily_cvegeo_parquets

        make_national_price_timeseries(
          daily_cvegeo_files = daily_cvegeo_parquets,
          out_dir = "outputs/graphs/national_prices"
        )
      },
      format = "file",
      iteration = "list"
    ),

    # -----------------------------
    # 1 mes
    # -----------------------------

    tar_target(
      station_spread_distributions_png_1m,
      {
        source(script_graph_functions)

        prepost_station_spreads_parquet_1m

        make_station_spread_distributions_all(
          prepost_station_parquet = prepost_station_spreads_parquet_1m,
          out_dir = "outputs/graphs/window=1/station_spreads",
          window_months = 1L
        )
      },
      format = "file",
      iteration = "list"
    ),

    tar_target(
      station_price_distributions_png_1m,
      {
        source(script_graph_functions)

        prepost_station_prices_parquet_1m

        make_station_price_distributions_all(
          prepost_station_price_parquet = prepost_station_prices_parquet_1m,
          out_dir = "outputs/graphs/window=1/station_prices",
          window_months = 1L
        )
      },
      format = "file",
      iteration = "list"
    ),

    tar_target(
      station_regular_quantile_overlays_png_1m,
      {
        source(script_graph_functions)

        station_regular_quantiles_parquet_1m

        make_station_price_quantile_overlays(
          station_quantiles_parquet = station_regular_quantiles_parquet_1m,
          out_dir = "outputs/graphs/window=1/station_price_quantiles/regular",
          window_months = 1L
        )
      },
      format = "file",
      iteration = "list"
    ),

    tar_target(
      station_premium_quantile_overlays_png_1m,
      {
        source(script_graph_functions)

        station_premium_quantiles_parquet_1m

        make_station_price_quantile_overlays(
          station_quantiles_parquet = station_premium_quantiles_parquet_1m,
          out_dir = "outputs/graphs/window=1/station_price_quantiles/premium",
          window_months = 1L
        )
      },
      format = "file",
      iteration = "list"
    ),

    tar_target(
      station_diesel_quantile_overlays_png_1m,
      {
        source(script_graph_functions)

        station_diesel_quantiles_parquet_1m

        make_station_price_quantile_overlays(
          station_quantiles_parquet = station_diesel_quantiles_parquet_1m,
          out_dir = "outputs/graphs/window=1/station_price_quantiles/diesel",
          window_months = 1L
        )
      },
      format = "file",
      iteration = "list"
    ),

    # -----------------------------
    # 3 meses
    # -----------------------------

    tar_target(
      station_spread_distributions_png_3m,
      {
        source(script_graph_functions)

        prepost_station_spreads_parquet_3m

        make_station_spread_distributions_all(
          prepost_station_parquet = prepost_station_spreads_parquet_3m,
          out_dir = "outputs/graphs/window=3/station_spreads",
          window_months = 3L
        )
      },
      format = "file",
      iteration = "list"
    ),

    tar_target(
      station_price_distributions_png_3m,
      {
        source(script_graph_functions)

        prepost_station_prices_parquet_3m

        make_station_price_distributions_all(
          prepost_station_price_parquet = prepost_station_prices_parquet_3m,
          out_dir = "outputs/graphs/window=3/station_prices",
          window_months = 3L
        )
      },
      format = "file",
      iteration = "list"
    ),

    tar_target(
      station_regular_quantile_overlays_png_3m,
      {
        source(script_graph_functions)

        station_regular_quantiles_parquet_3m

        make_station_price_quantile_overlays(
          station_quantiles_parquet = station_regular_quantiles_parquet_3m,
          out_dir = "outputs/graphs/window=3/station_price_quantiles/regular",
          window_months = 3L
        )
      },
      format = "file",
      iteration = "list"
    ),

    tar_target(
      station_premium_quantile_overlays_png_3m,
      {
        source(script_graph_functions)

        station_premium_quantiles_parquet_3m

        make_station_price_quantile_overlays(
          station_quantiles_parquet = station_premium_quantiles_parquet_3m,
          out_dir = "outputs/graphs/window=3/station_price_quantiles/premium",
          window_months = 3L
        )
      },
      format = "file",
      iteration = "list"
    ),

    tar_target(
      station_diesel_quantile_overlays_png_3m,
      {
        source(script_graph_functions)

        station_diesel_quantiles_parquet_3m

        make_station_price_quantile_overlays(
          station_quantiles_parquet = station_diesel_quantiles_parquet_3m,
          out_dir = "outputs/graphs/window=3/station_price_quantiles/diesel",
          window_months = 3L
        )
      },
      format = "file",
      iteration = "list"
    ),

    # -----------------------------
    # 6 meses
    # -----------------------------

    tar_target(
      station_spread_distributions_png_6m,
      {
        source(script_graph_functions)

        prepost_station_spreads_parquet_6m

        make_station_spread_distributions_all(
          prepost_station_parquet = prepost_station_spreads_parquet_6m,
          out_dir = "outputs/graphs/window=6/station_spreads",
          window_months = 6L
        )
      },
      format = "file",
      iteration = "list"
    ),

    tar_target(
      station_price_distributions_png_6m,
      {
        source(script_graph_functions)

        prepost_station_prices_parquet_6m

        make_station_price_distributions_all(
          prepost_station_price_parquet = prepost_station_prices_parquet_6m,
          out_dir = "outputs/graphs/window=6/station_prices",
          window_months = 6L
        )
      },
      format = "file",
      iteration = "list"
    ),

    tar_target(
      station_regular_quantile_overlays_png_6m,
      {
        source(script_graph_functions)

        station_regular_quantiles_parquet_6m

        make_station_price_quantile_overlays(
          station_quantiles_parquet = station_regular_quantiles_parquet_6m,
          out_dir = "outputs/graphs/window=6/station_price_quantiles/regular",
          window_months = 6L
        )
      },
      format = "file",
      iteration = "list"
    ),

    tar_target(
      station_premium_quantile_overlays_png_6m,
      {
        source(script_graph_functions)

        station_premium_quantiles_parquet_6m

        make_station_price_quantile_overlays(
          station_quantiles_parquet = station_premium_quantiles_parquet_6m,
          out_dir = "outputs/graphs/window=6/station_price_quantiles/premium",
          window_months = 6L
        )
      },
      format = "file",
      iteration = "list"
    ),

    tar_target(
      station_diesel_quantile_overlays_png_6m,
      {
        source(script_graph_functions)

        station_diesel_quantiles_parquet_6m

        make_station_price_quantile_overlays(
          station_quantiles_parquet = station_diesel_quantiles_parquet_6m,
          out_dir = "outputs/graphs/window=6/station_price_quantiles/diesel",
          window_months = 6L
        )
      },
      format = "file",
      iteration = "list"
    ),

    tar_target(
      graphs_outputs_files,
      {
        list.files(
          "outputs/graphs",
          recursive = TRUE,
          full.names = TRUE,
          pattern = "\\.(png|jpg|jpeg|webp|tif|tiff)$",
          ignore.case = TRUE
        ) |> sort()
      },
      format = "file"
    )
  )
}