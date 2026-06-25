suppressPackageStartupMessages({
  library(targets)
})

analysis_aggregations <- function() {
  list(

    tar_target(
      daily_cvegeo_parquets,
      {
        spreads_station_day_parquets

        years_vec <- 2017:2025
        vapply(
          years_vec,
          function(yy) {
            in_file <- sprintf(
              "data/analysis/spreads_station_day/year=%d/spreads_station_day.parquet",
              as.integer(yy)
            )
            agg_daily_cvegeo_one_year(
              in_parquet = in_file,
              year = as.integer(yy),
              out_dir = "data/analysis/daily_cvegeo"
            )
          },
          FUN.VALUE = character(1)
        )
      },
      format = "file",
      iteration = "list"
    ),

    tar_target(
      prepost_cvegeo_parquet_1m,
      {
        daily_cvegeo_parquets
        agg_prepost_cvegeo_from_daily(
          daily_cvegeo_files = daily_cvegeo_parquets,
          out_path = "data/analysis/prepost_cvegeo/window=1/prepost_cvegeo.parquet",
          window_months = 1L
        )
      },
      format = "file"
    ),

    tar_target(
      prepost_station_spreads_parquet_1m,
      {
        spreads_station_day_parquets
        agg_prepost_station_spreads(
          spreads_station_day_files = spreads_station_day_parquets,
          out_path = "data/analysis/prepost_station_spreads/window=1/prepost_station_spreads.parquet",
          window_months = 1L
        )
      },
      format = "file"
    ),

    tar_target(
      prepost_station_prices_parquet_1m,
      {
        spreads_station_day_parquets
        agg_prepost_station_prices(
          spreads_station_day_files = spreads_station_day_parquets,
          out_path = "data/analysis/prepost_station_prices/window=1/prepost_station_prices.parquet",
          window_months = 1L
        )
      },
      format = "file"
    ),

    tar_target(
      station_regular_quantiles_parquet_1m,
      {
        spreads_station_day_parquets
        agg_station_price_quantiles(
          spreads_station_day_files = spreads_station_day_parquets,
          price_var = "station_regular",
          out_path = "data/analysis/station_quantiles/window=1/station_regular_quantiles.parquet",
          window_months = 1L
        )
      },
      format = "file"
    ),

    tar_target(
      station_premium_quantiles_parquet_1m,
      {
        spreads_station_day_parquets
        agg_station_price_quantiles(
          spreads_station_day_files = spreads_station_day_parquets,
          price_var = "station_premium",
          out_path = "data/analysis/station_quantiles/window=1/station_premium_quantiles.parquet",
          window_months = 1L
        )
      },
      format = "file"
    ),

    tar_target(
      station_diesel_quantiles_parquet_1m,
      {
        spreads_station_day_parquets
        agg_station_price_quantiles(
          spreads_station_day_files = spreads_station_day_parquets,
          price_var = "station_diesel",
          out_path = "data/analysis/station_quantiles/window=1/station_diesel_quantiles.parquet",
          window_months = 1L
        )
      },
      format = "file"
    ),

    tar_target(
      prepost_cvegeo_parquet_3m,
      {
        daily_cvegeo_parquets
        agg_prepost_cvegeo_from_daily(
          daily_cvegeo_files = daily_cvegeo_parquets,
          out_path = "data/analysis/prepost_cvegeo/window=3/prepost_cvegeo.parquet",
          window_months = 3L
        )
      },
      format = "file"
    ),

    tar_target(
      prepost_station_spreads_parquet_3m,
      {
        spreads_station_day_parquets
        agg_prepost_station_spreads(
          spreads_station_day_files = spreads_station_day_parquets,
          out_path = "data/analysis/prepost_station_spreads/window=3/prepost_station_spreads.parquet",
          window_months = 3L
        )
      },
      format = "file"
    ),

    tar_target(
      prepost_station_prices_parquet_3m,
      {
        spreads_station_day_parquets
        agg_prepost_station_prices(
          spreads_station_day_files = spreads_station_day_parquets,
          out_path = "data/analysis/prepost_station_prices/window=3/prepost_station_prices.parquet",
          window_months = 3L
        )
      },
      format = "file"
    ),

    tar_target(
      station_regular_quantiles_parquet_3m,
      {
        spreads_station_day_parquets
        agg_station_price_quantiles(
          spreads_station_day_files = spreads_station_day_parquets,
          price_var = "station_regular",
          out_path = "data/analysis/station_quantiles/window=3/station_regular_quantiles.parquet",
          window_months = 3L
        )
      },
      format = "file"
    ),

    tar_target(
      station_premium_quantiles_parquet_3m,
      {
        spreads_station_day_parquets
        agg_station_price_quantiles(
          spreads_station_day_files = spreads_station_day_parquets,
          price_var = "station_premium",
          out_path = "data/analysis/station_quantiles/window=3/station_premium_quantiles.parquet",
          window_months = 3L
        )
      },
      format = "file"
    ),

    tar_target(
      station_diesel_quantiles_parquet_3m,
      {
        spreads_station_day_parquets
        agg_station_price_quantiles(
          spreads_station_day_files = spreads_station_day_parquets,
          price_var = "station_diesel",
          out_path = "data/analysis/station_quantiles/window=3/station_diesel_quantiles.parquet",
          window_months = 3L
        )
      },
      format = "file"
    ),

    tar_target(
      prepost_cvegeo_parquet_6m,
      {
        daily_cvegeo_parquets
        agg_prepost_cvegeo_from_daily(
          daily_cvegeo_files = daily_cvegeo_parquets,
          out_path = "data/analysis/prepost_cvegeo/window=6/prepost_cvegeo.parquet",
          window_months = 6L
        )
      },
      format = "file"
    ),

    tar_target(
      prepost_station_spreads_parquet_6m,
      {
        spreads_station_day_parquets
        agg_prepost_station_spreads(
          spreads_station_day_files = spreads_station_day_parquets,
          out_path = "data/analysis/prepost_station_spreads/window=6/prepost_station_spreads.parquet",
          window_months = 6L
        )
      },
      format = "file"
    ),

    tar_target(
      prepost_station_prices_parquet_6m,
      {
        spreads_station_day_parquets
        agg_prepost_station_prices(
          spreads_station_day_files = spreads_station_day_parquets,
          out_path = "data/analysis/prepost_station_prices/window=6/prepost_station_prices.parquet",
          window_months = 6L
        )
      },
      format = "file"
    ),

    tar_target(
      station_regular_quantiles_parquet_6m,
      {
        spreads_station_day_parquets
        agg_station_price_quantiles(
          spreads_station_day_files = spreads_station_day_parquets,
          price_var = "station_regular",
          out_path = "data/analysis/station_quantiles/window=6/station_regular_quantiles.parquet",
          window_months = 6L
        )
      },
      format = "file"
    ),

    tar_target(
      station_premium_quantiles_parquet_6m,
      {
        spreads_station_day_parquets
        agg_station_price_quantiles(
          spreads_station_day_files = spreads_station_day_parquets,
          price_var = "station_premium",
          out_path = "data/analysis/station_quantiles/window=6/station_premium_quantiles.parquet",
          window_months = 6L
        )
      },
      format = "file"
    ),

    tar_target(
      station_diesel_quantiles_parquet_6m,
      {
        spreads_station_day_parquets
        agg_station_price_quantiles(
          spreads_station_day_files = spreads_station_day_parquets,
          price_var = "station_diesel",
          out_path = "data/analysis/station_quantiles/window=6/station_diesel_quantiles.parquet",
          window_months = 6L
        )
      },
      format = "file"
    )
  )
}
