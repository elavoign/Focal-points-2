suppressPackageStartupMessages({
  library(targets)
})

shaun_mun_month <- function() {
  list(

    tar_target(
      balanced_panel_parquets,
      {

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

    tar_target(
      mun_month_prices_parquet,
      {
        balanced_panel_parquets
        volumes_mun_month_parquet
        compute_mun_month_prices(
          balanced_panel_files = balanced_panel_parquets,
          out_path             = "data/analysis/mun_month_prices/mun_month_prices.parquet",
          volumes_file         = volumes_mun_month_parquet
        )
      },
      format = "file"
    ),

    tar_target(
      mun_month_excel,
      {
        mun_month_prices_parquet
        write_shaun_excel(
          mun_month_parquet = mun_month_prices_parquet,
          out_xlsx          = "outputs/shaun/mun_month_ratios.xlsx"
        )
      },
      format = "file"
    ),

    tar_target(
      mun_month_graphs,
      {
        mun_month_prices_parquet
        write_shaun_graphs(
          mun_month_parquet = mun_month_prices_parquet,
          out_pdf           = "outputs/shaun/mun_month_graphs.pdf",
          out_dir           = "outputs/shaun/graphs_png"
        )
      },
      format = "file"
    ),

    tar_target(
      mun_month_poverty_parquet,
      {
        mun_month_prices_parquet
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
