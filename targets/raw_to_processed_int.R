# targets/raw_to_processed_int.R

suppressPackageStartupMessages({
  library(targets)
})

raw_to_processed_int <- function() {
  list(
    tar_target(
      international_regular_xls,
      "data/raw_public/international_prices/Regular_Dolars_per_Galon.xls",
      format = "file"
    ),
    tar_target(
      international_diesel_xls,
      "data/raw_public/international_prices/Disel_Dolars_per_Galon.xls",
      format = "file"
    ),
    tar_target(
      international_fx_xls,
      "data/raw_public/international_prices/Tipo_de_Cambio.xls",
      format = "file"
    ),

    # Construye la serie diaria ya completa (con LOCF en regular/diesel) y convertida
    tar_target(
      international_daily,
      build_international_daily(
        path_regular = international_regular_xls,
        path_diesel  = international_diesel_xls,
        path_fx      = international_fx_xls
      )
    ),

    # Escribe 1 parquet por año y devuelve tibble(year, path)
    tar_target(
      international_parquet_paths,
      write_international_parquets_by_year(
        df_international = international_daily,
        out_dir = "data/processed/international"
      )$path,
    format = "file"
    )
  )
}
