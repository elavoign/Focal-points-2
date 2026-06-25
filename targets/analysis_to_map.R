suppressPackageStartupMessages({
  library(targets)
})

analysis_to_map <- function() {

  list(

    tar_target(
      inegi_mg_2024_zip,
      inegi_mg_2024_zip_path(),
      format = "rds"
    ),

    tar_target(
      inegi_mg_2024_unzipped_done,
      ensure_unzipped_inegi_mg_2024(zip_path = inegi_mg_2024_zip),
      format = "file"
    ),

    tar_target(
      inegi_municipios_geo,
      build_municipios_geo_mg2024(unzip_done_file = inegi_mg_2024_unzipped_done),
      format = "file"
    ),

    tar_target(
      inegi_estados_geo,
      build_estados_geo_mg2024(unzip_done_file = inegi_mg_2024_unzipped_done),
      format = "file"
    ),

    tar_target(
      inegi_municipios_lookup,
      build_municipios_lookup_mg2024(municipios_geo_file = inegi_municipios_geo),
      format = "file"
    )
  )
}
