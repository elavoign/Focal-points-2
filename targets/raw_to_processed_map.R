raw_to_processed_map <- function() {

  list(
    tar_target(
        marco_geo_zip,
        {
        p <- "data/raw_public/inegi_mg_2024/794551163061_s.zip"
        if (!file.exists(p)) stop("Missing file: ", p)
        normalizePath(p, winslash = "/", mustWork = TRUE)
        }
    ),

    tar_target(
      municipios_geo,
      process_marco_geo(
        zip_path = marco_geo_zip,
        output_dir = "data/processed/geo"
      ),
      format = "file"
    )

  )
}
