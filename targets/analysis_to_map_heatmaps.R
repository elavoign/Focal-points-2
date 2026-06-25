suppressPackageStartupMessages({
  library(targets)
})

analysis_to_map_heatmaps <- function() {
  list(
    tar_target(
      script_spread_heatmaps,
      "R/Map/spread_heatmaps_functions.R",
      format = "file"
    ),

    tar_target(
      municipios_geoparquet,
      inegi_municipios_geo,
      format = "file"
    ),

    tar_target(
      cvegeo_spread_maps_png_1m,
      {
        source(script_spread_heatmaps, local = TRUE)

        prepost_cvegeo_parquet_1m
        municipios_geoparquet

        make_maps_cvegeo_all_spreads(
          prepost_cvegeo_parquet = prepost_cvegeo_parquet_1m,
          municipios_geoparquet  = municipios_geoparquet,
          out_dir = "outputs/maps/window=1/cvegeo_spreads",
          window_months = 1L
        )
      },
      format = "file",
      iteration = "list"
    ),

    tar_target(
      cvegeo_spread_maps_png_3m,
      {
        source(script_spread_heatmaps, local = TRUE)

        prepost_cvegeo_parquet_3m
        municipios_geoparquet

        make_maps_cvegeo_all_spreads(
          prepost_cvegeo_parquet = prepost_cvegeo_parquet_3m,
          municipios_geoparquet  = municipios_geoparquet,
          out_dir = "outputs/maps/window=3/cvegeo_spreads",
          window_months = 3L
        )
      },
      format = "file",
      iteration = "list"
    ),

    tar_target(
      cvegeo_spread_maps_png_6m,
      {
        source(script_spread_heatmaps, local = TRUE)

        prepost_cvegeo_parquet_6m
        municipios_geoparquet

        make_maps_cvegeo_all_spreads(
          prepost_cvegeo_parquet = prepost_cvegeo_parquet_6m,
          municipios_geoparquet  = municipios_geoparquet,
          out_dir = "outputs/maps/window=6/cvegeo_spreads",
          window_months = 6L
        )
      },
      format = "file",
      iteration = "list"
    ),

    tar_target(
      heatmaps_outputs_files,
      {

        force(cvegeo_spread_maps_png_1m)
        force(cvegeo_spread_maps_png_3m)
        force(cvegeo_spread_maps_png_6m)

        if (!dir.exists("outputs/maps")) {
          stop("No existe la carpeta outputs/maps (desde el working directory del pipeline).")
        }

        files <- list.files(
          "outputs/maps",
          recursive = TRUE,
          full.names = TRUE
        )

        files <- files[grepl("\\.(png|jpg|jpeg|webp|tif|tiff)$", files, ignore.case = TRUE)]
        sort(unique(files))
      },
      format = "file"
    )
  )
}
