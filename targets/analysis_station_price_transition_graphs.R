suppressPackageStartupMessages({
  library(targets)
})

analysis_station_price_transition_graphs <- function() {
  list(
    tar_target(
      station_price_transition_graph_files,
      {
        source("R/analysis_station_price_transition_graphs.R")

        build_all_transition_matrix_graphs(
          parquet_files = station_price_transition_files,
          out_dir_base = "outputs/station_price_transition_graphs"
        )
      },
      format = "file"
    )
  )
}
