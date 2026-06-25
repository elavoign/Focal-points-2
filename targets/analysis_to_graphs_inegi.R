analysis_to_graphs_inegi <- function() {
  list(

    tar_target(
      inegi_graph,
      {
        inegi_censo_parquet

        old_files <- plot_inegi_bars(
          in_parquet = inegi_censo_parquet
        )

        new_files <- plot_inegi_ebitda_panels(
          in_parquet = inegi_censo_parquet
        )

        c(old_files, new_files)
      },
      format = "file"
    )

  )
}
