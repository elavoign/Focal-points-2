# targets/analysis_regular_break_2399.R

suppressPackageStartupMessages({
  library(targets)
})

analysis_regular_break_2399 <- function() {
  list(
    tar_target(
      regular_break_2399_files,
      {
        source("R/analysis_regular_break_2399.R")

        build_regular_break_2399_outputs(
          years = 2025,
          threshold = 23.99,
          out_dir = "outputs/regular_break_2399"
        )
      },
      format = "file"
    )
  )
}