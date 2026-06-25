suppressPackageStartupMessages({
  library(targets)
  library(dplyr)
  library(readr)
})

ieps_targets <- function() {
  list(

    tar_target(
      ieps_url_single,
      "https://sidof.segob.gob.mx/notas/docFuente/5467667"
    ),

    tar_target(
      ieps_csv_single,
      ieps_html_to_csv(
        url = ieps_url_single,
        out_csv = "data/processed/ieps/ieps_single_5467667.csv"
      ),
      format = "file"
    ),

    tar_target(
      ieps_csv_single_qa,
      {
        df <- readr::read_csv(ieps_csv_single, show_col_types = FALSE)

        qa <- tibble::tibble(
          n_rows = nrow(df),
          n_cols = ncol(df),
          min_fecha = suppressWarnings(min(df$Fecha, na.rm = TRUE)),
          max_fecha = suppressWarnings(max(df$Fecha, na.rm = TRUE)),
          combustibles = paste(unique(df$Combustible), collapse = " | "),
          colnames = paste(names(df), collapse = ", ")
        )

        out <- "data/processed/ieps/ieps_single_5467667_QA.csv"
        dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
        readr::write_csv(qa, out)
        out
      },
      format = "file"
    )
  )
}
