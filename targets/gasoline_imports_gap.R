suppressPackageStartupMessages(library(targets))

gasoline_imports_gap <- function() {
  list(
    tar_target(
      gasoline_imports_gap_parquet,
      {
        eia_csv <- paste0(
          "data/raw_public/",
          "U.S._Exports_to_Mexico_of_Finished_Motor_Gasoline.csv"
        )
        process_gasoline_imports_gap(
          eia_csv     = eia_csv,
          out_parquet = "data/processed/imports_gap/gasoline_imports_gap.parquet",
          out_csv     = "data/processed/imports_gap/gasoline_imports_gap.csv",
          out_pdf     = "outputs/imports_gap/import_gap_plots.pdf",
          out_txt     = "outputs/imports_gap/interpretation_note.txt"
        )
      },
      format = "file"
    )
  )
}
