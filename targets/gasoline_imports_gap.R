# targets/gasoline_imports_gap.R
#
# Targets factory: exploratory U.S.–Mexico gasoline import gap.
# Depends on no other target — reads raw_public files directly.

suppressPackageStartupMessages(library(targets))

gasoline_imports_gap <- function() {
  list(
    tar_target(
      gasoline_imports_gap_parquet,
      process_gasoline_imports_gap(
        eia_csv     = "data/raw_public/U.S._Exports_to_Mexico_of_Finished_Motor_Gasoline.csv",
        out_parquet = "data/processed/imports_gap/gasoline_imports_gap.parquet",
        out_csv     = "data/processed/imports_gap/gasoline_imports_gap.csv",
        out_pdf     = "outputs/imports_gap/import_gap_plots.pdf",
        out_txt     = "outputs/imports_gap/interpretation_note.txt"
      ),
      format = "file"
    )
  )
}
