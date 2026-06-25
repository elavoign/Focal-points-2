suppressPackageStartupMessages({
  library(targets)
})

ieps_range_targets <- function() {
  list(
    tar_target(ieps_id_from, 5676159L),
    tar_target(ieps_id_to,   5778057L),

    tar_target(ieps_pub_min, as.Date("2017-01-01")),
    tar_target(ieps_pub_max, as.Date(Sys.Date())),

    tar_target(ieps_range_out_dir, "data/processed/ieps/range_raw"),
    tar_target(ieps_candidates_csv_path, "data/processed/ieps/ieps_candidates.csv"),
    tar_target(ieps_range_combined_csv_path, "data/processed/ieps/ieps_range_combined.csv"),

    tar_target(
      ieps_range_combined_csv,
      ieps_scrape_range_filtered(
        id_from = ieps_id_from,
        id_to   = ieps_id_to,
        min_pub = ieps_pub_min,
        max_pub = ieps_pub_max,
        out_dir = ieps_range_out_dir,
        combined_csv = ieps_range_combined_csv_path,
        candidates_csv = ieps_candidates_csv_path,
        chunk_size = 300,
        max_sleep = 37,
        resume = TRUE
      ),
      format = "file"
    )
  )
}
