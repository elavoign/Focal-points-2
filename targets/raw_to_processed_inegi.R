# targets/raw_to_processed_inegi.R

raw_to_processed_inegi <- function() {
  list(

    tar_target(
      inegi_censo_parquet,
      process_inegi_censo(),
      format = "file"
    )

  )
}