# R/Raw_to_Processed/process_terminal_year.R

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
})

parse_date_safe_legacy <- function(x) {
  if (inherits(x, "Date")) return(x)

  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_

  out <- rep(as.Date(NA), length(x))

  idx1 <- !is.na(x) & grepl("^\\d{4}-\\d{2}-\\d{2}", x)
  if (any(idx1)) out[idx1] <- as.Date(substr(x[idx1], 1, 10), "%Y-%m-%d")

  idx2 <- !is.na(x) & is.na(out) & grepl("^\\d{2}/\\d{2}/\\d{4}", x)
  if (any(idx2)) out[idx2] <- as.Date(substr(x[idx2], 1, 10), "%d/%m/%Y")

  idx3 <- !is.na(x) & is.na(out) & grepl("^\\d{4}/\\d{2}/\\d{2}", x)
  if (any(idx3)) out[idx3] <- as.Date(substr(x[idx3], 1, 10), "%Y/%m/%d")

  out
}

process_terminal_year_legacy <- function(in_csv, out_parquet, year) {
  if (is.null(year) || is.na(year)) stop("Terminal: year es obligatorio")

  df <- readr::read_csv(in_csv, show_col_types = FALSE, progress = FALSE)

  needed <- c("terminal", "fecha", "gasolina_regular", "gasolina_premium", "diesel")
  missing <- setdiff(needed, names(df))
  if (length(missing) > 0) {
    stop("Terminal: faltan columnas: ", paste(missing, collapse = ", "), " en: ", in_csv)
  }

  out <- df %>%
    transmute(
      terminal_id = terminal_id(.data$terminal),
      date = parse_date_safe_legacy(.data$fecha),
      year = as.integer(format(.data$date, "%Y")),
      regular = suppressWarnings(as.numeric(.data$gasolina_regular)),
      premium = suppressWarnings(as.numeric(.data$gasolina_premium)),
      diesel  = suppressWarnings(as.numeric(.data$diesel))
    ) %>%
    filter(.data$year == year) %>%
    mutate(
      flag_bad_date = is.na(.data$date),
      flag_missing_any_price = is.na(.data$regular) | is.na(.data$premium) | is.na(.data$diesel),
      flag_dup_terminal_day = duplicated(across(c(.data$terminal_id, .data$date)))
    )

  dir.create(dirname(out_parquet), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(out, out_parquet, compression = "zstd")

  invisible(out_parquet)
}
