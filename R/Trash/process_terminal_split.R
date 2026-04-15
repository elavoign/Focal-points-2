# R/Raw_to_Processed/process_terminal_split.R

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(arrow)
  library(stringi)
})

# =========================
# Normalizador consistente (si lo usas para otras cosas)
# =========================
normalize_text <- function(x) {
  if (length(x) == 0) return(character(0))

  s <- as.character(x)
  s[is.na(s)] <- ""

  s <- str_trim(s)
  s <- stringi::stri_trans_general(s, "Latin-ASCII")
  s <- str_to_upper(s)
  s <- str_replace_all(s, "[^A-Z0-9]+", "")

  s
}

process_terminal_year <- function(in_csv, year, out_parquet) {

  raw <- readr::read_csv(in_csv, show_col_types = FALSE)

  terminal_out <- raw %>%
    transmute(
      terminal_id = terminal_id_from_text(terminal),
      date = as.Date(fecha),
      year = as.integer(format(as.Date(fecha), "%Y")),
      regular = suppressWarnings(as.numeric(gasolina_regular)),
      premium = suppressWarnings(as.numeric(gasolina_premium)),
      diesel  = suppressWarnings(as.numeric(diesel))
    ) %>%
    filter(.data$year == !!year) %>%
    mutate(
      flag_bad_date = is.na(date),

      flag_missing_any_price =
        is.na(regular) |
        is.na(premium) |
        is.na(diesel),

      flag_nonpositive_any_price =
        (regular <= 0) |
        (premium <= 0) |
        (diesel <= 0)
    )

  dir.create(dirname(out_parquet), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(terminal_out, out_parquet)

  invisible(out_parquet)
}
