parse_date_safe <- function(x) {
  # x: character vector (puede traer NA, "", "2024-01-31", "31/01/2024", etc.)
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_

  out <- rep(as.Date(NA), length(x))

  # 1) YYYY-MM-DD (o con tiempo)
  idx1 <- !is.na(x) & grepl("^\\d{4}-\\d{2}-\\d{2}", x)
  if (any(idx1)) {
    out[idx1] <- as.Date(substr(x[idx1], 1, 10), format = "%Y-%m-%d")
  }

  # 2) DD/MM/YYYY (o con tiempo)
  idx2 <- !is.na(x) & is.na(out) & grepl("^\\d{2}/\\d{2}/\\d{4}", x)
  if (any(idx2)) {
    out[idx2] <- as.Date(substr(x[idx2], 1, 10), format = "%d/%m/%Y")
  }

  # 3) fallback: intenta YYYY/MM/DD
  idx3 <- !is.na(x) & is.na(out) & grepl("^\\d{4}/\\d{2}/\\d{2}", x)
  if (any(idx3)) {
    out[idx3] <- as.Date(substr(x[idx3], 1, 10), format = "%Y/%m/%d")
  }

  out
}
