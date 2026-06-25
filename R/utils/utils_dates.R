parse_date_safe <- function(x) {

  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_

  out <- rep(as.Date(NA), length(x))

  idx1 <- !is.na(x) & grepl("^\\d{4}-\\d{2}-\\d{2}", x)
  if (any(idx1)) {
    out[idx1] <- as.Date(substr(x[idx1], 1, 10), format = "%Y-%m-%d")
  }

  idx2 <- !is.na(x) & is.na(out) & grepl("^\\d{2}/\\d{2}/\\d{4}", x)
  if (any(idx2)) {
    out[idx2] <- as.Date(substr(x[idx2], 1, 10), format = "%d/%m/%Y")
  }

  idx3 <- !is.na(x) & is.na(out) & grepl("^\\d{4}/\\d{2}/\\d{2}", x)
  if (any(idx3)) {
    out[idx3] <- as.Date(substr(x[idx3], 1, 10), format = "%Y/%m/%d")
  }

  out
}
