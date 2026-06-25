suppressPackageStartupMessages({
  library(stringr)
})

station_id <- function(x) {
  s <- as.character(x)
  s[is.na(s)] <- ""

  s <- str_squish(s)

  s <- str_remove_all(s, '^"+|"+$')
  s <- str_remove_all(s, "^'+|'+$")

  s <- str_squish(s)
  s[s == ""] <- NA_character_

  s
}

numero_permiso_id <- station_id
