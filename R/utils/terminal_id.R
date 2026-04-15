suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tibble)
  library(stringi)
})

terminal_aliases <- tibble::tribble(
  ~terminal_guess_raw, ~terminal_id,
  "18DEMARZOAZC",      "AZCAPOTZALCO"
)

canon_terminal <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- trimws(x)

  x <- stringi::stri_trans_general(x, "Latin-ASCII")

  x <- x %>%
    str_to_upper() %>%
    str_replace("^\\s*TAD\\s+", "") %>%
    str_replace(",\\s*[A-Z]{1,4}(\\s+[A-Z]{1,4})*\\.?\\s*$", "") %>%
    str_replace_all(",", " ") %>%
    str_replace_all("\\.", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    str_replace("^CD\\s+", "") %>%
    str_replace("^NVO\\s+", "NUEVO ") %>%
    str_replace("^STA\\s+", "SANTA ") %>%
    str_replace("^S\\s+", "SAN ") %>%
    str_replace("TUXTLA\\s+GTZ\\s*$", "TUXTLA GUTIERREZ") %>%
    str_replace("GOMEZ\\s+PALACIO\\s*$", "GOMEZ PALACIO") %>%
    str_replace("TIERRA\\s+BLANCA\\s*$", "TIERRA BLANCA") %>%
    str_replace("SAN\\s+LUIS\\s+POTOSI\\s*$", "SAN LUIS POTOSI") %>%
    str_replace("SAN\\s+JUAN\\s+IXHUATEPEC\\s*$", "SAN JUAN IXHUATEPEC") %>%
    str_replace("^LAZARO\\s+CARDENAS\\s*$", "LAZARO CARDENAS") %>%
    str_replace("SANTA\\s+CATARINA\\s+MTY\\s*$", "SANTA CATARINA") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    str_replace_all("\\s+", "") %>%
    str_replace("(DF|NL|BCN|BCS|SLP|TAMPS|MEX|JAL|PUE|GRO|QRO|YUC|VER|OAX|CHIH|COAH|SON|SIN|DGO|ZAC|MICH|GTO)$", "")

  dplyr::na_if(x, "")
}

terminal_id <- function(x) {
  guess <- canon_terminal(x)

  tibble(terminal_guess_raw = guess) %>%
    left_join(terminal_aliases, by = "terminal_guess_raw") %>%
    mutate(
      terminal_id = coalesce(.data$terminal_id, .data$terminal_guess_raw),
      terminal_id = na_if(.data$terminal_id, "")
    ) %>%
    pull(.data$terminal_id)
}

terminal_id_from_text <- terminal_id
