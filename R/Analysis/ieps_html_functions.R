suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
  library(readr)
  library(xml2)
  library(rvest)
  library(httr2)
  library(tidyr)
})

normalize_colnames <- function(nms) {
  nms %>%
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    str_to_lower() %>%
    str_replace_all("[\u00A0]", " ") %>%
    str_replace_all("[()]", "") %>%
    str_replace_all("[^a-z0-9áéíóúñü/% ]", "") %>%
    str_replace_all("\\s+", "_")
}

parse_spanish_date <- function(x) {
  if (is.na(x) || !nzchar(x)) return(as.Date(NA))

  months <- c(
    "enero" = "01", "febrero" = "02", "marzo" = "03", "abril" = "04",
    "mayo" = "05", "junio" = "06", "julio" = "07", "agosto" = "08",
    "septiembre" = "09", "setiembre" = "09", "octubre" = "10",
    "noviembre" = "11", "diciembre" = "12"
  )

  s <- str_to_lower(x)
  m <- str_match(s, "(\\d{1,2})\\s*de\\s*([a-záéíóúñ]+)\\s*de\\s*(\\d{4})")
  if (anyNA(m[1, 2:4])) return(as.Date(NA))

  dd <- sprintf("%02d", as.integer(m[1, 2]))
  mm <- months[[m[1, 3]]]
  yyyy <- m[1, 4]
  if (is.null(mm)) return(as.Date(NA))

  as.Date(paste0(yyyy, "-", mm, "-", dd))
}

extract_period_dates <- function(txt) {
  m <- str_match(
    txt,
    "del\\s+(\\d{1,2}\\s+de\\s+[a-záéíóúñ]+\\s+de\\s+\\d{4})\\s+al\\s+(\\d{1,2}\\s+de\\s+[a-záéíóúñ]+\\s+de\\s+\\d{4})"
  )
  tibble(
    period_start = parse_spanish_date(m[1, 2]),
    period_end   = parse_spanish_date(m[1, 3])
  )
}

extract_entry_into_force <- function(txt) {
  m <- str_match(
    txt,
    "entr(a|ará)\\s+en\\s+vigor\\s+el\\s+(\\d{1,2}\\s+de\\s+[a-záéíóúñ]+\\s+de\\s+\\d{4})"
  )
  parse_spanish_date(m[1, 3])
}

fetch_ieps_html <- function(url, user_agent = "Mozilla/5.0 (R; ieps scraper)") {
  req <- request(url) |>
    req_user_agent(user_agent) |>
    req_timeout(60)

  resp <- req_perform(req)
  if (resp_status(resp) >= 400) {
    stop("HTTP error ", resp_status(resp), " for url: ", url)
  }

  html_txt <- resp_body_string(resp, encoding = "UTF-8")
  read_html(html_txt)
}

extract_ieps_tables <- function(html_doc, source_url = NA_character_) {

  txt <- html_text2(html_doc)
  txt <- str_replace_all(txt, "\\s+", " ") |> str_trim()

  period <- extract_period_dates(txt)
  entry  <- extract_entry_into_force(txt)

  tbl_nodes <- html_elements(html_doc, "table")
  if (length(tbl_nodes) == 0) {
    return(tibble(
      source_url = source_url,
      period_start = period$period_start,
      period_end = period$period_end,
      entry_into_force = entry,
      table_index = integer(),
      row_id = integer()
    ))
  }

  raw_tbls <- map(tbl_nodes, ~ html_table(.x, fill = TRUE))

  is_relevant <- function(df) {
    if (nrow(df) == 0 || ncol(df) == 0) return(FALSE)
    sample_cells <- df |> head(10) |> mutate(across(everything(), as.character))
    blob <- paste(c(names(df), unlist(sample_cells)), collapse = " ") |> str_to_lower()

    any(str_detect(blob, "combustible|gasolina|di[eé]sel|cuota|est[ií]mulo|pesos\\s*/\\s*litro|porcentaje"))
  }

  keep_idx <- which(map_lgl(raw_tbls, is_relevant))
  if (length(keep_idx) == 0) keep_idx <- seq_along(raw_tbls)

  out <- map2_dfr(keep_idx, raw_tbls[keep_idx], function(i, df) {
    names(df) <- normalize_colnames(names(df))
    df <- df |> mutate(across(everything(), ~ str_squish(as.character(.x))))

    if (!("combustible" %in% names(df))) {
      cand <- names(df)[str_detect(names(df), "combustible|producto|tipo")]
      if (length(cand) >= 1) df <- df |> rename(combustible = all_of(cand[1]))
    }

    df |>
      mutate(
        source_url = source_url,
        period_start = period$period_start,
        period_end = period$period_end,
        entry_into_force = entry,
        table_index = i,
        row_id = row_number()
      )
  })

  meta_cols <- c("source_url", "period_start", "period_end", "entry_into_force", "table_index", "row_id")
  out |> relocate(any_of(meta_cols))
}

reshape_ieps_tables_to_panel <- function(df) {
  stopifnot(all(c("table_index", "row_id") %in% names(df)))

  content_cols <- setdiff(names(df), c(
    "source_url","period_start","period_end","entry_into_force",
    "table_index","row_id"
  ))
  if (length(content_cols) < 2) stop("No se encontraron columnas de contenido suficientes.")

  c1 <- content_cols[1]
  c2 <- content_cols[2]

  df2 <- df %>%
    mutate(
      Fecha = entry_into_force,
      Combustible = .data[[c1]],
      value_raw = .data[[c2]]
    ) %>%
    select(source_url, Fecha, period_start, period_end, table_index, row_id, Combustible, value_raw)

  metric_map <- df2 %>%
    filter(row_id == 1) %>%
    transmute(table_index, metric_name = value_raw)

  df_long <- df2 %>%
    filter(row_id > 1) %>%
    left_join(metric_map, by = "table_index") %>%
    filter(!is.na(Combustible), Combustible != "", !is.na(metric_name), metric_name != "")

  parse_num <- function(x) {
    x2 <- str_squish(as.character(x))
    x2 <- str_replace_all(x2, "\\$", "")
    x2 <- str_replace_all(x2, ",", "")
    x2 <- str_replace_all(x2, "%", "")
    suppressWarnings(as.numeric(x2))
  }

  df_long <- df_long %>% mutate(value_num = parse_num(value_raw))

  panel <- df_long %>%
    select(source_url, Fecha, period_start, period_end, Combustible, metric_name, value_num) %>%
    distinct() %>%
    pivot_wider(
      names_from = metric_name,
      values_from = value_num
    ) %>%
    arrange(Fecha, Combustible)

  panel <- panel %>%
    filter(str_to_lower(Combustible) != "combustible")

  panel
}

ieps_html_to_csv <- function(url, out_csv) {
  html_doc <- fetch_ieps_html(url)
  raw <- extract_ieps_tables(html_doc, source_url = url)

  panel <- reshape_ieps_tables_to_panel(raw)

  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
  write_csv(panel, out_csv, na = "")
  out_csv
}
