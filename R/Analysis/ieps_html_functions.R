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

# ----------------------------
# Helpers: limpieza y fechas
# ----------------------------

normalize_colnames <- function(nms) {
  nms %>%
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    str_to_lower() %>%
    str_replace_all("[\u00A0]", " ") %>%  # non-breaking space
    str_replace_all("[()]", "") %>%
    str_replace_all("[^a-z0-9áéíóúñü/% ]", "") %>%
    str_replace_all("\\s+", "_")
}

# Convierte fechas en español del tipo "27 de diciembre de 2016" a Date
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

# Busca "del X al Y" (periodo) y regresa inicio/fin
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

# Busca una fecha de "entrada en vigor" si existe
extract_entry_into_force <- function(txt) {
  m <- str_match(
    txt,
    "entr(a|ará)\\s+en\\s+vigor\\s+el\\s+(\\d{1,2}\\s+de\\s+[a-záéíóúñ]+\\s+de\\s+\\d{4})"
  )
  parse_spanish_date(m[1, 3])
}

# ----------------------------
# (1) Descargar HTML
# ----------------------------
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

# ----------------------------
# (2) Extraer tablas + metadatos (formato "raw" apilado)
# ----------------------------
extract_ieps_tables <- function(html_doc, source_url = NA_character_) {
  # texto completo (para buscar fechas)
  txt <- html_text2(html_doc)
  txt <- str_replace_all(txt, "\\s+", " ") |> str_trim()

  # Extrae fechas
  period <- extract_period_dates(txt)
  entry  <- extract_entry_into_force(txt)

  # Todas las tablas del HTML
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

  # Convierte tablas a dataframes
  raw_tbls <- map(tbl_nodes, ~ html_table(.x, fill = TRUE))

  # Selecciona tablas "relevantes" (contienen keywords típicos)
  is_relevant <- function(df) {
    if (nrow(df) == 0 || ncol(df) == 0) return(FALSE)
    sample_cells <- df |> head(10) |> mutate(across(everything(), as.character))
    blob <- paste(c(names(df), unlist(sample_cells)), collapse = " ") |> str_to_lower()

    any(str_detect(blob, "combustible|gasolina|di[eé]sel|cuota|est[ií]mulo|pesos\\s*/\\s*litro|porcentaje"))
  }

  keep_idx <- which(map_lgl(raw_tbls, is_relevant))
  if (length(keep_idx) == 0) keep_idx <- seq_along(raw_tbls)

  # Limpia y apila
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

# ----------------------------
# (2.5) Reestructurar a panel: 3 filas (combustible) x N métricas (columnas)
# ----------------------------
reshape_ieps_tables_to_panel <- function(df) {
  stopifnot(all(c("table_index", "row_id") %in% names(df)))

  # Identifica columnas de contenido (tu ejemplo actual produce x1/x2, pero esto generaliza)
  content_cols <- setdiff(names(df), c(
    "source_url","period_start","period_end","entry_into_force",
    "table_index","row_id"
  ))
  if (length(content_cols) < 2) stop("No se encontraron columnas de contenido suficientes.")

  c1 <- content_cols[1]  # columna con combustible (incluye header "Combustible")
  c2 <- content_cols[2]  # columna con el valor o el nombre de la métrica (en row 1)

  df2 <- df %>%
    mutate(
      Fecha = entry_into_force,       # renombre pedido
      Combustible = .data[[c1]],       # renombre pedido
      value_raw = .data[[c2]]
    ) %>%
    select(source_url, Fecha, period_start, period_end, table_index, row_id, Combustible, value_raw)

  # 1) Nombre de la métrica = fila 1 de cada tabla
  metric_map <- df2 %>%
    filter(row_id == 1) %>%
    transmute(table_index, metric_name = value_raw)

  # 2) Filas de datos (combustibles): row_id > 1
  df_long <- df2 %>%
    filter(row_id > 1) %>%
    left_join(metric_map, by = "table_index") %>%
    filter(!is.na(Combustible), Combustible != "", !is.na(metric_name), metric_name != "")

  # 3) Parse numérico (quita $, %, comas). % queda como 26.05 (no 0.2605)
  parse_num <- function(x) {
    x2 <- str_squish(as.character(x))
    x2 <- str_replace_all(x2, "\\$", "")
    x2 <- str_replace_all(x2, ",", "")
    x2 <- str_replace_all(x2, "%", "")
    suppressWarnings(as.numeric(x2))
  }

  df_long <- df_long %>% mutate(value_num = parse_num(value_raw))

  # 4) Pivot a wide: 3 filas por combustible, columnas por métrica
  panel <- df_long %>%
    select(source_url, Fecha, period_start, period_end, Combustible, metric_name, value_num) %>%
    distinct() %>%
    pivot_wider(
      names_from = metric_name,
      values_from = value_num
    ) %>%
    arrange(Fecha, Combustible)

  # 5) Asegura que no se cuele la fila header "Combustible" como combustible
  panel <- panel %>%
    filter(str_to_lower(Combustible) != "combustible")

  panel
}

# ----------------------------
# (3) Wrapper final: url -> CSV con panel (3 filas x N métricas)
# ----------------------------
ieps_html_to_csv <- function(url, out_csv) {
  html_doc <- fetch_ieps_html(url)
  raw <- extract_ieps_tables(html_doc, source_url = url)

  panel <- reshape_ieps_tables_to_panel(raw)

  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
  write_csv(panel, out_csv, na = "")
  out_csv
}