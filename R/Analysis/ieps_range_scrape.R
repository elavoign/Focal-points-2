suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(xml2)
  library(rvest)
  library(httr2)
})

# ----------------------------
# Random backoff
# ----------------------------
sleep_uniform_0_37 <- function(max_sec = 37) {
  Sys.sleep(stats::runif(1, min = 0, max = max_sec))
}

# ----------------------------
# URLs
# ----------------------------
sidof_notas_url <- function(id) {
  paste0("https://sidof.segob.gob.mx/notas/", id)
}
sidof_doc_url <- function(id) {
  paste0("https://sidof.segob.gob.mx/notas/docFuente/", id)
}

# ----------------------------
# Fast fetch helper (string) con skip rápido
# ----------------------------
fetch_html_string_fast <- function(url, timeout_sec = 20) {
  req <- request(url) |>
    req_user_agent("Mozilla/5.0 (R; ieps scraper)") |>
    req_timeout(timeout_sec)

  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  if (is.null(resp)) return(NULL)
  if (resp_status(resp) >= 400) return(NULL)

  txt <- tryCatch(resp_body_string(resp, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(txt)) return(NULL)

  # Skip ultra-rápido si viene casi vacío
  if (nchar(txt) < 800) return(NULL)

  txt
}

# ----------------------------
# Extraer "Publicado" desde /notas/<id>
# (Esto es el filtro barato)
# ----------------------------
extract_published_date_from_notas <- function(html_txt) {
  # Aquí no te prometo 100% el patrón exacto en todos los IDs,
  # pero funciona en la mayoría: busca "Publicado:" y una fecha tipo dd/mm/aaaa o similar.
  # También soporta "Publicación" en algunos templates.

  txt <- str_replace_all(html_txt, "\\s+", " ")

  # intento 1: dd/mm/aaaa
  m1 <- str_match(txt, "(Publicado|Publicaci[oó]n)\\s*:?\\s*(\\d{2}/\\d{2}/\\d{4})")
  if (!is.na(m1[1,3])) {
    return(as.Date(m1[1,3], format = "%d/%m/%Y"))
  }

  # intento 2: fecha en español "9 de enero de 2026"
  m2 <- str_match(txt, "(Publicado|Publicaci[oó]n)\\s*:?\\s*(\\d{1,2}\\s+de\\s+[a-záéíóúñ]+\\s+de\\s+\\d{4})")
  if (!is.na(m2[1,3])) {
    # usa tu parse_spanish_date si ya existe en tu entorno
    return(parse_spanish_date(m2[1,3]))
  }

  as.Date(NA)
}

# - pide /notas/<id>
# - extrae publicado (si existe)
# - opcional: filtra por keywords del acuerdo IEPS
# ----------------------------
is_candidate_ieps_notas <- function(id,
                                   min_pub = as.Date("1900-01-01"),
                                   max_pub = as.Date("2100-01-01"),
                                   require_keywords = TRUE) {
  url <- sidof_notas_url(id)

  html_txt <- fetch_html_string_fast(url, timeout_sec = 15)
  if (is.null(html_txt)) return(tibble(
    doc_id = id,
    notas_url = url,
    published = as.Date(NA),
    candidate = FALSE
  ))

  pub <- extract_published_date_from_notas(html_txt)

  # keywords típicos del acuerdo IEPS semanal
  candidate_kw <- TRUE
  if (require_keywords) {
    blob <- str_to_lower(html_txt)
    candidate_kw <- str_detect(blob, "est[ií]mulo") &&
      str_detect(blob, "ieps|impuesto\\s+especial") &&
      str_detect(blob, "combustible|gasolina|di[eé]sel|cuota")
  }

  in_range <- !is.na(pub) && pub >= min_pub && pub <= max_pub

  tibble(
    doc_id = id,
    notas_url = url,
    published = pub,
    candidate = in_range && candidate_kw
  )
}

# ----------------------------
# Try docFuente extraction (ya pesado) con skip rápido
# ----------------------------
ieps_try_docFuente <- function(id,
                               max_sleep = 37,
                               sleep_before = TRUE) {
  if (sleep_before) sleep_uniform_0_37(max_sleep)

  url <- sidof_doc_url(id)
  html_txt <- fetch_html_string_fast(url, timeout_sec = 30)

  # skip rápido si no hay nada
  if (is.null(html_txt)) return(NULL)

  # otro filtro ultra barato: si ni siquiera hay "<table"
  if (!str_detect(str_to_lower(html_txt), "<table")) return(NULL)

  out <- tryCatch({
    html_doc <- read_html(html_txt)

    raw <- extract_ieps_tables(html_doc, source_url = url)
    if (nrow(raw) == 0) return(NULL)

    panel <- reshape_ieps_tables_to_panel(raw)
    if (nrow(panel) == 0) return(NULL)

    panel %>%
      select(Fecha, Combustible, everything(), -source_url, -period_start, -period_end) %>%
      mutate(doc_id = id)
  }, error = function(e) NULL)

  # si prefieres dormir después:
  if (!sleep_before) sleep_uniform_0_37(max_sleep)

  out
}

# ----------------------------
# Pipeline completo: rango -> filtra por "Publicado" -> scrape docFuente -> junta CSV final
# con checkpoints.
# ----------------------------
ieps_scrape_range_filtered <- function(
  id_from,
  id_to,
  min_pub = as.Date("2017-01-01"),
  max_pub = Sys.Date(),
  out_dir = "data/processed/ieps/range_raw",
  combined_csv = "data/processed/ieps/ieps_range_combined.csv",
  candidates_csv = "data/processed/ieps/ieps_candidates.csv",
  chunk_size = 500,
  max_sleep = 37,
  resume = TRUE
) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(combined_csv), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(candidates_csv), recursive = TRUE, showWarnings = FALSE)

  ids <- seq.int(id_from, id_to)

  # ----------------------------
  # (A) construir/recuperar candidates
  # ----------------------------
  if (resume && file.exists(candidates_csv)) {
    cand <- read_csv(candidates_csv, show_col_types = FALSE)
  } else {
    message("Construyendo candidates via /notas/<id> ... (barato)")
    cand_list <- vector("list", length(ids))
    for (k in seq_along(ids)) {
      cand_list[[k]] <- is_candidate_ieps_notas(
        ids[k],
        min_pub = min_pub,
        max_pub = max_pub,
        require_keywords = TRUE
      )
      # pequeño backoff (mucho más chico que el de docFuente)
      Sys.sleep(stats::runif(1, 0, 1.2))
    }
    cand <- bind_rows(cand_list)
    write_csv(cand, candidates_csv, na = "")
  }

  ids_keep <- cand %>%
    filter(candidate) %>%
    arrange(doc_id) %>%
    pull(doc_id)

  message("Candidates encontrados: ", length(ids_keep))

  if (length(ids_keep) == 0) {
    write_csv(tibble(), combined_csv)
    return(combined_csv)
  }

  # ----------------------------
  # (B) docFuente scrape con checkpoints por chunks
  # ----------------------------
  chunk_index <- ceiling(seq_along(ids_keep) / chunk_size)
  chk_files <- list.files(out_dir, pattern = "^chunk_\\d+\\.csv$", full.names = TRUE)
  done_chunks <- integer(0)

  if (resume && length(chk_files) > 0) {
    done_chunks <- chk_files %>%
      basename() %>%
      str_match("^chunk_(\\d+)\\.csv$") %>%
      .[,2] %>%
      as.integer() %>%
      sort()
  }

  for (ck in unique(chunk_index)) {
    if (resume && ck %in% done_chunks) next

    idx <- which(chunk_index == ck)
    ids_chunk <- ids_keep[idx]

    rows_list <- vector("list", length(ids_chunk))
    found <- 0L

    for (j in seq_along(ids_chunk)) {
      id <- ids_chunk[j]

      res <- ieps_try_docFuente(
        id,
        max_sleep = max_sleep,
        sleep_before = TRUE
      )

      if (!is.null(res)) {
        rows_list[[j]] <- res
        found <- found + 1L
      }
    }

    chunk_df <- bind_rows(rows_list)

    out_chunk <- file.path(out_dir, paste0("chunk_", ck, ".csv"))
    write_csv(chunk_df, out_chunk, na = "")
    message("Chunk ", ck, " guardado: ", out_chunk, " | docs con data: ", found)
  }

  # (C) combinar final (Fecha, Combustible, métricas)
  # ----------------------------
  all_chunks <- list.files(out_dir, pattern = "^chunk_\\d+\\.csv$", full.names = TRUE)
  df_all <- all_chunks %>%
    sort() %>%
    map_dfr(~ read_csv(.x, show_col_types = FALSE))

  df_final <- df_all %>%
    select(Fecha, Combustible, everything(), -doc_id) %>%
    distinct()

  write_csv(df_final, combined_csv, na = "")
  combined_csv
}