suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(httr2)
  library(rvest)
  library(stringr)
  library(purrr)
  library(lubridate)
  library(tibble)
})

.parse_excel_date <- function(x) {
  n <- suppressWarnings(as.numeric(x))
  as.Date(ifelse(is.na(n), NA_real_, n), origin = "1899-12-30")
}

.classify_url <- function(url) {
  dplyr::case_when(
    is.na(url) | !nzchar(url)                              ~ "none",
    str_detect(url, "dof\\.gob\\.mx.*codigo=")             ~ "dof",
    str_detect(url, "sidof\\.segob\\.gob\\.mx/notas/\\d+") ~ "sidof",
    TRUE                                                    ~ "none"
  )
}

read_ieps_with_urls <- function(xlsx_path) {
  raw <- readxl::read_excel(
    xlsx_path,
    sheet     = "DATOS",
    skip      = 1,
    col_names = FALSE,
    col_types = "text"
  )
  df <- raw[-1L, ]
  nc <- ncol(df)
  labels <- c(
    "fecha_dof", "url", "fecha_inicio", "fecha_fin",
    "magna_estimulo_pct", "magna_cuota",   "magna_ieps_base",
    "prem_estimulo_pct",  "prem_cuota",    "prem_ieps_base",
    "diesel_estimulo_pct","diesel_cuota",  "diesel_ieps_base",
    "aux1", "aux2"
  )
  names(df) <- labels[seq_len(nc)]

  df |>
    dplyr::mutate(
      fecha_inicio       = .parse_excel_date(fecha_inicio),
      fecha_fin          = .parse_excel_date(fecha_fin),
      magna_estimulo_pct = suppressWarnings(as.numeric(magna_estimulo_pct)),
      magna_cuota        = suppressWarnings(as.numeric(magna_cuota)),
      prem_estimulo_pct  = suppressWarnings(as.numeric(prem_estimulo_pct)),
      prem_cuota         = suppressWarnings(as.numeric(prem_cuota)),
      diesel_estimulo_pct= suppressWarnings(as.numeric(diesel_estimulo_pct)),
      diesel_cuota       = suppressWarnings(as.numeric(diesel_cuota)),
      url_type           = .classify_url(url)
    ) |>
    dplyr::select(fecha_inicio, fecha_fin, url, url_type,
                  magna_estimulo_pct, magna_cuota,
                  prem_estimulo_pct,  prem_cuota,
                  diesel_estimulo_pct, diesel_cuota)
}

extract_url_parts <- function(url, url_type) {
  if (url_type == "dof") {
    codigo <- str_match(url, "codigo=(\\d+)")[, 2]
    fecha  <- str_match(url, "fecha=([0-9/]+)")[, 2]
  } else if (url_type == "sidof") {
    codigo <- str_match(url, "/notas/(\\d+)")[, 2]
    fecha  <- NA_character_
  } else {
    codigo <- NA_character_
    fecha  <- NA_character_
  }
  list(codigo = codigo, fecha = fecha)
}

parse_pct <- function(x) {

  x2 <- str_replace_all(x, "[%\\s]", "")
  x2 <- str_replace_all(x2, ",", ".")
  v <- suppressWarnings(as.numeric(x2))
  v / 100
}

parse_mxn <- function(x) {

  x2 <- str_replace_all(x, "[$\\s]", "")
  x2 <- str_replace_all(x2, ",", ".")
  suppressWarnings(as.numeric(x2))
}

extract_ieps_from_text <- function(txt) {

  t <- str_replace_all(txt, "\\s+", " ")

  oct_menor  <- "(?i)Gasolina\\s+menor\\s+a\\s+9[12]\\s+octanos"
  oct_mayor  <- "(?i)Gasolina\\s+mayor\\s+o\\s+igual\\s+a\\s+9[12]\\s+octanos"

  pct_block_pos <- str_locate(t, "(?i)Porcentaje\\s+de\\s+Est[ií]mulo")[1, 1]
  pct_block <- if (!is.na(pct_block_pos)) substr(t, pct_block_pos, nchar(t)) else t

  pct_block_end <- str_locate(pct_block, "(?i)(Art[ií]culo|Monto\\s+del\\s+est[ií]mulo|Cuota)")[1, 1]
  pct_block_trimmed <- if (!is.na(pct_block_end)) substr(pct_block, 1, pct_block_end) else pct_block

  magna_pct_str <- str_match(
    pct_block_trimmed,
    paste0(oct_menor, "[^\\d]+(\\d{1,3}[.,]\\d{1,4})%")
  )[, 2]

  prem_pct_str <- str_match(
    pct_block_trimmed,
    paste0(oct_mayor, "[^\\d]+(\\d{1,3}[.,]\\d{1,4})%")
  )[, 2]

  diesel_pct_str <- str_match(
    pct_block_trimmed,
    "(?i)Di[eé]sel[^\\d]+(\\d{1,3}[.,]\\d{1,4})%"
  )[, 2]

  cuota_block_pos <- str_locate(t, "(?i)Cuota\\s+disminuida")[1, 1]
  if (is.na(cuota_block_pos)) {
    cuota_block_pos <- str_locate(t, "(?i)Cuota\\s*\\(pesos/litro\\)")[1, 1]
  }

  cuota_block <- if (!is.na(cuota_block_pos)) substr(t, cuota_block_pos, nchar(t)) else t

  cuota_block_end <- str_locate(cuota_block, "(?i)(Art[ií]culo\\s+Cuarto|TRANSITORIO|TRANSITORIOS|Ciudad de M)")[1, 1]
  cuota_block_trimmed <- if (!is.na(cuota_block_end)) substr(cuota_block, 1, cuota_block_end) else cuota_block

  magna_cuota_str <- str_match(
    cuota_block_trimmed,
    paste0(oct_menor, "[^\\d]+\\$?\\s*(\\d+[.,]\\d+)")
  )[, 2]

  prem_cuota_str <- str_match(
    cuota_block_trimmed,
    paste0(oct_mayor, "[^\\d]+\\$?\\s*(\\d+[.,]\\d+)")
  )[, 2]

  diesel_cuota_str <- str_match(
    cuota_block_trimmed,
    "(?i)Di[eé]sel[^\\d]+\\$?\\s*(\\d+[.,]\\d+)"
  )[, 2]

  tibble(
    magna_estimulo_pct_web  = ifelse(is.na(magna_pct_str),  NA_real_, parse_pct(magna_pct_str)),
    prem_estimulo_pct_web   = ifelse(is.na(prem_pct_str),   NA_real_, parse_pct(prem_pct_str)),
    diesel_estimulo_pct_web = ifelse(is.na(diesel_pct_str), NA_real_, parse_pct(diesel_pct_str)),
    magna_cuota_web         = ifelse(is.na(magna_cuota_str),  NA_real_, parse_mxn(magna_cuota_str)),
    prem_cuota_web          = ifelse(is.na(prem_cuota_str),   NA_real_, parse_mxn(prem_cuota_str)),
    diesel_cuota_web        = ifelse(is.na(diesel_cuota_str), NA_real_, parse_mxn(diesel_cuota_str))
  )
}

.fetch_text <- function(target_url, timeout_sec) {
  req <- request(target_url) |>
    req_user_agent("Mozilla/5.0 (R; ieps-verifier)") |>
    req_options(ssl_verifypeer = FALSE) |>
    req_timeout(timeout_sec)

  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  if (is.null(resp)) return(NULL)
  if (resp_status(resp) >= 400) return(NULL)

  txt <- tryCatch(resp_body_string(resp, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(txt) || nchar(txt) < 500) return(NULL)

  html <- tryCatch(read_html(txt), error = function(e) NULL)
  if (is.null(html)) return(NULL)

  html_text2(html)
}

fetch_and_parse_decree <- function(url, url_type, timeout_sec = 20) {
  if (url_type == "none") return(NULL)

  parts <- extract_url_parts(url, url_type)
  if (is.na(parts$codigo)) return(NULL)

  target_url <- if (url_type == "dof") {
    paste0(
      "https://www.dof.gob.mx/nota_detalle_popup.php?codigo=",
      parts$codigo,
      if (!is.na(parts$fecha)) paste0("&fecha=", parts$fecha) else ""
    )
  } else {

    paste0("https://sidof.segob.gob.mx/notas/docFuente/", parts$codigo)
  }

  visible <- .fetch_text(target_url, timeout_sec)
  if (is.null(visible)) return(NULL)

  blob <- str_to_lower(visible)
  is_ieps <- str_detect(blob, "est[ií]mulo") &&
    str_detect(blob, "cuota") &&
    str_detect(blob, "combustible|gasolina|di[eé]sel")

  if (!is_ieps) return(tibble(
    magna_estimulo_pct_web = NA_real_, prem_estimulo_pct_web = NA_real_,
    diesel_estimulo_pct_web = NA_real_, magna_cuota_web = NA_real_,
    prem_cuota_web = NA_real_, diesel_cuota_web = NA_real_,
    fetch_status = "no_ieps_content"
  ))

  result <- extract_ieps_from_text(visible)
  result$fetch_status <- "ok"
  result
}

verify_ieps_sample <- function(
  xlsx_path = "data/raw_public/IEPS_Combustibles_Mexico.xlsx",
  sleep_sec = 1,
  tol       = 0.001
) {
  message("Leyendo Excel...")
  rows <- read_ieps_with_urls(xlsx_path)
  message(sprintf(
    "Filas totales: %d  |  con link DOF: %d  |  con link SIDOF: %d  |  sin link: %d",
    nrow(rows),
    sum(rows$url_type == "dof"),
    sum(rows$url_type == "sidof"),
    sum(rows$url_type == "none")
  ))

  verifiable_rows <- rows |>
    dplyr::filter(url_type %in% c("dof", "sidof")) |>
    dplyr::arrange(fecha_inicio)

  no_url_rows <- rows |>
    dplyr::filter(url_type == "none") |>
    dplyr::arrange(fecha_inicio)

  results <- vector("list", nrow(verifiable_rows))

  for (i in seq_len(nrow(verifiable_rows))) {
    row <- verifiable_rows[i, ]
    url <- row$url

    parts <- extract_url_parts(url, row$url_type)
    message(sprintf(
      "[%3d/%d] fecha_inicio=%s | fuente=%-5s | codigo=%s",
      i, nrow(verifiable_rows),
      as.character(row$fecha_inicio),
      row$url_type, parts$codigo
    ))

    web <- fetch_and_parse_decree(url, row$url_type)

    if (is.null(web)) {
      results[[i]] <- tibble(
        fecha_inicio        = row$fecha_inicio,
        url                 = url,
        url_type            = row$url_type,
        fetch_status        = "fetch_error",
        magna_estimulo_pct_xlsx  = row$magna_estimulo_pct,
        magna_estimulo_pct_web   = NA_real_,
        magna_cuota_xlsx         = row$magna_cuota,
        magna_cuota_web          = NA_real_,
        prem_estimulo_pct_xlsx   = row$prem_estimulo_pct,
        prem_estimulo_pct_web    = NA_real_,
        prem_cuota_xlsx          = row$prem_cuota,
        prem_cuota_web           = NA_real_,
        diesel_estimulo_pct_xlsx = row$diesel_estimulo_pct,
        diesel_estimulo_pct_web  = NA_real_,
        diesel_cuota_xlsx        = row$diesel_cuota,
        diesel_cuota_web         = NA_real_,
        ok_magna_pct  = NA, ok_magna_cuota  = NA,
        ok_prem_pct   = NA, ok_prem_cuota   = NA,
        ok_diesel_pct = NA, ok_diesel_cuota = NA
      )
    } else {
      check <- function(a, b) {
        if (is.na(a) || is.na(b)) return(NA)
        abs(a - b) <= tol
      }

      results[[i]] <- tibble(
        fecha_inicio        = row$fecha_inicio,
        url                 = url,
        url_type            = row$url_type,
        fetch_status        = web$fetch_status,
        magna_estimulo_pct_xlsx  = row$magna_estimulo_pct,
        magna_estimulo_pct_web   = web$magna_estimulo_pct_web,
        magna_cuota_xlsx         = row$magna_cuota,
        magna_cuota_web          = web$magna_cuota_web,
        prem_estimulo_pct_xlsx   = row$prem_estimulo_pct,
        prem_estimulo_pct_web    = web$prem_estimulo_pct_web,
        prem_cuota_xlsx          = row$prem_cuota,
        prem_cuota_web           = web$prem_cuota_web,
        diesel_estimulo_pct_xlsx = row$diesel_estimulo_pct,
        diesel_estimulo_pct_web  = web$diesel_estimulo_pct_web,
        diesel_cuota_xlsx        = row$diesel_cuota,
        diesel_cuota_web         = web$diesel_cuota_web,
        ok_magna_pct  = check(row$magna_estimulo_pct,  web$magna_estimulo_pct_web),
        ok_magna_cuota= check(row$magna_cuota,         web$magna_cuota_web),
        ok_prem_pct   = check(row$prem_estimulo_pct,   web$prem_estimulo_pct_web),
        ok_prem_cuota = check(row$prem_cuota,          web$prem_cuota_web),
        ok_diesel_pct = check(row$diesel_estimulo_pct, web$diesel_estimulo_pct_web),
        ok_diesel_cuota=check(row$diesel_cuota,        web$diesel_cuota_web)
      )
    }

    if (i < nrow(verifiable_rows)) Sys.sleep(sleep_sec)
  }

  df <- bind_rows(results)

  df_no_url <- no_url_rows |>
    dplyr::transmute(
      fecha_inicio, url = NA_character_, url_type = "none",
      fetch_status = "no_url",
      magna_estimulo_pct_xlsx = magna_estimulo_pct, magna_estimulo_pct_web = NA_real_,
      magna_cuota_xlsx        = magna_cuota,        magna_cuota_web        = NA_real_,
      prem_estimulo_pct_xlsx  = prem_estimulo_pct,  prem_estimulo_pct_web  = NA_real_,
      prem_cuota_xlsx         = prem_cuota,         prem_cuota_web         = NA_real_,
      diesel_estimulo_pct_xlsx= diesel_estimulo_pct,diesel_estimulo_pct_web= NA_real_,
      diesel_cuota_xlsx       = diesel_cuota,       diesel_cuota_web       = NA_real_,
      ok_magna_pct = NA, ok_magna_cuota = NA, ok_prem_pct = NA,
      ok_prem_cuota = NA, ok_diesel_pct = NA, ok_diesel_cuota = NA
    )

  df_full <- bind_rows(df, df_no_url) |> dplyr::arrange(fecha_inicio)

  ok_cols <- c("ok_magna_pct","ok_magna_cuota","ok_prem_pct",
               "ok_prem_cuota","ok_diesel_pct","ok_diesel_cuota")

  message("\n========== RESUMEN ==========")
  message(sprintf("Filas totales en Excel:       %d", nrow(df_full)))
  message(sprintf("  - sin link (no verificable): %d", sum(df_full$fetch_status == "no_url")))
  message(sprintf("  - con link verificable:      %d", nrow(df)))
  message(sprintf("      Fetch exitoso:           %d", sum(df$fetch_status == "ok", na.rm = TRUE)))
  message(sprintf("      Sin contenido IEPS:      %d", sum(df$fetch_status == "no_ieps_content", na.rm = TRUE)))
  message(sprintf("      Error de fetch:          %d", sum(df$fetch_status == "fetch_error", na.rm = TRUE)))

  df_ok <- df_full |> filter(fetch_status == "ok")
  if (nrow(df_ok) > 0) {
    message("\nChecks (solo filas con fetch ok):")
    for (col in ok_cols) {
      n_ok    <- sum(df_ok[[col]] == TRUE,  na.rm = TRUE)
      n_fail  <- sum(df_ok[[col]] == FALSE, na.rm = TRUE)
      n_na    <- sum(is.na(df_ok[[col]]))
      message(sprintf("  %-25s  OK=%d  FAIL=%d  NA=%d", col, n_ok, n_fail, n_na))
    }

    any_fail <- df_ok |>
      filter(if_any(all_of(ok_cols), ~ !is.na(.) & . == FALSE))

    if (nrow(any_fail) > 0) {
      message(sprintf("\n*** %d filas con discrepancia vs. la fuente oficial ***", nrow(any_fail)))
      print(any_fail |> select(fecha_inicio, url_type, fetch_status,
                                starts_with("ok_"),
                                ends_with("_xlsx"), ends_with("_web")))
      message(
        "\n>>> Si confirmas que el Excel esta mal: corrigelo y vuelve a correr ",
        "tar_make(). targets detecta el cambio en `ieps_xlsx`, recalcula ",
        "`ieps_monthly_parquet` automaticamente, y eso a su vez dispara de ",
        "nuevo `pooled_regression_outputs`, `pooled_regression_restricted_outputs` ",
        "y `results_updated_pdf` (todo lo que depende del IEPS). No hace falta ",
        "forzar nada manualmente: el grafo de dependencias de targets ya lo cubre."
      )
    } else {
      message("\nTodo correcto: ninguna discrepancia encontrada en las filas verificables.")
    }
  }

  if (sum(df_full$fetch_status == "no_url") > 0) {
    message(sprintf(
      "\nNota: %d filas no tienen un link de fuente capturado en el Excel y no se ",
      sum(df_full$fetch_status == "no_url")
    ), "pueden verificar automaticamente (requeriria buscar manualmente el decreto correspondiente).")
  }

  invisible(df_full)
}

run_ieps_verification <- function(
  xlsx_path = "data/raw_public/IEPS_Combustibles_Mexico.xlsx",
  out_csv   = "data/processed/ieps/verify_ieps_vs_dof.csv",
  sleep_sec = 1
) {
  df_result <- verify_ieps_sample(xlsx_path = xlsx_path, sleep_sec = sleep_sec)
  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(df_result, out_csv)
  message("\nResultados guardados en: ", out_csv)
  invisible(df_result)
}
