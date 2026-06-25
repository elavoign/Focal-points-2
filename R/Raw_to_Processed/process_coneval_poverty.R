suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(tidyr)
  library(stringr)
  library(arrow)
  library(readr)
})

.read_coneval_sheet <- function(path, sheet) {

  raw <- readxl::read_excel(path, sheet = sheet,
                            col_names = FALSE, skip = 7,
                            .name_repair = "minimal")

  raw <- raw[grepl("^[0-9]+$", as.character(raw[[1]])), ]

  if (nrow(raw) == 0L) {
    warning("Sheet '", sheet, "' produced zero data rows after filtering.")
    return(NULL)
  }

  df <- raw[, c(1, 2, 3, 4, 7, 12)]
  names(df) <- c("CVE_ENT", "NOM_ENT", "CVEGEO", "NOM_MUN",
                 "pop_2020", "pov_pct_2020")

  df <- df |>
    dplyr::mutate(

      CVE_ENT = stringr::str_pad(as.character(.data$CVE_ENT), 2, "left", "0"),
      CVEGEO  = stringr::str_pad(as.character(.data$CVEGEO),  5, "left", "0"),
      NOM_ENT      = as.character(.data$NOM_ENT),
      NOM_MUN      = as.character(.data$NOM_MUN),
      pop_2020     = suppressWarnings(as.numeric(.data$pop_2020)),
      pov_pct_2020 = suppressWarnings(as.numeric(.data$pov_pct_2020)),
      sheet        = sheet
    ) |>
    dplyr::filter(!is.na(.data$CVEGEO), nchar(.data$CVEGEO) == 5L)

  df
}

.weighted_poverty <- function(df_long, partition_sheets) {
  df_long |>
    dplyr::filter(.data$sheet %in% partition_sheets) |>
    dplyr::group_by(.data$CVEGEO) |>
    dplyr::summarise(

      wt_sum  = sum(.data$pop_2020 * .data$pov_pct_2020, na.rm = TRUE),
      pop_sum = sum(
        dplyr::if_else(!is.na(.data$pov_pct_2020), .data$pop_2020, NA_real_),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      poverty_pct = dplyr::if_else(
        .data$pop_sum > 0, .data$wt_sum / .data$pop_sum, NA_real_
      )
    ) |>
    dplyr::select("CVEGEO", "poverty_pct")
}

process_coneval_poverty <- function(
  in_xlsx     = "data/raw_public/Indicadores_pobreza_grupos_municipal.xlsx",
  out_parquet = "data/processed/coneval/municipal_poverty_2020.parquet",
  out_csv     = "data/processed/coneval/municipal_poverty_2020.csv"
) {

  partitions <- list(
    sex = c("mujeres", "hombres"),
    age = c("nna", "jovenes", "adultos", "adultmay"),
    geo = c("rural", "urbano")
  )
  all_sheets <- unique(unlist(partitions))

  message("Reading CONEVAL sheets ...")
  sheets_data <- lapply(all_sheets, function(sh) {
    message("  ", sh)
    .read_coneval_sheet(in_xlsx, sh)
  })
  names(sheets_data) <- all_sheets

  mun_by_sheet <- lapply(sheets_data, function(d) sort(unique(d$CVEGEO)))
  n_mun <- sapply(mun_by_sheet, length)
  message(sprintf("  Municipalities per sheet: min=%d  max=%d",
                  min(n_mun), max(n_mun)))

  df_long <- dplyr::bind_rows(sheets_data)

  mun_meta <- df_long |>
    dplyr::distinct(
      .data$CVEGEO, .data$CVE_ENT, .data$NOM_ENT, .data$NOM_MUN
    ) |>
    dplyr::arrange(.data$CVEGEO)

  mun_meta <- mun_meta |>
    dplyr::group_by(.data$CVEGEO) |>
    dplyr::slice(1L) |>
    dplyr::ungroup()

  message("Computing partition-weighted poverty estimates ...")

  pov_sex <- .weighted_poverty(df_long, partitions$sex) |>
    dplyr::rename(poverty_sex = "poverty_pct")
  pov_age <- .weighted_poverty(df_long, partitions$age) |>
    dplyr::rename(poverty_age = "poverty_pct")
  pov_geo <- .weighted_poverty(df_long, partitions$geo) |>
    dplyr::rename(poverty_geo = "poverty_pct")

  poverty <- mun_meta |>
    dplyr::left_join(pov_sex, by = "CVEGEO") |>
    dplyr::left_join(pov_age, by = "CVEGEO") |>
    dplyr::left_join(pov_geo, by = "CVEGEO")

  cmp <- poverty |>
    dplyr::filter(
      !is.na(.data$poverty_sex),
      !is.na(.data$poverty_age),
      !is.na(.data$poverty_geo)
    ) |>
    dplyr::mutate(
      diff_sex_age = abs(.data$poverty_sex - .data$poverty_age),
      diff_sex_geo = abs(.data$poverty_sex - .data$poverty_geo),
      diff_age_geo = abs(.data$poverty_age - .data$poverty_geo),
      max_diff     = pmax(
        .data$diff_sex_age, .data$diff_sex_geo, .data$diff_age_geo
      )
    )

  n_within_2pp <- mean(cmp$max_diff < 2.0)
  n_within_5pp <- mean(cmp$max_diff < 5.0)
  cor_sex_age  <- cor(cmp$poverty_sex, cmp$poverty_age,  use = "complete.obs")
  cor_sex_geo  <- cor(cmp$poverty_sex, cmp$poverty_geo,  use = "complete.obs")
  cor_age_geo  <- cor(cmp$poverty_age, cmp$poverty_geo,  use = "complete.obs")

  message(
    "=== Partition comparison (n = ", nrow(cmp),
    " municipalities with all 3) ==="
  )
  message(sprintf(
    "  Max diff < 2 pp:    %.1f%% of municipalities", 100 * n_within_2pp
  ))
  message(sprintf(
    "  Max diff < 5 pp:    %.1f%% of municipalities", 100 * n_within_5pp
  ))
  message(sprintf("  Cor(sex, age):      %.4f", cor_sex_age))
  message(sprintf("  Cor(sex, geo):      %.4f", cor_sex_geo))
  message(sprintf("  Cor(age, geo):      %.4f", cor_age_geo))
  message(sprintf("  Max discrepancy:    %.2f pp (CVEGEO: %s)",
                  max(cmp$max_diff, na.rm = TRUE),
                  cmp$CVEGEO[which.max(cmp$max_diff)]))

  if (n_within_2pp < 0.90) {
    warning(sprintf(
      "Only %.0f%% of municipalities have max_diff < 2pp across partitions.",
      100 * n_within_2pp
    ))
  }

  poverty <- poverty |>
    dplyr::mutate(

      n_estimates = (!is.na(.data$poverty_sex)) +
        (!is.na(.data$poverty_age)) + (!is.na(.data$poverty_geo)),
      poverty_final = (
        .data$poverty_sex + .data$poverty_age + .data$poverty_geo
      ) / .data$n_estimates,

      flag_partition_divergence = dplyr::case_when(
        .data$n_estimates < 3L ~ NA,
        pmax(abs(.data$poverty_sex - .data$poverty_age),
             abs(.data$poverty_sex - .data$poverty_geo),
             abs(.data$poverty_age - .data$poverty_geo)) > 5.0 ~ TRUE,
        TRUE ~ FALSE
      )
    )

  n_diverged <- sum(poverty$flag_partition_divergence, na.rm = TRUE)
  if (n_diverged > 0L) {
    message(sprintf(
      "  WARNING: %d municipalities flagged (max_diff > 5pp)", n_diverged
    ))
    message("  Flagged municipalities:")
    poverty |>
      dplyr::filter(.data$flag_partition_divergence) |>
      dplyr::mutate(max_d = pmax(
        abs(.data$poverty_sex - .data$poverty_age),
        abs(.data$poverty_sex - .data$poverty_geo),
        abs(.data$poverty_age - .data$poverty_geo)
      )) |>
      dplyr::arrange(dplyr::desc(.data$max_d)) |>
      dplyr::select(
        "CVEGEO", "NOM_MUN", "NOM_ENT",
        "poverty_sex", "poverty_age", "poverty_geo", "max_d"
      ) |>
      head(20) |>
      (function(d) message(capture.output(print(as.data.frame(d)))))()
  }

  out <- poverty |>
    dplyr::select(
      "CVEGEO",
      "CVE_ENT",
      "NOM_ENT",
      "NOM_MUN",
      "poverty_sex",
      "poverty_age",
      "poverty_geo",
      "poverty_final",
      "n_estimates",
      "flag_partition_divergence"
    ) |>
    dplyr::arrange(.data$CVEGEO)

  message(sprintf(
    "  Final output: %d municipalities, %d with poverty_final non-NA",
    nrow(out),
    sum(!is.na(out$poverty_final))
  ))

  dir.create(dirname(out_parquet), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(out, out_parquet, compression = "zstd")
  message(sprintf("Parquet written: %s", out_parquet))

  readr::write_csv(out, out_csv)
  message(sprintf("CSV written:    %s", out_csv))

  invisible(out_parquet)
}
