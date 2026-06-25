suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(janitor)
  library(arrow)
  library(readr)
  library(stringr)
})

process_inegi_censo <- function(
  path_in = "data/raw_public/SAIC_Exporta_2026318_10052423.xlsx",
  path_out = "data/processed/inegi_censo/inegi_censo.parquet"
) {

  df <- read_excel(path_in, skip = 4)

  df <- df |> clean_names()

  df <- df |>
    rename(
      year = ano_censal,
      entidad = entidad,
      actividad = actividad_economica
    )

  df <- df |>
    mutate(
      actividad = stringr::str_squish(as.character(actividad))
    ) |>
    filter(
      grepl("468411", actividad)
    )

  df <- df |>
    rename(
      m000a = starts_with("m000a"),
      m010a = starts_with("m010a"),
      m020a = starts_with("m020a"),
      m030a = starts_with("m030a"),
      m050a = starts_with("m050a"),
      m700a = starts_with("m700a"),
      m090a = starts_with("m090a"),
      a800a = starts_with("a800a"),

      j000a = starts_with("j000a"),
      k010a = starts_with("k010a"),
      k020a = starts_with("k020a"),
      k030a = starts_with("k030a"),
      k042a = starts_with("k042a"),
      k412a = starts_with("k412a"),
      k050a = starts_with("k050a"),
      k610a = starts_with("k610a"),
      k620a = starts_with("k620a"),
      k060a = starts_with("k060a"),
      k070a = starts_with("k070a"),
      k810a = starts_with("k810a"),
      k820a = starts_with("k820a"),
      k910a = starts_with("k910a"),
      k950a = starts_with("k950a"),
      k096a = starts_with("k096a"),
      k976a = starts_with("k976a"),
      k090a = starts_with("k090a"),
      a700a = starts_with("a700a"),

      a131a = starts_with("a131a")
    )

  df <- df |>
    mutate(
      year = readr::parse_number(as.character(year)),
      across(-c(year, entidad, actividad), ~ readr::parse_number(as.character(.)))
    )

  df <- df |>
    filter(!is.na(year))

  df <- df |>
    mutate(
      ebitda = a131a - j000a,
      ebitda_revenue = dplyr::if_else(
        !is.na(m000a) & m000a != 0,
        ebitda / m000a,
        NA_real_
      )
    )

  dir.create(dirname(path_out), recursive = TRUE, showWarnings = FALSE)

  write_parquet(df, path_out)

  return(path_out)
}
