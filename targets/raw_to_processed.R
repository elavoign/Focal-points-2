# targets/raw_to_processed.R

suppressPackageStartupMessages({
  library(targets)
})

raw_to_processed <- function() list(

  # ========================
  # SCRIPTS
  # ========================

  tar_target(
    script_terminal_id,
    "R/utils/terminal_id.R",
    format = "file"
  ),

  tar_target(
    script_station_id,
    "R/utils/station_id.R",
    format = "file"
  ),

  tar_target(
    script_retail,
    "R/Raw_to_Processed/process_retail_year.R",
    format = "file"
  ),

  tar_target(
    script_terminal_year,
    "R/Raw_to_Processed/process_terminal_year.R",
    format = "file"
  ),

  tar_target(
    script_stations,
    "R/Raw_to_Processed/process_stations.R",
    format = "file"
  ),

  # ========================
  # INPUTS RAW
  # ========================

  tar_target(
    terminal_csv,
    "data/raw_public/terminal_prices/Terminal.csv",
    format = "file"
  ),

  tar_target(
    stations_rda,
    "data/raw_private/stations/Stations.rda",
    format = "file"
  ),

  # ========================
  # RETAIL (2017–2025)
  # ========================

  tar_target(
    retail_2017_parquet,
    {
      source(script_station_id)
      source(script_retail)
      out <- "data/processed/retail/year=2017/retail.parquet"
      process_retail_year(
        in_csv = "data/raw_public/prices_retail/Retail_2017.csv",
        out_parquet = out,
        year = 2017
      )
      out
    },
    format = "file",
    packages = c("readr", "dplyr", "stringr", "arrow")
  ),

  tar_target(
    retail_2018_parquet,
    {
      source(script_station_id)
      source(script_retail)
      out <- "data/processed/retail/year=2018/retail.parquet"
      process_retail_year(
        in_csv = "data/raw_public/prices_retail/Retail_2018.csv",
        out_parquet = out,
        year = 2018
      )
      out
    },
    format = "file",
    packages = c("readr", "dplyr", "stringr", "arrow")
  ),

  tar_target(
    retail_2019_parquet,
    {
      source(script_station_id)
      source(script_retail)
      out <- "data/processed/retail/year=2019/retail.parquet"
      process_retail_year(
        in_csv = "data/raw_public/prices_retail/Retail_2019.csv",
        out_parquet = out,
        year = 2019
      )
      out
    },
    format = "file",
    packages = c("readr", "dplyr", "stringr", "arrow")
  ),

  tar_target(
    retail_2020_parquet,
    {
      source(script_station_id)
      source(script_retail)
      out <- "data/processed/retail/year=2020/retail.parquet"
      process_retail_year(
        in_csv = "data/raw_public/prices_retail/Retail_2020.csv",
        out_parquet = out,
        year = 2020
      )
      out
    },
    format = "file",
    packages = c("readr", "dplyr", "stringr", "arrow")
  ),

  tar_target(
    retail_2021_parquet,
    {
      source(script_station_id)
      source(script_retail)
      out <- "data/processed/retail/year=2021/retail.parquet"
      process_retail_year(
        in_csv = "data/raw_public/prices_retail/Retail_2021.csv",
        out_parquet = out,
        year = 2021
      )
      out
    },
    format = "file",
    packages = c("readr", "dplyr", "stringr", "arrow")
  ),

  tar_target(
    retail_2022_parquet,
    {
      source(script_station_id)
      source(script_retail)
      out <- "data/processed/retail/year=2022/retail.parquet"
      process_retail_year(
        in_csv = "data/raw_public/prices_retail/Retail_2022.csv",
        out_parquet = out,
        year = 2022
      )
      out
    },
    format = "file",
    packages = c("readr", "dplyr", "stringr", "arrow")
  ),

  tar_target(
    retail_2023_parquet,
    {
      source(script_station_id)
      source(script_retail)
      out <- "data/processed/retail/year=2023/retail.parquet"
      process_retail_year(
        in_csv = "data/raw_public/prices_retail/Retail_2023.csv",
        out_parquet = out,
        year = 2023
      )
      out
    },
    format = "file",
    packages = c("readr", "dplyr", "stringr", "arrow")
  ),

  tar_target(
    retail_2024_parquet,
    {
      source(script_station_id)
      source(script_retail)
      out <- "data/processed/retail/year=2024/retail.parquet"
      process_retail_year(
        in_csv = "data/raw_public/prices_retail/Retail_2024.csv",
        out_parquet = out,
        year = 2024
      )
      out
    },
    format = "file",
    packages = c("readr", "dplyr", "stringr", "arrow")
  ),

  tar_target(
    retail_2025_parquet,
    {
      source(script_station_id)
      source(script_retail)
      out <- "data/processed/retail/year=2025/retail.parquet"
      process_retail_year(
        in_csv = "data/raw_public/prices_retail/Retail_2025.csv",
        out_parquet = out,
        year = 2025
      )
      out
    },
    format = "file",
    packages = c("readr", "dplyr", "stringr", "arrow")
  ),

  # ========================
  # TERMINAL (2017–2025)
  # ========================

  tar_target(
    terminal_parquet_paths,
    {
      source(script_terminal_id)
      source(script_terminal_year)
      process_terminal_all_years(
        in_csv  = terminal_csv,
        out_dir = "data/processed/terminal",
        years   = 2017:2025
      )
    },
    format = "file",
    iteration = "list",
    packages = c("readr", "dplyr", "arrow")
  ),

  # ========================
  # STATIONS
  # ========================

  tar_target(
    stations_parquet,
    {
      source(script_station_id)
      source(script_terminal_id)
      source(script_stations)
      out <- "data/processed/stations/stations.parquet"
      process_stations(in_rda = stations_rda, out_parquet = out)
      out
    },
    format = "file",
    packages = c("dplyr", "stringr", "arrow")
  )

)
