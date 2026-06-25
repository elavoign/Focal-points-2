suppressPackageStartupMessages({
  library(targets)
})

shaun_pooled_regression <- function() {
  list(

    tar_target(
      ieps_xlsx,
      "data/raw_public/IEPS_Combustibles_Mexico.xlsx",
      format = "file"
    ),
    tar_target(
      ieps_monthly_parquet,
      {
        process_ieps_combustibles(
          xlsx_path   = ieps_xlsx,
          out_daily   = "data/processed/ieps/ieps_daily.parquet",
          out_monthly = "data/processed/ieps/ieps_monthly.parquet"
        )
      },
      format = "file"
    ),

    tar_target(
      bloomberg_xlsx,
      "data/raw_public/GASOLINE.xlsx",
      format = "file"
    ),
    tar_target(
      bloomberg_gasoline_parquet,
      {
        process_gasoline_bloomberg(
          xlsx_path   = bloomberg_xlsx,
          intl_dir    = "data/processed/international",
          out_parquet = "data/processed/bloomberg/gasoline_bloomberg.parquet"
        )
      },
      format = "file"
    ),

    tar_target(
      income_car_owners_parquet,
      process_inegi_vehiculos_income(
        vehiculos_dir = "data/raw_public/Inegi Vehiculos",
        out_parquet   = "data/processed/inegi_vehiculos/municipal_income_car_owners.parquet"
      ),
      format = "file"
    ),

    tar_target(
      spread_diagnostic_outputs,
      {
        mun_month_poverty_parquet
        bloomberg_gasoline_parquet
        build_spread_diagnostic_plots(
          base_parquet      = mun_month_poverty_parquet,
          bloomberg_parquet = bloomberg_gasoline_parquet,
          terminal_dir      = "data/processed/terminal",
          out_dir           = "outputs/shaun/spread_diagnostics"
        )
      },
      format = "file"
    ),

    tar_target(
      results_updated_pdf,
      {
        pooled_regression_outputs
        mun_month_poverty_parquet
        ieps_monthly_parquet
        income_car_owners_parquet
        bloomberg_gasoline_parquet
        build_results_pdf(
          base_parquet         = mun_month_poverty_parquet,
          ieps_monthly_parquet = ieps_monthly_parquet,
          income_parquet       = income_car_owners_parquet,
          terminal_dir         = "data/processed/terminal",
          bloomberg_parquet    = bloomberg_gasoline_parquet,
          out_path             = "outputs/shaun/results_updated.pdf",
          precomputed_dir      = "outputs/shaun/pooled_regression"
        )
      },
      format = "file"
    ),

    tar_target(
      pooled_regression_outputs,
      {
        mun_month_poverty_parquet
        ieps_monthly_parquet
        income_car_owners_parquet
        bloomberg_gasoline_parquet
        run_shaun_pooled_regression(
          base_parquet            = mun_month_poverty_parquet,
          ieps_monthly_parquet    = ieps_monthly_parquet,
          income_parquet          = income_car_owners_parquet,
          terminal_dir            = "data/processed/terminal",
          bloomberg_parquet       = bloomberg_gasoline_parquet,
          out_dir                 = "outputs/shaun/pooled_regression"
        )
      },
      format = "file"
    ),

    tar_target(
      pooled_regression_restricted_outputs,
      {
        mun_month_poverty_parquet
        ieps_monthly_parquet
        income_car_owners_parquet
        bloomberg_gasoline_parquet
        run_shaun_pooled_regression(
          base_parquet            = mun_month_poverty_parquet,
          ieps_monthly_parquet    = ieps_monthly_parquet,
          income_parquet          = income_car_owners_parquet,
          terminal_dir            = "data/processed/terminal",
          bloomberg_parquet       = bloomberg_gasoline_parquet,
          out_dir                 = "outputs/shaun/pooled_regression_restricted",
          restricted_states       = c("07", "12", "20", "21")
        )
      },
      format = "file"
    )

  )
}
