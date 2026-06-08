suppressPackageStartupMessages({
  library(targets)
})

shaun_pooled_regression <- function() {
  list(

    # ------------------------------------------------------------------
    # 1. IEPS monthly series (from Excel — runs with partial data too)
    # ------------------------------------------------------------------
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

    # ------------------------------------------------------------------
    # 2. Bloomberg Gulf Coast gasoline spot prices (Regular 87 + Premium 93)
    # ------------------------------------------------------------------
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

    # ------------------------------------------------------------------
    # 3. Municipal income conditional on car ownership (INEGI Vehículos)
    # ------------------------------------------------------------------
    tar_target(
      income_car_owners_parquet,
      process_inegi_vehiculos_income(
        vehiculos_dir = "data/raw_public/Inegi Vehiculos",
        out_parquet   = "data/processed/inegi_vehiculos/municipal_income_car_owners.parquet"
      ),
      format = "file"
    ),

    # ------------------------------------------------------------------
    # 3b. Spread diagnostic plots (Shaun Point 2)
    #     Absolute and relative spread vs. price level for PEMEX terminal
    #     and Bloomberg Gulf Coast data.
    # ------------------------------------------------------------------
    tar_target(
      spread_diagnostic_outputs,
      {
        bloomberg_gasoline_parquet
        build_spread_diagnostic_plots(
          bloomberg_parquet = bloomberg_gasoline_parquet,
          terminal_dir      = "data/processed/terminal",
          out_dir           = "outputs/shaun/spread_diagnostics"
        )
      },
      format = "file"
    ),

    # ------------------------------------------------------------------
    # 4. Results PDF (updated — all specs + spread diagnostics)
    # ------------------------------------------------------------------
    tar_target(
      results_updated_pdf,
      {
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
          out_path             = "outputs/shaun/results_updated.pdf"
        )
      },
      format = "file"
    ),

    # ------------------------------------------------------------------
    # 5a. Pooled regressions — full sample
    # ------------------------------------------------------------------
    tar_target(
      pooled_regression_outputs,
      {
        mun_month_poverty_parquet   # upstream dependency
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

    # ------------------------------------------------------------------
    # 4b. Pooled regressions — restricted sample
    #     Excludes states with large informal gasoline markets:
    #     07 Chiapas | 12 Guerrero | 20 Oaxaca | 21 Puebla
    # ------------------------------------------------------------------
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
