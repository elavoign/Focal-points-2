# targets/elasticity_poverty.R
#
# Targets factory for Tasks C and D: municipality-specific price elasticities
# and their semiparametric relationship with municipal poverty.
#
# Depends on: mun_month_poverty_parquet  (from shaun_mun_month factory)
#
# New targets added:
#   mun_elasticities_flag
#     Runs the full elasticity × poverty analysis; returns a flag file.
#     Side-effects written to disk:
#       data/analysis/elasticity/mun_elasticities.parquet
#       outputs/shaun/elasticity/elasticity_summary.xlsx
#       outputs/shaun/elasticity/form1_scatter.pdf
#       outputs/shaun/elasticity/form1_bins.pdf
#       outputs/shaun/elasticity/form2_gam.pdf
#       outputs/shaun/elasticity/elasticity_poverty_bins.{parquet,csv}

suppressPackageStartupMessages({
  library(targets)
})

elasticity_poverty <- function() {
  list(
    # Main specification: annual time FE (retains seasonal + monthly variation)
    tar_target(
      mun_elasticities_flag,
      {
        mun_month_poverty_parquet  # explicit dependency

        run_elasticity_poverty_analysis(
          poverty_panel_parquet = mun_month_poverty_parquet,
          out_dir               = "outputs/shaun/elasticity",
          betas_parquet         = "data/analysis/elasticity/mun_elasticities.parquet",
          min_obs               = 12L,
          bin_width             = 2,
          time_fe_var           = "year",
          winsor_pct            = 0.02
        )
      },
      format = "file"
    ),

    # Robustness: year-month time FE (absorbs all common monthly shocks;
    # identification comes from idiosyncratic within-municipality price variation only)
    tar_target(
      mun_elasticities_yr_month_flag,
      {
        mun_month_poverty_parquet  # explicit dependency

        run_elasticity_poverty_analysis(
          poverty_panel_parquet = mun_month_poverty_parquet,
          out_dir               = "outputs/shaun/elasticity_yr_month",
          betas_parquet         = "data/analysis/elasticity/mun_elasticities_yr_month.parquet",
          min_obs               = 12L,
          bin_width             = 2,
          time_fe_var           = "year_month",
          winsor_pct            = 0.02
        )
      },
      format = "file"
    )
  )
}
