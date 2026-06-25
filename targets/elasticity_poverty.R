suppressPackageStartupMessages({
  library(targets)
})

elasticity_poverty <- function() {
  list(

    tar_target(
      mun_elasticities_flag,
      {
        mun_month_poverty_parquet

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

    tar_target(
      mun_elasticities_yr_month_flag,
      {
        mun_month_poverty_parquet

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
