suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(arrow)
  library(ggplot2)
  library(stringr)
  library(scales)
  library(grid)
})

read_prepost_cvegeo <- function(prepost_parquet) {
  df <- arrow::read_parquet(prepost_parquet, mmap = FALSE) %>% as_tibble()
  if (!("CVEGEO" %in% names(df))) stop("prepost_cvegeo parquet missing column CVEGEO")
  if (!("period" %in% names(df))) stop("prepost_cvegeo parquet missing column period (pre/post)")
  df %>% mutate(CVEGEO = stringr::str_pad(as.character(CVEGEO), 5, pad = "0"))
}

read_municipios_geo <- function(municipios_geoparquet) {
  sf::read_sf(municipios_geoparquet) %>%
    mutate(CVEGEO = stringr::str_pad(as.character(CVEGEO), 5, pad = "0"))
}

spread_vars_from_prepost <- function(df_prepost) {
  v <- names(df_prepost)
  v[grepl("^spread_", v)]
}

get_prepost_long_onevar <- function(df_prepost, var) {
  if (!(var %in% names(df_prepost))) stop(sprintf("Variable %s not found in prepost df.", var))

  pre  <- df_prepost %>% filter(period == "pre")  %>% select(CVEGEO, value = all_of(var))
  post <- df_prepost %>% filter(period == "post") %>% select(CVEGEO, value = all_of(var))

  diff <- post %>%
    left_join(pre, by = "CVEGEO", suffix = c("_post", "_pre")) %>%
    transmute(CVEGEO, value = value_post - value_pre)

  list(pre = pre, post = post, diff = diff)
}

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

pretty_var_label <- function(spread_var) {
  x <- spread_var
  x <- gsub("^spread_", "", x)
  x <- gsub("_", " ", x)

  x <- gsub("\\bretail\\b", "Retail", x)
  x <- gsub("\\bstation\\b", "Retail (Stations)", x)
  x <- gsub("\\bterminal\\b", "Terminal (Rack)", x)
  x <- gsub("\\bint\\b", "International", x)
  x <- gsub("\\bregular\\b", "Regular", x)
  x <- gsub("\\bdiesel\\b", "Diesel", x)
  x <- gsub("\\bpremium\\b", "Premium", x)

  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

window_label <- function(window_months) {
  ifelse(as.integer(window_months) == 1L, "1 month", paste0(as.integer(window_months), " months"))
}

period_title <- function(kind, reform_date = "2025-03-03", window_months = 1L) {
  wlab <- window_label(window_months)

  if (kind == "pre") {
    return(paste0("Pre period (", wlab, " before ", reform_date, ")"))
  }
  if (kind == "post") {
    return(paste0("Post period (", wlab, " after ", reform_date, ")"))
  }
  if (kind == "diff") {
    return("Change (Post − Pre)")
  }
  kind
}

compute_global_abs_limit <- function(df_prepost, vars) {
  df_pre  <- df_prepost %>% filter(period == "pre")
  df_post <- df_prepost %>% filter(period == "post")

  max_prepost <- suppressWarnings(
    max(abs(unlist(df_pre[, vars, drop = FALSE])),
        abs(unlist(df_post[, vars, drop = FALSE])),
        na.rm = TRUE)
  )

  max_diff <- 0
  for (v in vars) {
    pre  <- df_pre  %>% select(CVEGEO, pre  = all_of(v))
    post <- df_post %>% select(CVEGEO, post = all_of(v))
    dd   <- post %>% left_join(pre, by = "CVEGEO") %>% mutate(diff = post - pre)
    md   <- suppressWarnings(max(abs(dd$diff), na.rm = TRUE))
    if (is.finite(md)) max_diff <- max(max_diff, md)
  }

  L <- max(max_prepost, max_diff)
  if (!is.finite(L) || L <= 0) L <- 1
  L
}

plot_municipio_choropleth <- function(sf_mun, df_vals, title, subtitle, out_path) {
  gdf <- sf_mun %>% left_join(df_vals, by = "CVEGEO")

  p <- ggplot(gdf) +
    geom_sf(aes(fill = value), linewidth = 0) +
    coord_sf(datum = NA) +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      limits = c(-5, 5),
      oob = scales::squish,
      na.value = "grey92"
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 14, margin = margin(b = 4)),
      plot.subtitle = element_text(size = 11, color = "grey30", margin = margin(b = 8)),
      axis.title = element_blank(),
      axis.text  = element_blank(),
      panel.grid = element_blank(),
      legend.position   = "bottom",
      legend.title      = element_blank(),
      legend.text       = element_text(size = 11),
      legend.key.width  = unit(4.0, "cm"),
      legend.key.height = unit(1.0, "cm")
    )

  ensure_dir(dirname(out_path))
  ggsave(filename = out_path, plot = p, width = 10.5, height = 7.2, dpi = 240)
  out_path
}

make_maps_cvegeo_one_spread <- function(
  df_prepost,
  mun_sf,
  spread_var,
  out_dir,
  window_months = 1L,
  reform_date = "2025-03-03"
) {
  parts <- get_prepost_long_onevar(df_prepost, spread_var)
  label <- pretty_var_label(spread_var)

  out_pre  <- file.path(out_dir, "pre",  paste0(spread_var, ".png"))
  out_post <- file.path(out_dir, "post", paste0(spread_var, ".png"))
  out_diff <- file.path(out_dir, "diff", paste0(spread_var, ".png"))

  plot_municipio_choropleth(
    mun_sf, parts$pre,
    title = paste0("Municipality heat map: ", label),
    subtitle = period_title("pre", reform_date = reform_date, window_months = window_months),
    out_path = out_pre
  )

  plot_municipio_choropleth(
    mun_sf, parts$post,
    title = paste0("Municipality heat map: ", label),
    subtitle = period_title("post", reform_date = reform_date, window_months = window_months),
    out_path = out_post
  )

  plot_municipio_choropleth(
    mun_sf, parts$diff,
    title = paste0("Municipality heat map: ", label),
    subtitle = period_title("diff", reform_date = reform_date, window_months = window_months),
    out_path = out_diff
  )

  c(out_pre, out_post, out_diff)
}

make_maps_cvegeo_all_spreads <- function(
  prepost_cvegeo_parquet,
  municipios_geoparquet,
  out_dir = "outputs/maps/cvegeo_spreads",
  window_months = 1L,
  reform_date = "2025-03-03"
) {
  ensure_dir(out_dir)

  df_prepost <- read_prepost_cvegeo(prepost_cvegeo_parquet)
  mun_sf     <- read_municipios_geo(municipios_geoparquet)

  vars <- spread_vars_from_prepost(df_prepost)
  if (length(vars) == 0) stop("No spread_* variables found in prepost_cvegeo parquet.")

  out <- unlist(lapply(vars, function(v) {
    make_maps_cvegeo_one_spread(
      df_prepost = df_prepost,
      mun_sf     = mun_sf,
      spread_var = v,
      out_dir    = out_dir,
      window_months = window_months,
      reform_date = reform_date
    )
  }), use.names = FALSE)

  out
}
