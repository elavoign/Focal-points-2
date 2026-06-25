suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(arrow)
  library(ggplot2)
  library(scales)
  library(grid)
})

REFORM_DATE <- as.Date("2025-03-03")

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

theme_pub <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 14, margin = margin(b = 6)),
      plot.subtitle = element_text(size = 11, color = "grey30", margin = margin(b = 10)),
      plot.caption  = element_blank(),
      axis.title.x  = element_blank(),
      axis.title.y  = element_text(size = 11, margin = margin(r = 8)),
      axis.text     = element_text(color = "grey20"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text  = element_text(size = 10),
      legend.key.width  = grid::unit(4.0, "cm"),
      legend.key.height = grid::unit(1.0, "cm")
    )
}

save_png <- function(path, plot, w = 10, h = 6, dpi = 220) {
  ensure_dir(dirname(path))
  ggsave(filename = path, plot = plot, width = w, height = h, dpi = dpi)
  path
}

robust_range <- function(x, probs = c(0.01, 0.99)) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(c(0, 1))
  qs <- as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE))
  if (!is.finite(qs[1]) || !is.finite(qs[2]) || qs[1] == qs[2]) {
    rg <- range(x, na.rm = TRUE)
    if (!is.finite(rg[1]) || !is.finite(rg[2]) || rg[1] == rg[2]) return(c(0, 1))
    return(rg)
  }
  qs
}

window_label <- function(window_months) {
  ifelse(as.integer(window_months) == 1L, "1 month", paste0(as.integer(window_months), " months"))
}

plot_density_pretty <- function(df, xvar, title, subtitle, xlab,
                                xlim = NULL, out_path,
                                add_hist = TRUE,
                                fill_area = FALSE,
                                area_fill = "purple") {
  p <- ggplot(df, aes(x = .data[[xvar]]))

  if (fill_area) add_hist <- FALSE

  if (add_hist) {
    p <- p +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 60,
                     alpha = 0.18,
                     color = NA)
  }

  if (fill_area) {
    p <- p + geom_area(stat = "density", alpha = 0.25, fill = area_fill, na.rm = TRUE)
  }

  p <- p +
    geom_density(linewidth = 1.0, adjust = 1.05, na.rm = TRUE) +
    labs(
      title = title,
      subtitle = subtitle,
      y = "Density",
      x = xlab
    ) +
    theme_pub()

  if (!is.null(xlim)) {
    p <- p + coord_cartesian(xlim = xlim)
  }

  save_png(out_path, p, w = 9.5, h = 4, dpi = 220)
}

plot_density_overlay_pretty <- function(df_pre, df_post, xvar, title, subtitle, xlab,
                                        xlim = NULL, out_path,
                                        add_hist = FALSE) {
  dd <- bind_rows(
    df_pre  %>% transmute(value = .data[[xvar]], period = "Pre"),
    df_post %>% transmute(value = .data[[xvar]], period = "Post")
  )

  p <- ggplot(dd, aes(x = value, fill = period, color = period))

  if (add_hist) {
    p <- p +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 60,
                     position = "identity",
                     alpha = 0.10,
                     color = NA)
  }

  p <- p +
    geom_density(alpha = 0.20, linewidth = 1.0, adjust = 1.05, na.rm = TRUE) +
    labs(
      title = title,
      subtitle = subtitle,
      y = "Density",
      x = xlab
    ) +
    theme_pub()

  if (!is.null(xlim)) {
    p <- p + coord_cartesian(xlim = xlim)
  }

  save_png(out_path, p, w = 9.5, h = 4, dpi = 220)
}

make_national_price_timeseries <- function(daily_cvegeo_files,
                                          out_dir = "outputs/graphs/national_prices") {
  ensure_dir(out_dir)

  ds <- arrow::open_dataset(daily_cvegeo_files)

  nat <- ds %>%
    select(
      date,
      station_regular, terminal_regular, regular_int_mxn_l,
      station_diesel,  terminal_diesel,  diesel_int_mxn_l
    ) %>%
    collect() %>%
    mutate(date = as.Date(date)) %>%
    group_by(date) %>%
    summarise(
      station_regular  = mean(station_regular,  na.rm = TRUE),
      terminal_regular = mean(terminal_regular, na.rm = TRUE),
      int_regular      = mean(regular_int_mxn_l, na.rm = TRUE),
      station_diesel   = mean(station_diesel,   na.rm = TRUE),
      terminal_diesel  = mean(terminal_diesel,  na.rm = TRUE),
      int_diesel       = mean(diesel_int_mxn_l,  na.rm = TRUE),
      .groups = "drop"
    )

  plot_ts <- function(df, which = c("regular", "diesel")) {
    which <- match.arg(which)

    if (which == "regular") {
      long <- df %>%
        select(date,
               `Retail (stations)` = station_regular,
               `Terminal (rack)`   = terminal_regular,
               `International (MXN/L)` = int_regular) %>%
        pivot_longer(-date, names_to = "series", values_to = "value")

      title <- "National average price (Regular gasoline)"
      out <- file.path(out_dir, "national_prices_regular.png")
    } else {
      long <- df %>%
        select(date,
               `Retail (stations)` = station_diesel,
               `Terminal (rack)`   = terminal_diesel,
               `International (MXN/L)` = int_diesel) %>%
        pivot_longer(-date, names_to = "series", values_to = "value")

      title <- "National average price (Diesel)"
      out <- file.path(out_dir, "national_prices_diesel.png")
    }

    yr0 <- as.integer(format(min(long$date, na.rm = TRUE), "%Y"))
    yr1 <- as.integer(format(max(long$date, na.rm = TRUE), "%Y"))
    breaks_year <- seq(as.Date(paste0(yr0, "-01-01")),
                       as.Date(paste0(yr1, "-01-01")),
                       by = "1 year")

    subtitle <- paste0(
      "Daily national mean (simple average across CVEGEO). Reform date: ",
      format(REFORM_DATE, "%Y-%m-%d"),
      "."
    )

    p <- ggplot(long, aes(x = date, y = value, color = series)) +
      geom_line(linewidth = 0.9, na.rm = TRUE) +
      geom_vline(xintercept = REFORM_DATE, linetype = "dashed", color = "red", linewidth = 0.8) +
      scale_x_date(breaks = breaks_year, date_labels = "%Y") +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      labs(
        title = title,
        subtitle = subtitle,
        y = "MXN per liter"
      ) +
      theme_pub()

    save_png(out, p, w = 10.5, h = 6.2, dpi = 220)
  }

  c(plot_ts(nat, "regular"), plot_ts(nat, "diesel"))
}

spread_vars_from_station_prepost <- function(df) {
  v <- names(df)
  v[grepl("^spread_", v)]
}

make_station_spread_distributions_all <- function(prepost_station_parquet,
                                                  out_dir = "outputs/graphs/station_spreads",
                                                  window_months = 1L) {
  ensure_dir(out_dir)
  ensure_dir(file.path(out_dir, "pre"))
  ensure_dir(file.path(out_dir, "post"))
  ensure_dir(file.path(out_dir, "overlay"))
  ensure_dir(file.path(out_dir, "diff"))

  df <- arrow::read_parquet(prepost_station_parquet) %>% as_tibble(, mmap = FALSE)

  if (!("period" %in% names(df))) stop("Station pre/post parquet missing 'period' column.")
  if (!("station_id" %in% names(df))) stop("Station pre/post parquet missing 'station_id' column.")

  vars <- spread_vars_from_station_prepost(df)
  if (length(vars) == 0) stop("Station pre/post parquet has no spread_* columns.")

  df_pre  <- df %>% filter(period == "pre")
  df_post <- df %>% filter(period == "post")

  wlab <- window_label(window_months)
  outs <- c()

  for (v in vars) {
    x_all <- c(df_pre[[v]], df_post[[v]])
    xlim <- robust_range(x_all, probs = c(0.01, 0.99))

    dpre  <- df_pre  %>% select(station_id, pre  = all_of(v))
    dpost <- df_post %>% select(station_id, post = all_of(v))
    ddiff <- dpost %>% left_join(dpre, by = "station_id") %>% mutate(diff = post - pre)
    xlim_diff <- robust_range(ddiff$diff, probs = c(0.01, 0.99))

    xlab <- v

    out_pre  <- file.path(out_dir, "pre",     paste0(v, "_pre.png"))
    out_post <- file.path(out_dir, "post",    paste0(v, "_post.png"))
    out_ovl  <- file.path(out_dir, "overlay", paste0(v, "_pre_post.png"))
    out_diff <- file.path(out_dir, "diff",    paste0(v, "_diff.png"))

    subtitle_pre  <- paste0("Pre (average over the ", wlab, " before the reform on ", format(REFORM_DATE, "%Y-%m-%d"), ").")
    subtitle_post <- paste0("Post (average over the ", wlab, " after the reform on ", format(REFORM_DATE, "%Y-%m-%d"), ").")
    subtitle_ovl  <- paste0("Pre = ", wlab, " before reform, Post = ", wlab, " after reform (", format(REFORM_DATE, "%Y-%m-%d"), ").")

    outs <- c(
      outs,
      plot_density_pretty(
        df_pre, v,
        title = paste0("Station distribution: ", v),
        subtitle = subtitle_pre,
        xlab = xlab,
        xlim = xlim,
        out_path = out_pre,
        add_hist = TRUE
      ),
      plot_density_pretty(
        df_post, v,
        title = paste0("Station distribution: ", v),
        subtitle = subtitle_post,
        xlab = xlab,
        xlim = xlim,
        out_path = out_post,
        add_hist = TRUE
      ),
      plot_density_overlay_pretty(
        df_pre, df_post, v,
        title = paste0("Station distribution: ", v),
        subtitle = subtitle_ovl,
        xlab = xlab,
        xlim = xlim,
        out_path = out_ovl,
        add_hist = FALSE
      ),
      plot_density_pretty(
        ddiff, "diff",
        title = paste0("Station distribution (Post − Pre): ", v),
        subtitle = "Difference across stations.",
        xlab = paste0(v, " (Post − Pre)"),
        xlim = xlim_diff,
        out_path = out_diff,
        add_hist = FALSE,
        fill_area = TRUE,
        area_fill = "purple"
      )
    )
  }

  outs
}

price_vars_from_station_prepost <- function(df) {
  wanted <- c("station_regular", "station_premium", "station_diesel")
  wanted[wanted %in% names(df)]
}

pretty_station_price_label <- function(v) {
  dplyr::case_when(
    v == "station_regular" ~ "Regular gasoline",
    v == "station_premium" ~ "Premium gasoline",
    v == "station_diesel"  ~ "Diesel",
    TRUE ~ v
  )
}

make_station_price_distributions_all <- function(prepost_station_price_parquet,
                                                 out_dir = "outputs/graphs/station_prices",
                                                 window_months = 1L) {
  ensure_dir(out_dir)
  ensure_dir(file.path(out_dir, "pre"))
  ensure_dir(file.path(out_dir, "post"))
  ensure_dir(file.path(out_dir, "overlay"))
  ensure_dir(file.path(out_dir, "diff"))

  df <- arrow::read_parquet(prepost_station_price_parquet) %>% as_tibble(, mmap = FALSE)

  if (!("period" %in% names(df))) stop("Station price pre/post parquet missing 'period' column.")
  if (!("station_id" %in% names(df))) stop("Station price pre/post parquet missing 'station_id' column.")

  vars <- price_vars_from_station_prepost(df)
  if (length(vars) == 0) stop("Station price pre/post parquet has no station_* price columns.")

  df_pre  <- df %>% filter(period == "pre")
  df_post <- df %>% filter(period == "post")

  wlab <- window_label(window_months)
  outs <- c()

  for (v in vars) {
    x_all <- c(df_pre[[v]], df_post[[v]])
    xlim <- robust_range(x_all, probs = c(0.01, 0.99))

    dpre  <- df_pre  %>% select(station_id, pre  = all_of(v))
    dpost <- df_post %>% select(station_id, post = all_of(v))
    ddiff <- dpost %>% left_join(dpre, by = "station_id") %>% mutate(diff = post - pre)
    xlim_diff <- robust_range(ddiff$diff, probs = c(0.01, 0.99))

    fuel_lab <- pretty_station_price_label(v)
    xlab <- "MXN per liter"

    out_pre  <- file.path(out_dir, "pre",     paste0(v, "_pre.png"))
    out_post <- file.path(out_dir, "post",    paste0(v, "_post.png"))
    out_ovl  <- file.path(out_dir, "overlay", paste0(v, "_pre_post.png"))
    out_diff <- file.path(out_dir, "diff",    paste0(v, "_diff.png"))

    subtitle_pre  <- paste0("Pre (average over the ", wlab, " before the reform on ", format(REFORM_DATE, "%Y-%m-%d"), ").")
    subtitle_post <- paste0("Post (average over the ", wlab, " after the reform on ", format(REFORM_DATE, "%Y-%m-%d"), ").")
    subtitle_ovl  <- paste0("Pre = ", wlab, " before reform, Post = ", wlab, " after reform (", format(REFORM_DATE, "%Y-%m-%d"), ").")

    outs <- c(
      outs,
      plot_density_pretty(
        df_pre, v,
        title = paste0("Station price distribution: ", fuel_lab),
        subtitle = subtitle_pre,
        xlab = xlab,
        xlim = xlim,
        out_path = out_pre,
        add_hist = TRUE
      ),
      plot_density_pretty(
        df_post, v,
        title = paste0("Station price distribution: ", fuel_lab),
        subtitle = subtitle_post,
        xlab = xlab,
        xlim = xlim,
        out_path = out_post,
        add_hist = TRUE
      ),
      plot_density_overlay_pretty(
        df_pre, df_post, v,
        title = paste0("Station price distribution: ", fuel_lab),
        subtitle = subtitle_ovl,
        xlab = xlab,
        xlim = xlim,
        out_path = out_ovl,
        add_hist = FALSE
      ),
      plot_density_pretty(
        ddiff, "diff",
        title = paste0("Station price distribution (Post − Pre): ", fuel_lab),
        subtitle = "Difference across stations.",
        xlab = "MXN per liter (Post − Pre)",
        xlim = xlim_diff,
        out_path = out_diff,
        add_hist = FALSE,
        fill_area = TRUE,
        area_fill = "purple"
      )
    )
  }

  outs
}

make_terminal_int_distributions <- function(prepost_terminal_parquet,
                                            out_dir = "outputs/graphs/terminal_int",
                                            window_months = 1L) {
  ensure_dir(out_dir)
  ensure_dir(file.path(out_dir, "pre"))
  ensure_dir(file.path(out_dir, "post"))
  ensure_dir(file.path(out_dir, "overlay"))
  ensure_dir(file.path(out_dir, "diff"))

  df <- arrow::read_parquet(prepost_terminal_parquet) %>% as_tibble(, mmap = FALSE)

  if (!("period" %in% names(df))) stop("Terminal pre/post parquet missing 'period' column.")
  if (!("terminal_id" %in% names(df))) stop("Terminal pre/post parquet missing 'terminal_id' column.")

  needed <- c("spread_terminal_int_regular", "spread_terminal_int_diesel")
  miss <- setdiff(needed, names(df))
  if (length(miss) > 0) {
    stop(paste0("Terminal pre/post parquet missing: ", paste(miss, collapse = ", "), "."))
  }

  df_pre  <- df %>% filter(period == "pre")
  df_post <- df %>% filter(period == "post")

  wlab <- window_label(window_months)

  make_one <- function(var, tag) {
    x_all <- c(df_pre[[var]], df_post[[var]])
    xlim <- robust_range(x_all, probs = c(0.01, 0.99))

    dpre  <- df_pre  %>% select(terminal_id, pre  = all_of(var))
    dpost <- df_post %>% select(terminal_id, post = all_of(var))
    ddiff <- dpost %>% left_join(dpre, by = "terminal_id") %>% mutate(diff = post - pre)
    xlim_diff <- robust_range(ddiff$diff, probs = c(0.01, 0.99))

    xlab <- var

    out_pre  <- file.path(out_dir, "pre",     paste0("terminal_int_", tag, "_pre.png"))
    out_post <- file.path(out_dir, "post",    paste0("terminal_int_", tag, "_post.png"))
    out_ovl  <- file.path(out_dir, "overlay", paste0("terminal_int_", tag, "_pre_post.png"))
    out_diff <- file.path(out_dir, "diff",    paste0("terminal_int_", tag, "_diff.png"))

    subtitle_pre  <- paste0("Pre (average over the ", wlab, " before the reform on ", format(REFORM_DATE, "%Y-%m-%d"), ").")
    subtitle_post <- paste0("Post (average over the ", wlab, " after the reform on ", format(REFORM_DATE, "%Y-%m-%d"), ").")
    subtitle_ovl  <- paste0("Pre = ", wlab, " before reform, Post = ", wlab, " after reform (", format(REFORM_DATE, "%Y-%m-%d"), ").")

    c(
      plot_density_pretty(
        df_pre, var,
        title = paste0("Terminal vs international spread: ", tag),
        subtitle = subtitle_pre,
        xlab = xlab,
        xlim = xlim,
        out_path = out_pre,
        add_hist = TRUE
      ),
      plot_density_pretty(
        df_post, var,
        title = paste0("Terminal vs international spread: ", tag),
        subtitle = subtitle_post,
        xlab = xlab,
        xlim = xlim,
        out_path = out_post,
        add_hist = TRUE
      ),
      plot_density_overlay_pretty(
        df_pre, df_post, var,
        title = paste0("Terminal vs international spread: ", tag),
        subtitle = subtitle_ovl,
        xlab = xlab,
        xlim = xlim,
        out_path = out_ovl,
        add_hist = FALSE
      ),
      plot_density_pretty(
        ddiff, "diff",
        title = paste0("Terminal vs international (Post − Pre): ", tag),
        subtitle = "Difference across terminals.",
        xlab = paste0(var, " (Post − Pre)"),
        xlim = xlim_diff,
        out_path = out_diff,
        add_hist = FALSE,
        fill_area = TRUE,
        area_fill = "purple"
      )
    )
  }

  c(
    make_one("spread_terminal_int_regular", "regular"),
    make_one("spread_terminal_int_diesel",  "diesel")
  )
}

pretty_quantile_label <- function(x) {
  dplyr::case_when(
    x == "0-25"   ~ "0 to 25 percentile",
    x == "25-50"  ~ "25 to 50 percentile",
    x == "50-75"  ~ "50 to 75 percentile",
    x == "75-100" ~ "75 to 100 percentile",
    TRUE ~ as.character(x)
  )
}

make_station_price_quantile_overlays <- function(station_quantiles_parquet,
                                                 out_dir = "outputs/graphs/station_price_quantiles",
                                                 window_months = 1L) {
  ensure_dir(out_dir)

  df <- arrow::read_parquet(station_quantiles_parquet) %>% as_tibble(, mmap = FALSE)

  needed <- c("station_id", "price_pre", "price_post", "quantile_label", "price_var")
  miss <- setdiff(needed, names(df))
  if (length(miss) > 0) {
    stop(paste0(
      "Station quantiles parquet missing: ",
      paste(miss, collapse = ", "),
      "."
    ))
  }

  fuel_var <- unique(df$price_var)
  if (length(fuel_var) != 1) {
    stop("Expected exactly one price_var in station quantiles parquet.")
  }

  fuel_lab <- pretty_station_price_label(fuel_var[1])

  q_levels <- c("0-25", "25-50", "50-75", "75-100")
  df <- df %>%
    filter(quantile_label %in% q_levels) %>%
    mutate(quantile_label = factor(quantile_label, levels = q_levels))

  wlab <- window_label(window_months)
  outs <- c()

  for (q in q_levels) {
    dsub <- df %>% filter(quantile_label == q)

    if (nrow(dsub) == 0) next

    x_all <- c(dsub$price_pre, dsub$price_post)
    xlim <- robust_range(x_all, probs = c(0.01, 0.99))

    dd_pre  <- dsub %>% transmute(value = price_pre,  period = "Pre")
    dd_post <- dsub %>% transmute(value = price_post, period = "Post")

    out_file <- file.path(
      out_dir,
      paste0(fuel_var[1], "_quantile_", gsub("-", "_", q), "_pre_post.png")
    )

    subtitle_q <- paste0(
      pretty_quantile_label(q),
      ". Groups defined using the average station price in the ",
      wlab,
      " before the reform (",
      format(REFORM_DATE, "%Y-%m-%d"),
      ")."
    )

    outs <- c(
      outs,
      plot_density_overlay_pretty(
        df_pre = dd_pre,
        df_post = dd_post,
        xvar = "value",
        title = paste0("Station price distribution: ", fuel_lab),
        subtitle = subtitle_q,
        xlab = "MXN per liter",
        xlim = xlim,
        out_path = out_file,
        add_hist = FALSE
      )
    )
  }

  outs
}
