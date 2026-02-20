###############################################################################
# GCB_Data_Pl.R
# ──────────────────────────────────────────────────────────────────────────────
# Spatial distribution and time-series plots for nominal CPUE of each species.
#
# Output:
#   - Fig_S1 : 4-row panel (ALB_N, ALB_S, BET, YFT) with spatial mean map
#              and monthly time-series side by side
#
# Requires objects already in the workspace:
#   northern_alb_model_data, southern_alb_model_data,
#   bet_model_data, yft_model_data,
#   theme_gcb(), save_plot_all()
###############################################################################


# =============================================================================
# 0. Packages
# =============================================================================
library(dplyr)
library(ggplot2)
library(patchwork)
library(colorspace)
library(maps)
library(metR)
library(scales)


# =============================================================================
# 1. Settings
# =============================================================================
species_colors <- c(
  ALB_N = "#4B3F8C",
  ALB_S = "#00A6D6",
  BET   = "#1B9E77",
  YFT   = "#D95F02"
)

worldMap <- fortify(map_data("world2"))
BIN_DEG  <- 2.5


# =============================================================================
# 2. Data preparation helpers
# =============================================================================

# -- Bin to spatial grid and compute mean value --
prep_spatial_mean <- function(df, value_col,
                              lon_col = "Lon", lat_col = "Lat",
                              lon_lim = c(102, 290),
                              lat_lim = c(-53, 53),
                              bin_deg = 2.5) {

  stopifnot(all(c(value_col, lon_col, lat_col) %in% names(df)))

  df %>%
    transmute(
      Lon = .data[[lon_col]],
      Lat = .data[[lat_col]],
      Val = .data[[value_col]]
    ) %>%
    filter(is.finite(Lon), is.finite(Lat), is.finite(Val)) %>%
    filter(Lon >= lon_lim[1], Lon <= lon_lim[2],
           Lat >= lat_lim[1], Lat <= lat_lim[2]) %>%
    mutate(
      Lon = floor(Lon / bin_deg) * bin_deg + bin_deg / 2,
      Lat = floor(Lat / bin_deg) * bin_deg + bin_deg / 2
    ) %>%
    group_by(Lon, Lat) %>%
    summarise(Val = mean(Val, na.rm = TRUE), .groups = "drop")
}

# -- Monthly mean time-series --
prep_ts_mean <- function(df, value_col) {
  stopifnot(all(c("Year", "Month", value_col) %in% names(df)))

  df %>%
    transmute(
      Year  = as.integer(Year),
      Month = as.integer(Month),
      Val   = .data[[value_col]]
    ) %>%
    filter(is.finite(Val), !is.na(Year), !is.na(Month)) %>%
    group_by(Year, Month) %>%
    summarise(Val = mean(Val, na.rm = TRUE), .groups = "drop") %>%
    mutate(Date = as.Date(sprintf("%04d-%02d-01", Year, Month))) %>%
    arrange(Date)
}


# =============================================================================
# 3. Paired spatial + time-series plot builder
# =============================================================================
make_space_ts_pair_gcb <- function(df, value_col, label, base_col,
                                   lon_lim = c(102, 290),
                                   lat_lim = c(-53, 53),
                                   bin_deg = 2.5,
                                   val_lim,
                                   val_break,
                                   show_size_legend = TRUE,
                                   point_alpha = 0.85) {

  sp <- prep_spatial_mean(df, value_col,
                          lon_lim = lon_lim, lat_lim = lat_lim,
                          bin_deg = bin_deg)
  ts <- prep_ts_mean(df, value_col)

  # Gradient palette derived from the species base colour
  pal <- c(
    lighten(base_col, 0.65),
    lighten(base_col, 0.40),
    base_col,
    darken(base_col, 0.25),
    darken(base_col, 0.45)
  )

  sp <- sp %>%
    mutate(Val_plot = pmin(pmax(Val, val_lim[1]), val_lim[2]))

  # Spatial map
  p_sp <- ggplot() +
    geom_point(
      data  = sp,
      aes(x = Lon, y = Lat, size = Val_plot, color = Val_plot),
      alpha = point_alpha
    ) +
    geom_polygon(
      data = worldMap,
      aes(x = long, y = lat, group = group),
      fill = "grey70", color = "grey70", linewidth = 0.25
    ) +
    coord_sf(xlim = lon_lim, ylim = lat_lim, expand = FALSE) +
    scale_color_gradientn(
      colors = pal,
      name   = label,
      breaks = val_break,
      limits = val_lim
    ) +
    scale_size_continuous(
      name   = "Nominal CPUE",
      range  = c(0.25, 3.8),
      breaks = val_break,
      limits = val_lim,
      guide  = if (show_size_legend) "legend" else "none"
    ) +
    scale_x_longitude(name = "Longitude", breaks = seq(100, 300, 40)) +
    scale_y_latitude(name = "Latitude",   breaks = seq(-40, 40, 20)) +
    labs(title = paste0(label, " | Spatial mean")) +
    theme_gcb(base_size = 10) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0)
    ) +
    guides(
      size  = guide_legend(order = 1, override.aes = list(alpha = 1)),
      color = guide_colorbar(order = 2)
    )

  # Time-series panel
  p_ts <- ggplot(ts, aes(x = Date, y = Val)) +
    geom_smooth(
      method = "loess", se = FALSE,
      color = "grey70", linewidth = 2.0, alpha = 0.28
    ) +
    geom_line(color = base_col, linewidth = 0.9, alpha = 0.95) +
    scale_x_date(
      date_breaks = "2 years",
      date_labels = "%Y",
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    scale_y_continuous(
      breaks = pretty_breaks(n = 6),
      expand = expansion(mult = c(0.02, 0.06))
    ) +
    labs(
      title = paste0(label, " | Time-series mean"),
      x     = "Year",
      y     = paste0("nCPUE_", label)
    ) +
    theme_gcb(base_size = 10) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0),
      axis.title = element_text(face = "bold")
    )

  list(sp = p_sp, ts = p_ts)
}


# =============================================================================
# 4. Compute shared value limits across all species
# =============================================================================
sp_ALB_N <- prep_spatial_mean(northern_alb_model_data, "nCPUE_ALB",
                              lon_lim = c(102, 290), lat_lim = c(0, 53),
                              bin_deg = BIN_DEG)
sp_ALB_S <- prep_spatial_mean(southern_alb_model_data, "nCPUE_ALB",
                              lon_lim = c(102, 290), lat_lim = c(-53, 0),
                              bin_deg = BIN_DEG)
sp_BET   <- prep_spatial_mean(bet_model_data, "nCPUE_BET",
                              lon_lim = c(102, 290), lat_lim = c(-53, 53),
                              bin_deg = BIN_DEG)
sp_YFT   <- prep_spatial_mean(yft_model_data, "nCPUE_YFT",
                              lon_lim = c(102, 290), lat_lim = c(-53, 53),
                              bin_deg = BIN_DEG)

val_lim   <- range(c(sp_ALB_N$Val, sp_ALB_S$Val, sp_BET$Val, sp_YFT$Val),
                   na.rm = TRUE)
val_break <- pretty_breaks(n = 5)(val_lim)


# =============================================================================
# 5. Build species panels
# =============================================================================
p_ALB_N <- make_space_ts_pair_gcb(
  df = northern_alb_model_data, value_col = "nCPUE_ALB", label = "ALB_N",
  base_col = species_colors["ALB_N"],
  lon_lim = c(102, 290), lat_lim = c(0, 53), bin_deg = BIN_DEG,
  val_lim = val_lim, val_break = val_break,
  show_size_legend = TRUE
)

p_ALB_S <- make_space_ts_pair_gcb(
  df = southern_alb_model_data, value_col = "nCPUE_ALB", label = "ALB_S",
  base_col = species_colors["ALB_S"],
  lon_lim = c(102, 290), lat_lim = c(-53, 0), bin_deg = BIN_DEG,
  val_lim = val_lim, val_break = val_break,
  show_size_legend = FALSE
)

p_BET <- make_space_ts_pair_gcb(
  df = bet_model_data, value_col = "nCPUE_BET", label = "BET",
  base_col = species_colors["BET"],
  lon_lim = c(102, 290), lat_lim = c(-53, 53), bin_deg = BIN_DEG,
  val_lim = val_lim, val_break = val_break,
  show_size_legend = FALSE
)

p_YFT <- make_space_ts_pair_gcb(
  df = yft_model_data, value_col = "nCPUE_YFT", label = "YFT",
  base_col = species_colors["YFT"],
  lon_lim = c(102, 290), lat_lim = c(-53, 53), bin_deg = BIN_DEG,
  val_lim = val_lim, val_break = val_break,
  show_size_legend = FALSE
)


# =============================================================================
# 6. Assemble combined figure  -->  Fig_S1
# =============================================================================
p_space_ts_4 <-
  (p_ALB_N$sp | p_ALB_N$ts) /
  (p_ALB_S$sp | p_ALB_S$ts) /
  (p_BET$sp   | p_BET$ts)   /
  (p_YFT$sp   | p_YFT$ts)   +
  plot_layout(
    guides  = "collect",
    widths  = c(4, 1.5),
    heights = c(1, 1, 2, 2)
  ) +
  plot_annotation(tag_levels = "a") &
  theme(
    legend.position = "bottom",
    plot.tag        = element_text(face = "bold", size = 14)
  )

p_space_ts_4

save_plot_all(
  p             = p_space_ts_4,
  filename_base = "Fig_S1",
  width         = 14,
  height        = 12,
  dpi           = 600
)
