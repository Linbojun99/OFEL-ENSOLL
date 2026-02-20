###############################################################################
# GCB_Spatial_Lonlat_Pl.R
# ──────────────────────────────────────────────────────────────────────────────
# Longitudinal and latitudinal profiles of relative abundance change (%)
# during La Nina and El Nino events, combined with spatial maps.
#
# Workflow:
#   1) Compute mean delta relative abundance by Lon / Lat per ENSO event
#   2) Plot longitudinal profiles (x = Lon, y = Delta %)
#   3) Plot latitudinal profiles  (x = Delta %, y = Lat)
#   4) Combine with spatial maps (p_all from upstream) into Fig_4
#
# Output:
#   - Fig_4 : maps (left) + lon/lat profiles (right)
#
# Requires objects already in the workspace:
#   rel_ALB_N, rel_ALB_S, rel_BET, rel_YFT  (Delta_pct, Lon, Lat, Event)
#   p_all                                     (spatial map panel)
#   theme_gcb(), save_plot_all()
#   scale_x_longitude(), scale_y_latitude()   (custom ggplot scales)
###############################################################################


# =============================================================================
# 0. Packages
# =============================================================================
library(stringr)


# =============================================================================
# 1. Helpers & constants
# =============================================================================

# Normalise ENSO phase labels (handle encoding variants)
normalize_phase3 <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "Ni\\s*n?a", "Niña")
  x <- str_replace_all(x, "Nina",       "Niña")
  x <- str_replace_all(x, "El\\s*Nino", "El Niño")
  x <- str_replace_all(x, "La\\s*Nina", "La Niña")
  str_trim(x)
}

line_colors  <- c("La Niña" = "#2B5AA6", "El Niño" = "#D43B3B")
event_levels <- c("La Niña", "El Niño")
delta_lim    <- c(-125, 125)


# =============================================================================
# 2. Profile computation
# =============================================================================
make_lon_lat_profiles <- function(rel_df, species_name,
                                  value_col    = "Delta_pct",
                                  lon_col      = "Lon",
                                  lat_col      = "Lat",
                                  event_col    = "Event",
                                  event_levels = c("La Niña", "El Niño"),
                                  lon_lim      = c(102, 290),
                                  lat_lim      = c(-53, 53)) {

  df <- rel_df %>%
    mutate(
      Species = species_name,
      Event   = normalize_phase3(.data[[event_col]]),
      Event   = factor(Event, levels = event_levels)
    ) %>%
    filter(!is.na(Event),
           .data[[lon_col]] >= lon_lim[1], .data[[lon_col]] <= lon_lim[2],
           .data[[lat_col]] >= lat_lim[1], .data[[lat_col]] <= lat_lim[2],
           is.finite(.data[[value_col]]))

  lon_prof <- df %>%
    group_by(Species, Event, Lon = .data[[lon_col]]) %>%
    summarise(Delta = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
    arrange(Event, Lon)

  lat_prof <- df %>%
    group_by(Species, Event, Lat = .data[[lat_col]]) %>%
    summarise(Delta = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
    arrange(Event, Lat)

  list(lon = lon_prof, lat = lat_prof)
}


# =============================================================================
# 3. Plot builders
# =============================================================================

# -- Longitudinal profile --
plot_lon_profile <- function(lon_prof, species_title,
                             lon_lim     = c(102, 290),
                             delta_lim   = c(-100, 100),
                             line_colors = c("La Niña" = "#2B5AA6",
                                             "El Niño" = "#D43B3B")) {
  ggplot(lon_prof, aes(x = Lon, y = Delta, color = Event)) +
    geom_hline(yintercept = 0, linewidth = 0.4, linetype = 2, color = "grey35") +
    geom_line(linewidth = 2, alpha = 0.7) +
    scale_color_manual(values = line_colors, drop = FALSE, name = NULL) +
    scale_x_longitude(name = "Longitude", breaks = seq(100, 300, 40),
                      limits = lon_lim) +
    scale_y_continuous(name = expression(Delta ~ "Rel. abundance (%)"),
                       limits = delta_lim) +
    ggtitle(species_title) +
    theme_gcb(base_size = 10) +
    theme(
      panel.grid   = element_blank(),
      plot.title   = element_text(face = "bold", hjust = 0),
      axis.title.y = element_text(lineheight = 0.95)
    )
}

# -- Latitudinal profile (rotated: y = Lat, x = Delta) --
plot_lat_profile_y <- function(lat_prof, species_title,
                               lat_lim     = c(-53, 53),
                               delta_lim   = c(-125, 125),
                               line_colors = c("La Niña" = "#2B5AA6",
                                               "El Niño" = "#D43B3B")) {
  ggplot(lat_prof, aes(x = Delta, y = Lat, color = Event)) +
    geom_vline(xintercept = 0, linewidth = 0.4, linetype = 2, color = "grey35") +
    geom_path(linewidth = 2, alpha = 0.7) +
    scale_color_manual(values = line_colors, drop = FALSE, name = NULL) +
    scale_y_latitude(name = "Latitude", breaks = seq(-40, 40, 20),
                     limits = lat_lim) +
    scale_x_continuous(name = expression(Delta ~ "Rel. abundance (%)"),
                       limits = delta_lim) +
    ggtitle(species_title) +
    theme_gcb(base_size = 10) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0)
    )
}


# =============================================================================
# 4. Build profiles for each species
# =============================================================================

# ALB_N (0-53 N)
prof_ALB_N  <- make_lon_lat_profiles(rel_ALB_N, "ALB_N",
                                     lat_lim = c(0, 53), event_levels = event_levels)
p_ALB_N_lon <- plot_lon_profile(prof_ALB_N$lon, "ALB_N",
                                delta_lim = delta_lim, line_colors = line_colors)
p_ALB_N_lat <- plot_lat_profile_y(prof_ALB_N$lat, "ALB_N",
                                  lat_lim = c(0, 53), delta_lim = delta_lim,
                                  line_colors = line_colors)

# ALB_S (53 S-0)
prof_ALB_S  <- make_lon_lat_profiles(rel_ALB_S, "ALB_S",
                                     lat_lim = c(-53, 0), event_levels = event_levels)
p_ALB_S_lon <- plot_lon_profile(prof_ALB_S$lon, "ALB_S",
                                delta_lim = delta_lim, line_colors = line_colors)
p_ALB_S_lat <- plot_lat_profile_y(prof_ALB_S$lat, "ALB_S",
                                  lat_lim = c(-53, 0), delta_lim = delta_lim,
                                  line_colors = line_colors)

# BET (53 S-53 N)
prof_BET  <- make_lon_lat_profiles(rel_BET, "BET",
                                   lat_lim = c(-53, 53), event_levels = event_levels)
p_BET_lon <- plot_lon_profile(prof_BET$lon, "BET",
                              delta_lim = delta_lim, line_colors = line_colors)
p_BET_lat <- plot_lat_profile_y(prof_BET$lat, "BET",
                                lat_lim = c(-53, 53), delta_lim = delta_lim,
                                line_colors = line_colors)

# YFT (53 S-53 N)
prof_YFT  <- make_lon_lat_profiles(rel_YFT, "YFT",
                                   lat_lim = c(-53, 53), event_levels = event_levels)
p_YFT_lon <- plot_lon_profile(prof_YFT$lon, "YFT",
                              delta_lim = delta_lim, line_colors = line_colors)
p_YFT_lat <- plot_lat_profile_y(prof_YFT$lat, "YFT",
                                lat_lim = c(-53, 53), delta_lim = delta_lim,
                                line_colors = line_colors)


# =============================================================================
# 5. Assemble profile panel
# =============================================================================
final_profiles <-
  (p_ALB_N_lon | p_ALB_N_lat) /
  (p_ALB_S_lon | p_ALB_S_lat) /
  (p_BET_lon   | p_BET_lat)   /
  (p_YFT_lon   | p_YFT_lat)   +
  plot_layout(guides = "collect", widths = c(1, 1)) +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom")

final_profiles


# =============================================================================
# 6. Combine with spatial maps  -->  Fig_4
# =============================================================================
p_combined <-
  (p_all | final_profiles) +
  plot_annotation(tag_levels = "a") &
  theme(
    legend.position = "bottom",
    plot.tag        = element_text(face = "bold", size = 12)
  )

p_combined

save_plot_all(
  p             = p_combined,
  filename_base = "Fig_4",
  width         = 16,
  height        = 9,
  dpi           = 600
)
