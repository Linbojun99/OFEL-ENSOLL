###############################################################################
# GCB_Spatial_Pl.R
# ──────────────────────────────────────────────────────────────────────────────
# ENSO phase maps: delta relative abundance (%) vs Neutral.
#
# Workflow:
#   1) Compute grid-level percent change (ENSO - Neutral) / |Neutral|
#   2) Plot per species (ALB_N, ALB_S, BET, YFT) as raster tiles
#   3) Facet: Tuna ~ Event (La Nina / El Nino) with per-row lat windows
#
# Key design notes:
#   - coord_cartesian() is required (NOT coord_sf) for free_y facets
#   - Land polygons via maps::map_data(), re-grouped per facet to
#     avoid "striped" polygon artifacts
#
# Output:
#   - p_all : 4x2 faceted spatial map (used downstream by
#             GCB_Spatial_Lonlat_Pl.R to build Fig_4)
#   - rel_ALB_N, rel_ALB_S, rel_BET, rel_YFT : per-species delta tables
#
# Requires objects already in the workspace:
#   northern_alb_model_data, southern_alb_model_data,
#   bet_model_data, yft_model_data
#   (each with predicted columns pre_ALB / pre_BET / pre_YFT
#    and ENSO_Phase3, Lon, Lat)
#   scale_x_longitude(), scale_y_latitude()   (custom ggplot scales)
###############################################################################


# =============================================================================
# 0. Packages
# =============================================================================
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(colorspace)
  library(scales)
  library(maps)
  library(metR)
})


# =============================================================================
# 1. Helpers
# =============================================================================

# Normalise ENSO phase labels (handle encoding variants)
normalize_phase3 <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "Ni\\s*n?a", "Niña")
  x <- stringr::str_replace_all(x, "Nina",       "Niña")
  stringr::str_trim(x)
}

# Grid-level percent change relative to Neutral
calc_delta_neutral_phase3_pct <- function(df, pred_col,
                                          phase_col    = "ENSO_Phase3",
                                          lon_col      = "Lon",
                                          lat_col      = "Lat",
                                          neutral_name = "Neutral",
                                          event_levels = c("La Niña", "Neutral", "El Niño"),
                                          eps          = 1e-6,
                                          clamp        = NULL) {
  stopifnot(all(c(pred_col, phase_col, lon_col, lat_col) %in% names(df)))

  df2 <- df %>%
    transmute(
      Lon   = .data[[lon_col]],
      Lat   = .data[[lat_col]],
      Event = normalize_phase3(.data[[phase_col]]),
      Pred  = .data[[pred_col]]
    ) %>%
    mutate(Event = factor(Event, levels = event_levels)) %>%
    filter(!is.na(Event), is.finite(Lon), is.finite(Lat), is.finite(Pred))

  # Average prediction per Event x grid cell
  grid_event <- df2 %>%
    group_by(Event, Lon, Lat) %>%
    summarise(Pred = mean(Pred, na.rm = TRUE), .groups = "drop")

  # Neutral baseline
  neutral_baseline <- grid_event %>%
    filter(Event == neutral_name) %>%
    transmute(Lon, Lat, Neutral = Pred)

  # Percent change
  rel <- grid_event %>%
    left_join(neutral_baseline, by = c("Lon", "Lat")) %>%
    filter(Event != neutral_name) %>%
    mutate(Delta_pct = 100 * (Pred - Neutral) / (abs(Neutral) + eps)) %>%
    filter(is.finite(Delta_pct))

  if (!is.null(clamp)) {
    stopifnot(length(clamp) == 2)
    rel <- rel %>% mutate(Delta_pct = pmax(clamp[1], pmin(clamp[2], Delta_pct)))
  }

  rel
}

# Safe fallback if theme_gcb() is not yet loaded
if (!exists("theme_gcb")) {
  theme_gcb <- function(base_size = 10) theme_bw(base_size = base_size)
}


# =============================================================================
# 2. Compute delta % (ENSO - Neutral) per species
# =============================================================================
rel_ALB_N <- calc_delta_neutral_phase3_pct(northern_alb_model_data, "pre_ALB",
                                           clamp = c(-200, 200))
rel_ALB_S <- calc_delta_neutral_phase3_pct(southern_alb_model_data, "pre_ALB",
                                           clamp = c(-200, 200))
rel_BET   <- calc_delta_neutral_phase3_pct(bet_model_data, "pre_BET",
                                           clamp = c(-200, 200))
rel_YFT   <- calc_delta_neutral_phase3_pct(yft_model_data, "pre_YFT",
                                           clamp = c(-200, 200))


# =============================================================================
# 3. Combine into one table (La Nina / El Nino only)
# =============================================================================
tuna_levels <- c("ALB_N", "ALB_S", "BET", "YFT")
event_keep  <- c("La Niña", "El Niño")

rel_all <- bind_rows(
  rel_ALB_N %>% mutate(Tuna = "ALB_N"),
  rel_ALB_S %>% mutate(Tuna = "ALB_S"),
  rel_BET   %>% mutate(Tuna = "BET"),
  rel_YFT   %>% mutate(Tuna = "YFT")
) %>%
  mutate(
    Tuna  = factor(Tuna, levels = tuna_levels),
    Event = factor(Event, levels = c("La Niña", "Neutral", "El Niño"))
  ) %>%
  filter(Event %in% event_keep)


# =============================================================================
# 4. Per-species latitude windows
# =============================================================================
tuna_lat_lim <- tibble::tribble(
  ~Tuna,    ~lat_min, ~lat_max,
  "ALB_N",   0,        44.5,
  "ALB_S",  -48,        0,
  "BET",    -48,       44.5,
  "YFT",    -48,       44.5
)

rel_all2 <- rel_all %>%
  left_join(tuna_lat_lim, by = "Tuna") %>%
  filter(Lat >= lat_min, Lat <= lat_max) %>%
  select(-lat_min, -lat_max)


# =============================================================================
# 5. Land polygons (faceted + re-grouped)
# =============================================================================
worldMap <- maps::map("world2", plot = FALSE, fill = TRUE)
worldMap <- ggplot2::fortify(worldMap)

common_lon <- c(109, 290)
lat_buffer <- 5

worldMap_faceted <- tidyr::crossing(
  worldMap,
  Tuna  = factor(tuna_levels, levels = tuna_levels),
  Event = factor(event_keep,  levels = event_keep)
) %>%
  left_join(tuna_lat_lim, by = "Tuna") %>%
  filter(
    lat  >= (lat_min - lat_buffer),
    lat  <= (lat_max + lat_buffer),
    long >= common_lon[1],
    long <= common_lon[2]
  ) %>%
  group_by(Tuna, Event, group) %>%
  arrange(order, .by_group = TRUE) %>%
  mutate(group_f = cur_group_id()) %>%
  ungroup() %>%
  select(-lat_min, -lat_max)


# =============================================================================
# 6. Build spatial map  -->  p_all
# =============================================================================
pal <- colorspace::diverging_hcl(n = 200, palette = "Blue-Red 3")

p_all <- ggplot() +
  geom_raster(
    data = rel_all2,
    aes(x = Lon, y = Lat, fill = Delta_pct)
  ) +
  geom_polygon(
    data = worldMap_faceted,
    aes(x = long, y = lat, group = group_f),
    fill = "grey60", color = "grey60", linewidth = 0.25
  ) +
  scale_fill_gradientn(
    colors = pal,
    limits = c(-100, 100),
    oob    = scales::squish
  ) +
  coord_cartesian(xlim = common_lon, expand = FALSE) +
  facet_grid(
    Tuna ~ Event,
    scales = "free_y",
    space  = "free_y"
  ) +
  scale_y_latitude(name = "Latitude",  breaks = seq(-40, 40, 20)) +
  scale_x_longitude(name = "Longitude", breaks = seq(100, 300, 40)) +
  labs(fill = expression(Delta ~ "Relative abundance (%)" ["ENSO - Neutral"])) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white", colour = "black"),
    strip.text       = element_text(face = "bold", size = 11),
    panel.grid       = element_blank(),
    axis.title       = element_text(face = "bold"),
    axis.text        = element_text(size = 10),
    legend.position  = "bottom",
    plot.margin      = margin(6, 6, 6, 18)
  )

p_all
