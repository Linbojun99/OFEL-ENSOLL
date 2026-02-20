###############################################################################
# GCB_envData.R
# ──────────────────────────────────────────────────────────────────────────────
# Environmental data visualisation for the Pacific longline tuna study.
#
# Outputs:
#   - Fig_env_d      : Spatial maps of surface environmental variables
#   - Fig_S_env      : Density distributions + vertical profiles (supplement)
#   - Fig_MEIONI     : MEI and ONI time-series panel
#
# Requires objects already in the workspace:
#   pacific_env_data, mei_monthly, oni_long
#   save_plot_all()   (from GCB_Data.R)
###############################################################################


# =============================================================================
# 0. Packages
# =============================================================================
library(tidyverse)
library(ggplot2)
library(maps)
library(patchwork)
library(cmocean)
library(scales)
library(metR)
library(dplyr)
library(lubridate)


# =============================================================================
# 1. Common theme & base map
# =============================================================================
theme_gcb <- function(base_size = 10, base_family = "Helvetica") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      axis.title       = element_text(face = "bold", size = base_size + 1),
      axis.text        = element_text(size = base_size),
      axis.ticks       = element_line(linewidth = 0.5),
      axis.line        = element_line(linewidth = 0.6),
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold", size = base_size + 1),
      panel.spacing    = unit(1.0, "lines"),
      plot.margin      = margin(6, 8, 6, 8),
      legend.title     = element_text(size = base_size),
      legend.text      = element_text(size = base_size),
      legend.key.height = unit(0.45, "lines"),
      legend.key.width  = unit(1.2, "lines")
    )
}

theme_env <- function(base_size = 11) {
  theme_gcb(base_size = base_size) +
    theme(
      panel.grid       = element_blank(),
      strip.background = element_rect(fill = "white", color = NA),
      strip.text       = element_text(face = "bold", size = 12),
      plot.title       = element_text(face = "plain", hjust = 0),
      axis.text        = element_text(color = "black"),
      axis.title       = element_text(face = "bold"),
      legend.position  = "right",
      legend.title     = element_text(face = "plain"),
      legend.text      = element_text(color = "black"),
      legend.key.height = unit(1.1, "cm")
    )
}

world_map <- map_data("world2")


# =============================================================================
# 2. Variable labels and units
# =============================================================================
map_vars <- c("Temp_0", "SSH", "Chl_0", "MLD", "O2_0", "Sali_0", "Nppv_0")

# Panel titles (displayed above each facet)
var_title <- c(
  Temp_0 = "Temperature (depth 0)",
  SSH    = "SSH",
  Chl_0  = "Chl-a (depth 0)",
  MLD    = "MLD",
  O2_0   = "Oxygen (depth 0)",
  Sali_0 = "Salinity (depth 0)",
  Nppv_0 = "NPPV (depth 0)"
)

# Legend titles (units only)
var_unit <- list(
  Temp_0 = expression(degree * C),
  SSH    = expression(m),
  Chl_0  = expression(mg ~ m^{-3}),
  MLD    = expression(m),
  O2_0   = expression(mmol ~ m^{-3}),
  Sali_0 = expression(psu),
  Nppv_0 = expression(mg ~ m^{-3} ~ day^{-1})
)


# =============================================================================
# 3. Colour / scale style per variable
# =============================================================================
get_style_params <- function(var_name, x) {

  style <- list(pal = "matter", trans = "identity", limits = NULL)

  if (var_name == "SSH") {
    style$pal    <- "balance"
    lim          <- quantile(abs(x), probs = 0.98, na.rm = TRUE)
    style$limits <- c(-lim, lim)

  } else if (str_starts(var_name, "Chl")) {
    style$pal    <- "algae"
    style$trans  <- "log10"
    xx           <- x[x > 0]
    style$limits <- quantile(xx, probs = c(0.02, 0.98), na.rm = TRUE)

  } else if (str_starts(var_name, "Temp")) {
    style$pal    <- "thermal"
    style$limits <- quantile(x, probs = c(0.02, 0.98), na.rm = TRUE)

  } else if (str_starts(var_name, "Sali")) {
    style$pal    <- "haline"
    style$limits <- quantile(x, probs = c(0.02, 0.98), na.rm = TRUE)

  } else if (str_starts(var_name, "O2")) {
    style$pal    <- "tempo"
    style$limits <- quantile(x, probs = c(0.02, 0.98), na.rm = TRUE)

  } else if (var_name == "MLD") {
    style$pal    <- "deep"
    style$limits <- quantile(x, probs = c(0.02, 0.98), na.rm = TRUE)

  } else if (str_starts(var_name, "Nppv")) {
    style$pal    <- "turbid"
    style$trans  <- "log10"
    xx           <- x[x > 0]
    style$limits <- quantile(xx, probs = c(0.02, 0.98), na.rm = TRUE)
  }

  style
}


# =============================================================================
# 4. Prepare long-format environmental data (spatial mean)
# =============================================================================
env_long <- pacific_env_data %>%
  select(Lon, Lat, all_of(map_vars)) %>%
  pivot_longer(cols = all_of(map_vars),
               names_to = "Variable", values_to = "Value") %>%
  group_by(Variable, Lon, Lat) %>%
  summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  filter(is.finite(Lon), is.finite(Lat), is.finite(Value))

xlim_use <- range(pacific_env_data$Lon, na.rm = TRUE)
ylim_use <- range(pacific_env_data$Lat, na.rm = TRUE)


# =============================================================================
# 5. Single-variable map function
# =============================================================================
make_env_map <- function(df_one, var_name) {

  style <- get_style_params(var_name, df_one$Value)

  ttl  <- ifelse(var_name %in% names(var_title), var_title[[var_name]], var_name)
  unit <- ifelse(var_name %in% names(var_unit),  var_unit[[var_name]],  "")

  # For log10 variables, drop non-positive values to avoid transform errors
  if (identical(style$trans, "log10")) {
    df_one <- df_one %>% filter(Value > 0)
  }

  ggplot() +
    geom_tile(data = df_one, aes(x = Lon, y = Lat, fill = Value)) +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                 fill = "grey80", color = "grey80", linewidth = 0.1) +
    coord_sf(xlim = xlim_use, ylim = ylim_use, expand = FALSE) +
    scale_fill_cmocean(
      name   = style$pal,
      limits = style$limits,
      trans  = style$trans,
      oob    = scales::squish,
      guide  = guide_colorbar(
        title          = unit,
        barheight      = unit(0.35, "cm"),
        barwidth       = unit(2.6, "cm"),
        ticks.colour   = "black",
        frame.colour   = "black",
        title.position = "left",
        label.position = "bottom"
      )
    ) +
    scale_x_longitude(breaks = seq(120, 280, 40)) +
    scale_y_latitude(breaks = seq(-40, 40, 20)) +
    labs(title = ttl, x = NULL, y = NULL) +
    theme_env()
}


# =============================================================================
# 6. Build & assemble spatial maps  -->  Fig_env_d
# =============================================================================
plot_list <- purrr::map(map_vars, ~ {
  df_sub <- env_long %>% filter(Variable == .x)
  make_env_map(df_sub, .x)
})

final_plot <-
  wrap_plots(plot_list, ncol = 4) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag        = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

final_plot

save_plot_all(
  p             = final_plot,
  filename_base = "Fig_env_d",
  width         = 12,
  height        = 8,
  dpi           = 600
)


# =============================================================================
# 7. Vertical profiles (supplement figure)
# =============================================================================

# Select all depth-resolved columns: Temp_*, Sali_*, Chl_*, Nppv_*, O2_*
cols_vertical <- names(pacific_env_data)[
  str_detect(names(pacific_env_data),
             "^(Temp|Sali|Chl|Nppv|O2|Oxygen)_[0-9]+$")
]

vertical_long_all <- pacific_env_data %>%
  select(all_of(cols_vertical)) %>%
  pivot_longer(everything(), names_to = "Var_Depth", values_to = "Value") %>%
  separate(Var_Depth, into = c("Variable", "Depth"), sep = "_", convert = TRUE) %>%
  # Standardise naming: Oxygen -> O2
  mutate(Variable = if_else(Variable == "Oxygen", "O2", Variable)) %>%
  filter(is.finite(Value), is.finite(Depth)) %>%
  group_by(Variable, Depth) %>%
  summarise(
    Mean_Value = mean(Value, na.rm = TRUE),
    SD_Value   = sd(Value,   na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  arrange(Variable, Depth)

# Verify all expected variables are present
table(vertical_long_all$Variable)

# Panel titles (short names for vertical profiles)
var_title_vert <- c(
  Temp = "Temperature",
  Sali = "Salinity",
  O2   = "Oxygen",
  Chl  = "Chl-a",
  Nppv = "NPPV"
)

# X-axis units
var_unit_vert <- list(
  Temp = expression(degree * C),
  Sali = "psu",
  O2   = expression(mmol ~ m^{-3}),
  Chl  = expression(mg ~ m^{-3}),
  Nppv = expression(mg ~ m^{-3} ~ day^{-1})
)

make_vertical_profile <- function(df_one, theme_fun = theme_env, base_size = 10) {

  df_one <- df_one %>%
    filter(is.finite(Depth), is.finite(Mean_Value)) %>%
    arrange(Depth)

  v            <- unique(df_one$Variable)
  depth_breaks <- sort(unique(df_one$Depth))
  n_unique     <- length(depth_breaks)

  ttl  <- if (v %in% names(var_title_vert)) var_title_vert[[v]] else v
  unit <- if (v %in% names(var_unit_vert))  var_unit_vert[[v]]  else "Mean value"

  # Smoothing spline (or raw line when fewer than 4 depths)
  if (n_unique < 4) {
    smooth_df <- df_one %>% transmute(Depth, Smooth = Mean_Value)
  } else {
    depth_grid <- tibble(Depth = seq(min(depth_breaks), max(depth_breaks), by = 1))
    sp         <- stats::smooth.spline(x = df_one$Depth, y = df_one$Mean_Value)
    smooth_df  <- tibble(
      Depth  = depth_grid$Depth,
      Smooth = stats::predict(sp, x = depth_grid$Depth)$y
    ) %>% filter(is.finite(Smooth))
  }

  # Adjust x-axis tick density for Chl (very small values)
  x_scale <- NULL
  if (v == "Chl") {
    x_scale <- scale_x_continuous(
      breaks = scales::pretty_breaks(n = 3),
      labels = scales::label_number(accuracy = 0.0001)
    )
  }

  ggplot() +
    geom_path(
      data = smooth_df,
      aes(x = Smooth, y = Depth),
      linewidth = 1.05, color = "grey35"
    ) +
    geom_point(
      data = df_one,
      aes(x = Mean_Value, y = Depth),
      size = 2.0, color = "black"
    ) +
    scale_y_reverse(
      name         = "Depth (m)",
      breaks       = depth_breaks,
      minor_breaks = NULL
    ) +
    x_scale +
    labs(title = ttl, x = unit) +
    theme_fun(base_size)
}

p_vert_list_all <- vertical_long_all %>%
  split(.$Variable) %>%
  imap(~ make_vertical_profile(.x))

p_vertical_all <- wrap_plots(p_vert_list_all, ncol = 2) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "plain"))

p_vertical_all


# =============================================================================
# 8. Density distributions of all environmental variables
# =============================================================================
exclude_cols <- c("Lon", "Lat", "Year", "Month", "Date",
                  "ENSO", "ENSO_Phase3", "Phase", "Event",
                  "Trip", "Set", "ID")

env_long_dens <- pacific_env_data %>%
  select(where(is.numeric), -any_of(exclude_cols)) %>%
  pivot_longer(everything(), names_to = "var_raw", values_to = "Value") %>%
  filter(is.finite(Value)) %>%
  mutate(
    Var   = str_replace(var_raw, "_\\d+$", ""),
    Depth = suppressWarnings(as.integer(str_extract(var_raw, "(?<=_)\\d+$"))),
    Facet = var_raw
  )

# Facet order: grouped by variable, then ascending depth
facet_levels <- env_long_dens %>%
  distinct(Var, Depth, Facet) %>%
  mutate(Depth_sort = if_else(is.na(Depth), Inf, as.numeric(Depth))) %>%
  arrange(Var, Depth_sort, Facet) %>%
  pull(Facet)

env_long_dens <- env_long_dens %>%
  mutate(Facet = factor(Facet, levels = facet_levels))

p_env_density_sorted <- ggplot(env_long_dens, aes(x = Value)) +
  geom_density(linewidth = 0.9) +
  facet_wrap(~ Facet, scales = "free", ncol = 5) +
  labs(x = NULL, y = "Density") +
  theme_gcb(base_size = 9) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 8),
    axis.text  = element_text(size = 7)
  )

p_env_density_sorted


# =============================================================================
# 9. Combined supplement figure: density + vertical profiles  -->  Fig_S_env
# =============================================================================
p_env_l <- (p_env_density_sorted | p_vertical_all) +
  plot_layout(guides = "collect", widths = c(4, 2.5)) +
  plot_annotation(tag_levels = "a") &
  theme(
    legend.position = "bottom",
    plot.tag        = element_text(face = "bold", size = 14)
  )

p_env_l

save_plot_all(
  p             = p_env_l,
  filename_base = "Fig_S_env",
  width         = 12,
  height        = 9,
  dpi           = 600
)


# =============================================================================
# 10. MEI & ONI time-series panel  -->  Fig_MEIONI
# =============================================================================

# Build date columns
mei_df <- mei_monthly %>%
  mutate(Date = as.Date(sprintf("%04d-%02d-01", Year, Month))) %>%
  select(Date, MEI)

oni_df <- oni_long %>%
  mutate(Date = as.Date(sprintf("%04d-%02d-01", Year, Month))) %>%
  select(Date, ONI)

# Align time range to the intersection of both series
date_min <- max(min(mei_df$Date, na.rm = TRUE), min(oni_df$Date, na.rm = TRUE))
date_max <- min(max(mei_df$Date, na.rm = TRUE), max(oni_df$Date, na.rm = TRUE))
mei_df   <- mei_df %>% filter(Date >= date_min, Date <= date_max)
oni_df   <- oni_df %>% filter(Date >= date_min, Date <= date_max)

# ONI shading blocks (El Nino / La Nina periods based on +/- 0.5 threshold)
oni_blocks <- oni_df %>%
  transmute(
    Date,
    value      = ONI,
    enso_class = case_when(
      value >  0.5 ~ "El Niño",
      value < -0.5 ~ "La Niña",
      TRUE         ~ "Neutral"
    )
  ) %>%
  arrange(Date) %>%
  mutate(grp = cumsum(enso_class != lag(enso_class, default = first(enso_class)))) %>%
  group_by(grp, enso_class) %>%
  summarise(
    xstart = min(Date),
    xend   = max(Date) + months(1),
    .groups = "drop"
  ) %>%
  mutate(
    fill  = case_when(
      enso_class == "El Niño" ~ "#D43B3B",
      enso_class == "La Niña" ~ "#2B5AA6",
      TRUE                    ~ NA_character_
    ),
    alpha = if_else(enso_class %in% c("El Niño", "La Niña"), 0.12, 0)
  ) %>%
  filter(alpha > 0)

# Symmetric y-limits around zero
mei_maxabs <- max(abs(mei_df$MEI), na.rm = TRUE)
oni_maxabs <- max(abs(oni_df$ONI), na.rm = TRUE)

# Common x-axis scale
x_scale <- scale_x_date(
  date_breaks = "2 years",
  date_labels = "%Y",
  expand      = expansion(mult = c(0.005, 0.01))
)

# Line aesthetics
lw_mei <- 1.05
lw_oni <- 1.05
lt_mei <- "longdash"
lt_oni <- "solid"

# MEI panel (no shading, no threshold lines)
p_mei <- ggplot(mei_df, aes(x = Date, y = MEI)) +
  geom_line(linewidth = lw_mei, linetype = lt_mei, lineend = "round") +
  coord_cartesian(ylim = c(-mei_maxabs, mei_maxabs)) +
  x_scale +
  labs(x = NULL, y = "MEI") +
  theme_gcb(11) +
  theme(axis.title.x = element_blank())

# ONI panel (with ENSO shading and +/- 0.5 threshold lines)
p_oni <- ggplot() +
  geom_rect(
    data = oni_blocks,
    aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf,
        fill = fill, alpha = alpha),
    inherit.aes = FALSE
  ) +
  scale_fill_identity() +
  scale_alpha_identity() +
  geom_hline(yintercept = c(0.5, -0.5), linetype = "dashed", linewidth = 0.55) +
  geom_line(
    data = oni_df,
    aes(x = Date, y = ONI),
    linewidth = lw_oni, linetype = lt_oni, lineend = "round"
  ) +
  coord_cartesian(ylim = c(-oni_maxabs, oni_maxabs)) +
  x_scale +
  labs(x = "Year", y = "ONI") +
  theme_gcb(11)

# Combine panels
p_both <- (p_mei / p_oni) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag          = element_text(face = "bold", size = 12),
    plot.tag.position = c(0.01, 0.99)
  )

p_both

save_plot_all(
  p             = p_both,
  filename_base = "Fig_MEIONI",
  width         = 12,
  height        = 6,
  dpi           = 600
)
