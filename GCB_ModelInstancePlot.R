###############################################################################
# GCB_Model_instance.R
# ──────────────────────────────────────────────────────────────────────────────
# Tuning instance diagnostic plot: hyperparameter vs RMSE by species & batch.
#
# Output:
#   - Fig_Instance : faceted scatter of tuning evaluations
#
# Requires objects already in the workspace:
#   arch_all_plot          (combined tuning archive, all species)
#   species_colors         (named colour vector)
#   theme_gcb(), save_plot_all()
###############################################################################


# =============================================================================
# 0. Packages
# =============================================================================
library(dplyr)
library(ggplot2)
library(scales)


# =============================================================================
# 1. Species colour palette
# =============================================================================
species_colors <- c(
  ALB_N = "#4B3F8C",
  ALB_S = "#00A6D6",
  BET   = "#1B9E77",
  YFT   = "#D95F02"
)


# =============================================================================
# 2. Prepare plot data
# =============================================================================
df_plot <- arch_all_plot %>%
  mutate(
    Species     = factor(Species, levels = names(species_colors)),
    param       = factor(param),
    Batch       = as.character(Batch),
    # Highlight 6 key batches with distinct shapes; collapse the rest
    Batch_shape = if_else(Batch %in% c("1", "20", "40", "60", "80", "100"),
                          Batch, "Other")
  ) %>%
  filter(is.finite(x_value), is.finite(regr_rmse),
         !is.na(Species), !is.na(param))

# Shape mapping for the highlighted batches
shape_map <- c(
  "1"     = 16,
  "20"    = 17,
  "40"    = 15,
  "60"    = 18,
  "80"    = 3,
  "100"   = 4,
  "Other" = 16
)


# =============================================================================
# 3. Build faceted scatter plot  -->  Fig_Instance
# =============================================================================
p_instance <- ggplot(df_plot, aes(x = x_value, y = regr_rmse)) +
  # Background cloud: all other batches (faint)
  geom_point(
    data  = df_plot %>% filter(Batch_shape == "Other"),
    aes(color = Species),
    shape = 16, size = 1.6, alpha = 0.25
  ) +
  # Foreground: 6 key batches (distinct shapes, more opaque)
  geom_point(
    data  = df_plot %>% filter(Batch_shape != "Other"),
    aes(color = Species, shape = Batch_shape),
    size = 2.0, alpha = 0.9, stroke = 0.35
  ) +
  facet_grid(Species ~ param, scales = "free", switch = "y") +
  scale_color_manual(values = species_colors, guide = "none") +
  scale_shape_manual(
    values = shape_map,
    breaks = c("1", "20", "40", "60", "80", "100"),
    name   = "Batch"
  ) +
  theme_gcb() +
  theme(
    strip.placement  = "outside",
    panel.grid       = element_blank(),
    legend.position  = "bottom",
    legend.direction = "horizontal",
    strip.text.y.left = element_text(face = "plain")
  ) +
  guides(shape = guide_legend(nrow = 1, byrow = TRUE)) +
  labs(x = NULL, y = "regr.rmse")

p_instance

save_plot_all(
  p             = p_instance,
  filename_base = "Fig_Instance",
  width         = 16,
  height        = 9,
  dpi           = 600
)
