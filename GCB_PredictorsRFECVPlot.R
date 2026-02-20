###############################################################################
# GCB_RFECV.R
# ──────────────────────────────────────────────────────────────────────────────
# Plot RFECV (Recursive Feature Elimination with Cross-Validation) scores
# across species and rounds.
#
# Input:
#   CSV files named rfecv_scores_{species}_r{round}.csv
#   Columns: n_features, mean_test_score
#
# Outputs:
#   - Fig_RFECV_grid : faceted grid (species x round), one panel per round
#   - Fig_RFECV      : overlay all rounds per species, last round highlighted
#
# Requires objects already in the workspace:
#   ROOT, theme_gcb(), save_plot_all()
###############################################################################


# =============================================================================
# 0. Packages
# =============================================================================
library(tidyverse)
library(colorspace)
library(patchwork)
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

theme_rfecv <- function(base_size = 11) {
  theme_gcb(base_size = base_size) +
    theme(
      panel.grid      = element_blank(),
      strip.text      = element_text(face = "plain"),
      legend.position = "none"
    )
}


# =============================================================================
# 2. Locate and read RFECV score files
# =============================================================================
dir_rfecv <- file.path(ROOT, "Revise/Data_out/RFECV")

files <- list.files(
  dir_rfecv,
  pattern    = "^rfecv_scores_.*_r[0-9]+\\.(csv|txt|tsv)$",
  full.names = TRUE
)
stopifnot(length(files) > 0)

# -- Parse species and round from filename --
parse_meta <- function(path) {
  fn <- basename(path)
  m  <- stringr::str_match(fn, "^rfecv_scores_(.+)_r([0-9]+)\\.")
  if (any(is.na(m))) stop("Filename not matched: ", fn)
  tibble(file = path, species_raw = m[, 2], round = as.integer(m[, 3]))
}

meta <- map_dfr(files, parse_meta)

# Map raw species codes to display labels
species_map <- c(
  alb_n = "ALB_N",
  alb_s = "ALB_S",
  bet   = "BET",
  yft   = "YFT"
)

meta <- meta %>%
  mutate(
    species = dplyr::recode(species_raw, !!!species_map, .default = toupper(species_raw)),
    species = factor(species, levels = c("ALB_N", "ALB_S", "BET", "YFT"))
  ) %>%
  filter(species %in% names(species_colors))


# =============================================================================
# 3. Read and combine all CSV files
# =============================================================================
rfecv_all <- meta %>%
  mutate(data = map(file, ~ readr::read_csv(.x, show_col_types = FALSE))) %>%
  unnest(data) %>%
  mutate(
    n_features      = as.integer(n_features),
    mean_test_score = as.numeric(mean_test_score)
  ) %>%
  filter(is.finite(n_features), is.finite(mean_test_score)) %>%
  arrange(species, round, n_features)


# =============================================================================
# 4. Identify best n_features per species & round
# =============================================================================
rfecv_best <- rfecv_all %>%
  group_by(species, round) %>%
  slice_max(order_by = mean_test_score, n = 1, with_ties = FALSE) %>%
  ungroup()

# Facet labels (used by the grid plot)
rfecv_all <- rfecv_all %>%
  mutate(round_lab = paste0("Round: ", round))

rfecv_best <- rfecv_best %>%
  mutate(
    round_lab = paste0("Round: ", round),
    label_k   = paste0("k = ", n_features)
  )


# =============================================================================
# 5. Figure A: species x round facet grid  -->  Fig_RFECV_grid
# =============================================================================
p_grid <- ggplot(rfecv_all, aes(x = n_features, y = mean_test_score,
                                colour = species)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.6, alpha = 0.7) +
  
  # Optimal point (inherits colour = species)
  geom_point(data = rfecv_best, size = 2.6) +
  
  # Vertical dashed line at optimal feature count
  geom_vline(
    data      = rfecv_best,
    aes(xintercept = n_features, colour = species),
    linetype  = "dashed",
    linewidth = 0.6,
    show.legend = FALSE
  ) +
  
  # Label showing optimal k
  geom_label(
    data        = rfecv_best,
    aes(x = Inf, y = Inf, label = label_k),
    hjust       = 1.05,
    vjust       = 1.15,
    label.size  = 0,
    fill        = "white",
    alpha       = 0.85,
    size        = 3.4,
    colour      = "black",
    inherit.aes = FALSE
  ) +
  
  facet_grid(species ~ round_lab, scales = "free_y") +
  scale_colour_manual(values = species_colors, guide = "none") +
  labs(
    x     = "Number of selected features (RFECV)",
    y     = "Mean CV test score",
    title = "RFECV feature elimination trajectories by species and round"
  ) +
  theme_gcb() +
  theme(
    strip.text       = element_text(face = "bold"),
    plot.title       = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

print(p_grid)

save_plot_all(
  p             = p_grid,
  filename_base = "Fig_RFECV_grid",
  width         = 11,
  height        = 8,
  dpi           = 600
)


# =============================================================================
# 6. Figure B: overlay all rounds, highlight last round  -->  Fig_RFECV
# =============================================================================

# -- Round metadata per species --
round_info <- rfecv_all %>%
  group_by(species) %>%
  summarise(
    n_rounds   = n_distinct(round),
    last_round = max(round, na.rm = TRUE),
    .groups    = "drop"
  )

# -- Per-species y-range for smart label nudging --
ranges <- rfecv_all %>%
  group_by(species) %>%
  summarise(
    xmin = min(n_features, na.rm = TRUE),
    xmax = max(n_features, na.rm = TRUE),
    ymin = min(mean_test_score, na.rm = TRUE),
    ymax = max(mean_test_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    yr          = ymax - ymin,
    nudge_point = 0.03 * yr,
    nudge_label = 0.07 * yr,
    nudge_round = 0.11 * yr
  )

# -- Last-round subset --
rfecv_last <- rfecv_all %>%
  left_join(round_info %>% select(species, last_round), by = "species") %>%
  filter(round == last_round)

# -- Selected feature count (best score in last round) --
selected_last <- rfecv_last %>%
  group_by(species) %>%
  slice_max(mean_test_score, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(species, n_features, sel_score = mean_test_score) %>%
  left_join(ranges, by = "species") %>%
  mutate(
    y_point = sel_score + nudge_point,
    y_label = sel_score - nudge_label,
    y_round = sel_score - nudge_round
  )

# -- Round-count annotation positions --
round_info2 <- round_info %>%
  left_join(selected_last %>% select(species, n_features, y_round),
            by = "species") %>%
  mutate(x_round = n_features)

# -- Colour variants: light for background rounds, dark for last round --
col_last  <- species_colors
col_light <- lighten(col_last, amount = 0.45)

# -- Build overlay plot --
p_rfecv <- ggplot() +
  
  # (A) All rounds: light species colour
  geom_line(
    data = rfecv_all,
    aes(x = n_features, y = mean_test_score,
        group = interaction(species, round),
        color = species),
    linewidth = 0.7, alpha = 0.60
  ) +
  
  # (B) Last round: dark species colour (drawn on top)
  geom_line(
    data = rfecv_last,
    aes(x = n_features, y = mean_test_score, color = species),
    linewidth = 1.35, alpha = 0.95
  ) +
  
  # (C) Selected point (slightly above true score)
  geom_point(
    data = selected_last,
    aes(x = n_features, y = y_point, fill = species),
    shape = 21, size = 3.1, color = "black", stroke = 0.35
  ) +
  
  # (D) Selected-n label (below point)
  geom_label(
    data = selected_last,
    aes(x = n_features, y = y_label,
        label = paste0("Selected n = ", n_features),
        fill = species),
    color         = "white",
    size          = 3.0,
    label.size    = 0.2,
    label.r       = unit(0.12, "lines"),
    label.padding = unit(0.18, "lines")
  ) +
  
  # (E) Rounds count (below label)
  geom_text(
    data = round_info2,
    aes(x = x_round, y = y_round,
        label = paste0("Rounds: ", n_rounds)),
    inherit.aes = FALSE,
    hjust = 0.5, vjust = 1,
    size = 3.3, color = "grey20"
  ) +
  
  facet_wrap(~ species, ncol = 2, scales = "free_y") +
  scale_color_manual(values = col_light) +
  scale_fill_manual(values = col_last) +
  labs(
    x = "Number of selected features (n_features)",
    y = "Mean CV score"
  ) +
  theme_rfecv(11)

print(p_rfecv)

save_plot_all(
  p             = p_rfecv,
  filename_base = "Fig_RFECV",
  width         = 10,
  height        = 6,
  dpi           = 600
)


# =============================================================================
# 7. Summary table of optimal feature counts (diagnostic)
# =============================================================================
rfecv_best %>%
  select(species, round, n_features, mean_test_score) %>%
  arrange(species, round) %>%
  print(n = 100)
