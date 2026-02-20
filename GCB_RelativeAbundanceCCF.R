###############################################################################
# GCB_RA_CCF.R
# ──────────────────────────────────────────────────────────────────────────────
# Cross-correlation (CCF): Abundance vs MEI per tuna species.
#
# Convention: ccf(x = Abundance, y = MEI)
#   - Positive lag => Abundance leads MEI
#   - Negative lag => MEI leads Abundance
#
# Output:
#   - ccf_tbl    : lag with maximum |CCF| per species
#   - Fig_A_CCF  : 4-panel CCF barplots (ALB_N, ALB_S, BET, YFT)
#
# Requires objects already in the workspace:
#   monthly_long   (Year, Month, Tuna, Abundance, MEI)
#   species_colors, theme_gcb(), save_plot_all()
###############################################################################


# =============================================================================
# 0. Packages
# =============================================================================
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(grid)
library(patchwork)


# =============================================================================
# 1. Prepare monthly series (one value per Year-Month-Tuna)
# =============================================================================
stopifnot(all(c("Year", "Month", "Tuna", "Abundance", "MEI")
              %in% names(monthly_long)))

ccf_dat <- monthly_long %>%
  select(Year, Month, Tuna, Abundance, MEI) %>%
  filter(is.finite(Abundance), is.finite(MEI)) %>%
  group_by(Tuna, Year, Month) %>%
  summarise(
    Abundance = mean(Abundance, na.rm = TRUE),
    MEI       = mean(MEI, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(
    Time     = as.Date(sprintf("%04d-%02d-01", as.integer(Year), as.integer(Month))),
    Time_num = as.numeric(Time)
  ) %>%
  arrange(Tuna, Time_num)


# =============================================================================
# 2. Extract lag with maximum |CCF| per species
# =============================================================================
extract_ccf_peak_one <- function(df_one, lag_max = NULL, min_n = 24) {
  df2 <- df_one %>%
    arrange(Time_num) %>%
    filter(stats::complete.cases(Abundance, MEI))

  if (nrow(df2) < min_n) {
    return(tibble(lag = NA_real_, ccf = NA_real_, n_used = nrow(df2)))
  }

  ccf_res <- stats::ccf(
    x       = df2$Abundance,
    y       = df2$MEI,
    plot    = FALSE,
    lag.max = lag_max
  )

  cors <- as.numeric(ccf_res$acf)
  lags <- as.numeric(ccf_res$lag)
  idx  <- which.max(abs(cors))

  tibble(lag = lags[idx], ccf = cors[idx], n_used = nrow(df2))
}

ccf_tbl <- ccf_dat %>%
  group_by(Tuna) %>%
  summarise(
    out = list(extract_ccf_peak_one(cur_data(), lag_max = NULL, min_n = 24)),
    .groups = "drop"
  ) %>%
  unnest(out)

ccf_tbl


# =============================================================================
# 3. Single-species CCF barplot builder
# =============================================================================
make_ccf_plot <- function(df_one,
                          tuna_label,
                          tuna_color,
                          base_size   = 10,
                          base_family = "Helvetica",
                          ann_y       = 0.92,
                          lag_max     = NULL,
                          conf        = 0.95,
                          min_n       = 24) {

  df2 <- df_one %>%
    arrange(Time_num) %>%
    filter(stats::complete.cases(Abundance, MEI))

  if (nrow(df2) < min_n) {
    return(
      ggplot() +
        labs(title = paste0(tuna_label, ": insufficient data for CCF")) +
        theme_gcb(base_size = base_size, base_family = base_family)
    )
  }

  ccf_res <- stats::ccf(
    x       = df2$Abundance,
    y       = df2$MEI,
    plot    = FALSE,
    lag.max = lag_max
  )

  df_ccf  <- tibble(lag = as.numeric(ccf_res$lag), ccf = as.numeric(ccf_res$acf))
  idx_max <- which.max(abs(df_ccf$ccf))
  star    <- df_ccf[idx_max, , drop = FALSE]

  # Approximate CI band
  n_eff <- nrow(df2)
  ci    <- stats::qnorm((1 + conf) / 2) / sqrt(n_eff)

  star_lab <- paste0("Lag = ", star$lag, "\nCCF = ", sprintf("%.2f", star$ccf))

  ggplot(df_ccf, aes(x = lag, y = ccf)) +
    geom_hline(yintercept =  0,  linewidth = 0.5, color = "black") +
    geom_hline(yintercept =  ci, linetype = "dashed", linewidth = 0.5, color = "grey45") +
    geom_hline(yintercept = -ci, linetype = "dashed", linewidth = 0.5, color = "grey45") +

    # All bars
    geom_col(fill = tuna_color, width = 0.8, alpha = 0.85) +

    # Highlight peak bar
    geom_col(
      data = star, aes(x = lag, y = ccf),
      inherit.aes = FALSE,
      fill = "#D43B3B", width = 0.8
    ) +

    # Peak annotation (top-right corner)
    annotation_custom(
      grob = grid::textGrob(
        star_lab,
        x    = unit(0.98, "npc"),
        y    = unit(ann_y, "npc"),
        just = c("right", "top"),
        gp   = grid::gpar(
          fontsize   = base_size,
          fontface   = "bold",
          fontfamily = base_family,
          col        = "black"
        )
      )
    ) +

    labs(
      x     = "Lag (months)",
      y     = "CCF",
      title = paste0(tuna_label, ": Abundance vs MEI")
    ) +
    theme_gcb(base_size = base_size, base_family = base_family) +
    theme(
      legend.position = "none",
      plot.title      = element_text(face = "bold", size = base_size + 1, hjust = 0.5)
    )
}


# =============================================================================
# 4. Generate 4 panels and combine  -->  Fig_A_CCF
# =============================================================================
ccf_dat <- ccf_dat %>%
  mutate(Tuna = factor(Tuna, levels = names(species_colors)))

plot_list <- ccf_dat %>%
  group_split(Tuna) %>%
  set_names(levels(ccf_dat$Tuna)) %>%
  imap(~ make_ccf_plot(
    df_one      = .x,
    tuna_label  = .y,
    tuna_color  = species_colors[[.y]],
    base_size   = 10,
    base_family = "Helvetica",
    ann_y       = 0.92,
    lag_max     = NULL,
    conf        = 0.95,
    min_n       = 24
  ))

p_ccf_all <- (plot_list[["ALB_N"]] | plot_list[["ALB_S"]]) /
  (plot_list[["BET"]] | plot_list[["YFT"]]) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag          = element_text(face = "bold", size = 12),
    plot.tag.position = c(0.02, 0.98)
  )

p_ccf_all

save_plot_all(
  p             = p_ccf_all,
  filename_base = "Fig_A_CCF",
  width         = 12,
  height        = 9,
  dpi           = 600
)
