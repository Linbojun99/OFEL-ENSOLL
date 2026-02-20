###############################################################################
# GCB_COG-A_Pl.R
# ──────────────────────────────────────────────────────────────────────────────
# COG Lon / Lat vs MEI & ENSO Phase — Plotting Module
#
# Three-panel figure per coordinate (Lon, Lat):
#   a) COG ~ MEI: Linear vs GAM fit (per Tuna)
#   b) ENSO phase contrast: violin / box / median + GLS(AR1) p-value brackets
#   c) Shift from Neutral (degrees) using GLS tTable + 95% CI
#      Baseline = predicted Neutral at covariate means (NOT intercept)
#
# Key design:
#   - Lon treated as circular (0-360) when computing COG
#   - Lat stays linear (-90..90)
#   - Panels b/c use the SAME selected GLS(AR1) model
#
# Outputs:
#   - Fig_2 : COG Lon figure (a/b/c)
#   - Fig_3 : COG Lat figure (a/b/c)
#
# Requires objects already in the workspace:
#   northern_alb_model_data, southern_alb_model_data,
#   bet_model_data, yft_model_data,
#   gls_cog_lon_best1, gls_cog_lat_best1   (from GCB_COG-A.R)
#   theme_gcb(), save_plot_all()
###############################################################################


# =============================================================================
# 0. Packages
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(mgcv)
  library(nlme)
  library(grid)
  library(patchwork)
})


# =============================================================================
# 1. Colour palettes & helpers
# =============================================================================
species_colors <- c(
  ALB_N = "#4B3F8C",
  ALB_S = "#00A6D6",
  BET   = "#1B9E77",
  YFT   = "#D95F02"
)

enso_colors3 <- c(
  "La Niña" = "#2B5AA6",
  "Neutral" = "grey70",
  "El Niño" = "#D43B3B"
)

make_time <- function(year, month) {
  as.Date(sprintf("%04d-%02d-01", as.integer(year), as.integer(month)))
}

fp_eq <- function(p, digits = 3, eps = 1e-3) {
  p <- suppressWarnings(as.numeric(p))
  if (is.na(p)) return("p = NA")
  if (p < eps)  return("p < 0.001")
  paste0("p = ", formatC(p, format = "f", digits = digits))
}

p_stars <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  ""
}


# =============================================================================
# 2. Circular / weighted-mean helpers (Lon only)
# =============================================================================

# Wrap to 0..360
wrap360 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  (x %% 360 + 360) %% 360
}

# Linear weighted mean
wmean_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w)
  x <- x[ok]; w <- w[ok]
  sw <- sum(w, na.rm = TRUE)
  if (!is.finite(sw) || sw <= 0) return(NA_real_)
  sum(x * w, na.rm = TRUE) / sw
}

# Circular weighted mean for Lon (degrees, returns 0..360)
wmean_circular_360 <- function(lon_deg, w) {
  lon <- wrap360(lon_deg)
  ok  <- is.finite(lon) & is.finite(w) & (w > 0)
  lon <- lon[ok]; w <- w[ok]
  sw  <- sum(w)
  if (!is.finite(sw) || sw <= 0 || length(lon) == 0) return(NA_real_)
  ang <- lon * pi / 180
  S   <- sum(w * sin(ang)) / sw
  C   <- sum(w * cos(ang)) / sw
  mu  <- atan2(S, C) * 180 / pi
  wrap360(mu)
}


# =============================================================================
# 3. Build monthly COG (Lon circular, Lat linear)
# =============================================================================
build_monthly_cog_one <- function(dat, tuna_label,
                                  lon_col    = "Lon",
                                  lat_col    = "Lat",
                                  weight_col,
                                  year_col   = "Year",
                                  month_col  = "Month") {

  need <- c(year_col, month_col, lon_col, lat_col, weight_col)
  miss <- setdiff(need, names(dat))
  if (length(miss) > 0) stop("Missing column(s) in ", tuna_label, ": ",
                             paste(miss, collapse = ", "))

  keep_idx <- intersect(c("MEI", "ONI", "ENSO_Phase3"), names(dat))

  out <- dat %>%
    transmute(
      Year    = .data[[year_col]],
      Month   = .data[[month_col]],
      Lon_raw = .data[[lon_col]],
      Lat     = .data[[lat_col]],
      w       = .data[[weight_col]],
      MEI         = if ("MEI"         %in% keep_idx) MEI         else NA_real_,
      ONI         = if ("ONI"         %in% keep_idx) ONI         else NA_real_,
      ENSO_Phase3 = if ("ENSO_Phase3" %in% keep_idx) ENSO_Phase3 else NA
    ) %>%
    filter(is.finite(Year), is.finite(Month),
           is.finite(Lon_raw), is.finite(Lat),
           is.finite(w), w > 0) %>%
    mutate(Lon = wrap360(Lon_raw)) %>%
    group_by(Year, Month) %>%
    mutate(
      W_sum_month = sum(w, na.rm = TRUE),
      w_rel = if_else(is.finite(W_sum_month) & W_sum_month > 0,
                      w / W_sum_month, NA_real_)
    ) %>%
    summarise(
      MEI         = mean(MEI, na.rm = TRUE),
      ONI         = mean(ONI, na.rm = TRUE),
      ENSO_Phase3 = first(na.omit(ENSO_Phase3)),
      COG_Lon_raw = wmean_circular_360(Lon, w),
      COG_Lat_raw = wmean_safe(Lat, w),
      COG_Lon     = wmean_circular_360(Lon, w_rel),
      COG_Lat     = wmean_safe(Lat, w_rel),
      W_sum       = first(W_sum_month),
      n_cell      = n(),
      .groups     = "drop"
    ) %>%
    mutate(Time = make_time(Year, Month), Tuna = tuna_label) %>%
    select(Year, Month, Time, Tuna, any_of(c("MEI", "ONI", "ENSO_Phase3")),
           COG_Lon, COG_Lat, COG_Lon_raw, COG_Lat_raw, W_sum, n_cell) %>%
    arrange(Time)

  if ("ENSO_Phase3" %in% names(out)) {
    out <- out %>%
      mutate(ENSO_Phase3 = factor(
        as.character(ENSO_Phase3),
        levels = c("La Niña", "Neutral", "El Niño")
      ))
  }
  out
}

monthly_cog_long <- bind_rows(
  build_monthly_cog_one(northern_alb_model_data, "ALB_N", weight_col = "pre_ALB"),
  build_monthly_cog_one(southern_alb_model_data, "ALB_S", weight_col = "pre_ALB"),
  build_monthly_cog_one(bet_model_data,          "BET",   weight_col = "pre_BET"),
  build_monthly_cog_one(yft_model_data,          "YFT",   weight_col = "pre_YFT")
) %>%
  mutate(Tuna = factor(Tuna, levels = names(species_colors))) %>%
  arrange(Tuna, Time)

# Sanity check: raw vs normalized should be ~identical
sanity_cog_invariance <- monthly_cog_long %>%
  group_by(Tuna) %>%
  summarise(
    max_abs_diff_lon = max(abs(COG_Lon - COG_Lon_raw), na.rm = TRUE),
    max_abs_diff_lat = max(abs(COG_Lat - COG_Lat_raw), na.rm = TRUE),
    .groups = "drop"
  )
print(sanity_cog_invariance)

# Phase dataset for GLS
phase_cog_dat <- monthly_cog_long %>%
  filter(!is.na(ENSO_Phase3)) %>%
  mutate(
    ENSO_Phase3 = factor(as.character(ENSO_Phase3),
                         levels = c("Neutral", "La Niña", "El Niño")),
    Time_num = as.numeric(Time),
    trend    = Time_num,
    logW     = log(W_sum)
  ) %>%
  arrange(Tuna, Time_num)


# =============================================================================
# 4. GLS helpers (fit, refit, inference)
# =============================================================================

AICc_gls <- function(m) {
  k <- length(stats::coef(m))
  n <- stats::nobs(m)
  a <- stats::AIC(m)
  if (!is.finite(n) || !is.finite(k) || (n - k - 1) <= 0) return(NA_real_)
  a + (2 * k * (k + 1)) / (n - k - 1)
}

safe_gls_fit <- function(formula, data, method = "ML", use_ar1 = TRUE) {
  cor_struct <- if (use_ar1) nlme::corAR1(form = ~ Time_num) else NULL
  m <- tryCatch(
    nlme::gls(model = formula, data = data, method = method, correlation = cor_struct),
    error = function(e) e
  )
  if (inherits(m, "error")) {
    return(list(ok = FALSE, model = NULL,
                note = paste0("gls failed: ", conditionMessage(m)),
                rho = NA_real_))
  }
  rho <- tryCatch(
    as.numeric(coef(m$modelStruct$corStruct, unconstrained = FALSE)),
    error = function(e) NA_real_
  )
  list(ok = TRUE, model = m, note = NA_character_, rho = rho)
}

safe_refit_REML <- function(formula_str, data, use_ar1 = TRUE) {
  tryCatch(
    nlme::gls(
      model       = stats::as.formula(formula_str),
      data        = data,
      method      = "REML",
      correlation = if (use_ar1) nlme::corAR1(form = ~ Time_num) else NULL
    ),
    error = function(e) e
  )
}


# =============================================================================
# 5. Inference builder: baseline at covariate means
# =============================================================================

# Covariates in the model formula (excluding response and ENSO_Phase3)
get_covars_in_formula <- function(formula_str, response_col) {
  fml  <- stats::as.formula(formula_str)
  vars <- all.vars(fml)
  setdiff(vars, c(response_col, "ENSO_Phase3"))
}

# Predicted Neutral at covariate means (panel c baseline)
predict_baseline_neutral_at_means <- function(m, df_one, response_col) {
  if (is.null(m) || inherits(m, "error")) return(NA_real_)
  covs <- get_covars_in_formula(deparse(formula(m)), response_col)
  nd   <- list(ENSO_Phase3 = factor("Neutral", levels = levels(df_one$ENSO_Phase3)))
  for (v in covs) {
    if (v %in% names(df_one)) nd[[v]] <- mean(df_one[[v]], na.rm = TRUE)
  }
  as.numeric(predict(m, newdata = as.data.frame(nd)))
}

# Extract phase shifts + covariate effects from GLS tTable
extract_phase_shift_ttable <- function(m, df_one, response_col) {
  empty <- tibble(
    mu_neutral       = NA_real_,
    shift_lanina     = NA_real_, shift_lanina_lwr = NA_real_,
    shift_lanina_upr = NA_real_, p_lanina = NA_real_,
    shift_elnino     = NA_real_, shift_elnino_lwr = NA_real_,
    shift_elnino_upr = NA_real_, p_elnino = NA_real_,
    beta_mei  = NA_real_, p_mei  = NA_real_,
    beta_trend = NA_real_, p_trend = NA_real_,
    beta_logW = NA_real_, p_logW = NA_real_,
    rho_ar1_reml = NA_real_,
    note_infer = NA_character_
  )

  if (is.null(m) || inherits(m, "error")) {
    empty$note_infer <- "model is NULL/error"
    return(empty)
  }

  mu0 <- predict_baseline_neutral_at_means(m, df_one, response_col)

  tt <- summary(m)$tTable
  rn <- rownames(tt)

  get_est_se_p <- function(term) {
    if (term %in% rn) {
      c(est = as.numeric(tt[term, "Value"]),
        se  = as.numeric(tt[term, "Std.Error"]),
        p   = as.numeric(tt[term, "p-value"]))
    } else {
      c(est = NA_real_, se = NA_real_, p = NA_real_)
    }
  }

  la  <- get_est_se_p("ENSO_Phase3La Niña")
  el  <- get_est_se_p("ENSO_Phase3El Niño")
  mei <- get_est_se_p("MEI")
  tr  <- get_est_se_p("trend")
  lw  <- get_est_se_p("logW")
  z   <- 1.96

  tibble(
    mu_neutral       = mu0,
    shift_lanina     = la["est"],
    shift_lanina_lwr = la["est"] - z * la["se"],
    shift_lanina_upr = la["est"] + z * la["se"],
    p_lanina         = la["p"],
    shift_elnino     = el["est"],
    shift_elnino_lwr = el["est"] - z * el["se"],
    shift_elnino_upr = el["est"] + z * el["se"],
    p_elnino         = el["p"],
    beta_mei   = mei["est"], p_mei   = mei["p"],
    beta_trend = tr["est"],  p_trend = tr["p"],
    beta_logW  = lw["est"],  p_logW  = lw["p"],
    rho_ar1_reml = tryCatch(
      as.numeric(coef(m$modelStruct$corStruct, unconstrained = FALSE)),
      error = function(e) NA_real_
    ),
    note_infer = NA_character_
  )
}

# Build inference table from best-model table
build_phase_infer_tbl <- function(best_tbl, phase_dat, response_col,
                                  use_ar1 = TRUE) {
  stopifnot(all(c("Tuna", "formula") %in% names(best_tbl)))
  stopifnot(all(c("Tuna", "Time_num", "ENSO_Phase3", response_col)
                %in% names(phase_dat)))

  best_tbl %>%
    mutate(
      Tuna_chr = as.character(Tuna),
      df_one = map2(Tuna_chr, formula, ~ {
        fml       <- stats::as.formula(.y)
        vars_need <- unique(c(all.vars(fml), "Time_num"))
        phase_dat %>%
          filter(as.character(Tuna) == .x) %>%
          select(any_of(vars_need)) %>%
          filter(stats::complete.cases(.)) %>%
          arrange(Time_num) %>%
          mutate(ENSO_Phase3 = factor(as.character(ENSO_Phase3),
                                      levels = c("Neutral", "La Niña", "El Niño")))
      }),
      model_reml = map2(formula, df_one,
                        ~ safe_refit_REML(.x, .y, use_ar1 = use_ar1)),
      infer = map2(model_reml, df_one,
                   ~ extract_phase_shift_ttable(.x, .y, response_col))
    ) %>%
    select(-Tuna_chr, -df_one) %>%
    unnest(infer) %>%
    mutate(
      response       = response_col,
      p_lanina_label = map_chr(p_lanina, fp_eq),
      p_elnino_label = map_chr(p_elnino, fp_eq)
    )
}


# =============================================================================
# 6. Three-panel figure builder (a/b/c for Lon or Lat)
# =============================================================================
make_fig_abc_cog <- function(monthly_tbl, response_col, infer_tbl,
                             ylab_main, xlab_c) {

  enso_levels_plot <- c("La Niña", "Neutral", "El Niño")

  dat0 <- monthly_tbl %>%
    mutate(
      Tuna        = factor(as.character(Tuna), levels = names(species_colors)),
      ENSO_Phase3 = factor(as.character(ENSO_Phase3), levels = enso_levels_plot)
    ) %>%
    filter(is.finite(MEI), is.finite(.data[[response_col]])) %>%
    drop_na(MEI, .data[[response_col]], Tuna, ENSO_Phase3, Time)

  # ---- Panel (a): response ~ MEI (linear + GAM) ----
  fit_mei_lin_gam_one <- function(df_one, k = 5, grid_n = 200, min_n = 10) {
    df_one <- df_one %>% filter(is.finite(MEI), is.finite(.data[[response_col]]))
    if (nrow(df_one) < min_n) return(list(pred = tibble(), stat = tibble()))

    xg      <- seq(min(df_one$MEI), max(df_one$MEI), length.out = grid_n)
    grid_df <- tibble(MEI = xg)

    f_lin       <- stats::as.formula(paste0(response_col, " ~ MEI"))
    m_lin       <- lm(f_lin, data = df_one)
    p_lin       <- predict(m_lin, newdata = grid_df)
    p_lin_slope <- tryCatch(summary(m_lin)$coefficients["MEI", "Pr(>|t|)"],
                            error = function(e) NA_real_)

    f_gam    <- stats::as.formula(paste0(response_col, " ~ s(MEI, k = ", k, ")"))
    m_gam    <- mgcv::gam(f_gam, data = df_one, method = "REML")
    p_gam    <- predict(m_gam, newdata = grid_df)
    s_gam    <- summary(m_gam)
    edf      <- as.numeric(s_gam$s.table[1, "edf"])
    p_smooth <- as.numeric(s_gam$s.table[1, "p-value"])

    dAIC  <- AIC(m_gam) - AIC(m_lin)
    r_val <- suppressWarnings(cor(df_one$MEI, df_one[[response_col]],
                                  use = "complete.obs"))

    pred_df <- bind_rows(
      grid_df %>% mutate(pred = p_lin, Model = "Linear"),
      grid_df %>% mutate(pred = p_gam, Model = "GAM")
    )

    stat_df <- tibble(
      r = r_val, linear_p = fp_eq(p_lin_slope, digits = 3),
      gam_edf = edf, gam_p = fp_eq(p_smooth, digits = 3), dAIC = dAIC
    )

    list(pred = pred_df, stat = stat_df)
  }

  split_list <- dat0 %>% group_by(Tuna) %>% group_split()
  names(split_list) <- levels(dat0$Tuna)

  res_list <- imap(split_list, ~ fit_mei_lin_gam_one(.x, k = 5, grid_n = 200))
  pred_all <- imap_dfr(res_list, ~ .x$pred %>% mutate(Tuna = .y))
  stat_all <- imap_dfr(res_list, ~ .x$stat %>% mutate(Tuna = .y)) %>%
    mutate(Tuna = factor(Tuna, levels = names(species_colors)))

  anchor_tbl <- dat0 %>%
    group_by(Tuna) %>%
    summarise(
      x_left  = min(MEI, na.rm = TRUE),
      x_right = max(MEI, na.rm = TRUE),
      y_top   = max(.data[[response_col]], na.rm = TRUE),
      y_rng   = max(.data[[response_col]], na.rm = TRUE) -
                min(.data[[response_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(y_top1 = y_top + 0.08 * ifelse(y_rng == 0, 1, y_rng))

  stat_left2 <- stat_all %>%
    mutate(label_r = sprintf("r = %.2f", r)) %>%
    left_join(anchor_tbl, by = "Tuna")

  stat_right2 <- stat_all %>%
    mutate(label = paste0(
      "Linear: ", linear_p, "\n",
      "GAM: edf = ", sprintf("%.2f", gam_edf), ", ", gam_p, "\n",
      "\u0394AIC (GAM-Linear) = ", sprintf("%.1f", dAIC)
    )) %>%
    left_join(anchor_tbl, by = "Tuna")

  p_a <- ggplot(dat0, aes(x = MEI, y = .data[[response_col]])) +
    geom_point(aes(color = Tuna), alpha = 0.30, size = 1.6) +
    geom_line(
      data = pred_all %>% filter(Model == "Linear"),
      aes(x = MEI, y = pred, group = Tuna),
      inherit.aes = FALSE,
      color = "black", linetype = "dashed", linewidth = 1.3
    ) +
    geom_line(
      data = pred_all %>% filter(Model == "GAM"),
      aes(x = MEI, y = pred, color = Tuna, group = Tuna),
      inherit.aes = FALSE,
      linewidth = 2.1, alpha = 0.85
    ) +
    geom_text(
      data = stat_left2,
      aes(x = x_left, y = y_top1, label = label_r),
      inherit.aes = FALSE,
      hjust = 0, vjust = 1, size = 3.2, fontface = "bold", color = "black"
    ) +
    geom_text(
      data = stat_right2,
      aes(x = x_right, y = y_top1, label = label),
      inherit.aes = FALSE,
      hjust = 1, vjust = 1, size = 3.1, fontface = "bold", color = "black"
    ) +
    facet_wrap(~ Tuna, scales = "free_y", ncol = 1) +
    scale_color_manual(values = species_colors) +
    labs(x = "MEI", y = ylab_main) +
    theme_gcb(base_size = 10)

  # ---- Panel (b): violin + p-value brackets ----
  dat_violin <- dat0 %>%
    mutate(x_pos = case_when(
      ENSO_Phase3 == "La Niña" ~ 1,
      ENSO_Phase3 == "Neutral" ~ 3,
      ENSO_Phase3 == "El Niño" ~ 5,
      TRUE ~ NA_real_
    ))

  n_map <- dat_violin %>%
    distinct(Time, ENSO_Phase3) %>%
    count(ENSO_Phase3, name = "n") %>%
    tibble::deframe()

  x_labels <- c(
    paste0("La Niña\n(n = ", n_map[["La Niña"]], ")"),
    paste0("Neutral\n(n = ", n_map[["Neutral"]], ")"),
    paste0("El Niño\n(n = ", n_map[["El Niño"]], ")")
  )

  y_rng_tbl <- dat_violin %>%
    group_by(Tuna) %>%
    summarise(
      y_min      = min(.data[[response_col]], na.rm = TRUE),
      y_max      = max(.data[[response_col]], na.rm = TRUE),
      y_rng      = y_max - y_min,
      y_rng_safe = ifelse(y_rng == 0, 1, y_rng),
      .groups    = "drop"
    )

  sig_df <- infer_tbl %>%
    select(Tuna, p_lanina, p_elnino) %>%
    pivot_longer(cols = c(p_lanina, p_elnino),
                 names_to = "which", values_to = "p_value") %>%
    mutate(
      ENSO_Phase3 = case_when(
        which == "p_lanina" ~ "La Niña",
        which == "p_elnino" ~ "El Niño",
        TRUE ~ NA_character_
      ),
      label_p    = map_chr(p_value, fp_eq, digits = 3),
      label_star = map_chr(p_value, p_stars)
    ) %>%
    filter(!is.na(ENSO_Phase3)) %>%
    left_join(y_rng_tbl, by = "Tuna") %>%
    mutate(
      x1     = if_else(ENSO_Phase3 == "La Niña", 1, 3),
      x2     = if_else(ENSO_Phase3 == "La Niña", 3, 5),
      row_id = if_else(ENSO_Phase3 == "La Niña", 1L, 2L),
      y_br   = y_max + (0.10 + 0.10 * row_id) * y_rng_safe,
      y_p    = y_br + 0.03 * y_rng_safe,
      y_star = y_br - 0.05 * y_rng_safe
    )

  p_b <- ggplot(dat_violin, aes(x = x_pos, y = .data[[response_col]])) +
    geom_violin(
      aes(fill = ENSO_Phase3, group = x_pos),
      width = 1.35, scale = "width", trim = FALSE,
      color = "white", linewidth = 0.6, alpha = 0.95
    ) +
    geom_boxplot(
      aes(group = x_pos),
      width = 0.22, outlier.shape = NA,
      fill = "black", color = "black", linewidth = 0.65
    ) +
    stat_summary(
      fun = median, geom = "point",
      shape = 21, size = 2.3,
      fill = "white", color = "black", stroke = 0.8
    ) +
    # Significance brackets
    geom_segment(data = sig_df,
                 aes(x = x1, xend = x2, y = y_br, yend = y_br),
                 inherit.aes = FALSE, linewidth = 0.6, color = "black") +
    geom_segment(data = sig_df,
                 aes(x = x1, xend = x1, y = y_br, yend = y_br - 0.03 * y_rng_safe),
                 inherit.aes = FALSE, linewidth = 0.6, color = "black") +
    geom_segment(data = sig_df,
                 aes(x = x2, xend = x2, y = y_br, yend = y_br - 0.03 * y_rng_safe),
                 inherit.aes = FALSE, linewidth = 0.6, color = "black") +
    geom_text(data = sig_df,
              aes(x = (x1 + x2) / 2, y = y_p, label = label_p),
              inherit.aes = FALSE,
              size = 3.2, fontface = "bold", color = "black", vjust = 0) +
    geom_text(data = sig_df %>% filter(label_star != ""),
              aes(x = (x1 + x2) / 2, y = y_star, label = label_star),
              inherit.aes = FALSE,
              size = 3.4, fontface = "bold", color = "black", vjust = 1) +
    facet_wrap(~ Tuna, ncol = 1, scales = "free_y") +
    scale_x_continuous(breaks = c(1, 3, 5), labels = x_labels,
                       expand = expansion(mult = c(0.06, 0.06))) +
    scale_fill_manual(values = enso_colors3, drop = FALSE) +
    labs(x = "ENSO phase", y = ylab_main) +
    coord_cartesian(clip = "off") +
    theme_gcb(base_size = 10) +
    theme(plot.margin = margin(6, 10, 10, 8))

  # ---- Panel (c): shift from Neutral + baseline label ----
  tuna_levels <- names(species_colors)

  rel_df <- infer_tbl %>%
    mutate(Tuna = factor(as.character(Tuna), levels = tuna_levels)) %>%
    group_by(Tuna) %>%
    group_modify(~ {
      base <- .x$mu_neutral
      tibble(
        ENSO_Phase3 = factor(c("El Niño", "Neutral", "La Niña"),
                             levels = c("El Niño", "Neutral", "La Niña")),
        rel_change = c(.x$shift_elnino, 0, .x$shift_lanina),
        ci_low     = c(.x$shift_elnino_lwr, 0, .x$shift_lanina_lwr),
        ci_high    = c(.x$shift_elnino_upr, 0, .x$shift_lanina_upr),
        baseline   = base
      )
    }) %>%
    ungroup()

  x_anchor <- rel_df %>%
    group_by(Tuna) %>%
    summarise(x_right = max(ci_high, rel_change, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(x_right = ifelse(is.finite(x_right), x_right, 0))

  # Dynamic baseline label position (left of leftmost CI)
  x_left_tbl <- rel_df %>%
    group_by(Tuna) %>%
    summarise(
      x_left  = min(ci_low, rel_change, na.rm = TRUE),
      x_right = max(ci_high, rel_change, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      x_rng = ifelse(is.finite(x_right - x_left) & (x_right - x_left) > 0,
                     x_right - x_left, 1),
      x_lab = x_left - 0.08 * x_rng
    )

  neutral_lab <- rel_df %>%
    filter(ENSO_Phase3 == "Neutral") %>%
    left_join(x_left_tbl, by = "Tuna") %>%
    transmute(
      Tuna, ENSO_Phase3, x_lab,
      label_base = sprintf("Baseline = %.1f", baseline)
    )

  rel_df2 <- rel_df %>%
    left_join(x_anchor, by = "Tuna") %>%
    mutate(
      label = if_else(ENSO_Phase3 == "Neutral",
                      NA_character_, sprintf("%+.2f", rel_change)),
      lab_x = case_when(
        ENSO_Phase3 == "Neutral" ~ NA_real_,
        rel_change >= 0 ~ rel_change + 0.08 * ifelse(x_right == 0, 1, abs(x_right)) + 0.05,
        TRUE            ~ rel_change - 0.08 * ifelse(x_right == 0, 1, abs(x_right)) - 0.05
      )
    )

  p_c <- ggplot(rel_df2, aes(x = rel_change, y = ENSO_Phase3)) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.6, color = "grey45") +
    geom_errorbar(
      data = rel_df2 %>% filter(ENSO_Phase3 != "Neutral"),
      aes(y = ENSO_Phase3, xmin = ci_low, xmax = ci_high),
      inherit.aes = FALSE,
      width = 0.18, linewidth = 0.7, color = "black",
      orientation = "y"
    ) +
    geom_point(aes(color = ENSO_Phase3), size = 2.3) +
    geom_text(
      data = rel_df2 %>% filter(ENSO_Phase3 != "Neutral"),
      aes(x = lab_x, label = label),
      size = 3.2, fontface = "bold",
      vjust = -0.6, nudge_y = 0.10, color = "black"
    ) +
    geom_text(
      data = neutral_lab,
      aes(x = x_lab, y = ENSO_Phase3, label = label_base),
      inherit.aes = FALSE,
      hjust = 0, size = 3.2, fontface = "bold", color = "black",
      nudge_y = -0.32
    ) +
    facet_wrap(~ Tuna, nrow = 1) +
    scale_color_manual(values = enso_colors3, drop = FALSE) +
    scale_y_discrete(limits = c("El Niño", "Neutral", "La Niña")) +
    labs(x = xlab_c, y = "ENSO phase") +
    coord_cartesian(clip = "off") +
    theme_gcb(base_size = 10)

  # ---- Assemble ----
  final_fig <- (p_a + p_b) / p_c +
    plot_layout(heights = c(3.5, 1.2)) +
    plot_annotation(tag_levels = "a") &
    theme(
      plot.tag          = element_text(face = "bold", size = 12),
      plot.tag.position = c(0.02, 0.98)
    )

  list(fig = final_fig, p_a = p_a, p_b = p_b, p_c = p_c, panelc_table = rel_df2)
}


# =============================================================================
# 7. Run: build inference tables and generate figures
# =============================================================================

# Inference from already-selected best models
gls_cog_lon_infer <- build_phase_infer_tbl(gls_cog_lon_best1, phase_cog_dat,
                                           response_col = "COG_Lon", use_ar1 = TRUE)
gls_cog_lat_infer <- build_phase_infer_tbl(gls_cog_lat_best1, phase_cog_dat,
                                           response_col = "COG_Lat", use_ar1 = TRUE)

# Build figures
out_lon <- make_fig_abc_cog(
  monthly_tbl  = monthly_cog_long,
  response_col = "COG_Lon",
  infer_tbl    = gls_cog_lon_infer,
  ylab_main    = "COG Lon (deg, 0\u2013360\u00b0)",
  xlab_c       = "Shift from Neutral in COG Lon (deg, 0\u2013360\u00b0)"
)

out_lat <- make_fig_abc_cog(
  monthly_tbl  = monthly_cog_long,
  response_col = "COG_Lat",
  infer_tbl    = gls_cog_lat_infer,
  ylab_main    = "COG Lat (deg)",
  xlab_c       = "Shift from Neutral in COG Lat (deg)"
)

out_lon$fig
out_lat$fig

# Diagnostics
gls_cog_lon_infer %>% select(Tuna, mu_neutral, shift_lanina, shift_elnino,
                             p_lanina, p_elnino)
gls_cog_lat_infer %>% select(Tuna, mu_neutral, shift_lanina, shift_elnino,
                             p_lanina, p_elnino)


# =============================================================================
# 8. Save  -->  Fig_2 (Lon), Fig_3 (Lat)
# =============================================================================
save_plot_all(
  p             = out_lon$fig,
  filename_base = "Fig_2",
  width         = 12,
  height        = 10,
  dpi           = 600
)

save_plot_all(
  p             = out_lat$fig,
  filename_base = "Fig_3",
  width         = 12,
  height        = 10,
  dpi           = 600
)
