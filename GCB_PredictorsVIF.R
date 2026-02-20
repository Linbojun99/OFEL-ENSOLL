###############################################################################
# GCB_VIF.R
# ──────────────────────────────────────────────────────────────────────────────
# VIF stepwise elimination with full trace logging and diagnostic plot.
#
# Workflow:
#   1) Run VIF stepwise for each species (ALB_N, ALB_S, BET, YFT)
#   2) Collect per-step VIF values and dropped-variable logs
#   3) Plot max-VIF trajectory per species  -->  Fig_Model_VIF
#
# Requires objects already in the workspace:
#   northern_alb_model_data, southern_alb_model_data,
#   bet_model_data, yft_model_data,
#   alb_n_initial_vars, alb_s_initial_vars,
#   bet_initial_vars, yft_initial_vars,
#   species_colors, theme_gcb(), save_plot_all()
#   (produced by GCB_Data.R / GCB_Model.R)
###############################################################################


# =============================================================================
# 0. Packages
# =============================================================================
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(car)


# =============================================================================
# 1. Helper: convert car::vif() output to a named numeric vector
# =============================================================================
vif_to_numeric <- function(v_raw) {
  if (is.matrix(v_raw)) {
    # Generalised VIF (factors present): use GVIF^(1/(2*Df))
    if ("GVIF^(1/(2*Df))" %in% colnames(v_raw)) {
      v <- v_raw[, "GVIF^(1/(2*Df))"]
    } else if (all(c("GVIF", "Df") %in% colnames(v_raw))) {
      v <- v_raw[, "GVIF"]^(1 / (2 * v_raw[, "Df"]))
    } else {
      v <- v_raw[, ncol(v_raw)]
    }
    v <- as.numeric(v)
    names(v) <- rownames(v_raw)
    return(v)
  }
  as.numeric(v_raw)
}


# =============================================================================
# 2. VIF stepwise with full trace (per-step VIF for every variable)
# =============================================================================
vif_stepwise_trace <- function(df,
                               target,
                               keep_always   = c("Year", "Month", "Lon", "Lat"),
                               vif_threshold = 10,
                               verbose       = TRUE) {

  stopifnot(target %in% names(df))
  stopifnot(all(keep_always %in% names(df)))

  # Numeric predictors only
  x0        <- setdiff(names(df), target)
  numeric_x <- x0[sapply(df[, x0, drop = FALSE], is.numeric)]
  x         <- unique(c(intersect(keep_always, numeric_x),
                        setdiff(numeric_x, keep_always)))

  # Edge case: no removable predictors
  if (length(setdiff(x, keep_always)) < 1) {
    return(list(
      keep     = c(target, x),
      vif_log  = tibble(step = 0L, dropped = "STOP",
                        max_vif = NA_real_, n_vars = length(x)),
      vif_long = tibble(step = 0L, var = x, vif = NA_real_,
                        dropped = "STOP", max_vif = NA_real_,
                        threshold = vif_threshold)
    ))
  }

  vif_log  <- tibble(step = integer(), dropped = character(),
                     max_vif = double(), n_vars = integer())
  vif_long <- tibble(step = integer(), var = character(), vif = double(),
                     dropped = character(), max_vif = double(),
                     threshold = double())

  step <- 0L

  repeat {
    step <- step + 1L

    form <- as.formula(paste(target, "~", paste(x, collapse = " + ")))
    fit  <- try(lm(form, data = df), silent = TRUE)

    if (inherits(fit, "try-error")) {
      if (verbose) message("lm failed (singular). Stop.")
      vif_log <- bind_rows(vif_log,
                           tibble(step = step, dropped = "STOP",
                                  max_vif = NA_real_, n_vars = length(x)))
      break
    }

    v_raw <- try(car::vif(fit), silent = TRUE)
    if (inherits(v_raw, "try-error")) {
      if (verbose) message("car::vif failed. Stop.")
      vif_log <- bind_rows(vif_log,
                           tibble(step = step, dropped = "STOP",
                                  max_vif = NA_real_, n_vars = length(x)))
      break
    }

    v <- vif_to_numeric(v_raw)
    if (is.null(names(v))) {
      nm <- colnames(model.matrix(fit))
      nm <- nm[nm != "(Intercept)"]
      names(v) <- nm[seq_along(v)]
    }

    max_v <- max(v, na.rm = TRUE)

    # Record per-variable VIF for this step
    vif_long <- bind_rows(
      vif_long,
      tibble(step = step, var = names(v), vif = as.numeric(v),
             dropped = NA_character_, max_vif = max_v,
             threshold = vif_threshold)
    )

    # Converged: all VIFs below threshold
    if (!is.finite(max_v) || max_v <= vif_threshold) {
      vif_log <- bind_rows(vif_log,
                           tibble(step = step, dropped = "STOP",
                                  max_vif = max_v, n_vars = length(x)))
      vif_long <- vif_long %>%
        mutate(dropped = if_else(step == !!step, "STOP", dropped))
      break
    }

    # Drop the non-protected variable with the highest VIF
    cand <- setdiff(names(sort(v, decreasing = TRUE)), keep_always)
    if (length(cand) == 0) {
      vif_log <- bind_rows(vif_log,
                           tibble(step = step, dropped = "STOP",
                                  max_vif = max_v, n_vars = length(x)))
      vif_long <- vif_long %>%
        mutate(dropped = if_else(step == !!step, "STOP", dropped))
      break
    }

    drop_var <- cand[1]
    x <- setdiff(x, drop_var)

    vif_log <- bind_rows(vif_log,
                         tibble(step = step, dropped = drop_var,
                                max_vif = max_v, n_vars = length(x)))
    vif_long <- vif_long %>%
      mutate(dropped = if_else(step == !!step, drop_var, dropped))

    if (verbose) {
      message("VIF step ", step, ": drop ", drop_var,
              " (max VIF = ", round(max_v, 2), ")")
    }

    # Only protected variables remain
    if (length(setdiff(x, keep_always)) < 1) break
  }

  list(keep = c(target, x), vif_log = vif_log, vif_long = vif_long)
}

# -- Convenience wrapper: subset columns then run VIF trace --
build_task_with_vif_trace <- function(model_df,
                                      target,
                                      initial_vars,
                                      keep_always   = c("Year", "Month", "Lon", "Lat"),
                                      vif_threshold = 5,
                                      verbose       = TRUE) {

  df0 <- model_df[, initial_vars, drop = FALSE]

  res <- vif_stepwise_trace(
    df            = df0,
    target        = target,
    keep_always   = keep_always,
    vif_threshold = vif_threshold,
    verbose       = verbose
  )

  df_final <- df0[, res$keep, drop = FALSE]

  list(df = df_final, keep = res$keep,
       vif_log = res$vif_log, vif_long = res$vif_long)
}


# =============================================================================
# 3. Run VIF stepwise for all four species
# =============================================================================
THR <- 5

res_vif_alb_n1 <- build_task_with_vif_trace(
  model_df      = northern_alb_model_data,
  target        = "nCPUE_ALB",
  initial_vars  = alb_n_initial_vars,
  keep_always   = c("Year", "Month", "Lon", "Lat"),
  vif_threshold = THR,
  verbose       = TRUE
)

res_vif_alb_s1 <- build_task_with_vif_trace(
  model_df      = southern_alb_model_data,
  target        = "nCPUE_ALB",
  initial_vars  = alb_s_initial_vars,
  keep_always   = c("Year", "Month", "Lon", "Lat"),
  vif_threshold = THR,
  verbose       = TRUE
)

res_vif_bet1 <- build_task_with_vif_trace(
  model_df      = bet_model_data,
  target        = "nCPUE_BET",
  initial_vars  = bet_initial_vars,
  keep_always   = c("Year", "Month", "Lon", "Lat"),
  vif_threshold = THR,
  verbose       = TRUE
)

res_vif_yft1 <- build_task_with_vif_trace(
  model_df      = yft_model_data,
  target        = "nCPUE_YFT",
  initial_vars  = yft_initial_vars,
  keep_always   = c("Year", "Month", "Lon", "Lat"),
  vif_threshold = THR,
  verbose       = TRUE
)


# =============================================================================
# 4. Assemble combined tables
# =============================================================================
res_list <- list(
  ALB_N = res_vif_alb_n1,
  ALB_S = res_vif_alb_s1,
  BET   = res_vif_bet1,
  YFT   = res_vif_yft1
)

# Per-step VIF values for every variable
vif_long_all <- imap_dfr(res_list, ~ .x$vif_long %>% mutate(Tuna = .y)) %>%
  mutate(Tuna = factor(Tuna, levels = c("ALB_N", "ALB_S", "BET", "YFT")))

# Summary log: max VIF + remaining variable count per step
vif_log_all <- imap_dfr(res_list, ~ .x$vif_log %>% mutate(Tuna = .y)) %>%
  mutate(Tuna = factor(Tuna, levels = c("ALB_N", "ALB_S", "BET", "YFT")))

# Retained variables per species
vif_keep_tbl <- tibble(
  Tuna = names(res_list),
  keep = map(res_list, "keep")
) %>%
  unnest_longer(keep) %>%
  group_by(Tuna) %>%
  summarise(keep_vars = paste(keep, collapse = ", "), .groups = "drop")


# =============================================================================
# 5. Diagnostic plot: max-VIF trajectory  -->  Fig_Model_VIF
# =============================================================================
vif_max_tbl <- vif_log_all %>%
  mutate(threshold = THR) %>%
  select(Tuna, step, max_vif, dropped, n_vars, threshold)

vif_max_tbl2 <- vif_max_tbl %>%
  mutate(
    Tuna    = factor(Tuna, levels = names(species_colors)),
    dropped = ifelse(is.na(dropped), "", dropped),
    label_y = max_vif + 0.03 * (max(max_vif, na.rm = TRUE) -
                                  min(max_vif, na.rm = TRUE) + 1e-6)
  )

p_vif_max <- ggplot(vif_max_tbl2, aes(x = step, y = max_vif)) +
  geom_hline(aes(yintercept = threshold),
             linetype = "dashed", linewidth = 0.9, color = "grey30") +
  geom_line(aes(color = Tuna), linewidth = 1.4, lineend = "round") +
  geom_point(aes(fill = Tuna),
             shape = 21, color = "grey10",
             size = 2.8, stroke = 0.35) +
  geom_text(aes(y = label_y, label = dropped, color = Tuna),
            size = 3.2, fontface = "bold", vjust = 0) +
  facet_wrap(~ Tuna, scales = "free_y") +
  scale_color_manual(values = species_colors, guide = "none") +
  scale_fill_manual(values = species_colors,  guide = "none") +
  labs(x = "Step", y = "Max VIF") +
  theme_gcb(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

p_vif_max

save_plot_all(
  p             = p_vif_max,
  filename_base = "Fig_Model_VIF",
  width         = 10,
  height        = 8,
  dpi           = 600
)


# =============================================================================
# 6. Inspect top-5 VIF variables at each step (optional diagnostic)
# =============================================================================
vif_top_each_step <- vif_long_all %>%
  group_by(Tuna, step) %>%
  arrange(desc(vif)) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  select(Tuna, step, var, vif, dropped, max_vif, threshold)

vif_top_each_step
