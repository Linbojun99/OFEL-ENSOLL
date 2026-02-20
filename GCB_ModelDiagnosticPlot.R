###############################################################################
# GCB_Model_Dia_Pl.R
# ──────────────────────────────────────────────────────────────────────────────
# Model diagnostic plots: learning curves, observed vs predicted,
# residuals, for all four tuna species arranged in a 4-row x 4-column grid.
#
# Output:
#   - Fig_Model_Dia : combined performance diagnostics panel
#
# Requires objects already in the workspace:
#   fits            (named list of fit objects per species)
#   dfs             (named list of data.frames per species)
#   theme_gcb(), save_plot_all()
###############################################################################


# =============================================================================
# 0. Packages
# =============================================================================
library(tidyverse)
library(xgboost)
library(patchwork)


# =============================================================================
# 1. Global layout constants & species colours
# =============================================================================
PANEL_AR     <- 0.80
PANEL_MARGIN <- margin(6, 6, 6, 6)

species_colors <- c(
  ALB_N = "#4B3F8C",
  ALB_S = "#00A6D6",
  BET   = "#1B9E77",
  YFT   = "#D95F02"
)

# Apply uniform aspect ratio and margin to any subplot
fix_subplot_size <- function(p, ar = PANEL_AR) {
  p + theme(aspect.ratio = ar, plot.margin = PANEL_MARGIN)
}


# =============================================================================
# 2. Helper: extract evaluation log from an xgb.Booster
# =============================================================================
get_eval_log <- function(bst) {
  if (!is.null(bst$evaluation_log)) return(as_tibble(bst$evaluation_log))
  elog2 <- attr(bst, "evaluation_log", exact = TRUE)
  if (!is.null(elog2)) return(as_tibble(elog2))
  elog3 <- tryCatch(xgboost::xgb.evals.log(bst), error = function(e) NULL)
  if (!is.null(elog3)) return(as_tibble(elog3))
  NULL
}


# =============================================================================
# 3. Learning curve plot (legend inside top-left corner)
# =============================================================================
plot_learning_curve <- function(fit, species = "ALB_N", color = "grey30") {

  elog <- get_eval_log(fit$model_es)

  if (is.null(elog) || nrow(elog) == 0) {
    p <- ggplot() +
      annotate("text", x = 0, y = 0,
               label = paste0(species, "\n(no evaluation_log found)")) +
      theme_void() +
      labs(title = paste0(species, " | Learning curve"))
    return(fix_subplot_size(p))
  }

  if (!("iter" %in% names(elog))) elog$iter <- seq_len(nrow(elog))

  df_long <- elog %>%
    pivot_longer(cols = -iter, names_to = "set_raw", values_to = "metric") %>%
    mutate(
      set = case_when(
        str_detect(set_raw, "^train") ~ "Train",
        str_detect(set_raw, "^valid") ~ "Validation",
        TRUE ~ set_raw
      ),
      set = factor(set, levels = c("Train", "Validation"))
    )

  p <- ggplot(df_long, aes(x = iter, y = metric)) +
    geom_line(aes(linetype = set, color = set), linewidth = 1.6) +
    geom_vline(xintercept = fit$best_iter, linetype = "dashed", linewidth = 1.2) +
    scale_linetype_manual(values = c(Train = "solid", Validation = "dashed")) +
    scale_color_manual(values = c(Train = color, Validation = color)) +
    labs(
      x        = "Boosting iteration",
      y        = "Tweedie nloglik\n(lower is better)",
      title    = paste0(species, " | Learning curve"),
      linetype = NULL,
      color    = NULL
    ) +
    theme_gcb() +
    theme(
      legend.position      = c(0.02, 0.98),
      legend.justification = c(0, 1),
      legend.direction     = "vertical",
      legend.background    = element_rect(fill = scales::alpha("white", 0.75),
                                          color = NA),
      legend.key           = element_blank()
    )

  fix_subplot_size(p)
}


# =============================================================================
# 4. Goodness-of-fit metrics
# =============================================================================
calc_metrics <- function(y, pred) {
  y <- as.numeric(y); pred <- as.numeric(pred)
  ok   <- is.finite(y) & is.finite(pred)
  y    <- y[ok]; pred <- pred[ok]
  rmse <- sqrt(mean((y - pred)^2))
  mae  <- mean(abs(y - pred))
  r2   <- 1 - sum((y - pred)^2) / sum((y - mean(y))^2)
  corv <- suppressWarnings(cor(y, pred))
  tibble(RMSE = rmse, MAE = mae, R2 = r2, Cor = corv, n = length(y))
}


# =============================================================================
# 5. Observed vs Predicted scatter
# =============================================================================
plot_obs_pred <- function(model, df, target, x_cols,
                          title = NULL, color = "grey30") {

  X <- as.matrix(df[, x_cols, drop = FALSE])
  storage.mode(X) <- "double"
  pred <- predict(model, newdata = X)

  dd <- tibble(obs = df[[target]], pred = pred) %>%
    filter(is.finite(obs), is.finite(pred))

  met <- calc_metrics(dd$obs, dd$pred)

  p <- ggplot(dd, aes(x = pred, y = obs)) +
    geom_point(alpha = 0.25, size = 1.4, color = "grey55") +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1.7, color = color) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 1.2) +
    labs(x = "Predicted", y = "Observed", title = title) +
    annotate(
      "text", x = Inf, y = -Inf, hjust = 1.05, vjust = -0.6,
      label = sprintf("RMSE=%.3f\nMAE=%.3f\nR²=%.3f\nCor=%.3f",
                      met$RMSE, met$MAE, met$R2, met$Cor),
      size = 3.5
    ) +
    theme_gcb()

  list(plot = fix_subplot_size(p), metrics = met)
}


# =============================================================================
# 6. Residual plot
# =============================================================================
plot_residuals <- function(model, df, target, x_cols,
                           title = NULL, color = "grey30") {

  X <- as.matrix(df[, x_cols, drop = FALSE])
  storage.mode(X) <- "double"
  pred <- predict(model, newdata = X)
  res  <- df[[target]] - pred

  dd <- tibble(pred = pred, res = res) %>%
    filter(is.finite(pred), is.finite(res))

  p <- ggplot(dd, aes(x = pred, y = res)) +
    geom_point(alpha = 0.25, size = 1.2, color = "grey55") +
    geom_smooth(method = "loess", se = FALSE, linewidth = 1.7, color = color) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.2) +
    labs(x = "Predicted", y = "Residual\n(Obs - Pred)", title = title) +
    theme_gcb()

  fix_subplot_size(p)
}


# =============================================================================
# 7. Single-species vertical panel (learning curve / valid / resid / train)
# =============================================================================
make_perf_panel_one_vertical <- function(fit, df, target, species,
                                         color = "grey30",
                                         train_frac_fallback = 0.8,
                                         seed_fallback = 123) {

  # Use stored split if available; otherwise recreate with fallback seed
  if (!is.null(fit$split) &&
      !is.null(fit$split$train_rows) &&
      !is.null(fit$split$valid_rows)) {
    train_rows <- fit$split$train_rows
    valid_rows <- fit$split$valid_rows
  } else {
    set.seed(seed_fallback)
    n          <- nrow(df)
    train_rows <- sample.int(n, size = floor(train_frac_fallback * n))
    valid_rows <- setdiff(seq_len(n), train_rows)
    message("[", species, "] fit$split not found -> fallback split (seed=",
            seed_fallback, ")")
  }

  train_df <- df[train_rows, , drop = FALSE]
  valid_df <- df[valid_rows, , drop = FALSE]

  p_lc <- plot_learning_curve(fit, species = species, color = color)

  op_valid <- plot_obs_pred(
    fit$model_final, valid_df, target, fit$x_cols,
    title = paste0(species, " | Validation"), color = color
  )$plot

  p_res <- plot_residuals(
    fit$model_final, valid_df, target, fit$x_cols,
    title = paste0(species, " | Residuals (Validation)"), color = color
  )

  op_train <- plot_obs_pred(
    fit$model_final, train_df, target, fit$x_cols,
    title = paste0(species, " | Train"), color = color
  )$plot

  (p_lc / op_valid / p_res / op_train) +
    plot_layout(heights = c(1, 1, 1, 1))
}


# =============================================================================
# 8. Assemble all species into a 4-column combined figure  -->  Fig_Model_Dia
# =============================================================================
targets <- c(ALB_N = "nCPUE_ALB", ALB_S = "nCPUE_ALB",
             BET   = "nCPUE_BET", YFT   = "nCPUE_YFT")

species_order <- c("ALB_N", "ALB_S", "BET", "YFT")

panels <- map(species_order, \(sp) {
  make_perf_panel_one_vertical(
    fit     = fits[[sp]],
    df      = dfs[[sp]],
    target  = targets[[sp]],
    species = sp,
    color   = species_colors[[sp]]
  )
})

combined_perf <- wrap_plots(panels, ncol = 4, byrow = TRUE) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag          = element_text(face = "bold", size = 12),
    plot.tag.position = c(0.02, 0.98)
  )

combined_perf

save_plot_all(
  p             = combined_perf,
  filename_base = "Fig_Model_Dia",
  width         = 12,
  height        = 9,
  dpi           = 600
)
