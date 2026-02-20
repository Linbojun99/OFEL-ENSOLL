###############################################################################
# GCB_SHAP.R
# ──────────────────────────────────────────────────────────────────────────────
# SHAP plotting pipeline for XGBoost Tweedie species distribution models.
#
# Workflow:
#   1) Compute SHAP values via predict(predcontrib = TRUE)
#   2) Force feature alignment using x_cols_* (training column order)
#   3) Retain environmental variables only (drop Year/Month/Lon/Lat)
#   4) Plot: importance bar (top k) + dependence plots per species
#   5) Combine 4 species into a single figure
#
# Output:
#   - Fig_shap : combined 4-column panel (ALB_N, ALB_S, BET, YFT)
#
# Requires objects already in the workspace:
#   fit_alb_n, fit_alb_s, fit_bet, fit_yft   (each with $model_final)
#   northern_data_task_ALB, southern_data_task_ALB,
#   pacific_data_task_BET, pacific_data_task_YFT
#   x_cols_alb_northern, x_cols_alb_southern, x_cols_bet, x_cols_yft
#   save_plot_all()
###############################################################################


# =============================================================================
# 0. Packages
# =============================================================================
library(dplyr)
library(tibble)
library(purrr)
library(ggplot2)
library(patchwork)
library(mgcv)
library(shapviz)


# =============================================================================
# 1. Helpers & theme
# =============================================================================
`%||%` <- function(a, b) if (!is.null(a)) a else b

species_colors <- c(
  ALB_N = "#4B3F8C",
  ALB_S = "#00A6D6",
  BET   = "#1B9E77",
  YFT   = "#D95F02"
)

theme_shap <- function(base_size = 9, base_family = "Helvetica") {
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


# =============================================================================
# 2. Unit map for axis labels and transforms
# =============================================================================

# -- Label expression builders (avoid repeating identical entries) --
lab_oxygen  <- expression(bold(Oxygen ~ "(mmol/m"^3 * ")"))
lab_temp    <- expression(bold(Temperature ~ "(" * degree * "C)"))
lab_chl     <- expression("Chlorophyll (mg/m" ^ 3 * ")")
lab_nppv    <- expression(bold(NPPv ~ "(mg m" ^ {-3} * " day" ^ {-1} * ")"))

unit_map <- c(
  # Oxygen (mmol/m^3)
  setNames(
    lapply(c("O2_0", "O2_47", "O2_97", "O2_147", "O2_199",
             "O2_244", "O2_300", "O2_333", "O2_411", "O2_457"),
           function(v) list(transform = identity, xlab = lab_oxygen)),
    c("O2_0", "O2_47", "O2_97", "O2_147", "O2_199",
      "O2_244", "O2_300", "O2_333", "O2_411", "O2_457")
  ),
  # Temperature (deg C)
  setNames(
    lapply(c("Temp_0", "Temp_47", "Temp_92", "Temp_155", "Temp_186",
             "Temp_266", "Temp_318", "Temp_380", "Temp_453"),
           function(v) list(transform = identity, xlab = lab_temp)),
    c("Temp_0", "Temp_47", "Temp_92", "Temp_155", "Temp_186",
      "Temp_266", "Temp_318", "Temp_380", "Temp_453")
  ),
  # Salinity (psu)
  setNames(
    lapply(c("Sali_0", "Sali_47", "Sali_92", "Sali_155", "Sali_186",
             "Sali_266", "Sali_318", "Sali_380", "Sali_453"),
           function(v) list(transform = identity, xlab = "Salinity (psu)")),
    c("Sali_0", "Sali_47", "Sali_92", "Sali_155", "Sali_186",
      "Sali_266", "Sali_318", "Sali_380", "Sali_453")
  ),
  # Chlorophyll (mg/m^3)
  list(
    Chl_0  = list(transform = identity, xlab = lab_chl),
    Chl_47 = list(transform = identity, xlab = lab_chl)
  ),
  # SSH / MLD
  list(
    SSH = list(transform = identity, xlab = "Sea surface height (m)"),
    MLD = list(transform = identity, xlab = "Mixed layer depth (m)")
  ),
  # NPPv
  list(
    Nppv_0  = list(transform = identity, xlab = lab_nppv),
    Nppv_47 = list(transform = identity, xlab = lab_nppv),
    Nppv_97 = list(transform = identity, xlab = lab_nppv)
  )
)

get_xlab <- function(var) {
  if (!is.null(unit_map[[var]]$xlab)) return(unit_map[[var]]$xlab)
  var
}

transform_x <- function(var, x) {
  if (!is.null(unit_map[[var]]$transform)) return(unit_map[[var]]$transform(x))
  x
}


# =============================================================================
# 3. Environmental variable filter
# =============================================================================
is_env_var <- function(v) {
  grepl("^(Temp|Sali|O2|Chl|Nppv)_", v) | v %in% c("SSH", "MLD")
}


# =============================================================================
# 4. SHAP computation (aligned by x_cols training order)
# =============================================================================
compute_shap_one <- function(model_final, train_df, target, x_cols,
                             env_only = TRUE, n_sample = NULL, seed = 1) {
  stopifnot(target %in% names(train_df))
  stopifnot(all(x_cols %in% names(train_df)))

  df <- train_df
  if (!is.null(n_sample) && nrow(df) > n_sample) {
    set.seed(seed)
    df <- df[sample.int(nrow(df), n_sample), , drop = FALSE]
  }

  X_mat <- as.matrix(df[, x_cols, drop = FALSE])
  storage.mode(X_mat) <- "double"
  colnames(X_mat) <- x_cols

  shap_all <- predict(model_final, newdata = X_mat,
                      predcontrib = TRUE, approxcontrib = FALSE)

  p     <- length(x_cols)
  S_mat <- shap_all[, seq_len(p), drop = FALSE]
  colnames(S_mat) <- x_cols

  if (env_only) {
    env_vars <- x_cols[is_env_var(x_cols)]
    X_mat <- X_mat[, env_vars, drop = FALSE]
    S_mat <- S_mat[, env_vars, drop = FALSE]
  }

  sv <- shapviz::shapviz(S_mat, X = X_mat)
  list(sv = sv, X = X_mat, S = S_mat)
}


# =============================================================================
# 5. Importance bar chart (mean |SHAP| -> relative %)
# =============================================================================
plot_importance_env <- function(S_mat, fill_color = "grey40",
                                top_k = 5, title = NULL) {
  imp_all <- tibble(
    var        = colnames(S_mat),
    importance = colMeans(abs(S_mat), na.rm = TRUE)
  ) %>%
    arrange(desc(importance))

  imp_top <- imp_all %>%
    slice_head(n = top_k) %>%
    mutate(pct = importance / sum(importance) * 100)

  p <- ggplot(imp_top, aes(x = reorder(var, pct), y = pct)) +
    geom_col(fill = fill_color) +
    coord_flip() +
    labs(x = NULL, y = "Relative importance (%)", title = title) +
    theme_shap()

  list(plot = p, imp = imp_top)
}


# =============================================================================
# 6. Dependence plot for one variable
# =============================================================================
plot_dependence_one <- function(S_mat, X_mat, var, line_color = "grey20",
                                y_lim_fixed = c(-0.5, 0.5),
                                k_max = 20, loess_span = 0.7) {
  stopifnot(var %in% colnames(X_mat), var %in% colnames(S_mat))

  df <- tibble(
    X = transform_x(var, X_mat[, var]),
    Y = as.numeric(S_mat[, var])
  ) %>%
    filter(is.finite(X), is.finite(Y))

  n_uniq  <- length(unique(df$X))
  use_gam <- n_uniq >= 25
  k_use   <- min(k_max, max(5, n_uniq - 1))

  p <- ggplot(df, aes(x = X, y = Y)) +
    geom_point(size = 1.2, alpha = 0.25, color = "grey55") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = get_xlab(var), y = NULL) +
    theme_shap() +
    scale_y_continuous(limits = y_lim_fixed) +
    annotate("text", x = Inf, y = Inf, label = var,
             hjust = 1.08, vjust = 1.08, size = 3.5)

  if (use_gam) {
    p <- p + geom_smooth(
      method = "gam",
      formula = y ~ s(x, bs = "cs", k = k_use),
      se = FALSE, linewidth = 2.2, color = line_color
    )
  } else {
    p <- p + geom_smooth(
      method = "loess", span = loess_span,
      se = FALSE, linewidth = 2.2, color = line_color
    )
  }

  p
}


# =============================================================================
# 7. Single-species panel (importance + top_k dependence)
# =============================================================================
make_panel_one <- function(model_final, train_df, target, x_cols,
                           species     = "BET",
                           color       = "#1B7A3A",
                           top_k       = 5,
                           y_lim_fixed = c(-0.5, 0.5),
                           n_sample    = 5000,
                           seed        = 1) {

  shap_res <- compute_shap_one(
    model_final = model_final, train_df = train_df,
    target = target, x_cols = x_cols,
    env_only = TRUE, n_sample = n_sample, seed = seed
  )

  S_mat <- shap_res$S
  X_mat <- shap_res$X

  imp_res  <- plot_importance_env(S_mat, fill_color = color,
                                  top_k = top_k, title = species)
  top_vars <- imp_res$imp$var

  dep_plots <- map(top_vars, \(v) {
    plot_dependence_one(S_mat, X_mat, v,
                        line_color = color, y_lim_fixed = y_lim_fixed)
  })

  wrap_plots(c(list(imp_res$plot), dep_plots), ncol = 1)
}


# =============================================================================
# 8. Run: build panels for all species
# =============================================================================
panel_ALB_N <- make_panel_one(
  model_final = fit_alb_n$model_final,
  train_df    = northern_data_task_ALB,
  target      = "nCPUE_ALB",
  x_cols      = x_cols_alb_northern,
  species     = "ALB_N",
  color       = species_colors["ALB_N"],
  top_k       = 5,
  y_lim_fixed = c(-0.5, 0.5),
  n_sample    = 5000,
  seed        = 1
)

panel_ALB_S <- make_panel_one(
  model_final = fit_alb_s$model_final,
  train_df    = southern_data_task_ALB,
  target      = "nCPUE_ALB",
  x_cols      = x_cols_alb_southern,
  species     = "ALB_S",
  color       = species_colors["ALB_S"],
  top_k       = 5,
  y_lim_fixed = c(-0.5, 0.5),
  n_sample    = 5000,
  seed        = 1
)

panel_BET <- make_panel_one(
  model_final = fit_bet$model_final,
  train_df    = pacific_data_task_BET,
  target      = "nCPUE_BET",
  x_cols      = x_cols_bet,
  species     = "BET",
  color       = species_colors["BET"],
  top_k       = 5,
  y_lim_fixed = c(-0.5, 0.5),
  n_sample    = 5000,
  seed        = 1
)

panel_YFT <- make_panel_one(
  model_final = fit_yft$model_final,
  train_df    = pacific_data_task_YFT,
  target      = "nCPUE_YFT",
  x_cols      = x_cols_yft,
  species     = "YFT",
  color       = species_colors["YFT"],
  top_k       = 5,
  y_lim_fixed = c(-0.5, 0.5),
  n_sample    = 5000,
  seed        = 1
)


# =============================================================================
# 9. Combine and save  -->  Fig_shap
# =============================================================================
combined_shap <- wrap_plots(
  panel_ALB_N, panel_ALB_S, panel_BET, panel_YFT,
  nrow = 1
) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 14))

combined_shap

save_plot_all(
  p             = combined_shap,
  filename_base = "Fig_shap",
  width         = 12,
  height        = 10,
  dpi           = 600
)
