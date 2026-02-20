###############################################################################
# GCB_Model.R
# ──────────────────────────────────────────────────────────────────────────────
# Species distribution models for Pacific tuna using XGBoost (Tweedie)
#
# Workflow per species:
#   1) Trim extreme CPUE values (99.5th percentile)
#   2) VIF stepwise elimination to remove collinear covariates
#   3) Hyperparameter tuning via mlr3 grid search (5-fold CV)
#   4) Final model training with early stopping + full-data refit
#   5) Save model objects and predictions to .RData
#
# Species / stocks:
#   - Northern Pacific albacore  (ALB_N)
#   - Southern Pacific albacore  (ALB_S)
#   - Bigeye tuna                (BET)
#   - Yellowfin tuna             (YFT)
#
# Requires objects already in the workspace:
#   northern_alb_model_data, southern_alb_model_data,
#   bet_model_data, yft_model_data
#   (produced by GCB_Data.R)
###############################################################################


# =============================================================================
# 0. Packages
# =============================================================================
library(mlr3)
library(mlr3tuning)
library(mlr3learners)
library(mlr3viz)
library(paradox)
library(xgboost)
library(car)
library(ggplot2)
library(cowplot)
library(dplyr)

set.seed(123)


# =============================================================================
# 1. Project paths (auto-detect across devices)
# =============================================================================
ROOT_macmini <- "/Users/linbojun/Library/CloudStorage/OneDrive-共用文件庫－onedrive/Oceanic Fisheries Ecosystem Laboratory/ENSO_LL/GCB version/Revise"
ROOT_mac     <- "/Users/linbojun/Library/CloudStorage/OneDrive-個人/Oceanic Fisheries Ecosystem Laboratory/ENSO_LL/GCB version/Revise"
ROOT_pc      <- "D:/OneDrive/Oceanic Fisheries Ecosystem Laboratory/ENSO_LL/GCB version/Revise"

pick_root <- function(...) {
  roots <- c(...)
  ok    <- roots[file.exists(roots)]
  if (length(ok) == 0) stop("None of the project roots exist. Check ROOT_macmini / ROOT_mac / ROOT_pc.")
  ok[1]
}

PROJECT_ROOT <- pick_root(ROOT_macmini, ROOT_mac, ROOT_pc)
PATH_RDATA   <- file.path(PROJECT_ROOT, "RData")

message("Using PROJECT_ROOT = ", PROJECT_ROOT)


# =============================================================================
# 2. Helper functions
# =============================================================================

# -- Load a single-object .RData file and return it --
load_one <- function(path) {
  nm <- load(path)
  if (length(nm) != 1) return(invisible(nm))
  get(nm)
}

# -- Quick boxplot check --
plot_box <- function(x, main = NULL) {
  graphics::boxplot(x, main = main)
}

# -- Trim rows above a given quantile --
trim_by_quantile <- function(df, col, p = 0.995, keep_na = TRUE, verbose = TRUE) {
  stopifnot(col %in% names(df))
  x   <- df[[col]]
  thr <- as.numeric(stats::quantile(x, probs = p, na.rm = TRUE))

  n_before <- nrow(df)
  if (keep_na) {
    df2 <- df[is.na(x) | x <= thr, , drop = FALSE]
  } else {
    df2 <- df[x <= thr, , drop = FALSE]
  }
  n_after <- nrow(df2)

  if (verbose) {
    cat(sprintf("[trim_by_quantile] %s: p=%.3f thr=%.4f | removed=%d/%d (%.2f%%)\n",
                col, p, thr, n_before - n_after, n_before,
                100 * (n_before - n_after) / n_before))
  }
  attr(df2, "trim_threshold") <- thr
  df2
}

# -- Ensure directory exists --
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

# -- Save named objects to .RData with existence check --
save_rdata <- function(obj_names, file, envir = parent.frame()) {
  ensure_dir(dirname(file))
  missing <- obj_names[!vapply(obj_names, exists, logical(1),
                               envir = envir, inherits = FALSE)]
  if (length(missing) > 0) {
    stop("save_rdata: missing object(s): ", paste(missing, collapse = ", "),
         "\nFile: ", file)
  }
  save(list = obj_names, file = file, envir = envir)
  message("Saved: ", file)
}


# =============================================================================
# 3. VIF stepwise elimination
# =============================================================================
vif_stepwise <- function(df,
                         target,
                         keep_always   = c("Year", "Month", "Lon", "Lat"),
                         vif_threshold = 10,
                         verbose       = TRUE) {

  stopifnot(target %in% names(df))
  stopifnot(all(keep_always %in% names(df)))

  # Numeric predictors only (lm + vif requires numeric input)
  x0        <- setdiff(names(df), target)
  numeric_x <- x0[sapply(df[, x0, drop = FALSE], is.numeric)]
  x         <- unique(c(intersect(keep_always, numeric_x),
                        setdiff(numeric_x, keep_always)))

  if (length(setdiff(x, keep_always)) < 1) {
    return(list(
      keep = c(target, x),
      log  = tibble(step = 0, dropped = NA_character_, max_vif = NA_real_)
    ))
  }

  log_tbl <- tibble(step = integer(), dropped = character(), max_vif = double())
  step    <- 0

  repeat {
    step <- step + 1
    form <- as.formula(paste(target, "~", paste(x, collapse = " + ")))

    # Handle potential singularities
    fit <- try(lm(form, data = df), silent = TRUE)
    if (inherits(fit, "try-error")) {
      if (verbose) message("lm failed (singular). Stopping VIF stepwise.")
      break
    }

    v <- try(car::vif(fit), silent = TRUE)
    if (inherits(v, "try-error")) {
      if (verbose) message("vif() failed. Stopping VIF stepwise.")
      break
    }

    v     <- as.numeric(v)
    names(v) <- names(car::vif(fit))
    max_v <- max(v, na.rm = TRUE)

    # Converged: all VIFs below threshold
    if (!is.finite(max_v) || max_v <= vif_threshold) {
      log_tbl <- bind_rows(log_tbl, tibble(step = step, dropped = NA_character_, max_vif = max_v))
      break
    }

    # Drop the non-protected variable with the highest VIF
    cand <- setdiff(names(sort(v, decreasing = TRUE)), keep_always)
    if (length(cand) == 0) {
      log_tbl <- bind_rows(log_tbl, tibble(step = step, dropped = NA_character_, max_vif = max_v))
      break
    }
    drop_var <- cand[1]
    x <- setdiff(x, drop_var)

    log_tbl <- bind_rows(log_tbl, tibble(step = step, dropped = drop_var, max_vif = max_v))
    if (verbose) message("VIF step ", step, ": drop ", drop_var, " (max VIF = ", round(max_v, 2), ")")

    # Only protected variables remain
    if (length(setdiff(x, keep_always)) < 1) break
  }

  list(keep = c(target, x), log = log_tbl)
}

# -- Build initial covariate set, run VIF, return filtered data.frame --
build_task_with_vif <- function(model_df,
                                target,
                                initial_vars,
                                keep_always   = c("Year", "Month", "Lon", "Lat"),
                                vif_threshold = 10,
                                verbose       = TRUE) {
  df0 <- model_df[, initial_vars, drop = FALSE]
  res <- vif_stepwise(df0, target = target, keep_always = keep_always,
                      vif_threshold = vif_threshold, verbose = verbose)
  df_final <- df0[, res$keep, drop = FALSE]
  list(df = df_final, vif_log = res$log, keep = res$keep)
}


# =============================================================================
# 4. mlr3 tuning wrapper (grid search, 5-fold CV)
# =============================================================================
tune_xgb_tweedie_mlr3 <- function(task, n_evals = 100, folds = 5, seed = 123) {
  set.seed(seed)

  learner <- lrn("regr.xgboost", predict_type = "response")
  learner$param_set$values$objective <- "reg:tweedie"

  search_space <- ps(
    nrounds                = p_int(50, 300),
    max_depth              = p_int(3, 9),
    eta                    = p_dbl(0.01, 0.1),
    gamma                  = p_dbl(0, 0.2),
    colsample_bytree       = p_dbl(0.5, 0.9),
    min_child_weight       = p_int(1, 5),
    subsample              = p_dbl(0.5, 0.9),
    tweedie_variance_power = p_dbl(1.3, 1.7)
  )

  resampling <- rsmp("cv", folds = folds)

  inst <- TuningInstanceBatchSingleCrit$new(
    task         = task,
    learner      = learner,
    resampling   = resampling,
    measure      = msr("regr.rmse"),
    search_space = search_space,
    terminator   = trm("evals", n_evals = n_evals)
  )

  tuner <- tnr("grid_search")
  res   <- tuner$optimize(inst)

  list(instance = inst, result = res)
}


# =============================================================================
# 5. Final XGBoost training (early stopping + full-data refit)
# =============================================================================
train_xgb_tweedie_final <- function(df,
                                    target,
                                    best_params,
                                    train_frac            = 0.8,
                                    early_stopping_rounds = 10,
                                    seed                  = 123) {
  set.seed(seed)

  x_cols <- setdiff(names(df), target)

  idx      <- sample(nrow(df), size = floor(train_frac * nrow(df)))
  train_df <- df[idx, , drop = FALSE]
  valid_df <- df[-idx, , drop = FALSE]

  dtrain <- xgb.DMatrix(data  = as.matrix(train_df[, x_cols, drop = FALSE]),
                        label = train_df[[target]])
  dvalid <- xgb.DMatrix(data  = as.matrix(valid_df[, x_cols, drop = FALSE]),
                        label = valid_df[[target]])

  rho <- best_params$tweedie_variance_power

  params <- list(
    booster                = "gbtree",
    objective              = "reg:tweedie",
    tweedie_variance_power = rho,
    eval_metric            = paste0("tweedie-nloglik@", rho),
    eta                    = best_params$eta,
    max_depth              = best_params$max_depth,
    subsample              = best_params$subsample,
    colsample_bytree       = best_params$colsample_bytree,
    gamma                  = best_params$gamma,
    min_child_weight       = best_params$min_child_weight
  )

  nrounds <- best_params$nrounds

  # Train with early stopping on validation set
  model_es <- xgb.train(
    params                = params,
    data                  = dtrain,
    nrounds               = nrounds,
    evals                 = list(train = dtrain, valid = dvalid),
    early_stopping_rounds = early_stopping_rounds,
    verbose               = 0
  )

  best_iter <- xgb.attr(model_es, "best_iteration")
  if (is.null(best_iter)) best_iter <- nrounds
  best_iter <- as.integer(best_iter)

  # Refit on the full dataset using best iteration count
  dfull <- xgb.DMatrix(data  = as.matrix(df[, x_cols, drop = FALSE]),
                       label = df[[target]])

  model_final <- xgb.train(
    params  = params,
    data    = dfull,
    nrounds = best_iter,
    evals   = list(full = dfull),
    verbose = 0
  )

  preds <- predict(model_final, dfull)

  list(
    model_es    = model_es,
    model_final = model_final,
    best_iter   = best_iter,
    preds       = preds,
    params      = params,
    x_cols      = x_cols
  )
}


# =============================================================================
# 6. Northern Pacific albacore (ALB_N)
# =============================================================================
plot_box(northern_alb_model_data$nCPUE_ALB, "ALB_N nCPUE_ALB (raw)")

northern_alb_model_data <- trim_by_quantile(
  df      = northern_alb_model_data,
  col     = "nCPUE_ALB",
  p       = 0.995,
  keep_na = TRUE,
  verbose = TRUE
)

plot_box(northern_alb_model_data$nCPUE_ALB, "ALB_N nCPUE_ALB (trimmed)")

# -- VIF selection --
alb_n_initial_vars <- c(
  "nCPUE_ALB",
  "Year", "Month", "Lon", "Lat",
  "Temp_0", "Temp_47", "Temp_155", "Temp_266",
  "Sali_92", "Sali_155", "Sali_186", "Sali_266",
  "MLD", "Chl_0", "Chl_47",
  "O2_0", "O2_47", "O2_97", "O2_199", "O2_244", "O2_300",
  "Nppv_0"
)

res_vif_alb_n <- build_task_with_vif(
  model_df      = northern_alb_model_data,
  target        = "nCPUE_ALB",
  initial_vars  = alb_n_initial_vars,
  keep_always   = c("Year", "Month", "Lon", "Lat"),
  vif_threshold = 10,
  verbose       = TRUE
)

northern_data_task_ALB <- res_vif_alb_n$df %>% select(nCPUE_ALB, everything())
vif_log_alb_n          <- res_vif_alb_n$vif_log
print(vif_log_alb_n)

# -- Hyperparameter tuning --
task_alb_northern <- TaskRegr$new(
  id      = "alb_task_northern",
  backend = northern_data_task_ALB,
  target  = "nCPUE_ALB"
)

tuned_alb_n           <- tune_xgb_tweedie_mlr3(task_alb_northern, n_evals = 100, folds = 5, seed = 123)
instance_alb_northern <- tuned_alb_n$instance
print(tuned_alb_n$result)
autoplot(instance_alb_northern, theme = theme_bw())

# -- Final model --
best1_alb_northern <- instance_alb_northern$result$learner_param_vals[[1]]

fit_alb_n <- train_xgb_tweedie_final(
  df                    = northern_data_task_ALB,
  target                = "nCPUE_ALB",
  best_params           = best1_alb_northern,
  train_frac            = 0.8,
  early_stopping_rounds = 10,
  seed                  = 123
)

predictions_alb_northern        <- fit_alb_n$preds
northern_alb_model_data$pre_ALB <- predictions_alb_northern
plot_box(predictions_alb_northern, "ALB_N predictions (final)")

# -- Unpack & save (ALB_N) --
northern_ALB_xgb_model       <- fit_alb_n$model_es
northern_ALB_xgb_model_final <- fit_alb_n$model_final
predictions_alb_northern     <- fit_alb_n$preds
best_iter_alb_northern       <- fit_alb_n$best_iter
params_alb_northern          <- fit_alb_n$params
x_cols_alb_northern          <- fit_alb_n$x_cols

save_rdata("northern_ALB_xgb_model",
           file.path(PATH_RDATA, "northern_ALB_xgb_model.RData"))
save_rdata("northern_ALB_xgb_model_final",
           file.path(PATH_RDATA, "northern_ALB_xgb_model_final.RData"))
save_rdata("northern_alb_model_data",
           file.path(PATH_RDATA, "northern_alb_model_data_pre.RData"))
save_rdata("instance_alb_northern",
           file.path(PATH_RDATA, "instance_alb_northern.RData"))
save_rdata(c("predictions_alb_northern", "best_iter_alb_northern",
             "params_alb_northern", "x_cols_alb_northern"),
           file.path(PATH_RDATA, "northern_ALB_xgb_fitmeta.RData"))
if (exists("vif_log_alb_n", inherits = FALSE)) {
  save_rdata("vif_log_alb_n", file.path(PATH_RDATA, "vif_log_alb_n.RData"))
}


# =============================================================================
# 7. Southern Pacific albacore (ALB_S)
# =============================================================================
plot_box(southern_alb_model_data$nCPUE_ALB, "ALB_S nCPUE_ALB (raw)")

southern_alb_model_data <- trim_by_quantile(
  df      = southern_alb_model_data,
  col     = "nCPUE_ALB",
  p       = 0.995,
  keep_na = TRUE,
  verbose = TRUE
)

plot_box(southern_alb_model_data$nCPUE_ALB, "ALB_S nCPUE_ALB (trimmed)")

# -- VIF selection --
alb_s_initial_vars <- c(
  "nCPUE_ALB",
  "Year", "Month", "Lon", "Lat",
  "Temp_0", "Temp_266",
  "Sali_47", "Sali_266",
  "SSH", "MLD", "Chl_0",
  "O2_47", "O2_300",
  "Nppv_47"
)

res_vif_alb_s <- build_task_with_vif(
  model_df      = southern_alb_model_data,
  target        = "nCPUE_ALB",
  initial_vars  = alb_s_initial_vars,
  keep_always   = c("Year", "Month", "Lon", "Lat"),
  vif_threshold = 5,
  verbose       = TRUE
)

southern_data_task_ALB <- res_vif_alb_s$df %>% select(nCPUE_ALB, everything())
vif_log_alb_s          <- res_vif_alb_s$vif_log
print(vif_log_alb_s)

# -- Hyperparameter tuning --
task_alb_southern <- TaskRegr$new(
  id      = "alb_task_southern",
  backend = southern_data_task_ALB,
  target  = "nCPUE_ALB"
)

tuned_alb_s           <- tune_xgb_tweedie_mlr3(task_alb_southern, n_evals = 100, folds = 5, seed = 123)
instance_alb_southern <- tuned_alb_s$instance
print(tuned_alb_s$result)
autoplot(instance_alb_southern, theme = theme_bw())

# -- Final model --
best1_alb_southern <- instance_alb_southern$result$learner_param_vals[[1]]

fit_alb_s <- train_xgb_tweedie_final(
  df                    = southern_data_task_ALB,
  target                = "nCPUE_ALB",
  best_params           = best1_alb_southern,
  train_frac            = 0.8,
  early_stopping_rounds = 10,
  seed                  = 123
)

predictions_alb_southern        <- fit_alb_s$preds
southern_alb_model_data$pre_ALB <- predictions_alb_southern
plot_box(predictions_alb_southern, "ALB_S predictions (final)")

# -- Unpack & save (ALB_S) --
southern_ALB_xgb_model       <- fit_alb_s$model_es
southern_ALB_xgb_model_final <- fit_alb_s$model_final
predictions_alb_southern     <- fit_alb_s$preds
best_iter_alb_southern       <- fit_alb_s$best_iter
params_alb_southern          <- fit_alb_s$params
x_cols_alb_southern          <- fit_alb_s$x_cols

save_rdata("southern_ALB_xgb_model",
           file.path(PATH_RDATA, "southern_ALB_xgb_model.RData"))
save_rdata("southern_ALB_xgb_model_final",
           file.path(PATH_RDATA, "southern_ALB_xgb_model_final.RData"))
save_rdata("southern_alb_model_data",
           file.path(PATH_RDATA, "southern_alb_model_data_pre.RData"))
save_rdata("instance_alb_southern",
           file.path(PATH_RDATA, "instance_alb_southern.RData"))
save_rdata(c("predictions_alb_southern", "best_iter_alb_southern",
             "params_alb_southern", "x_cols_alb_southern"),
           file.path(PATH_RDATA, "southern_ALB_xgb_fitmeta.RData"))
if (exists("vif_log_alb_s", inherits = FALSE)) {
  save_rdata("vif_log_alb_s", file.path(PATH_RDATA, "vif_log_alb_s.RData"))
} else {
  message("Note: vif_log_alb_s not found; skipped saving.")
}


# =============================================================================
# 8. Bigeye tuna (BET)
# =============================================================================
plot_box(bet_model_data$nCPUE_BET, "BET nCPUE_BET (raw)")

bet_model_data <- trim_by_quantile(
  df      = bet_model_data,
  col     = "nCPUE_BET",
  p       = 0.995,
  keep_na = TRUE,
  verbose = TRUE
)

plot_box(bet_model_data$nCPUE_BET, "BET nCPUE_BET (trimmed)")

# -- VIF selection --
bet_initial_vars <- c(
  "nCPUE_BET",
  "Year", "Month", "Lon", "Lat",
  "Sali_186",
  "Chl_47",
  "O2_47", "O2_147", "O2_300", "O2_457",
  "Nppv_0"
)

res_vif_bet <- build_task_with_vif(
  model_df      = bet_model_data,
  target        = "nCPUE_BET",
  initial_vars  = bet_initial_vars,
  keep_always   = c("Year", "Month", "Lon", "Lat"),
  vif_threshold = 5,
  verbose       = TRUE
)

pacific_data_task_BET <- res_vif_bet$df %>% select(nCPUE_BET, everything())
vif_log_bet           <- res_vif_bet$vif_log
print(vif_log_bet)

# -- Hyperparameter tuning --
task_bet <- TaskRegr$new(
  id      = "bet_task",
  backend = pacific_data_task_BET,
  target  = "nCPUE_BET"
)

tuned_bet    <- tune_xgb_tweedie_mlr3(task_bet, n_evals = 100, folds = 5, seed = 123)
instance_bet <- tuned_bet$instance
print(tuned_bet$result)
autoplot(instance_bet, theme = theme_bw())

# -- Final model --
best1_bet <- instance_bet$result$learner_param_vals[[1]]

fit_bet <- train_xgb_tweedie_final(
  df                    = pacific_data_task_BET,
  target                = "nCPUE_BET",
  best_params           = best1_bet,
  train_frac            = 0.8,
  early_stopping_rounds = 10,
  seed                  = 123
)

predictions_bet          <- fit_bet$preds
bet_model_data$pre_BET   <- predictions_bet
bet_model_data           <- subset(bet_model_data, pre_BET < 15)
plot_box(bet_model_data$pre_BET, "BET predictions (final)")

# -- Unpack & save (BET) --
pacific_BET_xgb_model       <- fit_bet$model_es
pacific_BET_xgb_model_final <- fit_bet$model_final
predictions_bet             <- fit_bet$preds
best_iter_bet               <- fit_bet$best_iter
params_bet                  <- fit_bet$params
x_cols_bet                  <- fit_bet$x_cols

save_rdata("pacific_BET_xgb_model",
           file.path(PATH_RDATA, "pacific_BET_xgb_model.RData"))
save_rdata("pacific_BET_xgb_model_final",
           file.path(PATH_RDATA, "pacific_BET_xgb_model_final.RData"))
save_rdata("bet_model_data",
           file.path(PATH_RDATA, "bet_model_data_pre.RData"))
save_rdata("instance_bet",
           file.path(PATH_RDATA, "instance_bet.RData"))
save_rdata(c("predictions_bet", "best_iter_bet", "params_bet", "x_cols_bet"),
           file.path(PATH_RDATA, "pacific_BET_xgb_fitmeta.RData"))
if (exists("vif_log_bet", inherits = FALSE)) {
  save_rdata("vif_log_bet", file.path(PATH_RDATA, "vif_log_bet.RData"))
}


# =============================================================================
# 9. Yellowfin tuna (YFT)
# =============================================================================
plot_box(yft_model_data$nCPUE_YFT, "YFT nCPUE_YFT (raw)")

yft_model_data <- trim_by_quantile(
  df      = yft_model_data,
  col     = "nCPUE_YFT",
  p       = 0.995,
  keep_na = TRUE,
  verbose = TRUE
)

plot_box(yft_model_data$nCPUE_YFT, "YFT nCPUE_YFT (trimmed)")

# -- VIF selection --
yft_initial_vars <- c(
  "nCPUE_YFT",
  "Year", "Month", "Lon", "Lat",
  "Temp_47", "Temp_92", "Temp_155",
  "Sali_47", "Sali_186",
  "SSH", "MLD",
  "Chl_47",
  "O2_0", "O2_47", "O2_97", "O2_147", "O2_199",
  "Nppv_0", "Nppv_97"
)

res_vif_yft <- build_task_with_vif(
  model_df      = yft_model_data,
  target        = "nCPUE_YFT",
  initial_vars  = yft_initial_vars,
  keep_always   = c("Year", "Month", "Lon", "Lat"),
  vif_threshold = 5,
  verbose       = TRUE
)

pacific_data_task_YFT <- res_vif_yft$df %>% select(nCPUE_YFT, everything())
vif_log_yft           <- res_vif_yft$vif_log
print(vif_log_yft)

# -- Hyperparameter tuning --
task_yft <- TaskRegr$new(
  id      = "yft_task",
  backend = pacific_data_task_YFT,
  target  = "nCPUE_YFT"
)

tuned_yft    <- tune_xgb_tweedie_mlr3(task_yft, n_evals = 100, folds = 5, seed = 123)
instance_yft <- tuned_yft$instance
print(tuned_yft$result)
autoplot(instance_yft, theme = theme_bw())

# -- Final model --
best1_yft <- instance_yft$result$learner_param_vals[[1]]

fit_yft <- train_xgb_tweedie_final(
  df                    = pacific_data_task_YFT,
  target                = "nCPUE_YFT",
  best_params           = best1_yft,
  train_frac            = 0.8,
  early_stopping_rounds = 10,
  seed                  = 123
)

predictions_yft          <- fit_yft$preds
yft_model_data$pre_YFT   <- predictions_yft
plot_box(predictions_yft, "YFT predictions (final)")

# -- Unpack & save (YFT) --
pacific_YFT_xgb_model       <- fit_yft$model_es
pacific_YFT_xgb_model_final <- fit_yft$model_final
predictions_yft             <- fit_yft$preds
best_iter_yft               <- fit_yft$best_iter
params_yft                  <- fit_yft$params
x_cols_yft                  <- fit_yft$x_cols

save_rdata("pacific_YFT_xgb_model",
           file.path(PATH_RDATA, "pacific_YFT_xgb_model.RData"))
save_rdata("pacific_YFT_xgb_model_final",
           file.path(PATH_RDATA, "pacific_YFT_xgb_model_final.RData"))
save_rdata("yft_model_data",
           file.path(PATH_RDATA, "yft_model_data_pre.RData"))
save_rdata("instance_yft",
           file.path(PATH_RDATA, "instance_yft.RData"))
save_rdata(c("predictions_yft", "best_iter_yft", "params_yft", "x_cols_yft"),
           file.path(PATH_RDATA, "pacific_YFT_xgb_fitmeta.RData"))
if (exists("vif_log_yft", inherits = FALSE)) {
  save_rdata("vif_log_yft", file.path(PATH_RDATA, "vif_log_yft.RData"))
}
