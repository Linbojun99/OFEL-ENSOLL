###############################################################################
# GCB_RA.R
# ──────────────────────────────────────────────────────────────────────────────
# MEI vs Relative Abundance — Analysis Module (no plots)
#
# Workflow:
#   A) Utility functions
#   B) Build a single monthly (Year-Month) table of relative abundance +
#      climate indices (MEI / ONI)
#   C) Correlation: Pearson / Spearman (per Tuna)
#   D) Linear vs GAM comparison (AIC + p-values)
#   E) Lag screening: MEI leading abundance by k months
#   F) ENSO phase contrasts vs Neutral:
#        - GLS with AR(1) autocorrelation (per Tuna)
#        - Relative change (%) vs Neutral with model-based 95% CI
#
# Requires objects already in the workspace:
#   northern_alb_model_data  (with predicted column "pre_ALB")
#   southern_alb_model_data  (with predicted column "pre_ALB")
#   bet_model_data           (with predicted column "pre_BET")
#   yft_model_data           (with predicted column "pre_YFT")
#
# Optional columns: MEI, ONI, ENSO_Phase3
#   ENSO_Phase3 levels: c("La Nina", "Neutral", "El Nino")
###############################################################################


# =============================================================================
# 0. Packages
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(mgcv)
  library(nlme)
})


# =============================================================================
# A. Utility functions
# =============================================================================

# -- Year-Month to Date (first day of month) --
make_time <- function(year, month) {
  as.Date(sprintf("%04d-%02d-01", as.integer(year), as.integer(month)))
}

# -- p-value formatter --
fp_eq <- function(p, digits = 3, eps = 1e-3) {
  p <- suppressWarnings(as.numeric(p))
  if (is.na(p)) return("p = NA")
  if (p < eps)  return("p < 0.001")
  paste0("p = ", formatC(p, format = "f", digits = digits))
}

# -- Correlation test wrapper --
cor_test_simple <- function(x, y, method = "pearson", min_n = 5) {
  ok <- is.finite(x) & is.finite(y)
  x  <- x[ok]; y <- y[ok]
  if (length(x) < min_n) {
    return(tibble(n = length(x), r = NA_real_, p = NA_real_))
  }
  ct <- suppressWarnings(stats::cor.test(x, y, method = method))
  tibble(n = length(x), r = unname(ct$estimate), p = unname(ct$p.value))
}

# -- Bootstrap relative change (%) vs Neutral + CI --
boot_rel_change <- function(x_phase, x_neu, R = 2000, conf = 0.95,
                            min_n = 5, stat_fun = mean) {
  x_phase <- x_phase[is.finite(x_phase)]
  x_neu   <- x_neu[is.finite(x_neu)]

  if (length(x_phase) < min_n || length(x_neu) < min_n) {
    return(tibble(est = NA_real_, lo = NA_real_, hi = NA_real_))
  }

  est <- (stat_fun(x_phase) - stat_fun(x_neu)) / stat_fun(x_neu) * 100

  boots <- replicate(R, {
    ph <- sample(x_phase, replace = TRUE)
    ne <- sample(x_neu,   replace = TRUE)
    (stat_fun(ph) - stat_fun(ne)) / stat_fun(ne) * 100
  })

  alpha <- (1 - conf) / 2
  qs    <- quantile(boots, probs = c(alpha, 1 - alpha), na.rm = TRUE)
  tibble(est = est, lo = unname(qs[1]), hi = unname(qs[2]))
}


# =============================================================================
# B. Build monthly dataset (single source of truth)
# =============================================================================

# -- Monthly mean series for one tuna dataset --
build_monthly_one <- function(dat, abundance_col, tuna_label) {
  stopifnot(all(c("Year", "Month", abundance_col) %in% names(dat)))

  keep_idx <- intersect(c("MEI", "ONI", "ENSO_Phase3"), names(dat))

  out <- dat %>%
    group_by(Year, Month) %>%
    summarise(
      across(all_of(setdiff(keep_idx, "ENSO_Phase3")),
             ~ mean(.x, na.rm = TRUE)),
      # ENSO_Phase3 should be constant within (Year, Month); take first non-NA
      ENSO_Phase3 = if ("ENSO_Phase3" %in% keep_idx)
        dplyr::first(na.omit(ENSO_Phase3)) else NA,
      Abundance = mean(.data[[abundance_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Time = make_time(Year, Month),
      Tuna = tuna_label
    ) %>%
    select(Year, Month, Time, Tuna,
           any_of(c("MEI", "ONI", "ENSO_Phase3")), Abundance) %>%
    arrange(Time)

  # Ensure factor levels if ENSO column is present
  if ("ENSO_Phase3" %in% names(out)) {
    out <- out %>%
      mutate(ENSO_Phase3 = factor(
        as.character(ENSO_Phase3),
        levels = c("La Niña", "Neutral", "El Niño")
      ))
  }

  out
}

# -- Combine all species --
monthly_long <- bind_rows(
  build_monthly_one(northern_alb_model_data, "pre_ALB", "ALB_N"),
  build_monthly_one(southern_alb_model_data, "pre_ALB", "ALB_S"),
  build_monthly_one(bet_model_data,          "pre_BET", "BET"),
  build_monthly_one(yft_model_data,          "pre_YFT", "YFT")
) %>%
  arrange(Tuna, Time)


# =============================================================================
# C. Correlation: index vs Abundance (per Tuna)
# =============================================================================
run_cor_by_tuna <- function(dat, index = "MEI", method = "pearson") {
  stopifnot(all(c("Tuna", "Abundance", index) %in% names(dat)))

  dat %>%
    group_by(Tuna) %>%
    group_modify(~ {
      cor_test_simple(.x$Abundance, .x[[index]], method = method) %>%
        mutate(index = index, method = method)
    }) %>%
    ungroup() %>%
    mutate(
      r_label = sprintf("r = %.2f", r),
      p_label = map_chr(p, fp_eq)
    )
}

cor_mei_pearson  <- if ("MEI" %in% names(monthly_long))
  run_cor_by_tuna(monthly_long, "MEI", "pearson")  else tibble()
cor_mei_spearman <- if ("MEI" %in% names(monthly_long))
  run_cor_by_tuna(monthly_long, "MEI", "spearman") else tibble()

cor_oni_pearson  <- if ("ONI" %in% names(monthly_long))
  run_cor_by_tuna(monthly_long, "ONI", "pearson")  else tibble()
cor_oni_spearman <- if ("ONI" %in% names(monthly_long))
  run_cor_by_tuna(monthly_long, "ONI", "spearman") else tibble()


# =============================================================================
# D. Linear vs GAM comparison (per Tuna)
# =============================================================================
fit_linear_vs_gam <- function(dat, index = "MEI", k_smooth = 5,
                              min_n = 12, gam_method = "REML") {
  stopifnot(all(c("Tuna", "Abundance", index) %in% names(dat)))

  dat %>%
    group_by(Tuna) %>%
    group_modify(~ {
      df <- .x %>%
        filter(is.finite(Abundance), is.finite(.data[[index]])) %>%
        arrange(Time)

      if (nrow(df) < min_n) {
        return(tibble(
          n = nrow(df),
          aic_lin = NA_real_, aic_gam = NA_real_, dAIC = NA_real_,
          p_lin = NA_real_, edf = NA_real_, p_smooth = NA_real_
        ))
      }

      f_lin <- as.formula(paste0("Abundance ~ ", index))
      f_gam <- as.formula(paste0("Abundance ~ s(", index, ", k = ", k_smooth, ")"))

      m_lin <- lm(f_lin, data = df)
      m_gam <- mgcv::gam(f_gam, data = df, method = gam_method)

      # Linear slope p-value
      s_lin <- summary(m_lin)$coefficients
      p_lin <- tryCatch(s_lin[index, "Pr(>|t|)"], error = function(e) NA_real_)

      # GAM smooth statistics
      s_gam <- summary(m_gam)
      edf   <- as.numeric(s_gam$s.table[1, "edf"])
      p_sm  <- as.numeric(s_gam$s.table[1, "p-value"])

      aic_lin <- AIC(m_lin)
      aic_gam <- AIC(m_gam)

      tibble(
        n        = nrow(df),
        aic_lin  = aic_lin,
        aic_gam  = aic_gam,
        dAIC     = aic_gam - aic_lin,
        p_lin    = p_lin,
        edf      = edf,
        p_smooth = p_sm
      )
    }) %>%
    ungroup() %>%
    mutate(
      prefer = case_when(
        is.na(dAIC) ~ NA_character_,
        dAIC <= -2  ~ "GAM (better)",
        dAIC >=  2  ~ "Linear (better)",
        TRUE        ~ "Similar"
      ),
      p_lin_label    = map_chr(p_lin,    fp_eq),
      p_smooth_label = map_chr(p_smooth, fp_eq),
      edf_label      = if_else(is.na(edf), NA_character_,
                               sprintf("edf = %.2f", edf))
    )
}

model_cmp_mei <- if ("MEI" %in% names(monthly_long))
  fit_linear_vs_gam(monthly_long, "MEI", k_smooth = 5) else tibble()
model_cmp_mei


# =============================================================================
# E. Lag screening (MEI leading abundance by k months)
# =============================================================================

# -- Lag-scan correlation table --
lag_scan_cor <- function(dat, index = "MEI", max_lag = 12,
                         method = "pearson", min_n = 5) {
  stopifnot(all(c("Tuna", "Time", "Abundance", index) %in% names(dat)))
  lags <- 0:max_lag

  dat %>%
    arrange(Tuna, Time) %>%
    group_by(Tuna) %>%
    group_modify(~ {
      df <- .x
      map_dfr(lags, function(k) {
        x <- df$Abundance
        y <- dplyr::lag(df[[index]], k)
        cor_test_simple(x, y, method = method, min_n = min_n) %>%
          mutate(lag_month = k)
      })
    }) %>%
    ungroup()
}

lag_tbl_mei <- lag_scan_cor(monthly_long, index = "MEI",
                            max_lag = 12, method = "pearson", min_n = 5)

# -- Best lags --
best_lag_by_absr <- lag_tbl_mei %>%
  group_by(Tuna) %>%
  slice_max(order_by = abs(r), n = 1, with_ties = FALSE) %>%
  ungroup()

best_lag_by_p <- lag_tbl_mei %>%
  group_by(Tuna) %>%
  slice_min(order_by = p, n = 1, with_ties = FALSE) %>%
  ungroup()

best_lag_by_absr
best_lag_by_p


# =============================================================================
# F. ENSO phase contrasts via GLS with AR(1)
# =============================================================================

# -- F1. Data preparation --
phase_dat <- monthly_long %>%
  filter(is.finite(Abundance), !is.na(ENSO_Phase3)) %>%
  mutate(
    ENSO_Phase3 = factor(as.character(ENSO_Phase3),
                         levels = c("Neutral", "La Niña", "El Niño")),
    Time_num = as.numeric(Time)
  ) %>%
  arrange(Tuna, Time_num) %>%
  group_by(Tuna) %>%
  mutate(trend = as.numeric(Time_num)) %>%
  ungroup()

stopifnot(all(levels(phase_dat$ENSO_Phase3) ==
              c("Neutral", "La Niña", "El Niño")))


# -- F2. GLS fitting helpers --

# AICc for gls objects
AICc_gls <- function(m) {
  k <- length(stats::coef(m))
  n <- stats::nobs(m)
  a <- stats::AIC(m)
  if (!is.finite(n) || !is.finite(k) || (n - k - 1) <= 0) return(NA_real_)
  a + (2 * k * (k + 1)) / (n - k - 1)
}

# Safe GLS fit with panel-level AR(1): ~ Time_num | Tuna
safe_gls_fit <- function(formula, data, method = "ML", use_ar1 = TRUE) {
  cor_struct <- if (use_ar1) nlme::corAR1(form = ~ Time_num | Tuna) else NULL

  m <- tryCatch(
    nlme::gls(model = formula, data = data,
              method = method, correlation = cor_struct),
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

# Candidate model formulae (auto-detected)
make_gls_candidates <- function(df) {
  has_mei   <- "MEI"   %in% names(df) && any(is.finite(df$MEI))
  has_trend <- "trend" %in% names(df)

  cand <- list(A1_Phase = Abundance ~ ENSO_Phase3)
  if (has_trend)              cand$A2_Phase_trend     <- Abundance ~ ENSO_Phase3 + trend
  if (has_mei)                cand$A3_Phase_MEI       <- Abundance ~ ENSO_Phase3 + MEI
  if (has_mei && has_trend)   cand$A4_Phase_MEI_trend <- Abundance ~ ENSO_Phase3 + MEI + trend

  cand
}

# Fit all candidates for one tuna
fit_gls_candidates_one <- function(df_one,
                                   min_n_total      = 24,
                                   min_n_each_phase = 5,
                                   use_ar1          = TRUE,
                                   method_compare   = "ML") {

  df_one <- df_one %>%
    filter(is.finite(Abundance), !is.na(ENSO_Phase3)) %>%
    mutate(ENSO_Phase3 = factor(ENSO_Phase3,
                                levels = c("Neutral", "La Niña", "El Niño")))

  n_total <- nrow(df_one)
  n_neu   <- sum(df_one$ENSO_Phase3 == "Neutral")
  n_la    <- sum(df_one$ENSO_Phase3 == "La Niña")
  n_el    <- sum(df_one$ENSO_Phase3 == "El Niño")

  if (n_total < min_n_total || min(n_neu, n_la, n_el) < min_n_each_phase) {
    return(tibble(
      model_id = NA_character_, formula = NA_character_,
      n_used = n_total,
      AIC = NA_real_, AICc = NA_real_, BIC = NA_real_, logLik = NA_real_,
      rho_ar1 = NA_real_,
      n_total = n_total, n_neutral = n_neu, n_lanina = n_la, n_elnino = n_el,
      note = "insufficient samples",
      model = list(NULL)
    ))
  }

  cand <- make_gls_candidates(df_one)

  imap_dfr(cand, function(fml, id) {
    vars_need    <- unique(all.vars(fml))
    missing_vars <- setdiff(vars_need, names(df_one))

    if (length(missing_vars) > 0) {
      return(tibble(
        model_id = id, formula = deparse(fml), n_used = n_total,
        AIC = NA_real_, AICc = NA_real_, BIC = NA_real_, logLik = NA_real_,
        rho_ar1 = NA_real_,
        n_total = n_total, n_neutral = n_neu, n_lanina = n_la, n_elnino = n_el,
        note = paste0("missing var(s): ", paste(missing_vars, collapse = ", ")),
        model = list(NULL)
      ))
    }

    dsub <- df_one %>%
      select(all_of(c(vars_need, "Tuna", "Time_num"))) %>%
      filter(stats::complete.cases(.)) %>%
      arrange(Time_num)

    fit <- safe_gls_fit(fml, dsub, method = method_compare, use_ar1 = use_ar1)

    if (!fit$ok) {
      return(tibble(
        model_id = id, formula = deparse(fml), n_used = nrow(dsub),
        AIC = NA_real_, AICc = NA_real_, BIC = NA_real_, logLik = NA_real_,
        rho_ar1 = NA_real_,
        n_total = n_total, n_neutral = n_neu, n_lanina = n_la, n_elnino = n_el,
        note = fit$note,
        model = list(NULL)
      ))
    }

    m <- fit$model
    tibble(
      model_id = id, formula = deparse(fml), n_used = stats::nobs(m),
      AIC    = stats::AIC(m),
      AICc   = AICc_gls(m),
      BIC    = stats::BIC(m),
      logLik = as.numeric(stats::logLik(m)),
      rho_ar1 = fit$rho,
      n_total = n_total, n_neutral = n_neu, n_lanina = n_la, n_elnino = n_el,
      note  = fit$note,
      model = list(m)
    )
  })
}

# Select best models by delta-AICc + Akaike weights
select_best_models <- function(gls_tbl, delta_keep = 2) {
  gls_tbl %>%
    filter(!is.na(AICc)) %>%
    group_by(Tuna) %>%
    arrange(AICc, .by_group = TRUE) %>%
    mutate(
      delta_AICc = AICc - first(AICc),
      w_AICc     = exp(-0.5 * delta_AICc) / sum(exp(-0.5 * delta_AICc))
    ) %>%
    ungroup() %>%
    filter(delta_AICc <= delta_keep)
}

# Refit best model with REML (same AR(1) structure)
safe_refit_REML <- function(formula_str, data, use_ar1 = TRUE) {
  tryCatch(
    nlme::gls(
      model       = stats::as.formula(formula_str),
      data        = data,
      method      = "REML",
      correlation = if (use_ar1) nlme::corAR1(form = ~ Time_num | Tuna) else NULL
    ),
    error = function(e) e
  )
}

# Extract inference from a fitted GLS model (1-row tibble)
extract_inference <- function(m) {
  if (inherits(m, "error") || is.null(m)) {
    return(tibble(
      mu_neutral = NA_real_,
      diff_lanina = NA_real_, p_lanina = NA_real_,
      diff_elnino = NA_real_, p_elnino = NA_real_,
      beta_mei    = NA_real_, p_mei    = NA_real_,
      beta_trend  = NA_real_, p_trend  = NA_real_,
      rho_ar1_reml = NA_real_,
      note_infer = if (inherits(m, "error"))
        paste0("REML refit failed: ", conditionMessage(m))
      else "REML refit failed"
    ))
  }

  tt <- summary(m)$tTable
  rn <- rownames(tt)

  get_term <- function(term) {
    if (term %in% rn) {
      c(as.numeric(tt[term, "Value"]), as.numeric(tt[term, "p-value"]))
    } else {
      c(NA_real_, NA_real_)
    }
  }

  mu_neu <- if ("(Intercept)" %in% rn) as.numeric(tt["(Intercept)", "Value"]) else NA_real_
  la  <- get_term("ENSO_Phase3La Niña")
  el  <- get_term("ENSO_Phase3El Niño")
  mei <- get_term("MEI")
  tr  <- get_term("trend")

  rho <- tryCatch(
    as.numeric(coef(m$modelStruct$corStruct, unconstrained = FALSE)),
    error = function(e) NA_real_
  )

  tibble(
    mu_neutral   = mu_neu,
    diff_lanina  = la[1],  p_lanina = la[2],
    diff_elnino  = el[1],  p_elnino = el[2],
    beta_mei     = mei[1], p_mei    = mei[2],
    beta_trend   = tr[1],  p_trend  = tr[2],
    rho_ar1_reml = rho,
    note_infer   = NA_character_
  )
}


# -- F3. Run: fit all candidates, select best, extract inference --
gls_all_raw <- phase_dat %>%
  group_by(Tuna) %>%
  group_modify(~ fit_gls_candidates_one(
    mutate(.x, Tuna = .y$Tuna),
    min_n_total      = 24,
    min_n_each_phase = 5,
    use_ar1          = TRUE,
    method_compare   = "ML"
  )) %>%
  ungroup()

gls_all_raw

gls_best_set <- select_best_models(gls_all_raw, delta_keep = 2)

gls_best_1 <- gls_all_raw %>%
  filter(!is.na(AICc)) %>%
  group_by(Tuna) %>%
  slice_min(order_by = AICc, n = 1, with_ties = FALSE) %>%
  ungroup()

gls_phase_infer <- gls_best_1 %>%
  mutate(
    df_one     = map(Tuna, ~ phase_dat %>% filter(Tuna == .x)),
    model_reml = map2(formula, df_one, ~ safe_refit_REML(.x, .y, use_ar1 = TRUE)),
    infer      = map(model_reml, extract_inference)
  ) %>%
  select(-df_one) %>%
  unnest(infer)

gls_phase_infer


# =============================================================================
# G. Relative change vs Neutral (model-based 95% CI)
# =============================================================================
calc_rel_from_model <- function(m) {
  empty <- tibble(
    rel_lanina = NA_real_, rel_lanina_lwr = NA_real_, rel_lanina_upr = NA_real_,
    rel_elnino = NA_real_, rel_elnino_lwr = NA_real_, rel_elnino_upr = NA_real_,
    pct_lanina = NA_real_, pct_lanina_lwr = NA_real_, pct_lanina_upr = NA_real_,
    pct_elnino = NA_real_, pct_elnino_lwr = NA_real_, pct_elnino_upr = NA_real_
  )

  if (is.null(m)) return(empty)

  tt <- summary(m)$tTable
  rn <- rownames(tt)
  if (!"(Intercept)" %in% rn) return(empty)

  mu <- as.numeric(tt["(Intercept)", "Value"])

  get_est_se <- function(term) {
    if (term %in% rn) {
      c(est = as.numeric(tt[term, "Value"]),
        se  = as.numeric(tt[term, "Std.Error"]))
    } else {
      c(est = NA_real_, se = NA_real_)
    }
  }

  la <- get_est_se("ENSO_Phase3La Niña")
  el <- get_est_se("ENSO_Phase3El Niño")
  z  <- 1.96

  # Difference CI
  la_lwr <- la["est"] - z * la["se"]
  la_upr <- la["est"] + z * la["se"]
  el_lwr <- el["est"] - z * el["se"]
  el_upr <- el["est"] + z * el["se"]

  # Relative change vs Neutral
  rel_la <- la["est"] / mu
  rel_el <- el["est"] / mu

  tibble(
    rel_lanina = rel_la, rel_lanina_lwr = la_lwr / mu, rel_lanina_upr = la_upr / mu,
    rel_elnino = rel_el, rel_elnino_lwr = el_lwr / mu, rel_elnino_upr = el_upr / mu,
    pct_lanina     = 100 * rel_la,
    pct_lanina_lwr = 100 * la_lwr / mu,
    pct_lanina_upr = 100 * la_upr / mu,
    pct_elnino     = 100 * rel_el,
    pct_elnino_lwr = 100 * el_lwr / mu,
    pct_elnino_upr = 100 * el_upr / mu
  )
}

gls_phase_rel <- gls_phase_infer %>%
  rowwise() %>%
  mutate(rel = list(calc_rel_from_model(model_reml))) %>%
  ungroup() %>%
  unnest(rel) %>%
  select(
    Tuna, model_id, formula, n_used, AICc, rho_ar1_reml, mu_neutral,
    diff_lanina, p_lanina, rel_lanina, pct_lanina,
    rel_lanina_lwr, rel_lanina_upr, pct_lanina_lwr, pct_lanina_upr,
    diff_elnino, p_elnino, rel_elnino, pct_elnino,
    rel_elnino_lwr, rel_elnino_upr, pct_elnino_lwr, pct_elnino_upr
  )

gls_phase_rel
