###############################################################################
# GCB_COG-A.R
# ──────────────────────────────────────────────────────────────────────────────
# Centre of Gravity (COG) Lon / Lat vs MEI / ONI — Analysis Module
#
# Key design:
#   1) COG from normalized weights: w_rel = w / sum(w)  (distribution only)
#   2) Keep W_sum as abundance proxy; add logW for sensitivity checks
#   3) Keep raw COG (COG_*_raw) for sanity check
#   4) GLS candidates include phase (+ MEI / logW / trend).  Best model
#      selected by harmonized AICc rule across species.
#
# Workflow:
#   A) Utility functions
#   B) Build monthly COG dataset (normalized + raw)
#   C) Correlation: index vs COG (per Tuna)
#   D) Linear vs GAM comparison
#   E) Lag screening: index leading COG by k months
#   F) ENSO phase contrasts via GLS with AR(1)
#   G) COG vs total abundance correlation check
#
# Requires objects already in the workspace:
#   northern_alb_model_data, southern_alb_model_data,
#   bet_model_data, yft_model_data
#   (each with predicted columns pre_ALB / pre_BET / pre_YFT
#    and optional MEI, ONI, ENSO_Phase3)
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

# -- Year-Month to Date --
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
  x <- x[ok]; y <- y[ok]
  if (length(x) < min_n) return(tibble(n = length(x), r = NA_real_, p = NA_real_))
  ct <- suppressWarnings(stats::cor.test(x, y, method = method))
  tibble(n = length(x), r = unname(ct$estimate), p = unname(ct$p.value))
}

# -- Weighted mean (NA-safe) --
wmean_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w)
  x <- x[ok]; w <- w[ok]
  sw <- sum(w)
  if (!is.finite(sw) || sw <= 0) return(NA_real_)
  sum(x * w) / sw
}

# -- AICc for gls objects --
AICc_gls <- function(m) {
  k <- length(stats::coef(m))
  n <- stats::nobs(m)
  a <- stats::AIC(m)
  if (!is.finite(n) || !is.finite(k) || (n - k - 1) <= 0) return(NA_real_)
  a + (2 * k * (k + 1)) / (n - k - 1)
}

# -- Safe GLS fit with AR(1) --
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

# -- Refit best model with REML --
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

# -- Extract phase shifts + optional covariate effects from GLS --
extract_phase_shift_deg <- function(m) {
  empty <- tibble(
    mu_neutral    = NA_real_,
    shift_lanina  = NA_real_, shift_lanina_lwr = NA_real_,
    shift_lanina_upr = NA_real_, p_lanina = NA_real_,
    shift_elnino  = NA_real_, shift_elnino_lwr = NA_real_,
    shift_elnino_upr = NA_real_, p_elnino = NA_real_,
    beta_mei  = NA_real_, p_mei  = NA_real_,
    beta_trend = NA_real_, p_trend = NA_real_,
    beta_logW = NA_real_, p_logW = NA_real_,
    rho_ar1_reml = NA_real_,
    note_infer = NA_character_
  )

  if (inherits(m, "error") || is.null(m)) {
    empty$note_infer <- if (inherits(m, "error"))
      paste0("REML refit failed: ", conditionMessage(m))
    else "REML refit failed"
    return(empty)
  }

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

  mu  <- if ("(Intercept)" %in% rn) as.numeric(tt["(Intercept)", "Value"]) else NA_real_
  la  <- get_est_se_p("ENSO_Phase3La Niña")
  el  <- get_est_se_p("ENSO_Phase3El Niño")
  mei <- get_est_se_p("MEI")
  tr  <- get_est_se_p("trend")
  lw  <- get_est_se_p("logW")

  z      <- 1.96
  la_lwr <- la["est"] - z * la["se"]
  la_upr <- la["est"] + z * la["se"]
  el_lwr <- el["est"] - z * el["se"]
  el_upr <- el["est"] + z * el["se"]

  rho <- tryCatch(
    as.numeric(coef(m$modelStruct$corStruct, unconstrained = FALSE)),
    error = function(e) NA_real_
  )

  tibble(
    mu_neutral       = mu,
    shift_lanina     = la["est"], shift_lanina_lwr = la_lwr,
    shift_lanina_upr = la_upr,   p_lanina = la["p"],
    shift_elnino     = el["est"], shift_elnino_lwr = el_lwr,
    shift_elnino_upr = el_upr,   p_elnino = el["p"],
    beta_mei   = mei["est"], p_mei   = mei["p"],
    beta_trend = tr["est"],  p_trend = tr["p"],
    beta_logW  = lw["est"],  p_logW  = lw["p"],
    rho_ar1_reml = rho,
    note_infer   = NA_character_
  )
}


# =============================================================================
# B. Build monthly COG dataset
# =============================================================================
build_monthly_cog_one <- function(dat, tuna_label,
                                  lon_col    = "Lon",
                                  lat_col    = "Lat",
                                  weight_col,
                                  year_col   = "Year",
                                  month_col  = "Month") {

  need <- c(year_col, month_col, lon_col, lat_col, weight_col)
  miss <- setdiff(need, names(dat))
  if (length(miss) > 0) stop("Missing column(s): ", paste(miss, collapse = ", "))

  keep_idx <- intersect(c("MEI", "ONI", "ENSO_Phase3"), names(dat))

  out <- dat %>%
    transmute(
      Year  = .data[[year_col]],
      Month = .data[[month_col]],
      Lon   = .data[[lon_col]],
      Lat   = .data[[lat_col]],
      w     = .data[[weight_col]],
      MEI   = if ("MEI" %in% keep_idx) MEI else NA_real_,
      ONI   = if ("ONI" %in% keep_idx) ONI else NA_real_,
      ENSO_Phase3 = if ("ENSO_Phase3" %in% keep_idx) ENSO_Phase3 else NA
    ) %>%
    filter(is.finite(Year), is.finite(Month),
           is.finite(Lon),  is.finite(Lat),
           is.finite(w), w > 0) %>%
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
      COG_Lon_raw = wmean_safe(Lon, w),
      COG_Lat_raw = wmean_safe(Lat, w),
      COG_Lon     = wmean_safe(Lon, w_rel),
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
  arrange(Tuna, Time)

# Sanity check: raw vs normalized COG should be identical
sanity_cog_invariance <- monthly_cog_long %>%
  group_by(Tuna) %>%
  summarise(
    max_abs_diff_lon = max(abs(COG_Lon - COG_Lon_raw), na.rm = TRUE),
    max_abs_diff_lat = max(abs(COG_Lat - COG_Lat_raw), na.rm = TRUE),
    .groups = "drop"
  )
sanity_cog_invariance


# =============================================================================
# C. Correlation: index vs COG (per Tuna)
# =============================================================================
run_cor_by_tuna_cog <- function(dat, y = c("COG_Lon", "COG_Lat"),
                                index = "MEI", method = "pearson") {
  y <- match.arg(y)
  stopifnot(all(c("Tuna", y, index) %in% names(dat)))

  dat %>%
    group_by(Tuna) %>%
    group_modify(~ {
      cor_test_simple(.x[[y]], .x[[index]], method = method) %>%
        mutate(y = y, index = index, method = method)
    }) %>%
    ungroup() %>%
    mutate(r_label = sprintf("r = %.2f", r), p_label = map_chr(p, fp_eq))
}

cor_lon_mei_pearson  <- if ("MEI" %in% names(monthly_cog_long))
  run_cor_by_tuna_cog(monthly_cog_long, "COG_Lon", "MEI", "pearson")  else tibble()
cor_lon_mei_spearman <- if ("MEI" %in% names(monthly_cog_long))
  run_cor_by_tuna_cog(monthly_cog_long, "COG_Lon", "MEI", "spearman") else tibble()
cor_lon_oni_pearson  <- if ("ONI" %in% names(monthly_cog_long))
  run_cor_by_tuna_cog(monthly_cog_long, "COG_Lon", "ONI", "pearson")  else tibble()
cor_lon_oni_spearman <- if ("ONI" %in% names(monthly_cog_long))
  run_cor_by_tuna_cog(monthly_cog_long, "COG_Lon", "ONI", "spearman") else tibble()

cor_lat_mei_pearson  <- if ("MEI" %in% names(monthly_cog_long))
  run_cor_by_tuna_cog(monthly_cog_long, "COG_Lat", "MEI", "pearson")  else tibble()
cor_lat_mei_spearman <- if ("MEI" %in% names(monthly_cog_long))
  run_cor_by_tuna_cog(monthly_cog_long, "COG_Lat", "MEI", "spearman") else tibble()
cor_lat_oni_pearson  <- if ("ONI" %in% names(monthly_cog_long))
  run_cor_by_tuna_cog(monthly_cog_long, "COG_Lat", "ONI", "pearson")  else tibble()
cor_lat_oni_spearman <- if ("ONI" %in% names(monthly_cog_long))
  run_cor_by_tuna_cog(monthly_cog_long, "COG_Lat", "ONI", "spearman") else tibble()


# =============================================================================
# D. Linear vs GAM: index -> COG (per Tuna)
# =============================================================================
fit_linear_vs_gam_cog <- function(dat,
                                  y = c("COG_Lon", "COG_Lat"),
                                  index      = "MEI",
                                  k_smooth   = 5,
                                  min_n      = 24,
                                  gam_method = "REML") {
  y <- match.arg(y)
  stopifnot(all(c("Tuna", y, index) %in% names(dat)))

  dat %>%
    group_by(Tuna) %>%
    group_modify(~ {
      df <- .x %>%
        filter(is.finite(.data[[y]]), is.finite(.data[[index]])) %>%
        arrange(Time)

      if (nrow(df) < min_n) {
        return(tibble(
          n = nrow(df),
          aic_lin = NA_real_, aic_gam = NA_real_, dAIC = NA_real_,
          p_lin = NA_real_, edf = NA_real_, p_smooth = NA_real_
        ))
      }

      f_lin <- as.formula(paste0(y, " ~ ", index))
      f_gam <- as.formula(paste0(y, " ~ s(", index, ", k = ", k_smooth, ")"))

      m_lin <- lm(f_lin, data = df)
      m_gam <- mgcv::gam(f_gam, data = df, method = gam_method)

      s_lin <- summary(m_lin)$coefficients
      p_lin <- tryCatch(s_lin[index, "Pr(>|t|)"], error = function(e) NA_real_)

      s_gam <- summary(m_gam)
      edf   <- as.numeric(s_gam$s.table[1, "edf"])
      p_sm  <- as.numeric(s_gam$s.table[1, "p-value"])

      tibble(
        n = nrow(df),
        aic_lin = AIC(m_lin), aic_gam = AIC(m_gam),
        dAIC    = AIC(m_gam) - AIC(m_lin),
        p_lin   = p_lin, edf = edf, p_smooth = p_sm
      )
    }) %>%
    ungroup() %>%
    mutate(
      y = y, index = index,
      prefer = case_when(
        is.na(dAIC) ~ NA_character_,
        dAIC <= -2  ~ "GAM (better)",
        dAIC >=  2  ~ "Linear (better)",
        TRUE        ~ "Similar"
      ),
      p_lin_label    = map_chr(p_lin,    fp_eq),
      p_smooth_label = map_chr(p_smooth, fp_eq),
      edf_label      = if_else(is.na(edf), NA_character_, sprintf("edf = %.2f", edf))
    )
}

model_cmp_lon_mei <- if ("MEI" %in% names(monthly_cog_long))
  fit_linear_vs_gam_cog(monthly_cog_long, "COG_Lon", "MEI") else tibble()
model_cmp_lat_mei <- if ("MEI" %in% names(monthly_cog_long))
  fit_linear_vs_gam_cog(monthly_cog_long, "COG_Lat", "MEI") else tibble()
model_cmp_lon_oni <- if ("ONI" %in% names(monthly_cog_long))
  fit_linear_vs_gam_cog(monthly_cog_long, "COG_Lon", "ONI") else tibble()
model_cmp_lat_oni <- if ("ONI" %in% names(monthly_cog_long))
  fit_linear_vs_gam_cog(monthly_cog_long, "COG_Lat", "ONI") else tibble()


# =============================================================================
# E. Lag screening: index leading COG by k months
# =============================================================================
lag_scan_cor_cog <- function(dat,
                             y       = c("COG_Lon", "COG_Lat"),
                             index   = "MEI",
                             max_lag = 12,
                             method  = "pearson",
                             min_n   = 8) {
  y <- match.arg(y)
  stopifnot(all(c("Tuna", "Time", y, index) %in% names(dat)))
  lags <- 0:max_lag

  dat %>%
    arrange(Tuna, Time) %>%
    group_by(Tuna) %>%
    group_modify(~ {
      df <- .x
      map_dfr(lags, function(k) {
        yy <- df[[y]]
        xx <- dplyr::lag(df[[index]], k)
        cor_test_simple(yy, xx, method = method, min_n = min_n) %>%
          mutate(lag_month = k, y = y, index = index, method = method)
      })
    }) %>%
    ungroup()
}

lag_tbl_lon_mei <- if ("MEI" %in% names(monthly_cog_long))
  lag_scan_cor_cog(monthly_cog_long, "COG_Lon", "MEI") else tibble()
lag_tbl_lat_mei <- if ("MEI" %in% names(monthly_cog_long))
  lag_scan_cor_cog(monthly_cog_long, "COG_Lat", "MEI") else tibble()

best_lag_lon_mei_absr <- lag_tbl_lon_mei %>%
  group_by(Tuna) %>% slice_max(abs(r), n = 1, with_ties = FALSE) %>% ungroup()
best_lag_lat_mei_absr <- lag_tbl_lat_mei %>%
  group_by(Tuna) %>% slice_max(abs(r), n = 1, with_ties = FALSE) %>% ungroup()


# =============================================================================
# F. ENSO phase contrasts via GLS with AR(1)
# =============================================================================

# -- F1. Data preparation --
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

# -- F2. Candidate model formulae --
make_gls_candidates_cog <- function(df, response) {
  has_mei   <- "MEI"   %in% names(df) && any(is.finite(df$MEI))
  has_trend <- "trend" %in% names(df) && any(is.finite(df$trend))
  has_logW  <- "logW"  %in% names(df) && any(is.finite(df$logW))

  cand <- list(
    L1_Phase = as.formula(paste(response, "~ ENSO_Phase3"))
  )
  if (has_trend)              cand$L2_Phase_trend      <- as.formula(paste(response, "~ ENSO_Phase3 + trend"))
  if (has_logW)               cand$L3_Phase_logW       <- as.formula(paste(response, "~ ENSO_Phase3 + logW"))
  if (has_logW && has_trend)  cand$L4_Phase_logW_trend <- as.formula(paste(response, "~ ENSO_Phase3 + logW + trend"))
  if (has_mei)                cand$L5_Phase_MEI        <- as.formula(paste(response, "~ ENSO_Phase3 + MEI"))
  if (has_mei && has_trend)   cand$L6_Phase_MEI_trend  <- as.formula(paste(response, "~ ENSO_Phase3 + MEI + trend"))

  cand
}

# -- F3. Fit all candidates for one species --
fit_gls_candidates_one_cog <- function(df_one,
                                       response         = c("COG_Lon", "COG_Lat"),
                                       min_n_total      = 24,
                                       min_n_each_phase = 5,
                                       use_ar1          = TRUE,
                                       method_compare   = "ML") {
  response <- match.arg(response)

  df_one <- df_one %>%
    filter(is.finite(.data[[response]]), !is.na(ENSO_Phase3)) %>%
    mutate(ENSO_Phase3 = factor(ENSO_Phase3,
                                levels = c("Neutral", "La Niña", "El Niño")))

  n_total <- nrow(df_one)
  n_neu   <- sum(df_one$ENSO_Phase3 == "Neutral")
  n_la    <- sum(df_one$ENSO_Phase3 == "La Niña")
  n_el    <- sum(df_one$ENSO_Phase3 == "El Niño")

  if (n_total < min_n_total || min(n_neu, n_la, n_el) < min_n_each_phase) {
    return(tibble(
      response = response, model_id = NA_character_, formula = NA_character_,
      n_used = n_total,
      AIC = NA_real_, AICc = NA_real_, BIC = NA_real_, logLik = NA_real_,
      rho_ar1 = NA_real_,
      n_total = n_total, n_neutral = n_neu, n_lanina = n_la, n_elnino = n_el,
      note = "insufficient samples", model = list(NULL)
    ))
  }

  cand <- make_gls_candidates_cog(df_one, response)

  imap_dfr(cand, function(fml, id) {
    vars_need <- unique(all.vars(fml))
    dsub <- df_one %>%
      select(any_of(c(vars_need, "Time_num"))) %>%
      filter(stats::complete.cases(.)) %>%
      arrange(Time_num)

    fit <- safe_gls_fit(fml, dsub, method = method_compare, use_ar1 = use_ar1)

    if (!fit$ok) {
      return(tibble(
        response = response, model_id = id, formula = deparse(fml),
        n_used = nrow(dsub),
        AIC = NA_real_, AICc = NA_real_, BIC = NA_real_, logLik = NA_real_,
        rho_ar1 = NA_real_,
        n_total = n_total, n_neutral = n_neu, n_lanina = n_la, n_elnino = n_el,
        note = fit$note, model = list(NULL)
      ))
    }

    m <- fit$model
    tibble(
      response = response, model_id = id, formula = deparse(fml),
      n_used = stats::nobs(m),
      AIC    = stats::AIC(m),
      AICc   = AICc_gls(m),
      BIC    = stats::BIC(m),
      logLik = as.numeric(stats::logLik(m)),
      rho_ar1 = fit$rho,
      n_total = n_total, n_neutral = n_neu, n_lanina = n_la, n_elnino = n_el,
      note = NA_character_, model = list(m)
    )
  })
}

# -- F4. Run GLS for all species (both Lon and Lat) --
run_gls_all_tuna <- function(phase_dat, response) {
  phase_dat %>%
    group_by(Tuna) %>%
    group_modify(~ fit_gls_candidates_one_cog(
      .x, response = response,
      min_n_total = 24, min_n_each_phase = 5,
      use_ar1 = TRUE, method_compare = "ML"
    )) %>%
    ungroup()
}

# -- F5. Harmonized model selection across species --
#   1) Find one "common" model within delta-AICc for the most species
#   2) Per species: use common if within delta; otherwise fallback to
#      that species' own best (phase-preferred if possible)
select_best_harmonized <- function(gls_tbl,
                                   delta         = 2.1,
                                   phase_term    = "ENSO_Phase3",
                                   require_phase = TRUE) {
  stopifnot(all(c("Tuna", "AICc", "model_id", "formula") %in% names(gls_tbl)))

  df <- gls_tbl %>%
    filter(is.finite(AICc)) %>%
    mutate(
      Tuna     = as.character(Tuna),
      model_id = as.character(model_id),
      has_phase = grepl(phase_term, formula, fixed = TRUE)
    ) %>%
    group_by(Tuna) %>%
    mutate(
      AICc_min = min(AICc, na.rm = TRUE),
      dAICc    = AICc - AICc_min,
      in_delta = dAICc <= delta
    ) %>%
    ungroup()

  # Coverage: how many species accept each model within delta
  df_pool <- if (isTRUE(require_phase)) df %>% filter(has_phase) else df

  common_score <- df_pool %>%
    group_by(model_id, formula) %>%
    summarise(
      n_ok         = sum(in_delta, na.rm = TRUE),
      sum_dAICc_ok = sum(dAICc[in_delta], na.rm = TRUE),
      sum_AICc_ok  = sum(AICc[in_delta],  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(n_ok), sum_dAICc_ok, sum_AICc_ok)

  # Fallback: if no phase models exist, allow non-phase
  if (nrow(common_score) == 0) {
    common_score <- df %>%
      group_by(model_id, formula) %>%
      summarise(
        n_ok         = sum(in_delta, na.rm = TRUE),
        sum_dAICc_ok = sum(dAICc[in_delta], na.rm = TRUE),
        sum_AICc_ok  = sum(AICc[in_delta],  na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(n_ok), sum_dAICc_ok, sum_AICc_ok)
  }

  common_pick     <- common_score %>% slice(1)
  common_model_id <- common_pick$model_id[[1]]
  common_formula  <- common_pick$formula[[1]]

  # Per-species selection with fallback
  best_tbl <- df %>%
    group_by(Tuna) %>%
    group_modify(~ {
      d1 <- .x

      # Try common model
      d_common <- d1 %>% filter(model_id == common_model_id, in_delta)
      if (nrow(d_common) > 0) {
        return(d_common %>%
                 slice_min(AICc, n = 1, with_ties = FALSE) %>%
                 mutate(select_rule = "common_within_delta"))
      }

      # Fallback: within-delta, prefer phase models
      d_delta <- d1 %>% filter(in_delta)
      if (nrow(d_delta) > 0) {
        if (any(d_delta$has_phase)) {
          return(d_delta %>% filter(has_phase) %>%
                   slice_min(AICc, n = 1, with_ties = FALSE) %>%
                   mutate(select_rule = "fallback_best_phase_within_delta"))
        }
        return(d_delta %>%
                 slice_min(AICc, n = 1, with_ties = FALSE) %>%
                 mutate(select_rule = "fallback_best_within_delta"))
      }

      # Last resort: absolute best
      d1 %>%
        slice_min(AICc, n = 1, with_ties = FALSE) %>%
        mutate(select_rule = "fallback_absolute_best")
    }) %>%
    ungroup() %>%
    select(-AICc_min, -dAICc, -in_delta, -has_phase)

  attr(best_tbl, "common_model_id") <- common_model_id
  attr(best_tbl, "common_formula")  <- common_formula
  attr(best_tbl, "common_score")    <- common_score

  best_tbl
}


# -- F6. Run GLS candidates --
gls_cog_lon_raw <- phase_cog_dat %>%
  group_by(Tuna) %>%
  group_modify(~ fit_gls_candidates_one_cog(
    mutate(.x, Tuna = .y$Tuna), response = "COG_Lon",
    min_n_total = 24, min_n_each_phase = 5,
    use_ar1 = TRUE, method_compare = "ML"
  )) %>%
  ungroup()

gls_cog_lat_raw <- phase_cog_dat %>%
  group_by(Tuna) %>%
  group_modify(~ fit_gls_candidates_one_cog(
    mutate(.x, Tuna = .y$Tuna), response = "COG_Lat",
    min_n_total = 24, min_n_each_phase = 5,
    use_ar1 = TRUE, method_compare = "ML"
  )) %>%
  ungroup()

# -- F7. Select best models (harmonized) --
gls_cog_lon_best1 <- select_best_harmonized(gls_cog_lon_raw, delta = 2.1, require_phase = TRUE)
gls_cog_lat_best1 <- select_best_harmonized(gls_cog_lat_raw, delta = 2.1, require_phase = TRUE)

# Diagnostics
attr(gls_cog_lon_best1, "common_model_id")
attr(gls_cog_lon_best1, "common_formula")
attr(gls_cog_lat_best1, "common_model_id")
attr(gls_cog_lat_best1, "common_formula")

gls_cog_lon_best1 %>% select(Tuna, model_id, formula, AICc, select_rule)
gls_cog_lat_best1 %>% select(Tuna, model_id, formula, AICc, select_rule)

# -- F8. Refit with REML + extract inference --

# Helper: refit and extract for a best-model table
refit_and_infer <- function(best_tbl, phase_dat) {
  best_tbl %>%
    mutate(
      df_one = map2(Tuna, formula, ~ {
        vars_need <- unique(c(all.vars(as.formula(.y)), "Time_num"))
        phase_dat %>%
          filter(Tuna == .x) %>%
          select(any_of(vars_need)) %>%
          filter(stats::complete.cases(.)) %>%
          arrange(Time_num)
      }),
      model_reml = map2(formula, df_one,
                        ~ safe_refit_REML(.x, .y, use_ar1 = TRUE)),
      infer = map(model_reml, extract_phase_shift_deg)
    ) %>%
    select(-df_one) %>%
    unnest(infer)
}

# Add p-value labels
add_p_labels <- function(infer_tbl) {
  infer_tbl %>%
    mutate(
      p_lanina_label = map_chr(p_lanina, fp_eq),
      p_elnino_label = map_chr(p_elnino, fp_eq),
      p_mei_label    = map_chr(p_mei,    fp_eq),
      p_trend_label  = map_chr(p_trend,  fp_eq),
      p_logW_label   = map_chr(p_logW,   fp_eq)
    )
}

# Longitude
gls_cog_lon_infer <- refit_and_infer(gls_cog_lon_best1, phase_cog_dat)
gls_cog_lon_shift <- add_p_labels(gls_cog_lon_infer)
gls_cog_lon_shift

# Latitude
gls_cog_lat_infer <- refit_and_infer(gls_cog_lat_best1, phase_cog_dat)
gls_cog_lat_shift <- add_p_labels(gls_cog_lat_infer)
gls_cog_lat_shift


# =============================================================================
# G. COG vs total abundance correlation check
# =============================================================================
cog_vs_total_cor <- phase_cog_dat %>%
  group_by(Tuna) %>%
  summarise(
    r_lon_logW = cor(COG_Lon, logW, use = "complete.obs"),
    r_lat_logW = cor(COG_Lat, logW, use = "complete.obs"),
    .groups = "drop"
  )
cog_vs_total_cor
