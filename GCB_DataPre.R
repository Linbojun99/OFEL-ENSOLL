###############################################################################
# GCB_Data.R
# ──────────────────────────────────────────────────────────────────────────────
# Longline tuna nominal CPUE data pipeline (1993–2023)
#
# Workflow:
#   1) Read & clean WCPFC + IATTC longline nominal catch data
#   2) Aggregate to 5×5° grid / month / ocean area; compute CPUE per 1 000 hooks
#   3) Merge with environmental covariates (pre-built RData)
#   4) Download / parse MEI.v2 and CPC ONI v5
#   5) Assign CPC strict ENSO phase (≥ 5 consecutive overlapping seasons)
#   6) Build final species datasets: ALB_N / ALB_S / BET / YFT
#   7) Export .RData and .csv outputs
###############################################################################


# =============================================================================
# 0. Packages
# =============================================================================
library(tidyverse)
library(lubridate)
library(ggplot2)
library(cowplot)
library(dplyr)

set.seed(123)


# =============================================================================
# 1. Project paths (auto-detect across devices)
# =============================================================================
ROOT_macmini <- "/Users/linbojun/Library/CloudStorage/OneDrive-共用文件庫－onedrive/Oceanic Fisheries Ecosystem Laboratory/ENSO_LL/GCB version"
ROOT_mac     <- "/Users/linbojun/Library/CloudStorage/OneDrive-個人/Oceanic Fisheries Ecosystem Laboratory/ENSO_LL/GCB version"
ROOT_pc      <- "D:/OneDrive/Oceanic Fisheries Ecosystem Laboratory/ENSO_LL/GCB version"

ROOTS <- c(ROOT_macmini, ROOT_mac, ROOT_pc)
ROOT  <- ROOTS[dir.exists(ROOTS)][2]
if (is.na(ROOT)) stop("No valid ROOT found. Check ROOT_macmini / ROOT_mac / ROOT_pc paths.")
cat("Using ROOT:\n", ROOT, "\n\n")

PATH_DATA  <- file.path(ROOT, "Data")
PATH_RDATA <- file.path(ROOT, "Revise/RData")


# =============================================================================
# 2. User settings
# =============================================================================
YEAR_MIN <- 1993
YEAR_MAX <- 2023


# =============================================================================
# 3. Plot-saving helper
# =============================================================================
PATH_FIG <- file.path(ROOT, "Revise/Figures")
if (!dir.exists(PATH_FIG)) dir.create(PATH_FIG, recursive = TRUE)

save_plot_all <- function(p,
                          filename_base,
                          out_dir    = PATH_FIG,
                          width      = 14,
                          height     = 7.5,
                          dpi        = 600,
                          bg         = "white",
                          units      = "in",
                          device_pdf = cairo_pdf,
                          use_cairo_png = TRUE) {
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  dir_png <- file.path(out_dir, "png")
  dir_pdf <- file.path(out_dir, "pdf")
  dir_svg <- file.path(out_dir, "svg")
  
  dir.create(dir_png, recursive = TRUE, showWarnings = FALSE)
  dir.create(dir_pdf, recursive = TRUE, showWarnings = FALSE)
  dir.create(dir_svg, recursive = TRUE, showWarnings = FALSE)
  
  f_png <- file.path(dir_png, paste0(filename_base, ".png"))
  f_pdf <- file.path(dir_pdf, paste0(filename_base, ".pdf"))
  f_svg <- file.path(dir_svg, paste0(filename_base, ".svg"))
  
  # PNG (high-resolution raster)
  ggsave(
    filename = f_png, plot = p,
    width = width, height = height, units = units,
    dpi = dpi, bg = bg,
    device = if (use_cairo_png) ragg::agg_png else "png"
  )
  
  # PDF (vector)
  ggsave(
    filename = f_pdf, plot = p,
    width = width, height = height, units = units,
    bg = bg, device = device_pdf
  )
  
  # SVG (vector)
  ggsave(
    filename = f_svg, plot = p,
    width = width, height = height, units = units,
    bg = bg, device = svglite::svglite
  )
  
  message("Saved:\n", f_png, "\n", f_pdf, "\n", f_svg)
  invisible(list(png = f_png, pdf = f_pdf, svg = f_svg))
}


# =============================================================================
# 4. Helper functions
# =============================================================================

# -- Convert WCPFC lon_short / lat_short strings to numeric --
#    Longitude returned in 0–360; latitude in −90 to 90.
getLonLat <- function(value) {
  value <- as.character(value)
  flag  <- substr(value, nchar(value), nchar(value))
  num   <- as.numeric(substr(value, 1, nchar(value) - 1))
  
  if (flag == "S") return(-num)
  if (flag == "N") return(num)
  if (flag == "E") return(num)
  if (flag == "W") {
    if (num == 180) return(180)
    return(-num + 360)
  }
  NA_real_
}

# -- Coordinate utilities --
lon_to_360          <- function(lon) ifelse(lon < 0, lon + 360, lon)
to_cell_center_5deg <- function(x) x + 2.5

# -- CPUE per 1 000 hooks --
cpue_per_1000 <- function(catch_n, hooks_n) {
  ifelse(is.na(hooks_n) | hooks_n <= 0, NA_real_, catch_n / (hooks_n / 1000))
}

# -- WCPFC → IATTC flag-code mapping --
code_map <- tibble(
  WCPFC = c("AU","BZ","CK","CN","ES","FJ","FM","ID","JP","KI","KR","MH","NC","NU",
            "NZ","PF","PG","PH","PT","PW","SB","TO","TV","TW","US","VN","VU","WS"),
  IATTC = c("AUS","BLZ","COK","CHN","ESP","FJI","FSM","IDN","JPN","KIR","KOR","MHL","NCL","NIU",
            "NZL","PYF","PNG","PHL","PRT","PLW","SLB","TON","TUV","TWN","USA","VNM","VUT","WSM")
)

# -- Drop environmental columns whose depth suffix exceeds a threshold --
drop_depth_suffix_over <- function(df, max_depth) {
  nms <- names(df)
  m   <- stringr::str_match(nms, "^(Temp|Sali|O2|Chl|Nppv)_([0-9]+)$")
  
  is_env   <- !is.na(m[, 1])
  depth    <- suppressWarnings(as.integer(m[, 3]))
  drop_idx <- which(is_env & !is.na(depth) & depth > max_depth)
  
  if (length(drop_idx) == 0) return(df)
  df[, -drop_idx, drop = FALSE]
}


# =============================================================================
# 5. Climate-index readers
# =============================================================================

# -- MEI.v2 reader (monthly values mapped from bimonthly seasons) --
read_mei_v2_monthly <- function(
    url      = "https://www.psl.noaa.gov/enso/mei/data/meiv2.data",
    year_min = 1993,
    year_max = 2023) {
  
  txt   <- readLines(url, warn = FALSE)
  cand  <- grep("^\\s*\\d{4}\\b", txt, value = TRUE)
  split <- strsplit(cand, "\\s+")
  
  to_num <- function(v) suppressWarnings(as.numeric(v[nchar(v) > 0]))
  rows   <- lapply(split, function(v) {
    nums <- to_num(v)
    if (sum(!is.na(nums)) == 13) nums else NULL
  })
  rows <- rows[!sapply(rows, is.null)]
  mat  <- do.call(rbind, rows)
  
  df <- as.data.frame(mat)
  names(df) <- c("Year", "DJ", "JF", "FM", "MA", "AM", "MJ",
                 "JJ", "JA", "AS", "SO", "ON", "ND")
  df$Year <- as.integer(df$Year)
  
  long <- df %>%
    pivot_longer(cols = DJ:ND, names_to = "Season", values_to = "MEI") %>%
    mutate(MEI = na_if(MEI, -999))
  
  # Centre-month mapping: DJ → Jan, JF → Feb, …, ND → Dec
  season_to_month <- c(DJ = 1, JF = 2, FM = 3, MA = 4, AM = 5,  MJ = 6,
                       JJ = 7, JA = 8, AS = 9, SO = 10, ON = 11, ND = 12)
  
  long %>%
    mutate(Month = season_to_month[Season]) %>%
    filter(Year >= year_min, Year <= year_max) %>%
    arrange(Year, Month) %>%
    select(Year, Month, MEI)
}

# -- CPC ONI v5 reader (parse HTML table from CPC webpage) --
.norm_name <- function(x) {
  x <- stringr::str_squish(x)
  x <- stringr::str_to_upper(x)
  stringr::str_replace_all(x, "[^A-Z0-9]", "")
}

read_cpc_oni_v5_long <- function(
    url = "https://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php") {
  
  seasons   <- c("DJF","JFM","FMA","MAM","AMJ","MJJ","JJA","JAS","ASO","SON","OND","NDJ")
  seasons_n <- .norm_name(seasons)
  
  if (!requireNamespace("rvest", quietly = TRUE)) stop("Need package rvest.")
  if (!requireNamespace("purrr", quietly = TRUE)) stop("Need package purrr.")
  
  pg   <- rvest::read_html(url)
  tabs <- rvest::html_elements(pg, "table")
  tbls <- rvest::html_table(tabs, fill = TRUE)
  
  clean_one_tbl <- function(tb) {
    tb <- as.data.frame(tb, stringsAsFactors = FALSE)
    
    # If the first row looks like a header
    if (nrow(tb) >= 2 && all(stringr::str_detect(names(tb), "^X\\d+$"))) {
      hdr <- as.character(tb[1, , drop = TRUE])
      tb  <- tb[-1, , drop = FALSE]
      names(tb) <- hdr
    }
    
    nms_raw <- names(tb)
    nms_n   <- .norm_name(nms_raw)
    
    year_idx <- which(nms_n %in% c("YEAR", "YEARS"))
    if (length(year_idx) == 0) return(NULL)
    year_idx <- year_idx[1]
    
    season_idx <- match(seasons_n, nms_n)
    if (any(is.na(season_idx))) return(NULL)
    
    out <- tb[, c(year_idx, season_idx), drop = FALSE]
    names(out) <- c("Year", seasons)
    
    out <- out %>%
      mutate(
        Year = suppressWarnings(as.integer(.data$Year)),
        across(all_of(seasons), ~ suppressWarnings(as.numeric(.x)))
      ) %>%
      filter(!is.na(.data$Year))
    
    if (nrow(out) == 0) return(NULL)
    out
  }
  
  cand <- purrr::compact(purrr::map(tbls, clean_one_tbl))
  if (length(cand) == 0) stop("Failed to parse ONI from CPC ONI_v5.php.")
  
  oni_wide <- bind_rows(cand) %>%
    distinct(.data$Year, .keep_all = TRUE) %>%
    arrange(.data$Year)
  
  season_to_month <- tibble(Season = seasons, Month = 1:12)
  
  oni_wide %>%
    pivot_longer(cols = all_of(seasons), names_to = "Season", values_to = "ONI") %>%
    left_join(season_to_month, by = "Season") %>%
    arrange(Year, Month) %>%
    filter(is.finite(ONI)) %>%
    select(Year, Month, ONI)
}

# -- CPC strict ENSO event rule (≥ 5 consecutive seasons over ± 0.5 °C) --
add_strict_phase3 <- function(oni_long) {
  oni2 <- oni_long %>%
    arrange(.data$Year, .data$Month) %>%
    mutate(
      prelim = case_when(
        .data$ONI >=  0.5 ~ "warm",
        .data$ONI <= -0.5 ~ "cold",
        TRUE              ~ "neutral"
      ),
      run_id = cumsum(.data$prelim != lag(.data$prelim, default = first(.data$prelim)))
    )
  
  run_len <- oni2 %>%
    group_by(.data$run_id, .data$prelim) %>%
    summarise(run_n = n(), .groups = "drop")
  
  oni2 %>%
    left_join(run_len, by = c("run_id", "prelim")) %>%
    mutate(
      strict = case_when(
        prelim == "warm" & run_n >= 5 ~ "warm",
        prelim == "cold" & run_n >= 5 ~ "cold",
        TRUE ~ "neutral"
      ),
      ENSO_Phase3 = case_when(
        strict == "warm" ~ "El Niño",
        strict == "cold" ~ "La Niña",
        TRUE             ~ "Neutral"
      ),
      ENSO_Phase3 = factor(ENSO_Phase3, levels = c("La Niña", "Neutral", "El Niño"))
    ) %>%
    select(Year, Month, ONI, ENSO_Phase3)
}


# =============================================================================
# 6. Read raw longline data
# =============================================================================
path_wcpfc <- file.path(PATH_DATA, "WCPFC_L_PUBLIC_BY_FLAG_MON_8",
                        "WCPFC_L_PUBLIC_BY_FLAG_MON.CSV")
path_iattc <- file.path(PATH_DATA, "PublicLLTunaBillfish",
                        "PublicLLTunaBillfishNum.csv")

stopifnot(file.exists(path_wcpfc))
stopifnot(file.exists(path_iattc))

PATH_ENV_RDATA <- file.path(PATH_RDATA, "pacific_env_data.RData")


# =============================================================================
# 7. Process WCPFC longline data
# =============================================================================
wcpfc_raw <- read.csv(path_wcpfc, header = TRUE)

wcpfc <- wcpfc_raw %>%
  transmute(
    Year  = yy,
    Month = mm,
    lon_short,
    lat_short,
    Flag   = flag_id,
    hhooks = hhooks,
    ALB    = alb_n,
    YFT    = yft_n,
    BET    = bet_n
  ) %>%
  filter(Year >= YEAR_MIN, Year <= YEAR_MAX) %>%
  filter(!is.na(hhooks), hhooks > 0) %>%
  mutate(
    Lon  = purrr::map_dbl(lon_short, getLonLat),
    Lat  = purrr::map_dbl(lat_short, getLonLat),
    Lon  = to_cell_center_5deg(Lon),
    Lat  = to_cell_center_5deg(Lat),
    Area = "WCPO",
    Flag = as.factor(Flag)
  ) %>%
  select(Year, Month, Lon, Lat, Area, Flag, hhooks, ALB, BET, YFT)


# =============================================================================
# 8. Process IATTC longline data
# =============================================================================
iattc_raw <- read.csv(path_iattc, header = TRUE)
n_cols    <- grep("n$", names(iattc_raw), value = TRUE)

iattc <- iattc_raw %>%
  # Drop rows where total species count exceeds hook count
  filter(!((rowSums(across(all_of(n_cols)), na.rm = TRUE) - Hooks) > 0)) %>%
  filter(Year >= YEAR_MIN, Year <= YEAR_MAX) %>%
  transmute(
    Year,
    Month,
    Lon    = LonC5,
    Lat    = LatC5,
    Flag,
    hhooks = Hooks / 100,
    ALB    = ALBn,
    BET    = BETn,
    YFT    = YFTn,
    Area   = "EPO"
  ) %>%
  mutate(
    Lon  = lon_to_360(Lon),
    Flag = as.factor(Flag)
  ) %>%
  filter(!is.na(hhooks), hhooks > 0) %>%
  select(Year, Month, Lon, Lat, Area, Flag, hhooks, ALB, BET, YFT)


# =============================================================================
# 9. Harmonise flag codes & combine datasets
# =============================================================================
pacific_ll_flag <- bind_rows(wcpfc, iattc) %>%
  left_join(code_map, by = c("Flag" = "WCPFC")) %>%
  mutate(Flag = ifelse(is.na(IATTC), as.character(Flag), IATTC)) %>%
  select(-IATTC) %>%
  mutate(Flag = as.factor(Flag))


# =============================================================================
# 10. Aggregate & compute CPUE per 1 000 hooks
# =============================================================================
pacific_tuna_ll_data <- pacific_ll_flag %>%
  group_by(Year, Month, Lon, Lat, Area) %>%
  summarise(
    ALB    = sum(ALB, na.rm = TRUE),
    BET    = sum(BET, na.rm = TRUE),
    YFT    = sum(YFT, na.rm = TRUE),
    hhooks = sum(hhooks, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    hooks_n   = hhooks * 100,
    nCPUE_ALB = cpue_per_1000(ALB, hooks_n),
    nCPUE_BET = cpue_per_1000(BET, hooks_n),
    nCPUE_YFT = cpue_per_1000(YFT, hooks_n)
  ) %>%
  select(-hooks_n)


# =============================================================================
# 11. Remove WCPO / EPO spatial overlap (same Year / Month / Lon / Lat)
# =============================================================================
overlap_keys <- inner_join(
  pacific_tuna_ll_data %>% filter(Area == "WCPO") %>% select(Year, Month, Lon, Lat),
  pacific_tuna_ll_data %>% filter(Area == "EPO")  %>% select(Year, Month, Lon, Lat),
  by = c("Year", "Month", "Lon", "Lat")
) %>% distinct()

pacific_tuna_ll_data <- pacific_tuna_ll_data %>%
  anti_join(overlap_keys, by = c("Year", "Month", "Lon", "Lat"))


# =============================================================================
# 12. Load environmental covariates & merge
# =============================================================================
load(PATH_ENV_RDATA)
if (!exists("pacific_env_data")) stop("pacific_env_data not found after loading PATH_ENV_RDATA.")

pacific_tuna_model_data <- left_join(
  pacific_tuna_ll_data, pacific_env_data,
  by = c("Year", "Month", "Lon", "Lat")
) %>% as.data.frame()


# =============================================================================
# 13. Attach climate indices: MEI.v2 + ONI v5 + strict ENSO phase
# =============================================================================
mei_monthly <- read_mei_v2_monthly(year_min = YEAR_MIN, year_max = YEAR_MAX)

oni_long <- read_cpc_oni_v5_long() %>%
  filter(Year >= YEAR_MIN, Year <= YEAR_MAX)

oni_phase_tbl <- add_strict_phase3(oni_long)

pacific_tuna_model_data <- pacific_tuna_model_data %>%
  left_join(mei_monthly,   by = c("Year", "Month")) %>%
  left_join(oni_phase_tbl, by = c("Year", "Month"))

# Drop rows missing any ENSO index
pacific_tuna_model_data <- pacific_tuna_model_data %>%
  filter(!is.na(MEI), !is.na(ONI), !is.na(ENSO_Phase3))


# =============================================================================
# 14. Split into species-specific datasets
# =============================================================================

# ALB_N: North Pacific (Lat > 0), depth ≤ 300 m
northern_alb_model_data <- pacific_tuna_model_data %>%
  filter(Lat > 0) %>%
  select(-nCPUE_BET, -nCPUE_YFT, -BET, -YFT) %>%
  drop_depth_suffix_over(max_depth = 300) %>%
  na.omit()

# ALB_S: South Pacific (Lat ≤ 0), depth ≤ 300 m
southern_alb_model_data <- pacific_tuna_model_data %>%
  filter(Lat <= 0) %>%
  select(-nCPUE_BET, -nCPUE_YFT, -BET, -YFT) %>%
  drop_depth_suffix_over(max_depth = 300) %>%
  na.omit()

# BET: all depths (≤ 500 m retained by default)
bet_model_data <- pacific_tuna_model_data %>%
  select(-nCPUE_ALB, -nCPUE_YFT, -ALB, -YFT) %>%
  na.omit()

# YFT: depth ≤ 200 m
yft_model_data <- pacific_tuna_model_data %>%
  select(-nCPUE_ALB, -nCPUE_BET, -ALB, -BET) %>%
  drop_depth_suffix_over(max_depth = 200) %>%
  na.omit()

# Move CPUE column to the first position
northern_alb_model_data <- northern_alb_model_data %>% select(nCPUE_ALB, everything())
southern_alb_model_data <- southern_alb_model_data %>% select(nCPUE_ALB, everything())
bet_model_data          <- bet_model_data          %>% select(nCPUE_BET, everything())
yft_model_data          <- yft_model_data          %>% select(nCPUE_YFT, everything())


# =============================================================================
# 15. Save outputs (.RData)
# =============================================================================
save(pacific_tuna_ll_data,
     file = file.path(PATH_RDATA, "pacific_tuna_ll_data_1993_2023.RData"))

save(pacific_tuna_model_data,
     file = file.path(PATH_RDATA, "pacific_tuna_model_data_1993_2023.RData"))

save(northern_alb_model_data, southern_alb_model_data,
     bet_model_data, yft_model_data,
     file = file.path(PATH_RDATA, "tuna_model_datasets_1993_2023.RData"))

cat("Done.\n",
    "pacific_tuna_ll_data rows: ",    nrow(pacific_tuna_ll_data),    "\n",
    "pacific_tuna_model_data rows: ",  nrow(pacific_tuna_model_data), "\n",
    "ALB_N rows: ", nrow(northern_alb_model_data), "\n",
    "ALB_S rows: ", nrow(southern_alb_model_data), "\n",
    "BET rows: ",   nrow(bet_model_data),          "\n",
    "YFT rows: ",   nrow(yft_model_data),          "\n", sep = "")


# =============================================================================
# 16. Save outputs (.csv)
# =============================================================================
OUT_DIR <- file.path(ROOT, "Revise/Data_out")

f_alb_n <- file.path(OUT_DIR, sprintf("northern_alb_model_data_%d_%d.csv", YEAR_MIN, YEAR_MAX))
f_alb_s <- file.path(OUT_DIR, sprintf("southern_alb_model_data_%d_%d.csv", YEAR_MIN, YEAR_MAX))
f_bet   <- file.path(OUT_DIR, sprintf("bet_model_data_%d_%d.csv",          YEAR_MIN, YEAR_MAX))
f_yft   <- file.path(OUT_DIR, sprintf("yft_model_data_%d_%d.csv",          YEAR_MIN, YEAR_MAX))

write.csv(northern_alb_model_data, f_alb_n, row.names = FALSE)
write.csv(southern_alb_model_data, f_alb_s, row.names = FALSE)
write.csv(bet_model_data,          f_bet,   row.names = FALSE)
write.csv(yft_model_data,          f_yft,   row.names = FALSE)

cat("Saved CSV:\n",
    "ALB_N: ", f_alb_n, "\n",
    "ALB_S: ", f_alb_s, "\n",
    "BET:   ", f_bet,   "\n",
    "YFT:   ", f_yft,   "\n", sep = "")