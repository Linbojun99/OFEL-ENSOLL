# OFEL-ENSOLL
# ENSO Effects on Pacific Tuna Distribution and Abundance

Code for the manuscript submitted.

## Overview

This repository contains the R analysis pipeline for examining how ENSO (El Niño–Southern Oscillation) affects the spatial distribution and relative abundance of four Pacific tuna species using longline fishery data from 1993 to 2023.

**Species studied:**

| Code   | Species                        | Spatial domain   |
|--------|--------------------------------|------------------|
| ALB_N  | Albacore (*Thunnus alalunga*)  | North Pacific    |
| ALB_S  | Albacore (*Thunnus alalunga*)  | South Pacific    |
| BET    | Bigeye tuna (*Thunnus obesus*) | Pacific-wide     |
| YFT    | Yellowfin tuna (*Thunnus albacares*) | Pacific-wide |

## Pipeline

The scripts are designed to be run sequentially. Each script documents its inputs, outputs, and dependencies in its file header.

### Data preparation

| Script | Description |
|--------|-------------|
| `GCB_Data.R` | Read and clean WCPFC + IATTC longline data; aggregate to 5°×5° monthly grid; compute nominal CPUE per 1 000 hooks; merge environmental layers; download MEI.v2 and CPC ONI v5; assign strict ENSO phase; split into species-specific datasets |
| `GCB_envData.R` | Environmental data visualisation (depth profiles, correlation matrices, spatial maps); defines shared `theme_gcb()` |

### Species distribution modelling

| Script | Description |
|--------|-------------|
| `GCB_Model.R` | XGBoost Tweedie species distribution models (hyperparameter tuning via `mlr3`, spatial block CV, final model fit and prediction) |
| `GCB_Model_VIF.R` | VIF-based stepwise feature elimination with trace logging |
| `GCB_RFECV.R` | Recursive feature elimination with cross-validation (RFECV) score plots |
| `GCB_Model_instance.R` | Tuning instance diagnostic plots |
| `GCB_Model_Dia_Pl.R` | Model diagnostic plots (observed vs predicted, residuals, Q-Q) |
| `GCB_SHAP.R` | SHAP value computation and visualisation (importance bars + dependence plots) |

### ENSO–abundance analysis

| Script | Description |
|--------|-------------|
| `GCB_RA.R` | Relative abundance vs ENSO index analysis module (correlation, linear vs GAM, lag screening, GLS AR(1) phase contrasts, relative change vs Neutral with 95% CI) |
| `GCB_RA_Pl.R` | Three-panel figure: abundance–MEI relationship, ENSO phase violin plots, and relative change from Neutral → **Fig 1** |
| `GCB_RA_CCF.R` | Cross-correlation analysis (abundance vs MEI) → **Fig A (CCF)** |

### Centre of gravity analysis

| Script | Description |
|--------|-------------|
| `GCB_COG-A.R` | Centre of gravity (COG Lon/Lat) computation, GLS candidate models with harmonised AICc selection |
| `GCB_COG-A_Pl.R` | Three-panel COG figures (COG–MEI relationship, ENSO phase contrast, shift from Neutral) → **Fig 2** (Lon), **Fig 3** (Lat) |

### Spatial mapping

| Script | Description |
|--------|-------------|
| `GCB_Spatial_Pl.R` | ENSO phase spatial maps (Δ relative abundance % vs Neutral) |
| `GCB_Spatial_Lonlat_Pl.R` | Longitudinal and latitudinal profiles of abundance change, combined with spatial maps → **Fig 4** |

### Data exploration

| Script | Description |
|--------|-------------|
| `GCB_Data_Pl.R` | Spatial distribution and time-series plots for nominal CPUE → **Fig S1** |

## Data sources

- **Longline catch and effort**: [WCPFC public domain data](https://www.wcpfc.int/node/29510) and [IATTC public longline data](https://www.iattc.org/en-US/Data/Public-domain)
- **ENSO indices**: [NOAA MEI.v2](https://www.psl.noaa.gov/enso/mei/) and [CPC ONI v5](https://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php)
- **Environmental data**: Ocean temperature, salinity, dissolved oxygen, chlorophyll, net primary productivity, SSH, and MLD at multiple depth layers (from Copernicus Marine Service)

## Requirements

The analysis was developed with R ≥ 4.3. Key packages include:

- **Modelling**: `xgboost`, `mlr3`, `mlr3tuning`, `mlr3learners`, `shapviz`, `mgcv`, `nlme`
- **Data wrangling**: `tidyverse`, `lubridate`
- **Visualisation**: `ggplot2`, `patchwork`, `colorspace`, `metR`, `maps`
- **Utilities**: `car` (VIF), `rvest` (ONI web scraping), `ragg` + `svglite` (plot export)

## License

This project is licensed under the MIT License. See `LICENSE` for details.
