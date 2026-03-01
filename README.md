# Fsoil_aridlands

Ecosystem and soil respiration responses to rainfall pulse events in an arid grassland at the **US-Wkg** site (Walnut Gulch Experimental Watershed, Arizona), 2012–2020. The project tests whether separate respiration models for pulse (post-rain) and non-pulse (dry background) periods outperform a single lumped model for both eddy-covariance RECO and chamber-based Rsoil.

---

## Requirements

- **R ≥ 4.3**
- Packages: `tidyverse`, `minpack.lm`, `ggpubr`, `ggrepel`, `units`, `glue`, `rlang`
- Raw data files in `data/` (not in the repository — obtain separately)

---

## Running the pipeline

Always run from the **project root** (the folder containing `R/` and `data/`).

```r
# Full pipeline (scripts 01–14 in order)
source("R/run_all.R")
```

Individual scripts can be sourced directly in the order listed below.

---

## Script descriptions and run order

### Setup

| Script | Description |
|--------|-------------|
| `R/00_setup.R` | Loads required packages, sets the shared ggplot theme, creates output directories, and defines reusable helper functions (`rmse`, `mape`, `rsq`, `fit_nlsLM`, `save_plot`). Sourced automatically by every other script. |

---

### Eddy-tower pipeline (scripts 01–07)

These scripts process the half-hourly AmeriFlux eddy-covariance data and culminate in the RECO model fits.

| Order | Script | Inputs | Key outputs | Description |
|-------|--------|--------|-------------|-------------|
| 1 | `01_read_eddy.R` | Raw AmeriFlux HH CSVs (`data/`) | `out/derived/USWkg12_20_summary.csv` | Reads three AmeriFlux half-hourly files (2012–2020), renames columns to project conventions, and writes a daily-mean summary with precipitation, SWC, soil temperature, NEE, GPP, and RECO. |
| 2 | `02_define_pulses.R` | `USWkg12_20_summary.csv` | `years_sum1_DM.csv`, `years_sum_Pulse0_DM.csv`, `years_sum_Pulse1_DM.csv` | Defines rainfall-event pulse windows (>5 mm → 8 days; >10 mm → 14 days; >20 mm → 20 days; overlapping windows take the maximum). Adds `max_pulse_duration`, `days_since_rain_event`, and `PulseFlag` columns. |
| 3 | `03_rain_pulse_figs.R` | `USWkg12_20_summary.csv` | Figures in `out/figs/` | Exploratory figures: histogram of rain event sizes, inter-event dry-spell lengths, and pulse timing. |
| 4 | `04_seasonal_pulses.R` | `USWkg12_20_summary.csv` | Figures in `out/figs/` | Plots seasonal distributions of pulse events (Winter/Spring/Summer) and their rainfall totals. |
| 5 | `05_gpp_vs_reco.R` | `USWkg12_20_summary.csv` | Figures in `out/figs/` | Visualises the relationship between GPP and RECO for pulse vs non-pulse days, split by season. |
| 6 | `06_swc_st_space.R` | `USWkg12_20_summary.csv` | Figures in `out/figs/` | Plots soil water content vs soil temperature state space for eddy-tower days, colour-coded by pulse/non-pulse and season. |
| 7 | `07_threshold15_RECO.R` | `years_sum1_DM.csv` | `out/derived/metrics_RECO_15.csv`, figures in `out/figs/` | Fits three RECO models (All-data, NonPulse, Pulse) using `nlsLM` with global GPPmax, splitting on the SWC ≥ 15% threshold. Builds a `Reco_Combined` switched prediction (SWC-based switch consistent with training splits). Writes RMSE/MAPE/R² metrics and time-series and cumulative plots. |

**Alternative entry point:** `R/101_ingest_daily_pulses_min.R` replaces scripts 01 + 02 in a single step and writes `out/derived/eddy_daily_with_pulse.csv`.

---

### Chamber pipeline (scripts 08–14)

These scripts process the LI-COR automated chamber data and fit the Rsoil models.

| Order | Script | Inputs | Key outputs | Description |
|-------|--------|--------|-------------|-------------|
| 8 | `08_read_chamber.R` | `data/KN_soil_resp17_20_longHead.csv`, `years_sum1_DM.csv` | `All_summary_chamber.csv`, `Pulse_sum_chamber.csv`, `NonPulse_sum_chamber.csv` | Reads raw 7-port chamber data, joins pulse definitions from the eddy daily summary, renames port columns, computes per-port means and daily averages, and writes three split files (all days / pulse days / non-pulse days) using the rainfall-event criterion (`pulseIND`). |
| 9 | `09_pulse_np_chamber.R` | — | — | Placeholder script; the pulse/non-pulse splits are already produced by `08_read_chamber.R`. |
| 10 | `10_gpp_vs_rsoil_chamber.R` | Chamber summary CSVs | Figures in `out/figs/` | Plots GPP vs Rsoil relationships from chamber data for pulse and non-pulse periods. |
| 11 | `11_swc_st_chamber.R` | Chamber summary CSVs | Figures in `out/figs/` | Plots soil water content vs soil temperature state space for chamber measurement days. |
| 12/13 | `12_13_chamber_models.R` | `All_summary_chamber.csv`, `Pulse_sum_chamber.csv`, `NonPulse_sum_chamber.csv` | `out/model_eval/chamber/params_metrics_12_13.csv`, `out/derived/Chamber_model_predictions_12_13.csv`, figures | Fits All, NonPulse, and Pulse Rsoil models using global GPPmax and `nlsLM`. Builds `pred_Thr`, a switched prediction that applies the Pulse model on rainfall-event days and the NonPulse model otherwise (`max_pulse_duration > 0`), consistent with how the NP/P models were trained. Writes parameters, RMSE/R²/AIC metrics, annual-sums plot, and obs-vs-pred scatter. |
| 14 | `14_Robust_RsoilModels.R` | `All_summary_chamber.csv`, `Pulse_sum_chamber.csv`, `NonPulse_sum_chamber.csv` | `out/model_eval/chamber/14/params_metrics.csv`, figures in `out/figs/14/` | Most rigorous modelling script. Uses 30 multi-start fits, leave-one-year-out cross-validation for All/NP/P models, and a threshold grid search. Reports MBE, MAE, RMSE, R², and AIC alongside parameter correlation and driver collinearity diagnostics. |

---

### QC and publication figures (scripts 101–105)

These scripts are run after the main pipeline and do not modify derived data.

| Script | Description |
|--------|-------------|
| `101_ingest_daily_pulses_min.R` | Minimal replacement for scripts 01 + 02: ingests eddy HH files and writes `eddy_daily_with_pulse.csv` in one step. |
| `102_qc_plots_min.R` | QC plots from `eddy_daily_with_pulse.csv`: rainfall histogram, dry-spell lengths, SWC and temperature time series. |
| `103_Disprop_Pulse_Impact.R` | Publication figure: disproportionate pulse impact — days per class per year, annual RECO totals by class, disproportion panel. Writes to `out/figs/pub/`. |
| `104_Temp_Moisture_Space_Figure.R` | Publication figure: temperature–moisture state space coloured by pulse/non-pulse. Writes to `out/figs/pub/`. |
| `105_Temp_Moisture_DELTA_SuppFigure.R` | Supplementary figure: delta (pulse minus non-pulse) in temperature–moisture space. Writes to `out/figs/pub/`. |

---

## Repository structure

```
Fsoil_aridlands/
├── R/                    # Active scripts (run these)
│   ├── 00_setup.R
│   ├── run_all.R         # Orchestrates 01–14
│   ├── 01–07_*.R         # Eddy tower pipeline
│   ├── 08–14_*.R         # Chamber pipeline
│   ├── 101–105_*.R       # QC and publication figures
│   └── Scripts_RESP.R    # Reusable helper library
├── data/                 # Raw input data (not in repo)
├── out/
│   ├── derived/          # Intermediate CSVs
│   ├── figs/             # Figures (pub/ subfolder for publication-ready)
│   ├── chamber/          # Chamber model outputs
│   └── model_eval/       # Model metrics and diagnostics
├── Archive/ & Deprecated/ # Old scripts — do not use
└── CHANGES.md            # Bug fix log
```

---

## Branches

| Branch | Purpose |
|--------|---------|
| `master` | Main branch |
| `bug-fixes` | Active bug-fix branch (branched from `PulseResp_Aridlands_refac`) |
| `PulseResp_Aridlands_refac` | Refactored pipeline base |

See `CHANGES.md` for the current bug status.
