# CLAUDE.md — Project Context for Fsoil_aridlands

## Scientific Context

This project studies **ecosystem and soil respiration responses to rainfall pulse events** in an arid grassland ecosystem at the **US-Wkg site** (Walnut Gulch Experimental Watershed, Arizona). The central hypothesis is that post-rain "pulse" periods disproportionately drive annual respiration, and that separate models for pulse vs. non-pulse conditions outperform a single lumped model.

- **Eddy covariance data**: US-Wkg AmeriFlux, half-hourly, 2012–2020 (gap-filled & flux-partitioned)
- **Chamber data**: 7-port automated soil respiration chambers (LI-COR), 2017–2020
- **Key fluxes**: NEE, GPP, RECO (eddy); Rsoil (chambers)
- **Key drivers**: SWC (5/15/30 cm), Tsoil (5/15/30 cm), precipitation (P)
- **Season definitions**: Winter = DOY 305–366 & 1–59; Spring = DOY 60–181; Summer = DOY 182–304

---

## Project Structure

```
Fsoil_aridlands/
├── R/                        # Active refactored scripts (use these)
│   ├── 00_setup.R            # Packages, theme, helper fns (rmse/mape/rsq, fit_nlsLM)
│   ├── run_all.R             # Pipeline orchestrator — sources 01–14 in order
│   ├── 01–07_*.R             # Eddy tower pipeline
│   ├── 08–14_*.R             # Chamber pipeline
│   ├── 101–105_*.R           # QC and publication figures
│   └── Scripts_RESP.R        # Reusable helper function library
├── data/                     # Raw input data (do not modify)
├── out/
│   ├── derived/              # Intermediate CSVs (pipeline outputs)
│   ├── figs/                 # All figures; pub/ subfolder for publication-ready
│   ├── chamber/              # Chamber model outputs
│   └── model_eval/chamber/   # Model evaluation metrics and diagnostics
├── Archive/ & Deprecated/    # Old scripts — ignore
└── *.R (root)                # Legacy scripts — ignore; superseded by R/
```

---

## Pulse Classification Logic

A "pulse event" is triggered by a rainfall day with sum > 5 mm:
- Rain > 5 mm → 8-day pulse window
- Rain > 10 mm → 14-day pulse window
- Rain > 20 mm → 20-day pulse window
- Overlapping windows take the **maximum** duration

Key columns: `sum_R` (daily precip mm), `max_pulse_duration`, `days_since_rain_event`, `PulseFlag` (1 = pulse, 0 = non-pulse)

2012 is excluded from publication figures due to sparse coverage.

---

## Core Respiration Model

The same nonlinear model is used for both RECO (eddy) and Rsoil (chamber):

```
R = Fref × ((GPP/GPPmax + n)/(1+n)) × (1 - c4×(0.1 - SWC)²) × exp(b4×T)
```

| Parameter | Role |
|-----------|------|
| `Fref` | Baseline respiration scale |
| `n` | Sets minimum when GPP=0: floor = `n/(1+n)`; ceiling = 1 when GPP=GPPmax |
| `c4` | Moisture parabola curvature; **optimum fixed at SWC=0.1**; positive = has optimum, negative = inverts shape |
| `b4` | Temperature sensitivity (exponential, not Q10) |
| `GPPmax` | Normalization scalar — **must always be the global max across all days** |

- SWC is always expressed as a **fraction [0–1]** in model fitting (convert if values > 1.5)
- Fitted using `nlsLM` (Levenberg-Marquardt) from `minpack.lm`. Helper `fit_nlsLM()` in `00_setup.R`.

### Model Variants

Four variants are fitted and compared in each script:

| Variant | Trained on | Purpose |
|---------|-----------|---------|
| **All** | All days | Lumped baseline |
| **NonPulse (NP)** | Dry/non-pulse days | Captures background respiration |
| **Pulse (P)** | Wet/pulse days | Captures post-rain response |
| **Switch/Threshold** | NP and P separately | Applies NP or P parameters depending on conditions |

### Two Classification Schemes (keep consistent within a script)

The pipeline uses **two different ways** to define "pulse" vs "non-pulse". They are correlated but not equivalent — a pulse-window day can dry out below 15% SWC, and a non-pulse day can retain residual moisture above 15%.

| Scheme | Definition | Used in |
|--------|-----------|---------|
| **Rainfall-event** | Within rainfall-triggered window (from `02_define_pulses.R`) | `08_read_chamber.R` splits; `12_13`, `13`, `14` model fitting |
| **SWC-threshold** | `SWC ≥ 15%` (fraction ≥ 0.15) | `07` and `12` model fitting and switching |

**Rule**: the classification used for fitting must match the classification used for switching predictions. Do not mix them within a script.

---

## Scientific Rules — Never Violate

These rules encode scientific decisions made by the research team. They are not stylistic preferences. Any code edit that would violate one of these rules must be rejected or explicitly confirmed by the PI before proceeding.

1. **P-NP model uses rainfall-event classification exclusively.**
   Training data, prediction, and switch logic for the P-NP model must always use the rainfall-event criterion: `max_pulse_duration > 0` for pulse, `max_pulse_duration == 0` for non-pulse. Never substitute the SWC threshold for this model.

2. **15% threshold model uses SWC classification exclusively.**
   Training data, prediction, and switch logic for the threshold model must always use `meanSWC >= 0.15` (fraction). Never substitute rainfall-event labels for this model.

3. **The two classification schemes are scientifically distinct and never interchangeable.**
   The purpose of the study is to compare models built on rainfall-event vs SWC-threshold definitions. Mixing them within a script is a scientific error, not just a coding error.

4. **GPPmax must always be the global maximum from the full dataset.**
   Never compute GPPmax from a subset (e.g. pulse days only, non-pulse days only, a single year). Both fitting and prediction must use the same global GPPmax scalar.

5. **Cross-validation must refit the model inside the loop.**
   Any leave-one-year-out or other held-out CV must refit the model on training years only, then predict the held-out year. Never pass full-data predictions into a CV evaluation of a held-out period.

6. **The moisture term must never produce negative predictions.**
   The term `1 - c4*(0.1 - SWC)²` must remain non-negative across all observed SWC values. The upper bound on `c4` must be constrained to `1/(SWC_max_observed - 0.1)²`, or predictions must be clamped to ≥ 0 post-hoc.

7. **Study site and period are fixed.**
   The site is Walnut Gulch Kendall (US-Wkg), Sonoran Desert. The analysis period is 2012–2020 for eddy covariance and 2017–2020 for chamber measurements. Any changes to data filtering or date ranges must be explicitly confirmed by the PI before implementation.

8. **Do not upgrade R or install new packages without explicit instruction.**
   Figure-related packages (`ggrepel`, `ggpubr`) are not required for the analysis scripts (01–14). Do not install them as part of bug-fix work.

9. **Bug fixes and documentation changes must be in separate commits.**
   Never mix scientific corrections with cosmetic edits, comment changes, or documentation updates in the same commit.

10. **Confirm the active branch before running any script that writes output files.**
    Before running any script that produces output in `out/`, confirm the active branch and verify it is not `master`.

---

## Known Bugs (identified 2026-02-26)

### ~~Bug 1~~ — `R/07_threshold15_RECO.R`: GPPmax mismatch between fitting and prediction — **FIXED** (branch `bug-fixes`, commit e7f91c5)
- **Was**: NP and Pulse models fitted using per-subset GPPmax; predictions used global `All_GPPmax`. Parameters `Fref` and `n` were calibrated under a different normalization than used at prediction time.
- **Fix applied**: removed per-subset `GPPmax_NP_15` / `GPPmax_P_15` columns; changed `m_np` and `m_p` formulas to use `All_meanGPP/All_GPPmax`, consistent with `m_all` and all prediction blocks.

### ~~Bug 2~~ — `R/07_threshold15_RECO.R` lines 55–60: Classification mismatch in `Reco_Combined` — **FIXED** (branch `bug-fixes`)
- **Was**: Models are fitted on SWC-threshold splits (`SWC < 15%` vs `SWC ≥ 15%`), but `Reco_Combined` switched using rainfall-event labels (`max_pulse_duration == 0` vs `c(8,14,20)`), with a spurious `MeanM_15` fallback.
- **Fix applied**: replaced `case_when` with `ifelse(All_meanSWC5 >= 0.15, PulseM_15, NonPulseM_15)`; `max_pulse_duration` replaced by `All_meanSWC5` in the `select()`.

### ~~Bug 3~~ — `R/12_13_chamber_models.R` lines 86 & 92: Classification mismatch in `pred_Thr` — **FIXED** (branch `bug-fixes`)
- **Was**: NP and Pulse models fitted on rainfall-event-based splits (from `NonPulse_sum_chamber.csv` / `Pulse_sum_chamber.csv`), but `pred_Thr` switched using SWC: `ifelse(meanSWC >= 0.15, pred_P, pred_NP)`.
- **Fix applied**: added `max_pulse_duration` to the `select()` on line 86; changed switch on line 92 to `ifelse(max_pulse_duration > 0, pred_P, pred_NP)`, matching the rainfall-event criterion used in training.

### Bug 4 — `R/14_Robust_RsoilModels.R` lines 217–228: `cv_year_block_threshold` is not true CV
- **Problem**: The function receives pre-computed `pred_NP_all` and `pred_P_all` from models fitted on **all data including the held-out year**. There is data leakage — the reported `cv_Thr` is optimistic and cannot be compared fairly to `cv_All`, `cv_NP`, and `cv_P` which are true leave-one-year-out.
- **Fix**: refit the NP and P component models on training years inside the loop, then apply the switch to the held-out year.

### Bug 5 — All model scripts: moisture term can go negative at high SWC
- **Problem**: `1 - c4×(0.1 - SWC)²` goes negative when `c4 > 1/(SWC - 0.1)²`. With upper bounds of c4=50–200 and pulse-period SWC reaching 0.25–0.35, predictions become physically impossible (negative respiration). No clamp is applied after prediction.
- **Example**: c4=50, SWC=0.25 → moisture term = `1 - 50×0.0225 = −0.125`.
- **Fix**: tighten c4 upper bound to `1/(SWC_max_observed - 0.1)²`, or clamp predictions to ≥ 0 after computing.

---

## Data Flow (Derived Files)

```
Raw AmeriFlux HH CSVs
  └─► 01_read_eddy.R → out/derived/USWkg12_20_summary.csv
        └─► 02_define_pulses.R → years_sum1_DM.csv          ← all days + pulse labels
                               → years_sum_Pulse0_DM.csv     ← non-pulse days
                               → years_sum_Pulse1_DM.csv     ← pulse days

Raw chamber CSV (KN_soil_resp17_20_longHead.csv)
  └─► 08_read_chamber.R → All_summary_chamber.csv
                        → Pulse_sum_chamber.csv
                        → NonPulse_sum_chamber.csv
        └─► 12_13_chamber_models.R → Chamber_model_predictions_12_13.csv
            14_Robust_RsoilModels.R → Chamber_model_predictions_14.csv
                                    → out/model_eval/chamber/14/params_metrics.csv
```

Alternative entry point: `101_ingest_daily_pulses_min.R` replaces 01+02 and writes `eddy_daily_with_pulse.csv`.

---

## Key Column Name Conventions

| Raw AmeriFlux name | Renamed to |
|--------------------|------------|
| `SWC_1_1_1`        | `SWC5`     |
| `SWC_1_2_1`        | `SWC15`    |
| `SWC_1_3_1`        | `SWC30`    |
| `TS_1_1_1`         | `ST5`      |
| `TS_1_2_1`         | `ST15`     |
| `TS_1_3_1`         | `ST30`     |
| `TA_1_2_1`         | `AT2`      |
| `TA_1_1_1`         | `AT6`      |
| `RH_1_2_1`         | `RH2`      |
| `RH_1_1_1`         | `RH6`      |

Chamber ports renamed from e.g. `Port.1Soil.Resp.um.co2.m2.s` → `Rsoil1`, `Port.1VWC` → `VWC1`, `Port.1Soil.Temp.deg.C` → `Tsoil1` (ports 1–7).

---

## Branches

| Branch | Purpose |
|--------|---------|
| `master` | Main branch — target for PRs |
| `PulseResp_Aridlands_refac` | Refactored pipeline (Anastasia Makhnykina + Dave Moore); base for bug-fixes |
| `bug-fixes` | **Current working branch** — fixing the known model bugs; branched from `PulseResp_Aridlands_refac` |

---

## Running the Pipeline

**Repository**: https://github.com/davidjpmoore/Fsoil_aridlands

### Getting the code

```bash
# First time
git clone https://github.com/davidjpmoore/Fsoil_aridlands.git
cd Fsoil_aridlands
git checkout bug-fixes

# Already cloned
git fetch origin
git checkout bug-fixes
git pull
```

### Running scripts

Always run from the **project root** (the folder containing `R/` and `data/`). Open `SoilvEcorespiration.Rproj` in RStudio to ensure this automatically.

```r
# Full pipeline
source("R/run_all.R")

# Just the eddy RECO model (script 07 and its dependencies)
source("R/01_read_eddy.R")
source("R/02_define_pulses.R")
source("R/07_threshold15_RECO.R")

# Just the chamber models (scripts 12/13/14 and their dependency)
source("R/08_read_chamber.R")
source("R/12_13_chamber_models.R")
source("R/14_Robust_RsoilModels.R")
```

**Note**: raw data files in `data/` are not in the repo. They must be present locally before running scripts 01 or 08.

### Key outputs to check after running script 07

| File | What it tells you |
|------|------------------|
| `out/derived/metrics_RECO_15.csv` | RMSE, MAPE, R² for all model variants |
| `out/figs/reco_ts_overlay_15.png` | Time-series of observed vs modelled RECO |
| `out/figs/reco_cumulative_15.png` | Cumulative RECO — good for spotting systematic bias |
| R console | Printed coefficients for `m_np`, `m_p`, `m_all` |

---

## Full Pipeline Run Required Before Manuscript Results

> **The 2012–2018 AmeriFlux half-hourly eddy file is missing from the repository.**
>
> The file `data/AddedPartionedCflux_US-Wkg_HH_201212312330_201812312330.csv` is required
> by `01_read_eddy.R` to regenerate `out/derived/years_sum1_DM.csv` from raw data. Until
> it is obtained and a full pipeline run is completed (scripts 01 → 02 → 07), the eddy
> summary file in `out/derived/` is a manually copied placeholder from `data/years_sum1_DM.csv`.
>
> **All model outputs and metrics produced while this file is missing must be treated as
> provisional.** A full run from raw data — `source("R/run_all.R")` — is required before
> any results are used in the manuscript.
>
> Scripts that depend on this file carry a prominent `WARNING` comment at the top:
> `05_gpp_vs_reco.R`, `07_threshold15_RECO.R`, `08_read_chamber.R`,
> `103_Disprop_Pulse_Impact.R`, `104_Temp_Moisture_Space_Figure.R`,
> `105_Temp_Moisture_DELTA_SuppFigure.R`.

---

## Open Architectural Questions (resolve before further development)

### 1. Three-way RECO comparison not yet implemented

The manuscript comparison requires three full-record predictions for the same flux
variable: (1) lumped MeanAll, (2) SWC-threshold switch, and (3) rainfall-event switch.

For **Rsoil (chamber)**, the two switch models exist in separate legacy scripts:
- `12_threshold15_RSOIL.R` — SWC-threshold switch (trained and switched on SWC ≥ 15%)
- `13_chamber_model.R` — rainfall-event splits (no switch; `12_13_chamber_models.R` adds the switch)

For **RECO (eddy)**, the rainfall-event switch model **does not exist anywhere in the
codebase**. Script `07_threshold15_RECO.R` produces only the SWC-threshold switch.
A rainfall-event switch for RECO would require fitting separate NP/Pulse models on
`max_pulse_duration == 0` and `> 0` splits, then applying an event-based switch —
this has not been implemented.

**Do not add this without explicit PI confirmation of the design.**

### 2. Relationship between scripts 12, 13, and 12_13 needs clarification

Three scripts fit Rsoil chamber models:

| Script | Classification | Switch | Status in run_all.R |
|--------|---------------|--------|---------------------|
| `12_threshold15_RSOIL.R` | SWC ≥ 15% | Yes (pred_15) | Not called |
| `13_chamber_model.R` | Rainfall-event | No switch | Not called |
| `12_13_chamber_models.R` | Rainfall-event | Yes (Pred_Threshold) | Called |

`run_all.R` only calls `12_13_chamber_models.R`. The legacy scripts 12 and 13 are
superseded but not deleted. `99_chamber_model_investigation.R` reads outputs from the
legacy 12 and 13 scripts, which are no longer produced by the active pipeline.

**Before further development:** confirm which scripts are canonical, whether 12 and 13
should be retired, and what the intended three-way comparison structure is for both
RECO and Rsoil.

---

## Notes & Gotchas

- Run all scripts from the **project root**, not from inside `R/`. Scripts use `if (file.exists("R/00_setup.R"))` to handle both cases.
- SWC in raw data is in **percent (%)**; model fitting requires **fraction [0–1]**. Scripts check `max(SWC) > 1.5` and divide by 100 if needed.
- Missing/fill values: `-9999` and `NaN` are treated as `NA` on read.
- The `12_13_chamber_models.R` script is what `run_all.R` actually calls (not `12_threshold15_RSOIL.R` or `13_chamber_model.R` individually).
- `14_Robust_RsoilModels.R` is the most rigorous modelling script — it adds multi-start optimization (30 starts), year-blocked CV, and threshold grid search.
- Publication figures go to `out/figs/pub/` (scripts 103, 104, 105).
- **Two open bugs remain** (Bugs 4–5 above) — do not assume model output is fully correct until these are fixed. Bugs 1, 2, and 3 are resolved on `bug-fixes`.
