# Fsoil_aridlands

Ecosystem and soil respiration responses to rainfall pulse events at the Walnut Gulch
Kendall Grassland (US-Wkg), a semi-arid grassland in southeastern Arizona. The study
tests whether separate respiration models for pulse (post-rain) and non-pulse (dry
background) periods outperform a single lumped model, using eddy covariance RECO
(2012–2020) and automated soil chamber Rsoil (2017–2020). Two classification schemes
are compared: rainfall-event windows and a soil water content threshold.

---

## Repository structure

```
Fsoil_aridlands/
├── R/                        # Active analysis scripts — run these
│   ├── run_all.R             # Full pipeline orchestrator (scripts 01–14 + pub figs)
│   ├── 00_setup.R            # Packages, shared theme, helper functions
│   ├── 01–07b_*.R            # Eddy covariance pipeline (read → pulses → RECO models)
│   ├── 08–14_*.R             # Chamber pipeline (read → Rsoil models)
│   └── 101–105_*.R           # QC diagnostics and publication figures
├── data/                     # Raw input files (not tracked — see below)
├── out/                      # All pipeline outputs (not tracked by git)
│   ├── derived/              # Intermediate CSVs and model predictions
│   ├── figs/                 # Figures; pub/ subfolder for publication-ready
│   └── model_eval/           # Model metrics, parameter tables, diagnostics
└── legacy/                   # Read-only reference material
    ├── Eddy model_2.R        # Original RECO model (Anastasia Makhnykina, 1162 lines)
    ├── Chamber model.R       # Original Rsoil model (Anastasia Makhnykina, 1187 lines)
    ├── 15% Threshold.R       # Original SWC-threshold RECO model
    ├── 15% for Rsoil.R       # Original SWC-threshold Rsoil model
    ├── root_scripts/         # Older root-level scripts superseded by R/
    └── figures_exploratory/  # Pre-pipeline exploratory figures
```

---

## Raw data requirements

The following four files must be present in `data/` before the pipeline can run.
They are not included in this repository and must be obtained from the data providers.

| File | Description |
|------|-------------|
| `data/AddedPartionedCflux_US-Wkg_HH_201212312330_201812312330.csv` | AmeriFlux half-hourly gap-filled and flux-partitioned eddy covariance data, US-Wkg, 2012–2018 |
| `data/Wkg_Ameriflux_2017-2020 with added partitioning/GapfilledPartitionedFluxes_US-Wkg_HH_201812312330_201912312330.csv` | AmeriFlux half-hourly eddy covariance data, US-Wkg, 2018–2019 |
| `data/Wkg_Ameriflux_2017-2020 with added partitioning/GapfilledPartitionedFluxes_US-Wkg_HH_201912312330_202012302330.csv` | AmeriFlux half-hourly eddy covariance data, US-Wkg, 2019–2020 |
| `data/KN_soil_resp17_20_longHead.csv` | LI-COR 7-port automated soil respiration chamber data, Kendall site, 2017–2020 |

> **Note:** The 2018–2019 and 2019–2020 eddy files must be placed inside a subdirectory
> named exactly `data/Wkg_Ameriflux_2017-2020 with added partitioning/` — the space
> and capitalisation in the folder name must be preserved exactly as shown.

---

## How to run

```bash
git clone https://github.com/davidjpmoore/Fsoil_aridlands.git
cd Fsoil_aridlands
git checkout bug-fixes
```

Copy the four raw data files into `data/`, preserving the subdirectory structure
described above. Then, in the R console:

```r
source("R/run_all.R")
```

The pipeline checks for all four raw files before starting and halts with a
descriptive error if any are missing. Always run from the project root (the folder
containing `R/` and `data/`).

---

## Output

All outputs are written to `out/`, which is excluded from version control via
`.gitignore`. A complete pipeline run produces derived CSVs in `out/derived/`,
figures in `out/figs/`, and model evaluation tables in `out/model_eval/`.

---

## Branch status

`bug-fixes` is the current working branch. Five modelling bugs identified in the
legacy scripts have been corrected (GPPmax normalisation mismatch, classification
scheme mismatch in switch predictions, data leakage in cross-validation, and
unclamped moisture term). The rainfall-event P-NP switch model for RECO
(`R/07b_pnp_RECO.R`) has been added and the full pipeline verified end-to-end.
The branch is ready for PI review before merging to `master`.

See `CHANGES.md` for full details.

---

## Citation and data access

US-Wkg AmeriFlux data: Russell Scott, USDA.
Data available via the AmeriFlux network (https://ameriflux.lbl.gov, site ID US-Wkg).
