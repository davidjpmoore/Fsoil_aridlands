# run_all.R -------------------------------------------------------------------
# Master pipeline script for the refactored Fsoil_aridlands analysis.
#
# This script runs the full analysis pipeline for the US-Wkg (Walnut Gulch
# Kendall) arid grassland respiration pulse study (2012-2020 eddy covariance;
# 2017-2020 chamber data).  It is organised into three streams:
#
#   Stream 1 - Eddy covariance: read raw data, define pulses, EDA figures, RECO model
#   Stream 2 - Chamber:         read raw data, EDA figures, Rsoil models
#   Stream 3 - Publication figures: shared style + manuscript/supplement figures
#
# Run from the project root (the folder containing R/ and data/):
#   source("R/run_all.R")
#
# Scripts deliberately NOT sourced here:
#   12_threshold15_RSOIL.R  -- superseded by 12_13_chamber_models.R; retained
#                               for legacy reference only (PI decision pending)
#   13_chamber_model.R      -- superseded by 12_13_chamber_models.R; retained
#                               for legacy reference only (PI decision pending)
#   101_ingest_daily_pulses_min.R -- alternative entry point replacing 01+02;
#                                    not used in the primary pipeline
#   102_qc_plots_min.R      -- companion to 101; not in primary pipeline
# -----------------------------------------------------------------------------

pipeline_start <- proc.time()

# --- 0. Working-directory guard ----------------------------------------------
if (!dir.exists("R") || !dir.exists("data")) {
  stop(
    "run_all.R must be sourced from the project root ",
    "(the folder containing R/ and data/)."
  )
}

# --- 1. Raw data file check --------------------------------------------------
# Halt immediately with a descriptive error if any required raw input is absent.
required_raw <- c(
  "data/AddedPartionedCflux_US-Wkg_HH_201212312330_201812312330.csv",
  "data/Wkg_Ameriflux_2017-2020 with added partitioning/GapfilledPartitionedFluxes_US-Wkg_HH_201812312330_201912312330.csv",
  "data/Wkg_Ameriflux_2017-2020 with added partitioning/GapfilledPartitionedFluxes_US-Wkg_HH_201912312330_202012302330.csv",
  "data/KN_soil_resp17_20_longHead.csv"
)

missing_raw <- required_raw[!file.exists(required_raw)]
if (length(missing_raw) > 0) {
  stop(
    "Required raw data file(s) not found - pipeline cannot proceed.\n",
    "Missing:\n",
    paste0("  ", missing_raw, collapse = "\n"), "\n\n",
    "Obtain these files from AmeriFlux (US-Wkg) or the PI's data archive ",
    "and place them in data/ before re-running."
  )
}
message("Raw data check passed - all ", length(required_raw), " required files present.")

# --- 2. Shared setup ---------------------------------------------------------
source("R/00_setup.R")

# Helper: source a script with before/after timestamps
run_script <- function(path) {
  message("\n[", format(Sys.time(), "%H:%M:%S"), "] >> Starting ", path)
  sys.source(path, envir = .GlobalEnv, chdir = FALSE)
  message("[", format(Sys.time(), "%H:%M:%S"), "] << Finished ", path)
}

# =============================================================================
# STREAM 1 -- Eddy covariance pipeline
# =============================================================================
message("\n", strrep("=", 70))
message("STREAM 1: Eddy covariance pipeline")
message(strrep("=", 70))

# Read and aggregate half-hourly AmeriFlux eddy covariance data (2012-2020)
# into daily summaries -> out/derived/USWkg12_20_summary.csv
run_script("R/01_read_eddy.R")

# Classify days into rainfall-pulse windows (>5/10/20 mm thresholds) and
# write daily CSVs: years_sum1_DM.csv, years_sum_Pulse0_DM.csv,
# years_sum_Pulse1_DM.csv -> out/derived/
run_script("R/02_define_pulses.R")

# REVIEW NEEDED -- included for completeness, confirm role before final pipeline
# Histograms of rain events >5 mm and inter-event dry-spell durations
# -> out/figs/rain_gt5_hist.png, dry_spell_hist.png
run_script("R/03_rain_pulse_figs.R")

# REVIEW NEEDED -- included for completeness, confirm role before final pipeline
# Season-tagged RECO histograms during pulse events (Winter/Spring/Summer)
# and composite pulse-response curves (RECO + SWC) for hand-picked windows
# -> out/figs/season_*_reco_hist.png, composite_*.png
run_script("R/04_seasonal_pulses.R")

# REVIEW NEEDED -- included for completeness, confirm role before final pipeline
# Scatter plots of GPP vs RECO with linear regression for All / Pulse /
# Non-pulse periods -> out/figs/gpp_vs_reco_*.png
# NOTE: requires ggpubr; produces no derived data, diagnostic use only
run_script("R/05_gpp_vs_reco.R")

# REVIEW NEEDED -- included for completeness, confirm role before final pipeline
# Bubble plots of SWC vs soil temperature sized by RECO magnitude,
# separately for pulse and non-pulse periods -> out/figs/bubble_*.png
run_script("R/06_swc_st_space.R")

# Fit RECO nonlinear models (All-data, NonPulse, Pulse, 15%-SWC-threshold
# switch) using nlsLM; write metrics -> out/derived/metrics_RECO_15.csv
run_script("R/07_threshold15_RECO.R")

# Fit RECO nonlinear models using rainfall-event P-NP switch (ported from
# legacy/Eddy model_2.R with Bug 1 and formula fixes); write metrics
# -> out/derived/metrics_RECO_PNP.csv
run_script("R/07b_pnp_RECO.R")

# =============================================================================
# STREAM 2 -- Chamber pipeline
# =============================================================================
message("\n", strrep("=", 70))
message("STREAM 2: Chamber pipeline")
message(strrep("=", 70))

# Read raw LI-COR chamber data (2017-2020), merge with pulse flags from 02,
# compute daily port-averaged summaries -> out/derived/All_summary_chamber.csv,
# Pulse_sum_chamber.csv, NonPulse_sum_chamber.csv
run_script("R/08_read_chamber.R")

# REVIEW NEEDED -- included for completeness, confirm role before final pipeline
# Minimal script; prints a confirmation message that the pulse/non-pulse chamber
# splits were already created by 08_read_chamber.R. No output files produced.
run_script("R/09_pulse_np_chamber.R")

# REVIEW NEEDED -- included for completeness, confirm role before final pipeline
# Scatter plots of GPP vs Rsoil with linear regression for All / Pulse /
# Non-pulse chamber periods -> out/figs/gpp_vs_rsoil_*.png
# NOTE: requires ggpubr; produces no derived data, diagnostic use only
run_script("R/10_gpp_vs_rsoil_chamber.R")

# REVIEW NEEDED -- included for completeness, confirm role before final pipeline
# Bubble plots of SWC vs Tsoil sized by Rsoil, separately for pulse and
# non-pulse chamber periods -> out/figs/st_vs_rsoil_pulse.png,
# swc_vs_rsoil_nonpulse.png
run_script("R/11_swc_st_chamber.R")

# Fit Rsoil nonlinear models (All, NonPulse, Pulse, rainfall-event P-NP switch)
# using nlsLM -> out/derived/Chamber_model_predictions_12_13.csv,
# out/model_eval/chamber/params_metrics_12_13.csv
run_script("R/12_13_chamber_models.R")

# Multi-start optimisation (30 starts), year-blocked leave-one-out CV,
# and threshold grid search for robust Rsoil model comparison
# -> out/derived/Chamber_model_predictions_14.csv,
#    out/model_eval/chamber/14/params_metrics.csv
run_script("R/14_Robust_RsoilModels.R")

# Figures 6e-6h: Rsoil model time-series and cumulative overlays (mirrors
# Figs 6a-6d for RECO) using pre-computed predictions from script 14
# -> out/figs/Fig6e_RsoilModels_Thr_TimeSeries.png
# -> out/figs/Fig6f_RsoilModels_Thr_Cumulative.png
# -> out/figs/Fig6g_RsoilModels_PNP_TimeSeries.png
# -> out/figs/Fig6h_RsoilModels_PNP_Cumulative.png
run_script("R/15_Rsoil_figs.R")

# =============================================================================
# STREAM 3 -- Publication figures
# =============================================================================
message("\n", strrep("=", 70))
message("STREAM 3: Publication figures")
message(strrep("=", 70))

# Load shared ggplot2 theme (theme_in_ticks*), pulse colour palette (PAL_PULSE),
# heatmap colour scale, point/bar constants, save dimensions, and axis label
# expressions used by all publication figure scripts (103, 104, 105).
# No data files read; no side effects beyond defining objects.
run_script("R/000_figure_style.R")

# Disproportionate pulse contribution figure: pulse-day fraction vs pulse
# fraction of annual RECO, plus annual RECO bar chart
# -> out/figs/pub/Disprop_Pulse_Impact_legend.png
run_script("R/103_Disprop_Pulse_Impact.R")

# Temperature x moisture state-space figure: SWC vs Tsoil bubble plot
# coloured by pulse/non-pulse, sized by RECO
# -> out/figs/pub/Temp_Moisture_Space_Figure.png
run_script("R/104_Temp_Moisture_Space_Figure.R")

# Supplementary DELTA-RECO heatmap: diverging colour grid of RECO difference
# (pulse minus non-pulse) across SWC x Tsoil bins
# -> out/figs/pub/Temp_Moisture_DELTA_SuppFigure.png,
#    out/figs/pub/Temp_Moisture_Space_BubbleOnly.png
run_script("R/105_Temp_Moisture_DELTA_SuppFigure.R")

# =============================================================================
# Done
# =============================================================================
elapsed <- proc.time() - pipeline_start
message(
  "\n", strrep("=", 70), "\n",
  "Pipeline complete.  Total elapsed time: ",
  sprintf("%.1f", elapsed["elapsed"]), " seconds.\n",
  strrep("=", 70)
)
