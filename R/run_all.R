# run_all.R ---------------------------------------------------------------

# Ensure we're at the project root (the folder that contains R/ and data/)
# If you routinely run this via `source("R/run_all.R")` from the root,
# the wd is already correct. Otherwise, uncomment one of the strategies below.

# --- Strategy A (assert root, don't change it automatically)
if (!dir.exists("R") || !dir.exists("data")) {
  stop("Please run run_all.R from the project root (folder that contains R/ and data/).")
}

# --- Strategy B (auto-hop to root when called from inside R/)
# if (basename(getwd()) == "R" && dir.exists("../data")) setwd("..")

# Load setup once (optional; scripts also source it)
source("R/00_setup.R")

scripts <- file.path("R", c(
  "01_read_eddy.R",
  "02_define_pulses.R",
  "03_rain_pulse_figs.R",
  "04_seasonal_pulses.R",
  "05_gpp_vs_reco.R",
  "06_swc_st_space.R",
  "07_threshold15_RECO.R",
  "08_read_chamber.R",
  "09_pulse_np_chamber.R",
  "10_gpp_vs_rsoil_chamber.R",
  "11_swc_st_chamber.R",
  "12_13_chamber_models.R",
  "14_Robust_RsoilModels.R"
))

# "12_threshold15_RSOIL.R",
# "13_chamber_model.R"

for (s in scripts) {
  message(">> Running ", s)
  # IMPORTANT: chdir = FALSE keeps wd at the project root,
  # so all "data/..." paths inside scripts resolve correctly.
  sys.source(s, envir = .GlobalEnv, chdir = FALSE)
}
message("All done.")
