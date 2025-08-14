# =====================================================
# >>> 15% Threshold for Rsoil (robust) — GLOBAL GPPmax
# =====================================================
#
# 13-03-2025 (revised)
# Anastasia Makhnykina + pipeline fixes
# Global GPPmax change by Dave Moore
# 
# What the model is, and what the script does (now with global GPPmax⁡)
# The model (same for all fits)
# For any day,
# R  =  Fref  (GPPGPPmax⁡+n)1+n  (1−c4 (0.1−SWC)2)  e b4 T.
# •	Fref : baseline respiration scale
# •	n : strength of the light term (how much GPP/GPPmax⁡ boosts R)
# •	c4 : curvature of the moisture parabola around SWC = 0.1
# •	b4 : temperature sensitivity (simple exponential in °C)
# •	GPPmax⁡: global maximum over all days (this revision)
# What the script does (step by step)
# 1.	Load daily summaries (meanRsoil, meanTsoil, meanSWC, meanGPP).
# If SWC is in %, convert to a 0–1 fraction.
# 2.	Create a moisture flag Threshold_15 = 1 if meanSWC ≥ 0.15, else 0.
# Build two subsets <15% and ≥15% (used only for fitting, not for normalization).
# 3.	Compute one global GPPmax⁡ across all days: All_GPPmax.
# This scalar is used in every fit and every prediction.
# 4.	Fit the same model three times (using nlsLM with bounds):
#   o	All days → parameters (Fref,L,nL,c4,L,b4,L)
# o	<15% days → (Fref,NP,nNP,c4,NP,b4,NP)
# o	≥15% days → (Fref,P,nP,c4,P,b4,P)
# Each fit uses G/All_G in the light term (i.e., global normalization), not per-group GPPmax.
# 5.	Predict four full-length series for every day:
#   o	pred_all — apply “All” parameters.
# o	pred_np — apply “<15%” parameters (even on ≥15% days).
# o	pred_p — apply “≥15%” parameters (even on <15% days).
# o	pred_15 — switch: use pred_p where SWC ≥ 0.15, else pred_np.
# 6.	Report diagnostics (RMSE, MAPE, R²) for each series vs meanRsoil.
# (Optional: plot measured vs the 15% switch series.)
# Why make GPPmax⁡ global?
#   •	It ensures the light-term scaling is consistent across parameter sets.
# •	Comparisons between pred_np and pred_p (and the switch series) are cleaner because they no longer differ due to different normalizations—only due to parameter differences and the switch rule.


# Portable setup import (works whether run from project root or from R/)
setup_path <- if (file.exists("R/00_setup.R")) "R/00_setup.R" else "00_setup.R"
if (file.exists(setup_path)) source(setup_path)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(ggplot2)
  library(minpack.lm)  # for nlsLM
})

# ---------- helpers ----------
numify <- function(df, cols) {
  df %>% mutate(across(all_of(cols), ~ suppressWarnings(as.numeric(.x))))
}
require_cols <- function(df, cols, df_name) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) stop("Missing columns in ", df_name, ": ", paste(missing, collapse = ", "))
}
finite_only <- function(...) {
  df <- tibble(...)
  ok <- Reduce(`&`, lapply(df, function(x) is.finite(x)))
  df[ok, , drop = FALSE]
}
fit_nlsLM_safe <- function(formula, data, start, lower = NULL, upper = NULL) {
  tryCatch(
    nlsLM(formula, data = data, start = start,
          lower = lower, upper = upper,
          control = nls.lm.control(maxiter = 1000, ftol = 1e-10, ptol = 1e-10)),
    error = function(e) { message("nlsLM failed: ", conditionMessage(e)); NULL }
  )
}

# --- saving helper (safe, minimal) ---
fig_dir <- "out/figs"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

save_last_plot <- function(name, width = 6, height = 5, dpi = 300) {
  p <- ggplot2::last_plot()
  if (is.null(p)) {
    message("No last plot found for ", name, " — skipping.")
    return(invisible(FALSE))
  }
  ggplot2::ggsave(file.path(fig_dir, name), plot = p, width = width, height = height, dpi = dpi)
  message("Saved: ", file.path(fig_dir, name))
  invisible(TRUE)
}

# ---------- read data ----------
all_path  <- "out/derived/All_summary_chamber.csv"
np_path   <- "out/derived/NonPulse_sum_chamber.csv"
p_path    <- "out/derived/Pulse_sum_chamber.csv"

summary_Cham   <- read_csv(all_path, show_col_types = FALSE)
NonPulse_Cham  <- read_csv(np_path,  show_col_types = FALSE)
Pulse_Cham     <- read_csv(p_path,   show_col_types = FALSE)

for (nm in c("summary_Cham","NonPulse_Cham","Pulse_Cham")) {
  df <- get(nm)
  require_cols(df, c("date","meanRsoil","meanTsoil","meanSWC","meanGPP"), nm)
  assign(nm, numify(df, c("meanRsoil","meanTsoil","meanSWC","meanGPP")))
}

# ---------- build 15% threshold groups (use all-time table) ----------
years_sum2 <- summary_Cham %>%
  transmute(
    date = as.Date(date),
    meanRsoil = meanRsoil,
    meanTsoil = meanTsoil,
    meanSWC   = meanSWC,      # may be fraction (0-1) or percent (0-100)
    meanGPP   = meanGPP
  )

# Standardize SWC to fraction 0-1 for modeling
if (max(years_sum2$meanSWC, na.rm = TRUE) > 1.5) {
  years_sum2 <- years_sum2 %>% mutate(meanSWC = meanSWC / 100)
}

years_sum2 <- years_sum2 %>%
  mutate(Threshold_15 = as.numeric(meanSWC >= 0.15))

years_sum2_15more <- years_sum2 %>% filter(Threshold_15 == 1)
years_sum2_15less <- years_sum2 %>% filter(Threshold_15 == 0)

# ---------- GLOBAL GPPmax (applies to ALL fits and ALL predictions) ----------
All_GPPmax <- suppressWarnings(max(years_sum2$meanGPP, na.rm = TRUE))
if (!is.finite(All_GPPmax) || All_GPPmax <= 0) {
  stop("No finite/positive meanGPP values in all-time table; cannot fit models.")
}

# ---------- prepare modeling frames (use GLOBAL GPPmax in every frame) ----------
df_all <- finite_only(
  y = years_sum2$meanRsoil,
  G = years_sum2$meanGPP,
  S = years_sum2$meanSWC,
  T = years_sum2$meanTsoil
)
df_np <- finite_only(
  y = years_sum2_15less$meanRsoil,
  G = years_sum2_15less$meanGPP,
  S = years_sum2_15less$meanSWC,
  T = years_sum2_15less$meanTsoil
)
df_p  <- finite_only(
  y = years_sum2_15more$meanRsoil,
  G = years_sum2_15more$meanGPP,
  S = years_sum2_15more$meanSWC,
  T = years_sum2_15more$meanTsoil
)

if (nrow(df_np) < 20) warning("<15% group has few rows (", nrow(df_np), "); fit may be unstable.")
if (nrow(df_p)  < 20) warning("≥15% group has few rows (", nrow(df_p),  "); fit may be unstable.")

# add a column with the same global GPPmax for each modeling frame
df_all$All_G <- All_GPPmax
df_np$All_G  <- All_GPPmax
df_p$All_G   <- All_GPPmax

# ---------- model form (IDENTICAL equation; GLOBAL GPPmax replaces group-specific normals) ----------
# y = Fref * ((G/All_G + n) / (1 + n)) * (1 - c4 * (0.1 - S)^2) * exp(b4 * T)

form_all <- y ~ FrefL  * ((G/All_G + nL)/ (1 + nL)) * (1 - c4L  * (0.1 - S)^2) * exp(b4L * T)
form_np  <- y ~ FrefNP * ((G/All_G + nNP)/(1 + nNP)) * (1 - c4NP * (0.1 - S)^2) * exp(b4NP * T)
form_p   <- y ~ FrefP  * ((G/All_G + nP )/(1 + nP )) * (1 - c4P  * (0.1 - S)^2) * exp(b4P  * T)

# ---------- starting values and bounds ----------
start_all <- list(FrefL = 0.9,  c4L  = 5, b4L = 0.03, nL  = 0.13)
start_np  <- list(FrefNP= 0.9,  c4NP = 5, b4NP= 0.02, nNP = 0.10)
start_p   <- list(FrefP = 0.45, c4P  = 2, b4P = 0.04, nP  = 0.30)

# Bounds to keep terms sane (prevents “non-sensible value” issues)
lower_all <- c(FrefL=0,   c4L=-50, b4L=0,    nL=0)
upper_all <- c(FrefL=10,  c4L= 50, b4L=0.15, nL=1)

lower_np  <- c(FrefNP=0,  c4NP=-50, b4NP=0,  nNP=0)
upper_np  <- c(FrefNP=10, c4NP= 50, b4NP=0.15,nNP=1)

lower_p   <- c(FrefP=0,   c4P=-50,  b4P=0,   nP=0)
upper_p   <- c(FrefP=10,  c4P= 50,  b4P=0.15,nP=1)

# ---------- fits (each with GLOBAL GPPmax in the model frame) ----------
fit_all <- fit_nlsLM_safe(form_all, df_all, start_all, lower_all, upper_all)
fit_np  <- fit_nlsLM_safe(form_np,  df_np,  start_np,  lower_np,  upper_np)
fit_p   <- fit_nlsLM_safe(form_p,   df_p,   start_p,   lower_p,   upper_p)

if (is.null(fit_all)) stop("All-time fit failed.")
if (is.null(fit_np))  stop("<15% fit failed.")
if (is.null(fit_p))   stop("≥15% fit failed.")

coef_all <- coef(fit_all); coef_np <- coef(fit_np); coef_p <- coef(fit_p)
print(list(all = coef_all, np = coef_np, pulse = coef_p))

# ---------- predict full series (ALWAYS using GLOBAL GPPmax) ----------
pred_all <- with(years_sum2,
                 coef_all[["FrefL"]] *
                   ((meanGPP/All_GPPmax + coef_all[["nL"]])/(1 + coef_all[["nL"]])) *
                   (1 - coef_all[["c4L"]] * (0.1 - meanSWC)^2) *
                   exp(coef_all[["b4L"]] * meanTsoil)
)

pred_np <- with(years_sum2,
                coef_np[["FrefNP"]] *
                  ((meanGPP/All_GPPmax + coef_np[["nNP"]])/(1 + coef_np[["nNP"]])) *
                  (1 - coef_np[["c4NP"]] * (0.1 - meanSWC)^2) *
                  exp(coef_np[["b4NP"]] * meanTsoil)
)

pred_p <- with(years_sum2,
               coef_p[["FrefP"]] *
                 ((meanGPP/All_GPPmax + coef_p[["nP"]])/(1 + coef_p[["nP"]])) *
                 (1 - coef_p[["c4P"]] * (0.1 - meanSWC)^2) *
                 exp(coef_p[["b4P"]] * meanTsoil)
)

# ---------- combined series (by SWC threshold) ----------
years_sum2 <- years_sum2 %>%
  mutate(
    pred_all = pred_all,
    pred_np  = pred_np,
    pred_p   = pred_p,
    pred_15  = ifelse(Threshold_15 == 1, pred_p, pred_np)
  )

# ---------- quick diagnostics ----------
rmse <- function(obs, mod) sqrt(mean((obs - mod)^2, na.rm = TRUE))
mape <- function(obs, mod) mean(abs((obs - mod)/obs), na.rm = TRUE)*100
rsq  <- function(obs, mod) cor(obs, mod, use = "complete.obs")^2

cat("\nRMSE / MAPE / R^2 (GLOBAL GPPmax):\n")
cat("  All:  ", rmse(years_sum2$meanRsoil, years_sum2$pred_all),
    mape(years_sum2$meanRsoil, years_sum2$pred_all),
    rsq(years_sum2$meanRsoil,  years_sum2$pred_all), "\n")
cat("  <15%: ", rmse(years_sum2$meanRsoil, years_sum2$pred_np),
    mape(years_sum2$meanRsoil, years_sum2$pred_np),
    rsq(years_sum2$meanRsoil,  years_sum2$pred_np), "\n")
cat("  ≥15%: ", rmse(years_sum2$meanRsoil, years_sum2$pred_p),
    mape(years_sum2$meanRsoil, years_sum2$pred_p),
    rsq(years_sum2$meanRsoil,  years_sum2$pred_p), "\n")
cat("  15%:  ", rmse(years_sum2$meanRsoil, years_sum2$pred_15),
    mape(years_sum2$meanRsoil, years_sum2$pred_15),
    rsq(years_sum2$meanRsoil,  years_sum2$pred_15), "\n")

# ---------- (optional) simple plot ----------
plot(years_sum2$date, years_sum2$meanRsoil, type="p", cex=0.6, col="blue",
     xlab="Date", ylab="Rsoil (umol m-2 s-1)")
points(years_sum2$date, years_sum2$pred_15, pch=16, cex=0.4, col="darkgreen")
legend("topleft", legend=c("Measured","15% model (global GPPmax)"), col=c("blue","darkgreen"), pch=c(1,16), bty="n")

save_last_plot("rsoil_ts_overlay_15.png", width = 7, height = 4.5)

# ---- write-out data for analysis script:
readr::write_csv(years_sum2 %>% select(date, pred_all, pred_np, pred_p, pred_15),"out/derived/threshold15_predictions.csv")
