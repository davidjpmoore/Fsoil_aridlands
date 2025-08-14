# =====================================================
# >>> 13_chamber_model.R  (GLOBAL GPPmax; All / NonPulse / Pulse)
# =====================================================
#
# Purpose:
#   Fit the same Rsoil ~ (light * moisture * temperature) model on:
#     1) All days
#     2) Event Non-Pulse days
#     3) Event Pulse days
#   using ONE global GPPmax computed from the full series.
#   Predict across all days for each fit; print metrics; save CSV & plots.
#
# Assumptions:
#   - Daily GPP is available and finite (if not, we stop).
#   - SWC is a fraction [0,1]. If it looks like %, we convert (/100).
#
# Outputs:
#   out/derived/Chamber_model_predictions.csv
#     columns: date, meanRsoil, Pred_All, Pred_NonPulse, Pred_Pulse
#
# #   Plots saved under out/figs/ and also printed to the Plots pane.
# #
# Step-by-step walkthrough (what the script does)
# Read inputs
# All_summary_chamber.csv, NonPulse_sum_chamber.csv, Pulse_sum_chamber.csv
# Cast columns to numeric; convert SWC to fraction if needed.
# Compute one global 
# GPPmax 
# ⁡
# GPPmax_global <- max(summary_Cham$meanGPP, na.rm = TRUE)
# Guard that it’s finite and positive.
# Define the model
# R =Fref*GPPlim*moisture*temperature
# R=Fref =*GPPlim*moisture*temperature
# GPPlim = ((GPP/GPPmax + n)/(1+n))
# moisture = (1 - c4*(0.1 - SWC)^2)
# temperature = exp(b4*T)
# Fit three models (All / NonPulse / Pulse)
# Each subset is filtered to finite rows and given GPPmax = GPPmax_global.
# nlsLM bounds (13-style):
#   Fref ∈ [0,5], c4 ∈ [0,200], b4 ∈ [0,0.2], n ∈ [0,5].
# Predict across all dates for each fit
# Build newdata_all with GPPmax = GPPmax_global added.
# Compute Pred_All, Pred_NonPulse, Pred_Pulse.
# Diagnostics & plots
# Print RMSE, MAPE, R² for each prediction vs measured.
# Time-series overlay and measured vs predicted (All model) scatter; print and save.
# Write a tidy CSV for downstream comparison
# out/derived/Chamber_model_predictions.csv with date, meanRsoil, Pred_All, Pred_NonPulse, Pred_Pulse.
# =====================================================

# --- portable setup ---
setup_path <- if (file.exists("R/00_setup.R")) "R/00_setup.R" else "00_setup.R"
if (file.exists(setup_path)) source(setup_path)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(ggplot2)
  library(minpack.lm)
  library(ggpubr)
})

# ---- I/O setup ----
ch_out_dir <- "out/chamber"
me_out_dir <- "out/model_eval/chamber"
fig_dir    <- "out/figs"
dir.create(ch_out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(me_out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir,    recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# Inputs (produced by 08_read_chamber.R)
# ---------------------------
in_dir <- "out/derived"
files <- c(
  all   = file.path(in_dir, "All_summary_chamber.csv"),
  np    = file.path(in_dir, "NonPulse_sum_chamber.csv"),
  pulse = file.path(in_dir, "Pulse_sum_chamber.csv")
)
missing <- names(files)[!file.exists(files)]
if (length(missing)) {
  stop("Missing required inputs in ", in_dir, ": ",
       paste(basename(files[missing]), collapse = ", "),
       "\nRun R/08_read_chamber.R first.")
}

# ---------- helpers ----------
coerce_ch <- function(df) {
  df %>%
    mutate(
      date      = as.Date(date),
      meanSWC   = suppressWarnings(as.numeric(meanSWC)),
      meanTsoil = suppressWarnings(as.numeric(meanTsoil)),
      meanGPP   = suppressWarnings(as.numeric(meanGPP)),
      meanRsoil = suppressWarnings(as.numeric(meanRsoil))
    )
}
make_fraction_swc <- function(df) {
  if (max(df$meanSWC, na.rm = TRUE) > 1.5) {
    df <- df %>% mutate(meanSWC = meanSWC / 100)
  }
  df
}
finite_rows <- function(df, cols) {
  ok <- Reduce(`&`, lapply(df[cols], is.finite))
  df[ok, , drop = FALSE]
}
rmse <- function(obs, mod) sqrt(mean((obs - mod)^2, na.rm = TRUE))
mape <- function(obs, mod) mean(abs((obs - mod)/obs), na.rm = TRUE) * 100
rsq  <- function(obs, mod) {
  ok <- is.finite(obs) & is.finite(mod)
  if (!any(ok)) return(NA_real_)
  o <- obs[ok]; p <- mod[ok]
  1 - sum((o - p)^2)/sum((o - mean(o))^2)
}
save_last_plot <- function(name, width = 6, height = 5, dpi = 300) {
  p <- ggplot2::last_plot()
  if (inherits(p, "ggplot")) {
    print(p) # ensure it appears in Plots pane
    ggplot2::ggsave(file.path(fig_dir, name), plot = p, width = width, height = height, dpi = dpi)
    message("Saved: ", file.path(fig_dir, name))
  } else {
    message("No ggplot object to save for ", name)
  }
}

# ---------- read & harmonize data ----------
summary_Cham  <- readr::read_csv(files["all"],   show_col_types = FALSE) |> coerce_ch()  |> make_fraction_swc()
NonPulse_Cham <- readr::read_csv(files["np"],    show_col_types = FALSE) |> coerce_ch()  |> make_fraction_swc()
Pulse_Cham    <- readr::read_csv(files["pulse"], show_col_types = FALSE) |> coerce_ch()  |> make_fraction_swc()

# Guard: GPP must exist daily in ALL (per your spec)
if (!all(is.finite(summary_Cham$meanGPP))) {
  stop("Non-finite meanGPP detected in All_summary_chamber.csv. Daily GPP is required.")
}

# ---------- GLOBAL GPPmax (one scalar used everywhere) ----------
GPPmax_global <- max(summary_Cham$meanGPP, na.rm = TRUE)
if (!is.finite(GPPmax_global) || GPPmax_global <= 0) {
  stop("Global GPPmax is not finite/positive — check meanGPP in All_summary_chamber.csv")
}

# ---------------------------
# Model (identical structure for all fits)
# R = Fref * ((GPP/GPPmax + n)/(1+n)) * (1 - c4*(0.1 - SWC)^2) * exp(b4*T)
# ---------------------------
form <- meanRsoil ~ Fref *
  ((meanGPP/GPPmax + n)/(1 + n)) *
  (1 - c4 * (0.1 - meanSWC)^2) *
  exp(b4 * meanTsoil)

# ---------------------------
# Fit wrapper (uses GLOBAL GPPmax; 13-style bounds)
# ---------------------------
fit_subset <- function(df, label, starts = list(Fref = 0.75, c4 = 5.0, b4 = 0.03, n = 0.15)) {
  d <- df %>%
    select(meanRsoil, meanSWC, meanTsoil, meanGPP) %>%
    finite_rows(c("meanRsoil","meanSWC","meanTsoil","meanGPP")) %>%
    mutate(GPPmax = GPPmax_global)
  
  if (nrow(d) < 20) {
    stop("Insufficient rows for ", label, " fit after filtering finite values (n=", nrow(d), ").")
  }
  
  nlsLM(
    form, data = d, start = starts,
    lower   = c(Fref = 0, c4 = 0,   b4 = 0,   n = 0),   # 13-style: c4 >= 0, n up to 5
    upper   = c(Fref = 5, c4 = 200, b4 = 0.2, n = 5),
    control = nls.lm.control(maxiter = 1000)
  )
}

# ---------------------------
# Fit models (All / NonPulse / Pulse)
# ---------------------------
message("Fitting All ...")
fit_all   <- fit_subset(summary_Cham,  "All",
                        starts = list(Fref = 0.75, c4 = 5.0, b4 = 0.03, n = 0.15))

message("Fitting NonPulse ...")
fit_np    <- fit_subset(NonPulse_Cham, "NonPulse",
                        starts = list(Fref = 0.75, c4 = 5.0, b4 = 0.03, n = 0.15))

message("Fitting Pulse ...")
fit_pulse <- fit_subset(Pulse_Cham,    "Pulse",
                        starts = list(Fref = 0.75, c4 = 5.0, b4 = 0.03, n = 0.15))

# ---------------------------
# Predict over ALL dates for comparison
# ---------------------------
newdata_all <- summary_Cham %>%
  select(date, meanRsoil, meanSWC, meanTsoil, meanGPP) %>%
  mutate(GPPmax = GPPmax_global)

Pred_All       <- as.numeric(predict(fit_all,   newdata = newdata_all))
Pred_NonPulse  <- as.numeric(predict(fit_np,    newdata = newdata_all))
Pred_Pulse     <- as.numeric(predict(fit_pulse, newdata = newdata_all))

# ---------------------------
# Metrics
# ---------------------------
metrics <- tibble::tibble(
  model = c("All","NonPulse","Pulse"),
  RMSE  = c(rmse(newdata_all$meanRsoil, Pred_All),
            rmse(newdata_all$meanRsoil, Pred_NonPulse),
            rmse(newdata_all$meanRsoil, Pred_Pulse)),
  MAE   = c(mape(newdata_all$meanRsoil, Pred_All),
            mape(newdata_all$meanRsoil, Pred_NonPulse),
            mape(newdata_all$meanRsoil, Pred_Pulse)),
  R2    = c(rsq(newdata_all$meanRsoil, Pred_All),
            rsq(newdata_all$meanRsoil, Pred_NonPulse),
            rsq(newdata_all$meanRsoil, Pred_Pulse))
)
print(metrics)

# ---------------------------
# Plots (printed & saved)
# ---------------------------

# 1) Time series overlay
p_ts <- ggplot(newdata_all, aes(x = as.Date(date))) +
  geom_point(aes(y = meanRsoil), color = "blue", size = 1.2, alpha = 0.7) +
  geom_line(aes(y = Pred_All),      linewidth = 0.8, alpha = 0.9, na.rm = TRUE, color = "forestgreen") +
  geom_line(aes(y = Pred_NonPulse), linewidth = 0.6, alpha = 0.7, na.rm = TRUE, color = "red3") +
  geom_line(aes(y = Pred_Pulse),    linewidth = 0.6, alpha = 0.7, na.rm = TRUE, color = "cyan4") +
  theme_bw() +
  labs(x = "Date",
       y = expression(paste("Rsoil, ", mu, "mol m"^-2, " s"^-1)),
       title = "Measured vs Modelled Rsoil (All / NonPulse / Pulse fits; GLOBAL GPPmax)") +
  theme(text = element_text(size = 13))
print(p_ts)
ggsave(file.path(fig_dir, "Meas_Modeled_all_RSOIL_globalGPPmax.png"),
       p_ts, width = 7, height = 4.8, dpi = 300)

# 2) Measured vs Modelled (All model)
p_all <- ggplot(newdata_all, aes(x = meanRsoil, y = Pred_All)) +
  geom_point(shape = 1) +
  theme_bw() + theme(text = element_text(size = 13)) +
  ggpubr::stat_regline_equation(aes(label = after_stat(eq.label)),
                                label.x = 0, label.y = max(Pred_All, na.rm = TRUE)) +
  ggpubr::stat_regline_equation(aes(label = after_stat(rr.label)),
                                label.x = 0, label.y = max(Pred_All, na.rm = TRUE)*0.93) +
  stat_smooth(method = "lm", formula = y ~ x, linewidth = 0.8) +
  labs(x = expression(paste("Measured Rsoil, ", mu, "mol m"^-2, " s"^-1)),
       y = expression(paste("Modelled Rsoil (All fit), ", mu, "mol m"^-2, " s"^-1)))
print(p_all)
ggsave(file.path(fig_dir, "ch_model_fit_all_globalGPPmax.png"),
       p_all, width = 6.2, height = 4.8, dpi = 300)

# ---------------------------
# Write predictions for downstream comparisons
# ---------------------------
out_pred <- file.path(in_dir, "Chamber_model_predictions.csv")
readr::write_csv(
  tibble::tibble(
    date = newdata_all$date,
    meanRsoil = newdata_all$meanRsoil,
    Pred_All = Pred_All,
    Pred_NonPulse = Pred_NonPulse,
    Pred_Pulse = Pred_Pulse
  ),
  out_pred
)
message("Wrote predictions: ", out_pred)

