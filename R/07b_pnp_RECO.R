# 07b_pnp_RECO.R --------------------------------------------------------------
# Rainfall-event P-NP switch model for ecosystem respiration (RECO).
#
# Fits three models and generates a combined switch prediction:
#   NonPulse (NP)  — trained on rainfall-event non-pulse days
#   Pulse    (P)   — trained on rainfall-event pulse days
#   All            — lumped baseline (all days)
#   PNP_Combined   — switch prediction: P params on pulse days, NP elsewhere
#
# Ported from legacy/Eddy model_2.R with the following corrections:
#   Bug 1 fix  — legacy used per-subset GPPmax in fitting (GPPmax_NP, GPPmax_P)
#                but global GPPmax in prediction. This script uses one global
#                GPPmax = max(years_sum1$meanGPP) throughout: in fitting AND
#                prediction.
#   Formula fix — legacy wrote (GPP/GPPmax + n)/1 + n, which R parses as
#                 (GPP/GPPmax + n) + n due to operator precedence. This script
#                 uses the correct form: (GPP/GPPmax + n)/(1 + n).
#
# Classification scheme: RAINFALL-EVENT only (Scientific Rule 1).
#   Non-pulse:  max_pulse_duration == 0
#   Pulse:      max_pulse_duration >  0
#   Switch:     max_pulse_duration >  0  ->  Pulse params
#               max_pulse_duration == 0  ->  NonPulse params
#   Never substitute the SWC threshold for this model.
#
# Inputs:
#   out/derived/years_sum1_DM.csv   (from 01_read_eddy.R + 02_define_pulses.R)
#
# Outputs:
#   out/derived/metrics_RECO_PNP.csv
#   out/figs/reco_ts_overlay_PNP.png
#   out/figs/reco_cumulative_PNP.png
#
# WARNING: out/derived/years_sum1_DM.csv must be regenerated from raw eddy data
# by running 01_read_eddy.R then 02_define_pulses.R before using any results in
# the manuscript. A full pipeline run (source("R/run_all.R")) is required.
# -----------------------------------------------------------------------------

setup_path <- if (file.exists("R/00_setup.R")) "R/00_setup.R" else "00_setup.R"
source(setup_path)

# --- 1. Read and prepare data ------------------------------------------------

ys1 <- read_csv("out/derived/years_sum1_DM.csv", show_col_types = FALSE) %>%
  mutate(
    All_meanSWC5 = meanSWC5 / 100,              # convert % to fraction [0-1] for model
    All_meanST5  = meanST5,
    All_meanGPP  = meanGPP,
    All_GPPmax   = max(meanGPP, na.rm = TRUE)   # global max — identical scalar in all fits
                                                 # and all predictions (Bug 1 fix)
  )

# Rainfall-event classification splits (Scientific Rule 1)
df_np <- filter(ys1, max_pulse_duration == 0)   # non-pulse days
df_p  <- filter(ys1, max_pulse_duration >  0)   # pulse days

message("Training rows  NonPulse: ", nrow(df_np),
        "  Pulse: ", nrow(df_p),
        "  Total: ", nrow(ys1))

# --- 2. Fit models -----------------------------------------------------------
# Bounds: c4 clamped to [-35, 35] so that the moisture term 1 - c4*(0.1-SWC)^2
# cannot go negative at the maximum observed daily-mean SWC (~0.269).
# Derivation: 1/(0.269 - 0.1)^2 ≈ 35. (Scientific Rule 6)
# All other bounds match 07_threshold15_RECO.R.

lower <- c(Fref = 0,  c4 = -35, b4 = 0,    n = 0.0001)
upper <- c(Fref = 10, c4 =  35, b4 = 0.20, n = 1.0)

# Non-pulse model — trained on rainfall-event non-pulse days
# Start values informed by legacy/Eddy model_2.R NP parameters (lines 74-78)
m_np <- fit_nlsLM(
  meanRECO ~ Fref * ((All_meanGPP / All_GPPmax + n) / (1 + n)) *
    (1 - c4 * (0.1 - All_meanSWC5)^2) * exp(b4 * All_meanST5),
  data  = df_np,
  start = c(Fref = 0.75, c4 = 8.0,  b4 = 0.034, n = 0.13),
  lower = lower, upper = upper
)

# Pulse model — trained on rainfall-event pulse days
# Start values informed by legacy/Eddy model_2.R P parameters (lines 114-118)
m_p <- fit_nlsLM(
  meanRECO ~ Fref * ((All_meanGPP / All_GPPmax + n) / (1 + n)) *
    (1 - c4 * (0.1 - All_meanSWC5)^2) * exp(b4 * All_meanST5),
  data  = df_p,
  start = c(Fref = 0.62, c4 = -10.0, b4 = 0.044, n = 0.24),
  lower = lower, upper = upper
)

# All-data model — lumped baseline for comparison
# Start values informed by legacy/Eddy model_2.R All parameters (lines 160-164)
m_all <- fit_nlsLM(
  meanRECO ~ Fref * ((All_meanGPP / All_GPPmax + n) / (1 + n)) *
    (1 - c4 * (0.1 - All_meanSWC5)^2) * exp(b4 * All_meanST5),
  data  = ys1,
  start = c(Fref = 1.1, c4 = -8.0, b4 = 0.036, n = 0.08),
  lower = lower, upper = upper
)

co_np  <- coef(m_np)
co_p   <- coef(m_p)
co_all <- coef(m_all)

message("\nNonPulse coefficients:"); print(round(co_np,  6))
message("\nPulse coefficients:");    print(round(co_p,   6))
message("\nAll coefficients:");      print(round(co_all, 6))

# --- 3. Predict over full record ---------------------------------------------
# All three parameter sets are applied to every row in ys1 using the global
# GPPmax. pmax(..., 0) clamp prevents physically impossible negative predictions
# (Scientific Rule 6).

pred_np <- pmax(with(ys1,
  co_np[["Fref"]] *
    ((All_meanGPP / All_GPPmax + co_np[["n"]]) / (1 + co_np[["n"]])) *
    (1 - co_np[["c4"]] * (0.1 - All_meanSWC5)^2) *
    exp(co_np[["b4"]] * All_meanST5)
), 0)

pred_p <- pmax(with(ys1,
  co_p[["Fref"]] *
    ((All_meanGPP / All_GPPmax + co_p[["n"]]) / (1 + co_p[["n"]])) *
    (1 - co_p[["c4"]] * (0.1 - All_meanSWC5)^2) *
    exp(co_p[["b4"]] * All_meanST5)
), 0)

pred_all <- pmax(with(ys1,
  co_all[["Fref"]] *
    ((All_meanGPP / All_GPPmax + co_all[["n"]]) / (1 + co_all[["n"]])) *
    (1 - co_all[["c4"]] * (0.1 - All_meanSWC5)^2) *
    exp(co_all[["b4"]] * All_meanST5)
), 0)

# --- 4. Build output dataframe and switch prediction -------------------------
# Switch criterion matches the training criterion exactly (Scientific Rule 1):
# pulse days (max_pulse_duration > 0) use Pulse parameters; all other days use
# NonPulse parameters.

out <- ys1 %>%
  select(date, meanRECO, max_pulse_duration, All_meanSWC5) %>%
  mutate(
    NonPulseM_PN = pred_np,
    PulseM_PN    = pred_p,
    MeanM_PN     = pred_all,
    Reco_PNP     = pmax(ifelse(max_pulse_duration > 0, PulseM_PN, NonPulseM_PN), 0)
  )

# --- 5. Metrics --------------------------------------------------------------

metrics <- tibble(
  model = c("PNP_Combined", "Pulse_PN", "NonPulse_PN", "MeanAll"),
  RMSE  = c(rmse(out$meanRECO, out$Reco_PNP),
            rmse(out$meanRECO, out$PulseM_PN),
            rmse(out$meanRECO, out$NonPulseM_PN),
            rmse(out$meanRECO, out$MeanM_PN)),
  MAPE  = c(mape(out$meanRECO, out$Reco_PNP),
            mape(out$meanRECO, out$PulseM_PN),
            mape(out$meanRECO, out$NonPulseM_PN),
            mape(out$meanRECO, out$MeanM_PN)),
  R2    = c(rsq(out$meanRECO, out$Reco_PNP),
            rsq(out$meanRECO, out$PulseM_PN),
            rsq(out$meanRECO, out$NonPulseM_PN),
            rsq(out$meanRECO, out$MeanM_PN))
)

write_csv(metrics, "out/derived/metrics_RECO_PNP.csv")
message("\nMetrics (written to out/derived/metrics_RECO_PNP.csv):")
print(metrics)

# --- 6. Figures --------------------------------------------------------------

# Time-series scatter: observed vs all three model overlays
p_ts <- ggplot(out, aes(date)) +
  geom_point(aes(y = meanRECO),     color = "blue",   size = 0.8, alpha = 0.8) +
  geom_point(aes(y = NonPulseM_PN), color = "red",    size = 0.5, alpha = 0.5) +
  geom_point(aes(y = PulseM_PN),    color = "cyan4",  size = 0.5, alpha = 0.5) +
  geom_point(aes(y = MeanM_PN),     color = "green4", size = 0.5, alpha = 0.5) +
  labs(
    y     = expression(paste("Reco (", mu, "mol ", m^{-2}, " ", s^{-1}, ")")),
    x     = NULL,
    title = "RECO time series — rainfall-event P-NP model"
  )
save_plot(p_ts, "reco_ts_overlay_PNP.png", w = 7.5)

# Cumulative RECO: measured vs P-NP switch vs lumped mean
cum <- out %>%
  drop_na(meanRECO, Reco_PNP, MeanM_PN) %>%
  arrange(date) %>%
  mutate(
    culMeasured = cumsum(meanRECO),
    culPNP      = cumsum(Reco_PNP),
    culMean     = cumsum(MeanM_PN)
  )

p_cum <- ggplot(cum, aes(date)) +
  geom_line(aes(y = culMeasured), color = "blue") +
  geom_line(aes(y = culMean),     color = "red") +
  geom_line(aes(y = culPNP),      color = "green4") +
  labs(
    y     = "Cumulative Reco",
    x     = NULL,
    title = "Cumulative RECO — rainfall-event P-NP model"
  )
save_plot(p_cum, "reco_cumulative_PNP.png", w = 7.5)

message("Figures written to out/figs/")
