# 15_Rsoil_figs.R ---------------------------------------------------------------
# Figures 6e–6h: Rsoil model overlays — analogous to Figs 6a–6d for RECO.
#
# Reads pre-computed predictions from 14_Robust_RsoilModels.R and produces:
#   Fig 6e  — time-series scatter, SWC-threshold model overlays
#   Fig 6f  — cumulative Rsoil, SWC-threshold switch vs lumped mean
#   Fig 6g  — time-series scatter, rainfall-event P-NP model overlays
#   Fig 6h  — cumulative Rsoil, P-NP switch vs lumped mean
#
# Classification scheme mapping (mirrors 07 / 07b for RECO):
#   Figs 6e–6f  ->  SWC threshold (Pred_ThrLow, Pred_ThrHigh, Pred_Thr)
#   Figs 6g–6h  ->  Rainfall-event P-NP (Pred_NP, Pred_P, Pred_PN)
#
# Input:  out/derived/Chamber_model_predictions_14.csv
# Output: out/figs/Fig6e_RsoilModels_Thr_TimeSeries.png
#         out/figs/Fig6f_RsoilModels_Thr_Cumulative.png
#         out/figs/Fig6g_RsoilModels_PNP_TimeSeries.png
#         out/figs/Fig6h_RsoilModels_PNP_Cumulative.png
# -------------------------------------------------------------------------------

setup_path <- if (file.exists("R/00_setup.R")) "R/00_setup.R" else "00_setup.R"
source(setup_path)

# --- 1. Read predictions -------------------------------------------------------

preds <- read_csv("out/derived/Chamber_model_predictions_14.csv",
                  show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

# The file may contain two rows per date (Bounds = "12" and "13") when
# BOUNDS_STYLE = "12and13" was used in script 14.  Pick the bounds style
# that gives the best threshold RMSE so figures reflect the optimal fit.
if (n_distinct(preds$Bounds) > 1) {
  best_bounds <- preds %>%
    filter(is.finite(Pred_Thr) & is.finite(meanRsoil)) %>%
    group_by(Bounds) %>%
    summarise(rmse_thr = sqrt(mean((meanRsoil - Pred_Thr)^2)), .groups = "drop") %>%
    slice_min(rmse_thr, n = 1, with_ties = FALSE) %>%
    pull(Bounds)
  preds <- filter(preds, Bounds == best_bounds)
}

y_lab_rsoil <- expression(paste(R[soil], " (", mu, "mol ", m^{-2}, " ", s^{-1}, ")"))

# --- 2. Fig 6e: time-series scatter — SWC threshold model ---------------------

p6e <- ggplot(preds, aes(date)) +
  geom_point(aes(y = meanRsoil),    color = "blue",   size = 0.8, alpha = 0.8) +
  geom_point(aes(y = Pred_ThrLow),  color = "red",    size = 0.5, alpha = 0.5) +
  geom_point(aes(y = Pred_ThrHigh), color = "cyan4",  size = 0.5, alpha = 0.5) +
  geom_point(aes(y = Pred_All),     color = "green4", size = 0.5, alpha = 0.5) +
  labs(y = y_lab_rsoil, x = NULL,
       title = paste0("Rsoil time series — SWC threshold model",
                      " (thr = ", sprintf("%.2f", preds$Thr_used[1]), ")"))
save_plot(p6e, "Fig6e_RsoilModels_Thr_TimeSeries.png", w = 7.5)

# --- 3. Fig 6f: cumulative Rsoil — threshold switch ---------------------------

cum_thr <- preds %>%
  drop_na(meanRsoil, Pred_Thr, Pred_All) %>%
  arrange(date) %>%
  mutate(
    culMeasured = cumsum(meanRsoil),
    culThr      = cumsum(Pred_Thr),
    culMean     = cumsum(Pred_All)
  )

p6f <- ggplot(cum_thr, aes(date)) +
  geom_line(aes(y = culMeasured), color = "blue") +
  geom_line(aes(y = culMean),     color = "red") +
  geom_line(aes(y = culThr),      color = "green4") +
  labs(y = "Cumulative Rsoil", x = NULL,
       title = "Cumulative Rsoil — SWC threshold model")
save_plot(p6f, "Fig6f_RsoilModels_Thr_Cumulative.png", w = 7.5)

# --- 4. Fig 6g: time-series scatter — rainfall-event P-NP model ---------------

p6g <- ggplot(preds, aes(date)) +
  geom_point(aes(y = meanRsoil), color = "blue",   size = 0.8, alpha = 0.8) +
  geom_point(aes(y = Pred_NP),   color = "red",    size = 0.5, alpha = 0.5) +
  geom_point(aes(y = Pred_P),    color = "cyan4",  size = 0.5, alpha = 0.5) +
  geom_point(aes(y = Pred_All),  color = "green4", size = 0.5, alpha = 0.5) +
  labs(y = y_lab_rsoil, x = NULL,
       title = "Rsoil time series — rainfall-event P-NP model")
save_plot(p6g, "Fig6g_RsoilModels_PNP_TimeSeries.png", w = 7.5)

# --- 5. Fig 6h: cumulative Rsoil — P-NP switch --------------------------------

cum_pnp <- preds %>%
  drop_na(meanRsoil, Pred_PN, Pred_All) %>%
  arrange(date) %>%
  mutate(
    culMeasured = cumsum(meanRsoil),
    culPN       = cumsum(Pred_PN),
    culMean     = cumsum(Pred_All)
  )

p6h <- ggplot(cum_pnp, aes(date)) +
  geom_line(aes(y = culMeasured), color = "blue") +
  geom_line(aes(y = culMean),     color = "red") +
  geom_line(aes(y = culPN),       color = "green4") +
  labs(y = "Cumulative Rsoil", x = NULL,
       title = "Cumulative Rsoil — rainfall-event P-NP model")
save_plot(p6h, "Fig6h_RsoilModels_PNP_Cumulative.png", w = 7.5)

message("Figures written to out/figs/")
