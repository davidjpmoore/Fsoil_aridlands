# R/99_chamber_model_investigation.R
# Compare script 12 vs 13 predictions vs observations, with pulse slicing
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(lubridate); library(ggplot2); library(tidyr)
})

in_dir  <- "out/derived"
fig_dir <- "out/figs/modeleval"
tab_dir <- "out/tables/modeleval"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

# --- read predictions produced by scripts 12 and 13 (no refitting here) ---
p12 <- read_csv(file.path(in_dir,"threshold15_predictions.csv"), show_col_types = FALSE)
p13 <- read_csv(file.path(in_dir,"Chamber_model_predictions.csv"), show_col_types = FALSE)

# Expect columns:
# p12: date, pred_all, pred_np, pred_p, pred_15
# p13: date, meanRsoil, Pred_All, Pred_NonPulse, Pred_Pulse
stopifnot(all(c("date","pred_all","pred_np","pred_p","pred_15") %in% names(p12)))
stopifnot(all(c("date","meanRsoil","Pred_All","Pred_NonPulse","Pred_Pulse") %in% names(p13)))

df <- p13 %>%
  mutate(date = as.Date(date)) %>%
  left_join(p12 %>% mutate(date = as.Date(date)), by = "date") %>%
  mutate(year = year(date), month = month(date)) %>%
  # reconstruct pulse flag from the 12-style switch used at prediction time
  mutate(Pulse = if_else(abs(pred_15 - pred_p) < abs(pred_15 - pred_np), 1L, 0L))

# --- tidy comparison frame ---
long <- df %>%
  select(date, year, month, Pulse, meanRsoil,
         Pred_All, Pred_NonPulse, Pred_Pulse,   # from 13
         pred_all, pred_np, pred_p, pred_15) %>% # from 12
  pivot_longer(
    cols = c(Pred_All, Pred_NonPulse, Pred_Pulse, pred_all, pred_np, pred_p, pred_15),
    names_to = "model", values_to = "pred"
  )

# --- metrics helpers (fast, NA-robust) ---
rmse <- function(o, p) sqrt(mean((o - p)^2, na.rm = TRUE))
mae  <- function(o, p) mean(abs(o - p), na.rm = TRUE)
mbe  <- function(o, p) mean(p - o, na.rm = TRUE)
r2   <- function(o, p) { ok <- is.finite(o) & is.finite(p); if (!any(ok)) return(NA_real_)
1 - sum((o-p)[ok]^2)/sum((o[ok]-mean(o[ok]))^2)
}

summarise_metrics <- function(d) {
  tibble(
    RMSE = rmse(d$meanRsoil, d$pred),
    MAE  = mae (d$meanRsoil, d$pred),
    MBE  = mbe (d$meanRsoil, d$pred),
    R2   = r2  (d$meanRsoil, d$pred),
    N    = sum(is.finite(d$meanRsoil) & is.finite(d$pred))
  )
}

# --- cumulative metrics ---
cum_metrics <- long %>% group_by(model) %>% reframe(summarise_metrics(cur_data_all()))
write_csv(cum_metrics, file.path(tab_dir, "metrics_cumulative.csv"))

# --- by year / by month / by pulse ---
by_year  <- long %>% group_by(year, model)           %>% reframe(summarise_metrics(cur_data_all()))
by_month <- long %>% group_by(year, month, model)    %>% reframe(summarise_metrics(cur_data_all()))
by_pulse <- long %>% group_by(Pulse, model)          %>% reframe(summarise_metrics(cur_data_all()))

write_csv(by_year,  file.path(tab_dir, "metrics_by_year.csv"))
write_csv(by_month, file.path(tab_dir, "metrics_by_year_month.csv"))
write_csv(by_pulse, file.path(tab_dir, "metrics_by_pulse.csv"))

# --- leaderboards: who wins per slice (small tables) ---
winner <- function(d) d %>% slice_min(RMSE, n = 1, with_ties = FALSE)
lb_year   <- by_year  %>% group_by(year)        %>% reframe(winner(cur_data_all()))
lb_month  <- by_month %>% group_by(year, month) %>% reframe(winner(cur_data_all()))
lb_pulse  <- by_pulse %>% group_by(Pulse)       %>% reframe(winner(cur_data_all()))

write_csv(lb_year,  file.path(tab_dir, "leaderboard_by_year.csv"))
write_csv(lb_month, file.path(tab_dir, "leaderboard_by_year_month.csv"))
write_csv(lb_pulse, file.path(tab_dir, "leaderboard_by_pulse.csv"))

# --- compact, empirical response curves (binned means) for SWC and Tsoil ---
# Uses existing daily pairs: for each model, bin predictor and average pred & obs.
mk_response <- function(df, x, nbins = 12) {
  xnum <- df[[x]]
  brks <- quantile(xnum[is.finite(xnum)], probs = seq(0,1,length.out = nbins+1), na.rm = TRUE)
  brks[1] <- -Inf; brks[length(brks)] <- Inf
  df %>%
    mutate(bin = cut(xnum, breaks = brks, include.lowest = TRUE)) %>%
    group_by(model, bin) %>%
    summarise(
      x_mid = median(xnum, na.rm = TRUE),
      y_obs = mean(meanRsoil, na.rm = TRUE),
      y_mod = mean(pred,      na.rm = TRUE),
      N     = n(), .groups = "drop"
    )
}

# Need SWC & Tsoil: join from p13 frame
resp_base <- long %>%
  left_join(df %>% select(date, meanSWC = meanSWC, meanTsoil = meanTsoil), by = "date")

swc_resp  <- mk_response(resp_base, "meanSWC",  nbins = 12)
ts_resp   <- mk_response(resp_base, "meanTsoil", nbins = 12)

# Plot (concise): model response vs SWC / Tsoil (binned means)
p_swc <- ggplot(swc_resp, aes(x = x_mid)) +
  geom_line(aes(y = y_mod, group = model)) +
  geom_point(aes(y = y_obs), alpha = 0.5, size = 1) +
  facet_wrap(~ model, scales = "free_y") +
  theme_bw() + theme(panel.grid = element_blank()) +
  labs(x = "SWC (fraction)", y = "Rsoil", title = "Empirical response: SWC")

p_t   <- ggplot(ts_resp, aes(x = x_mid)) +
  geom_line(aes(y = y_mod, group = model)) +
  geom_point(aes(y = y_obs), alpha = 0.5, size = 1) +
  facet_wrap(~ model, scales = "free_y") +
  theme_bw() + theme(panel.grid = element_blank()) +
  labs(x = "Tsoil (°C)", y = "Rsoil", title = "Empirical response: Tsoil")

ggsave(file.path(fig_dir, "response_swc.png"), p_swc, width = 8, height = 5, dpi = 300)
ggsave(file.path(fig_dir, "response_tsoil.png"), p_t,   width = 8, height = 5, dpi = 300)
