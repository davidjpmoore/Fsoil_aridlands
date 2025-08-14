# =====================================================
# >>> 12_13_chamber_models.R  (combined + harmonized)
# =====================================================
# Fits identical model with GLOBAL GPPmax on:
#   - ALL days         (All)
#   - Non-Pulse days   (NP)
#   - Pulse days       (P)
# Builds the Threshold (SWC>=0.15) "switch" predictions from NP/P fits.
# Outputs:
#   1) params+metrics CSV (RMSE, R2, AIC) for All/NP/P and Threshold (AIC=NA)
#   2) annual sums plot (obs + four model series)
#   3) obs vs pred scatter (faceted by model)
#   4) daily predictions CSV for downstream comparison
# =====================================================

# --- setup ---
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(lubridate)
  library(ggplot2); library(tidyr); library(minpack.lm)
})

in_dir   <- "out/derived"
fig_dir  <- "out/figs"
eval_dir <- "out/model_eval/chamber"
dir.create(fig_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(eval_dir, recursive = TRUE, showWarnings = FALSE)

# --- Inputs used by both legacy 12 and 13 ---
files <- c(
  all   = file.path(in_dir, "All_summary_chamber.csv"),
  np    = file.path(in_dir, "NonPulse_sum_chamber.csv"),
  pulse = file.path(in_dir, "Pulse_sum_chamber.csv")
)
missing <- names(files)[!file.exists(files)]
if (length(missing)) {
  stop("Missing inputs in ", in_dir, ": ", paste(basename(files[missing]), collapse = ", "),
       "\nRun R/08_read_chamber.R to produce these.")
}

coerce_and_clean <- function(df) {
  df %>%
    mutate(
      date      = as.Date(date),
      meanSWC   = suppressWarnings(as.numeric(meanSWC)),
      meanTsoil = suppressWarnings(as.numeric(meanTsoil)),
      meanGPP   = suppressWarnings(as.numeric(meanGPP)),
      meanRsoil = suppressWarnings(as.numeric(meanRsoil))
    ) %>%
    { if (max(.$meanSWC, na.rm = TRUE) > 1.5) mutate(., meanSWC = meanSWC/100) else . } %>%
    filter(is.finite(meanSWC), is.finite(meanTsoil), is.finite(meanGPP), is.finite(meanRsoil))
}

df_all   <- read_csv(files["all"],   show_col_types = FALSE) |> coerce_and_clean()
df_np    <- read_csv(files["np"],    show_col_types = FALSE) |> coerce_and_clean()
df_pulse <- read_csv(files["pulse"], show_col_types = FALSE) |> coerce_and_clean()

# --- GLOBAL GPPmax (one scalar for **all** fits and predictions)
GPPmax_global <- max(df_all$meanGPP, na.rm = TRUE)
if (!is.finite(GPPmax_global) || GPPmax_global <= 0) stop("Global GPPmax invalid.")

# --- identical model form (as in your 12/13 description)
# R = Fref * ((GPP/GPPmax + n)/(1+n)) * (1 - c4*(0.1 - SWC)^2) * exp(b4*Tsoil)
form <- meanRsoil ~ Fref *
  ((meanGPP/GPPmax + n)/(1 + n)) *
  (1 - c4 * (0.1 - meanSWC)^2) *
  exp(b4 * meanTsoil)

# --- fit helper (keeps 13-style nonnegativity for c4, n)
fit_one <- function(df, label, starts = list(Fref=.75, c4=5, b4=.03, n=.15)) {
  d <- df %>% mutate(GPPmax = GPPmax_global)
  # Consistent bounds, 12-style: allow c4 < 0
  nlsLM(
    form, data = d, start = starts,
    lower   = c(Fref = 0,   c4 = -200, b4 = 0,   n = 0),
    upper   = c(Fref = 5,   c4 =  200, b4 = 0.2, n = 5),
    control = nls.lm.control(maxiter = 1000)
  )
}


fit_All <- fit_one(df_all,   "All")
fit_NP  <- fit_one(df_np,    "NonPulse")
fit_P   <- fit_one(df_pulse, "Pulse")

# --- daily predictions on ALL dates for all three fits
new_all <- df_all %>% select(date, meanSWC, meanTsoil, meanGPP, meanRsoil) %>% mutate(GPPmax=GPPmax_global)
pred_All <- as.numeric(predict(fit_All,  newdata=new_all))
pred_NP  <- as.numeric(predict(fit_NP,   newdata=new_all))
pred_P   <- as.numeric(predict(fit_P,    newdata=new_all))

# --- threshold switch (>= 0.15 -> Pulse fit; else NonPulse fit)
pred_Thr <- ifelse(new_all$meanSWC >= 0.15, pred_P, pred_NP)

# --- metrics + AIC utilities
rss  <- function(o,p) sum((o-p)^2, na.rm=TRUE)
rmse <- function(o,p) sqrt(mean((o-p)^2, na.rm=TRUE))
r2   <- function(o,p){ok<-is.finite(o)&is.finite(p); if(!any(ok)) return(NA_real_)
1 - sum((o-p)[ok]^2)/sum((o[ok]-mean(o[ok]))^2)}
aic_from_fit <- function(fit, n_obs) {
  k <- length(coef(fit))
  rss_val <- sum(residuals(fit)^2)
  n_obs * log(rss_val / n_obs) + 2 * k
}



obs <- new_all$meanRsoil
n_obs <- sum(is.finite(obs) & is.finite(pred_All))

params_tbl <- dplyr::bind_rows(
  tibble::tibble(
    model = "All",
    Fref  = unname(coef(fit_All)["Fref"]),
    c4    = unname(coef(fit_All)["c4"]),
    b4    = unname(coef(fit_All)["b4"]),
    n     = unname(coef(fit_All)["n"]),
    RMSE  = rmse(obs, pred_All),
    R2    = r2(obs, pred_All),
    AIC   = aic_from_fit(fit_All, n_obs)
  ),
  tibble::tibble(
    model = "NonPulse",
    Fref  = unname(coef(fit_NP)["Fref"]),
    c4    = unname(coef(fit_NP)["c4"]),
    b4    = unname(coef(fit_NP)["b4"]),
    n     = unname(coef(fit_NP)["n"]),
    RMSE  = rmse(obs, pred_NP),
    R2    = r2(obs, pred_NP),
    AIC   = aic_from_fit(fit_NP, n_obs)
  ),
  tibble::tibble(
    model = "Pulse",
    Fref  = unname(coef(fit_P)["Fref"]),
    c4    = unname(coef(fit_P)["c4"]),
    b4    = unname(coef(fit_P)["b4"]),
    n     = unname(coef(fit_P)["n"]),
    RMSE  = rmse(obs, pred_P),
    R2    = r2(obs, pred_P),
    AIC   = aic_from_fit(fit_P, n_obs)
  ),
  tibble::tibble(
    model = "ThresholdSwitch",
    Fref  = NA_real_, c4 = NA_real_, b4 = NA_real_, n = NA_real_,
    RMSE  = rmse(obs, pred_Thr),
    R2    = r2(obs, pred_Thr),
    AIC   = NA_real_   # not a single fit, so AIC is undefined
  )
)


# --- write parameters+metrics
out_params <- file.path(eval_dir, "params_metrics_12_13.csv")
write_csv(params_tbl, out_params)
message("Wrote: ", out_params)

# --- write daily predictions for downstream scripts
pred_df <- new_all %>%
  transmute(
    date, meanRsoil, meanSWC, meanTsoil, meanGPP,
    Pred_All = pred_All, Pred_NonPulse = pred_NP, Pred_Pulse = pred_P, Pred_Threshold = pred_Thr
  )
out_preds <- file.path(in_dir, "Chamber_model_predictions_12_13.csv")
write_csv(pred_df, out_preds)
message("Wrote: ", out_preds)

# --- Plot 1: annual sums of obs + model series
annual <- pred_df %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    Obs = sum(meanRsoil, na.rm=TRUE),
    All = sum(Pred_All, na.rm=TRUE),
    NonPulse = sum(Pred_NonPulse, na.rm=TRUE),
    Pulse = sum(Pred_Pulse, na.rm=TRUE),
    Threshold = sum(Pred_Threshold, na.rm=TRUE),
    .groups="drop"
  ) %>%
  pivot_longer(-year, names_to="Series", values_to="Sum")

p_annual <- ggplot(annual, aes(year, Sum, group=Series)) +
  geom_line() + geom_point(size=1) +
  theme_bw() + theme(panel.grid = element_blank()) +
  labs(x="Year", y=expression(paste("Annual ", R[soil])),
       title="Annual sums: Observed vs Models (All / NP / Pulse / Threshold)")
ggsave(file.path(fig_dir, "12_13_annual_sums.png"), p_annual, width=8, height=5, dpi=300)

# --- Plot 2: Observed vs Predicted (facets)
scatter_long <- pred_df %>%
  pivot_longer(c(Pred_All, Pred_NonPulse, Pred_Pulse, Pred_Threshold),
               names_to="Model", values_to="Pred")
p_scatter <- ggplot(scatter_long, aes(meanRsoil, Pred)) +
  geom_point(shape=1, alpha=0.8, size=1.6) +
  geom_abline(slope=1, intercept=0, linetype="dashed") +
  facet_wrap(~ Model, scales="free") +
  theme_bw() + theme(panel.grid = element_blank()) +
  labs(x=expression(paste("Observed ", R[soil])),
       y=expression(paste("Predicted ", R[soil])),
       title="Observed vs Predicted (faceted by model)")
ggsave(file.path(fig_dir, "12_13_obs_vs_pred.png"), p_scatter, width=8, height=6, dpi=300)

message("Done.")
