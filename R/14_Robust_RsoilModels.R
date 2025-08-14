# =====================================================
# >>> 14_Robust_RsoilModels.R
# =====================================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(lubridate)
  library(ggplot2); library(tidyr); library(minpack.lm)
})

# ------------------ Config (edit here) ------------------
# --- Fast add-ons toggles ---
CV_FOR_NP_P_THR   <- TRUE   # add year-blocked CV for NP, P, Threshold
ADD_MBE_MAE       <- TRUE   # add bias & absolute error to metrics
SHOW_PARAM_CORR   <- TRUE   # compute param correlation across multi-starts
SHOW_DRIVER_COLL  <- TRUE   # summarize driver collinearity (All/NP/P)
PLOT_RESIDUALS    <- TRUE   # residual vs fitted / drivers / QQ
IN_DIR      <- "out/derived"
FIG_DIR     <- "out/figs/14"
EVAL_DIR    <- "out/model_eval/chamber/14"
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(EVAL_DIR, recursive = TRUE, showWarnings = FALSE)

USE_ROBUST_GPP <- TRUE         # TRUE -> use q=0.99 instead of literal max
GPP_Q          <- 0.99
MULTISTART     <- 30           # number of random starts per fit
THRESH_SEARCH  <- TRUE         # grid search SWC cutoff
THRESH_GRID    <- seq(0.05, 0.30, by = 0.01)
BOUNDS_STYLE   <- "12and13"    # "12" (c4 can be <0), "13" (c4>=0), or "12and13"
SEED           <- 2025

# ------------------ Data ------------------
files <- c(
  all   = file.path(IN_DIR, "All_summary_chamber.csv"),
  np    = file.path(IN_DIR, "NonPulse_sum_chamber.csv"),
  pulse = file.path(IN_DIR, "Pulse_sum_chamber.csv")
)
missing <- names(files)[!file.exists(files)]
if (length(missing)) stop("Missing inputs: ", paste(basename(files[missing]), collapse = ", "))

coerce_clean <- function(d) {
  d %>%
    mutate(
      date      = as.Date(date),
      meanSWC   = suppressWarnings(as.numeric(meanSWC)),
      meanTsoil = suppressWarnings(as.numeric(meanTsoil)),
      meanGPP   = suppressWarnings(as.numeric(meanGPP)),
      meanRsoil = suppressWarnings(as.numeric(meanRsoil))
    ) %>%
    { if (max(.$meanSWC, na.rm=TRUE) > 1.5) mutate(., meanSWC = meanSWC/100) else . } %>%
    filter(is.finite(meanSWC), is.finite(meanTsoil), is.finite(meanGPP), is.finite(meanRsoil))
}
df_all   <- read_csv(files["all"],   show_col_types = FALSE) |> coerce_clean()
df_np    <- read_csv(files["np"],    show_col_types = FALSE) |> coerce_clean()
df_pulse <- read_csv(files["pulse"], show_col_types = FALSE) |> coerce_clean()

# Robust GPP scale
GPP_scale <- if (USE_ROBUST_GPP) quantile(df_all$meanGPP, probs = GPP_Q, na.rm = TRUE) else max(df_all$meanGPP, na.rm = TRUE)
if (!is.finite(GPP_scale) || GPP_scale <= 0) stop("Invalid GPP scale.")

# ------------------ Model & helpers ------------------
mae <- function(o,p) mean(abs(o-p), na.rm=TRUE)
mbe <- function(o,p) mean(p-o,     na.rm=TRUE)

# safe correlation that returns NA_real_ when there aren't >=2 complete pairs
safe_r2 <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) >= 2) {
    r <- suppressWarnings(cor(x[ok], y[ok]))
    if (is.finite(r)) r^2 else NA_real_
  } else NA_real_
}

safe_rmse <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) sqrt(mean((x[ok] - y[ok])^2)) else NA_real_
}

# param correlation from multi-starts (matrix), robust to few starts
param_corr <- function(coef_mat) {
  if (is.null(coef_mat) || nrow(coef_mat) < 5) return(matrix(NA_real_, 4, 4, dimnames=list(c("Fref","c4","b4","n"), c("Fref","c4","b4","n"))))
  suppressWarnings(cor(coef_mat, use="pairwise.complete.obs"))
}


form <- meanRsoil ~ Fref *
  ((meanGPP/GPPmax + n)/(1 + n)) *
  (1 - c4 * (0.1 - meanSWC)^2) *
  exp(b4 * meanTsoil)

rmse <- function(o,p) sqrt(mean((o-p)^2, na.rm=TRUE))
r2   <- function(o,p){ok<-is.finite(o)&is.finite(p); if(!any(ok)) return(NA_real_)
1 - sum((o-p)[ok]^2)/sum((o[ok]-mean(o[ok]))^2)}
aic_from_fit <- function(fit, n_obs) {
  k <- length(coef(fit)); rss_val <- sum(residuals(fit)^2)
  n_obs * log(rss_val / n_obs) + 2 * k
}

# bounds sets
get_bounds <- function(style=c("12","13")){
  style <- match.arg(style)
  if (style=="12") list(lower=c(Fref=0, c4=-200, b4=0,   n=0), upper=c(Fref=5, c4=200, b4=0.2, n=5))
  else             list(lower=c(Fref=0, c4=   0, b4=0,   n=0), upper=c(Fref=5, c4=200, b4=0.2, n=5))
}


# Fit low/high SWC models around a candidate threshold, then predict all days
fit_threshold <- function(df_all, thr, style, start_hint = NULL, min_n = 20) {
  low  <- df_all  %>% filter(meanSWC <  thr)
  high <- df_all  %>% filter(meanSWC >= thr)
  
  # skip thresholds that leave too few rows to fit
  if (nrow(low)  < min_n || nrow(high) < min_n) return(NULL)
  
  f_low  <- fit_multistart(low,  style, start_hint = start_hint)
  f_high <- fit_multistart(high, style, start_hint = start_hint)
  
  pred_low_all  <- predict_all_days(f_low$best,  new_all)
  pred_high_all <- predict_all_days(f_high$best, new_all)
  pred_comb     <- ifelse(new_all$meanSWC >= thr, pred_high_all, pred_low_all)
  
  list(thr = thr,
       f_low = f_low, f_high = f_high,
       pred_low_all = pred_low_all, pred_high_all = pred_high_all,
       pred_comb = pred_comb)
}



# multi-start fitter
# ---- REPLACE your current fit_multistart() with this ----
fit_multistart <- function(df, style, nstart = MULTISTART, start_hint = NULL) {
  set.seed(SEED)
  b <- get_bounds(style)
  d <- df %>% mutate(GPPmax = GPP_scale)
  
  draw_start <- function(){
    list(
      Fref = runif(1, 0.2, 2.0),
      c4   = if (style=="12") runif(1, -20, 20) else runif(1, 0, 20),
      b4   = runif(1, 0.005, 0.08),
      n    = runif(1, 0.01, 0.8)
    )
  }
  
  # build starts; use hint first (clipped to bounds)
  starts <- list()
  if (!is.null(start_hint)) {
    sh <- as.list(start_hint[c("Fref","c4","b4","n")])
    for (nm in names(sh)) {
      sh[[nm]] <- max(b$lower[[nm]], min(b$upper[[nm]], as.numeric(sh[[nm]])))
    }
    starts[[length(starts)+1]] <- sh
  }
  while (length(starts) < nstart) starts[[length(starts)+1]] <- draw_start()
  
  fits <- vector("list", length(starts)); rss <- rep(Inf, length(starts))
  for (i in seq_along(starts)) {
    st <- starts[[i]]
    fit <- try(
      nlsLM(form, data=d, start=st, lower=b$lower, upper=b$upper,
            control=nls.lm.control(maxiter=1000)),
      silent=TRUE
    )
    if (!inherits(fit,"try-error")) {
      fits[[i]] <- fit
      rss[i] <- sum(residuals(fit)^2)
    }
  }
  if (all(!is.finite(rss))) stop("All multi-start fits failed (style ", style, ").")
  best <- fits[[which.min(rss)]]
  
  coefs <- do.call(rbind, lapply(Filter(Negate(is.null), fits), coef))
  list(best = best,
       coef_spread = apply(coefs, 2, IQR, na.rm=TRUE),
       coef_draws  = coefs)
}


predict_all_days <- function(fit, new_all) as.numeric(predict(fit, newdata=new_all))

# year-block CV (leave-one-year-out)
cv_year_block <- function(fit, df_train, df_full) {
  yrs <- sort(unique(year(df_full$date)))
  rmse_test <- c()
  for (y in yrs) {
    train <- df_train %>% filter(year(date)!=y) %>% mutate(GPPmax=GPP_scale)
    test  <- df_full  %>% filter(year(date)==y) %>% mutate(GPPmax=GPP_scale)
    # refit on train
    f2 <- try(nlsLM(form, data=train, start=as.list(coef(fit)),
                    lower=get_bounds("12")$lower, upper=get_bounds("12")$upper,
                    control=nls.lm.control(maxiter=500)), silent=TRUE)
    if (inherits(f2,"try-error")) { rmse_test <- c(rmse_test, NA_real_); next }
    pred <- as.numeric(predict(f2, newdata=test))
    rmse_test <- c(rmse_test, rmse(test$meanRsoil, pred))
  }
  mean(rmse_test, na.rm=TRUE)
}

# NP/P CV: train on subset (!= year), test on same subset (== year)
cv_year_block_subset <- function(df_subset, fit_seed, bounds_style) {
  yrs <- sort(unique(year(df_subset$date)))
  rm <- c()
  for (y in yrs) {
    train <- df_subset %>% filter(year(date)!=y) %>% mutate(GPPmax=GPP_scale)
    test  <- df_subset %>% filter(year(date)==y)  %>% mutate(GPPmax=GPP_scale)
    f2 <- try(nlsLM(form, data=train, start=as.list(coef(fit_seed)),
                    lower=get_bounds(bounds_style)$lower,
                    upper=get_bounds(bounds_style)$upper,
                    control=nls.lm.control(maxiter=500)), silent=TRUE)
    if (inherits(f2,"try-error")) { rm <- c(rm, NA_real_); next }
    pred <- as.numeric(predict(f2, newdata=test))
    rm   <- c(rm, rmse(test$meanRsoil, pred))
  }
  mean(rm, na.rm=TRUE)
}

# Threshold CV: held-out year; apply fixed NP/P fits and a single cutoff
cv_year_block_threshold <- function(df_all_full, pred_NP_all, pred_P_all, cutoff) {
  yrs <- sort(unique(year(df_all_full$date)))
  rm <- c()
  for (y in yrs) {
    test  <- df_all_full %>% filter(year(date)==y)
    pt    <- ifelse(test$meanSWC >= cutoff,
                    pred_P_all[year(df_all_full$date)==y],
                    pred_NP_all[year(df_all_full$date)==y])
    rm <- c(rm, rmse(test$meanRsoil, pt))
  }
  mean(rm, na.rm=TRUE)
}


# ------------------ Fit(s) ------------------
# new_all <- df_all %>% select(date, meanSWC, meanTsoil, meanGPP, meanRsoil) %>% mutate(GPPmax = GPP_scale)


# Build Pulse flag (1=pulse, 0=non-pulse) on the main new_all table
pulse_dates <- df_pulse %>% transmute(date, PulseFlag = 1L)
new_all <- df_all %>%
  select(date, meanSWC, meanTsoil, meanGPP, meanRsoil) %>%
  left_join(pulse_dates, by = "date") %>%
  mutate(PulseFlag = if_else(is.na(PulseFlag), 0L, PulseFlag),
         GPPmax    = GPP_scale)

# # (later, after you compute pred_NP and pred_P)
# Pred_PN <- ifelse(new_all$PulseFlag == 1L, pred_P, pred_NP)



styles <- switch(BOUNDS_STYLE,
                 "12"="12",
                 "13"="13",
                 "12and13"=c("12","13"))

results_list <- list()

for (style in styles) {
  # multi-start per subset
  f_All <- fit_multistart(df_all,   style)
  f_NP  <- fit_multistart(df_np,    style)
  f_P   <- fit_multistart(df_pulse, style)
  
  pred_All <- predict_all_days(f_All$best, new_all)
  pred_NP  <- predict_all_days(f_NP$best,  new_all)
  pred_P   <- predict_all_days(f_P$best,   new_all)  
  
  # Single Pulse/Non-pulse model series (apply NP params on NP days, P params on P days)
  Pred_PN <- ifelse(new_all$PulseFlag == 1L, pred_P, pred_NP)
  
  # ---------------------------
  # Threshold search with re-fitting at each cutoff
  # Use the 'All' fit as a warm start to stabilize low/high fits
  # ---------------------------
  cands <- if (THRESH_SEARCH) THRESH_GRID else 0.15
  best <- NULL; best_rmse <- Inf
  for (thr in cands) {
    th <- try(fit_threshold(df_all, thr, style, start_hint = coef(f_All$best)), silent = TRUE)
    if (inherits(th, "try-error") || is.null(th)) next
    r  <- rmse(new_all$meanRsoil, th$pred_comb)
    if (is.finite(r) && r < best_rmse) { best <- th; best_rmse <- r }
  }
  
  # If no threshold candidate worked, fall back (keep NP/P components and mark Thr as NA)
  if (is.null(best)) {
    best_thr     <- NA_real_
    pred_ThrLow  <- rep(NA_real_, nrow(new_all))
    pred_ThrHigh <- rep(NA_real_, nrow(new_all))
    pred_Thr_opt <- rep(NA_real_, nrow(new_all))
    thr_low_fit  <- NULL
    thr_high_fit <- NULL
  } else {
    best_thr     <- best$thr
    pred_ThrLow  <- best$pred_low_all
    pred_ThrHigh <- best$pred_high_all
    pred_Thr_opt <- best$pred_comb
    thr_low_fit  <- best$f_low$best
    thr_high_fit <- best$f_high$best
  }
  
  # ---------------------------
  # CV metrics (computed after threshold selection)
  # ---------------------------
  obs   <- new_all$meanRsoil
  n_obs <- sum(is.finite(obs) & is.finite(pred_All))
  
  cv_All <- cv_year_block(f_All$best, df_all, df_all)
  cv_NP  <- if (CV_FOR_NP_P_THR) cv_year_block_subset(df_np,    f_NP$best,  style) else NA_real_
  cv_P   <- if (CV_FOR_NP_P_THR) cv_year_block_subset(df_pulse, f_P$best,   style) else NA_real_
  cv_Thr <- if (CV_FOR_NP_P_THR && !is.null(best))
    cv_year_block_threshold(new_all, pred_ThrLow, pred_ThrHigh, best_thr)
  else NA_real_
  
  # ---------------------------
  # Parameter correlation matrices (CSV)
  # ---------------------------
  if (SHOW_PARAM_CORR) {
    write.csv(param_corr(f_All$coef_draws), file.path(EVAL_DIR, paste0("param_corr_All_", style, ".csv")), row.names=TRUE)
    write.csv(param_corr(f_NP$coef_draws),  file.path(EVAL_DIR, paste0("param_corr_NonPulse_", style, ".csv")), row.names=TRUE)
    write.csv(param_corr(f_P$coef_draws),   file.path(EVAL_DIR, paste0("param_corr_Pulse_", style, ".csv")), row.names=TRUE)
  }
  
  # ---------------------------
  # Driver collinearity (write once)
  # ---------------------------
  if (SHOW_DRIVER_COLL && style == styles[1]) {
    dc <- bind_rows(
      df_all   %>% mutate(Set="All")   %>% select(Set, meanSWC, meanTsoil, meanGPP),
      df_np    %>% mutate(Set="NP")    %>% select(Set, meanSWC, meanTsoil, meanGPP),
      df_pulse %>% mutate(Set="Pulse") %>% select(Set, meanSWC, meanTsoil, meanGPP)
    ) %>%
      group_by(Set) %>%
      summarise(
        cor_SWC_T = cor(meanSWC,  meanTsoil, use="pairwise.complete.obs"),
        cor_SWC_G = cor(meanSWC,  meanGPP,   use="pairwise.complete.obs"),
        cor_T_G   = cor(meanTsoil,meanGPP,   use="pairwise.complete.obs"),
        .groups="drop"
      )
    write_csv(dc, file.path(EVAL_DIR, "driver_collinearity.csv"))
  }
  
  # ---------------------------
  # Metrics table (initialize for this style)
  # ---------------------------
  params_tbl <- dplyr::bind_rows(
    tibble(model=paste0("All_",style),
           Fref=coef(f_All$best)["Fref"], c4=coef(f_All$best)["c4"], b4=coef(f_All$best)["b4"], n=coef(f_All$best)["n"],
           RMSE=rmse(obs,pred_All), MAE=mae(obs,pred_All), MBE=mbe(obs,pred_All), R2=r2(obs,pred_All),
           AIC=aic_from_fit(f_All$best,n_obs), CV_RMSE_YearBlock=cv_All,
           cIQR_Fref=f_All$coef_spread["Fref"], cIQR_c4=f_All$coef_spread["c4"], cIQR_b4=f_All$coef_spread["b4"], cIQR_n=f_All$coef_spread["n"]),
    tibble(model=paste0("NonPulse_",style),
           Fref=coef(f_NP$best)["Fref"], c4=coef(f_NP$best)["c4"], b4=coef(f_NP$best)["b4"], n=coef(f_NP$best)["n"],
           RMSE=rmse(obs,pred_NP), MAE=mae(obs,pred_NP), MBE=mbe(obs,pred_NP), R2=r2(obs,pred_NP),
           AIC=aic_from_fit(f_NP$best,n_obs), CV_RMSE_YearBlock=cv_NP,
           cIQR_Fref=f_NP$coef_spread["Fref"], cIQR_c4=f_NP$coef_spread["c4"], cIQR_b4=f_NP$coef_spread["b4"], cIQR_n=f_NP$coef_spread["n"]),
    tibble(model=paste0("Pulse_",style),
           Fref=coef(f_P$best)["Fref"], c4=coef(f_P$best)["c4"], b4=coef(f_P$best)["b4"], n=coef(f_P$best)["n"],
           RMSE=rmse(obs,pred_P), MAE=mae(obs,pred_P), MBE=mbe(obs,pred_P), R2=r2(obs,pred_P),
           AIC=aic_from_fit(f_P$best,n_obs), CV_RMSE_YearBlock=cv_P,
           cIQR_Fref=f_P$coef_spread["Fref"], cIQR_c4=f_P$coef_spread["c4"], cIQR_b4=f_P$coef_spread["b4"], cIQR_n=f_P$coef_spread["n"]),
    tibble(model=paste0("ThresholdSwitch_",style,"(thr=",sprintf("%.2f",best_thr),")"),
           Fref=NA_real_, c4=NA_real_, b4=NA_real_, n=NA_real_,
           RMSE=rmse(obs,pred_Thr_opt), MAE=mae(obs,pred_Thr_opt), MBE=mbe(obs,pred_Thr_opt), R2=r2(obs,pred_Thr_opt),
           AIC=NA_real_, CV_RMSE_YearBlock=cv_Thr,
           cIQR_Fref=NA_real_, cIQR_c4=NA_real_, cIQR_b4=NA_real_, cIQR_n=NA_real_)
  )
  
  # Append fitted threshold sides only if we have them
  if (!is.null(thr_low_fit) && !is.null(thr_high_fit)) {
    params_tbl <- bind_rows(
      params_tbl,
      tibble(model = paste0("Thr_low_", style, "(<", sprintf("%.2f", best_thr), ")"),
             Fref = coef(thr_low_fit)["Fref"],  c4 = coef(thr_low_fit)["c4"],
             b4 = coef(thr_low_fit)["b4"],      n  = coef(thr_low_fit)["n"],
             RMSE = rmse(obs, pred_ThrLow), MAE = mae(obs, pred_ThrLow),
             MBE  = mbe(obs, pred_ThrLow),  R2  = r2(obs, pred_ThrLow),
             AIC  = aic_from_fit(thr_low_fit, n_obs), CV_RMSE_YearBlock = NA_real_,
             cIQR_Fref = best$f_low$coef_spread["Fref"], cIQR_c4 = best$f_low$coef_spread["c4"],
             cIQR_b4   = best$f_low$coef_spread["b4"],   cIQR_n  = best$f_low$coef_spread["n"]),
      tibble(model = paste0("Thr_high_", style, "(≥", sprintf("%.2f", best_thr), ")"),
             Fref = coef(thr_high_fit)["Fref"], c4 = coef(thr_high_fit)["c4"],
             b4 = coef(thr_high_fit)["b4"],     n  = coef(thr_high_fit)["n"],
             RMSE = rmse(obs, pred_ThrHigh), MAE = mae(obs, pred_ThrHigh),
             MBE  = mbe(obs, pred_ThrHigh), R2  = r2(obs, pred_ThrHigh),
             AIC  = aic_from_fit(thr_high_fit, n_obs), CV_RMSE_YearBlock = NA_real_,
             cIQR_Fref = best$f_high$coef_spread["Fref"], cIQR_c4 = best$f_high$coef_spread["c4"],
             cIQR_b4   = best$f_high$coef_spread["b4"],   cIQR_n  = best$f_high$coef_spread["n"])
    )
  }
  
  # ---------------------------
  # Predictions table for this style
  # ---------------------------
  preds_tbl <- new_all %>%
    transmute(date, meanRsoil, meanSWC, meanTsoil, meanGPP,
              Pred_All  = pred_All,
              Pred_PN   = Pred_PN,         # single PN model series
              Pred_NP   = pred_NP,         # components (optional)
              Pred_P    = pred_P,          # components (optional)
              Pred_Thr  = pred_Thr_opt,    # combined threshold series (NA if no threshold workable)
              Pred_ThrLow  = pred_ThrLow,  # NA if no threshold workable
              Pred_ThrHigh = pred_ThrHigh, # NA if no threshold workable
              Thr_used  = best_thr,
              Bounds    = style)
  
  results_list[[style]] <- list(params = params_tbl, preds = preds_tbl)
} # end for (style in styles)


# ===========================
# Bind & write outputs
# ===========================
params_out <- bind_rows(lapply(results_list, `[[`, "params"))
preds_out  <- bind_rows(lapply(results_list, `[[`, "preds"))

write_csv(params_out, file.path(EVAL_DIR, "params_metrics.csv"))
write_csv(preds_out,  file.path(IN_DIR,  "Chamber_model_predictions_14.csv"))
message("Wrote: ", file.path(EVAL_DIR,"params_metrics.csv"))
message("Wrote: ", file.path(IN_DIR, "Chamber_model_predictions_14.csv"))

# ===========================
# Minimal plots (saved)
# ===========================
# Annual sums incl. PN and Thr
annual <- preds_out %>%
  group_by(Bounds, year = year(date)) %>%
  summarise(
    Obs = sum(meanRsoil, na.rm=TRUE),
    All = sum(Pred_All,  na.rm=TRUE),
    PN  = sum(Pred_PN,   na.rm=TRUE),
    NP  = sum(Pred_NP,   na.rm=TRUE),
    P   = sum(Pred_P,    na.rm=TRUE),
    Thr = sum(Pred_Thr,  na.rm=TRUE),
    .groups="drop"
  ) %>%
  pivot_longer(-c(Bounds,year), names_to="Series", values_to="Sum")

p_annual <- ggplot(annual, aes(year, Sum, color=Series)) +
  geom_line() + geom_point(size=1) +
  facet_wrap(~ Bounds, scales="free_y") +
  theme_bw() + theme(panel.grid=element_blank()) +
  labs(x="Year", y=expression(paste("Annual ", R[soil])),
       title="Annual sums (Obs vs Models) by bounds style")
ggsave(file.path(FIG_DIR, "annual_sums_bounds.png"), p_annual, width=9, height=5, dpi=300)

# Obs vs Pred (faceted by model incl. PN)
p_sc <- preds_out %>%
  select(Bounds, meanRsoil, Pred_All, Pred_PN, Pred_NP, Pred_P, Pred_Thr) %>%
  pivot_longer(-c(Bounds,meanRsoil), names_to="Model", values_to="Pred") %>%
  ggplot(aes(meanRsoil, Pred)) +
  geom_point(shape=1, alpha=0.7, size=1.3) +
  geom_abline(slope=1, intercept=0, linetype="dashed") +
  facet_grid(Bounds ~ Model, scales="free") +
  theme_bw() + theme(panel.grid=element_blank()) +
  labs(x=expression(paste("Observed ", R[soil])),
       y=expression(paste("Predicted ", R[soil])),
       title="Observed vs Predicted (by bounds style)")
ggsave(file.path(FIG_DIR, "obs_vs_pred_bounds.png"), p_sc, width=11, height=6, dpi=300)

# Residual ACF (All model, example)
resids <- preds_out %>% mutate(res_All = Pred_All - meanRsoil)
png(file.path(FIG_DIR, "residual_acf_All.png"), width=800, height=500)
acf(resids$res_All[is.finite(resids$res_All)], main="ACF of residuals (All model)")
dev.off()

# Residual panels (optional)
if (PLOT_RESIDUALS) {
  one <- preds_out %>% filter(Bounds == unique(preds_out$Bounds)[1]) %>%
    mutate(res = Pred_All - meanRsoil)
  p1 <- ggplot(one, aes(Pred_All, res)) + geom_point(alpha=0.5, size=1) +
    geom_hline(yintercept=0, linetype="dashed") +
    theme_bw() + theme(panel.grid=element_blank()) +
    labs(x="Fitted (All)", y="Residual", title="Residual vs Fitted (All)")
  p2 <- ggplot(one, aes(meanSWC, res)) + geom_point(alpha=0.5, size=1) +
    geom_hline(yintercept=0, linetype="dashed") +
    theme_bw() + theme(panel.grid=element_blank()) +
    labs(x="SWC", y="Residual", title="Residual vs SWC")
  p3 <- ggplot(one, aes(meanTsoil, res)) + geom_point(alpha=0.5, size=1) +
    geom_hline(yintercept=0, linetype="dashed") +
    theme_bw() + theme(panel.grid=element_blank()) +
    labs(x="Tsoil", y="Residual", title="Residual vs Tsoil")
  qq <- qqnorm(one$res, plot.it=FALSE); qqdf <- data.frame(x=qq$x, y=qq$y)
  p4 <- ggplot(qqdf, aes(x, y)) + geom_point(alpha=0.6, size=1) +
    geom_abline(slope=1, intercept=0, linetype="dashed") +
    theme_bw() + theme(panel.grid=element_blank()) +
    labs(x="Theoretical Quantiles", y="Sample Quantiles", title="QQ Plot (Residuals)")
  ggsave(file.path(FIG_DIR, "residual_vs_fitted_All.png"), p1, width=6, height=4, dpi=300)
  ggsave(file.path(FIG_DIR, "residual_vs_SWC_All.png"),     p2, width=6, height=4, dpi=300)
  ggsave(file.path(FIG_DIR, "residual_vs_Tsoil_All.png"),   p3, width=6, height=4, dpi=300)
  ggsave(file.path(FIG_DIR, "residual_QQ_All.png"),         p4, width=6, height=4, dpi=300)
}

# ===========================
# Build combined predictions data frame for diagnostics (3-model view)
# ===========================
# Build combined predictions data frame (3-model view)
df_preds <- preds_out %>%
  select(date, obs = meanRsoil,
         Pred_All, Pred_PN, Pred_Thr,
         Pred_NP, Pred_P, Pred_ThrLow, Pred_ThrHigh,
         Bounds, Thr_used) %>%
  pivot_longer(cols = starts_with("Pred_"),
               names_to = "series", values_to = "pred") %>%
  mutate(model = case_when(
    series == "Pred_All" ~ "All",
    series == "Pred_PN"  ~ "Pulse/Non-pulse",
    series == "Pred_Thr" ~ "Threshold",
    series %in% c("Pred_NP","Pred_P") ~ "PN components",
    series %in% c("Pred_ThrLow","Pred_ThrHigh") ~ "Threshold components",
    TRUE ~ "Other"
  ))

# Keep only the 3 final model series
df_preds_3 <- df_preds %>% filter(series %in% c("Pred_All","Pred_PN","Pred_Thr"))

# Drop all-NA series (e.g., Threshold when no valid split)
df_preds_3 <- df_preds_3 %>%
  group_by(series) %>%
  filter(any(is.finite(pred))) %>%
  ungroup()

# Summary table robust to empty series
df_preds_3 %>%
  group_by(series) %>%
  summarise(
    n_pairs = sum(is.finite(pred) & is.finite(obs)),
    RMSE    = safe_rmse(pred, obs),
    R2      = safe_r2(pred, obs),
    .groups = "drop"
  ) %>%
  print()

# Time series (3 panels)
ggplot(df_preds_3, aes(x = date)) +
  geom_point(aes(y = obs), color = "black", size = 0.8, alpha = 0.5) +
  geom_line(aes(y = pred, color = series), size = 0.8) +
  scale_color_manual(values = c(Pred_All="#1b9e77", Pred_PN="#d95f02", Pred_Thr="#7570b3"),
                     labels = c(Pred_All="All", Pred_PN="Pulse/Non-pulse", Pred_Thr="Threshold")) +
  facet_wrap(~ series, scales = "free_y", ncol = 1,
             labeller = as_labeller(c(Pred_All="All", Pred_PN="Pulse/Non-pulse", Pred_Thr="Threshold"))) +
  labs(y = expression(R[soil]~(µmol~CO[2]~m^{-2}~s^{-1})),
       x = "Date", color = NULL,
       title = "Model predictions vs. observations (three-model view)") +
  theme_bw() + theme(legend.position = "bottom")

# Scatter (3 panels)
ggplot(df_preds_3, aes(x = obs, y = pred, color = series)) +
  geom_point(alpha = 0.6, size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_manual(values = c(Pred_All="#1b9e77", Pred_PN="#d95f02", Pred_Thr="#7570b3"),
                     labels = c(Pred_All="All", Pred_PN="Pulse/Non-pulse", Pred_Thr="Threshold")) +
  facet_wrap(~ series, scales = "free",
             labeller = as_labeller(c(Pred_All="All", Pred_PN="Pulse/Non-pulse", Pred_Thr="Threshold"))) +
  labs(x = expression(Observed~R[soil]), y = expression(Predicted~R[soil]),
       color = NULL, title = "Predicted vs Observed (three-model view)") +
  theme_bw() + theme(legend.position = "bottom")

