# 07_threshold15_RECO.R ---------------------------------------------------

# find setup no matter where this script is sourced from
setup_path <- if (file.exists("R/00_setup.R")) "R/00_setup.R" else "00_setup.R"
source(setup_path)

ys1 <- read_csv("out/derived/years_sum1_DM.csv", show_col_types = FALSE) %>%
  mutate(
    All_meanSWC5 = meanSWC5/100,
    All_meanST5  = meanST5,
    All_meanGPP  = meanGPP,
    All_GPPmax   = max(meanGPP, na.rm=TRUE),
    Threshold_15 = (meanSWC5 >= 15)
  )

less <- filter(ys1, !Threshold_15) %>%
  mutate(meanSWC5_NP_15 = meanSWC5/100,
         meanST5_NP_15  = meanST5,
         meanGPP_NP_15  = meanGPP,
         GPPmax_NP_15   = max(meanGPP, na.rm=TRUE))

more <- filter(ys1, Threshold_15) %>%
  mutate(meanSWC5_P_15 = meanSWC5/100,
         meanST5_P_15  = meanST5,
         meanGPP_P_15  = meanGPP,
         GPPmax_P_15   = max(meanGPP, na.rm=TRUE))

lower <- c(Fref=0,   c4=-100, b4=0,     n=0.0001)
upper <- c(Fref=10,  c4= 100, b4=0.20,  n=1.0)

m_np <- fit_nlsLM(
  meanRECO ~ Fref*((meanGPP_NP_15/GPPmax_NP_15 + n)/(1 + n)) *
    (1 - c4*(0.1 - meanSWC5_NP_15)^2) * exp(b4*meanST5_NP_15),
  data=less, start=c(Fref=0.75, c4=30, b4=0.04, n=0.08),
  lower=lower, upper=upper
)
m_p  <- fit_nlsLM(
  meanRECO ~ Fref*((meanGPP_P_15/GPPmax_P_15 + n)/(1 + n)) *
    (1 - c4*(0.1 - meanSWC5_P_15)^2) * exp(b4*meanST5_P_15),
  data=more, start=c(Fref=0.35, c4=-10, b4=0.06, n=0.40),
  lower=lower, upper=upper
)
m_all <- fit_nlsLM(
  meanRECO ~ Fref*((All_meanGPP/All_GPPmax + n)/(1 + n)) *
    (1 - c4*(0.1 - All_meanSWC5)^2) * exp(b4*All_meanST5),
  data=ys1, start=c(Fref=1.1, c4=-8, b4=0.036, n=0.08),
  lower=lower, upper=upper
)

co_np  <- coef(m_np); co_p <- coef(m_p); co_all <- coef(m_all)

pred_np  <- with(ys1,  co_np[["Fref"]]*((All_meanGPP/All_GPPmax + co_np[["n"]])/(1+co_np[["n"]]))*
                   (1 - co_np[["c4"]]*(0.1 - All_meanSWC5)^2) * exp(co_np[["b4"]]*All_meanST5))
pred_p   <- with(ys1,  co_p [["Fref"]]*((All_meanGPP/All_GPPmax + co_p [["n"]])/(1+co_p [["n"]]))*
                   (1 - co_p [["c4"]]*(0.1 - All_meanSWC5)^2) * exp(co_p [["b4"]]*All_meanST5))
pred_all <- with(ys1,  co_all[["Fref"]]*((All_meanGPP/All_GPPmax + co_all[["n"]])/(1+co_all[["n"]]))*
                   (1 - co_all[["c4"]]*(0.1 - All_meanSWC5)^2) * exp(co_all[["b4"]]*All_meanST5))

out <- ys1 %>%
  select(date, meanRECO, max_pulse_duration) %>%
  mutate(NonPulseM_15 = pred_np,
         PulseM_15    = pred_p,
         MeanM_15     = pred_all,
         Reco_Combined = dplyr::case_when(
           max_pulse_duration == 0 ~ NonPulseM_15,
           max_pulse_duration %in% c(8,14,20) ~ PulseM_15,
           TRUE ~ MeanM_15
         ))

metrics <- tibble(
  model = c("Combined15","Pulse15","NonPulse15","MeanAll"),
  RMSE  = c(rmse(out$meanRECO, out$Reco_Combined),
            rmse(out$meanRECO, out$PulseM_15),
            rmse(out$meanRECO, out$NonPulseM_15),
            rmse(out$meanRECO, out$MeanM_15)),
  MAPE  = c(mape(out$meanRECO, out$Reco_Combined),
            mape(out$meanRECO, out$PulseM_15),
            mape(out$meanRECO, out$NonPulseM_15),
            mape(out$meanRECO, out$MeanM_15)),
  R2    = c(rsq(out$meanRECO, out$Reco_Combined),
            rsq(out$meanRECO, out$PulseM_15),
            rsq(out$meanRECO, out$NonPulseM_15),
            rsq(out$meanRECO, out$MeanM_15))
)
write_csv(metrics, "out/derived/metrics_RECO_15.csv")

p_ts <- ggplot(out, aes(date)) +
  geom_point(aes(y=meanRECO), color="blue", size=0.8) +
  geom_point(aes(y=NonPulseM_15), color="red",   size=0.5, alpha=.6) +
  geom_point(aes(y=PulseM_15),    color="cyan4", size=0.5, alpha=.6) +
  geom_point(aes(y=MeanM_15),     color="green4",size=0.5, alpha=.6) +
  labs(y = expression(paste("Reco (", mu, "mol ", m^{-2}," ", s^{-1},")")), x = NULL,
       title = "RECO time series with model overlays (15% threshold)")
save_plot(p_ts, "reco_ts_overlay_15.png", w=7.5)

cum <- out %>%
  drop_na(meanRECO, Reco_Combined, MeanM_15) %>%
  arrange(date) %>%
  mutate(
    culMeasured = cumsum(meanRECO),
    culCombined = cumsum(Reco_Combined),
    culMean     = cumsum(MeanM_15)
  )
p_cum <- ggplot(cum, aes(date)) +
  geom_line(aes(y=culMeasured), color="blue") +
  geom_line(aes(y=culMean),     color="red")  +
  geom_line(aes(y=culCombined), color="green4") +
  labs(y="Cumulative Reco", x=NULL, title="Cumulative RECO")
save_plot(p_cum, "reco_cumulative_15.png", w=7.5)
