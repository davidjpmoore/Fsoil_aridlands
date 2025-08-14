# 04_seasonal_pulses.R ----------------------------------------------------

# find setup no matter where this script is sourced from
setup_path <- if (file.exists("R/00_setup.R")) "R/00_setup.R" else "00_setup.R"
source(setup_path)

USWkg12_20_summary <- read_csv("out/derived/USWkg12_20_summary.csv", show_col_types = FALSE) %>%
  mutate(DOY = yday(date), year = year(date))

# Identify rain days >5 mm for pulse events
USW9sum <- USWkg12_20_summary %>%
  mutate(bigRmm = if_else(sum_R > 5, sum_R, 0)) %>%
  filter(bigRmm > 0)

# Season tags (Winter: DOY 305-366 & 1-59; Spring: 60-181; Summer: 182-304)
USW9sum <- USW9sum %>%
  mutate(
    Season = case_when(
      DOY %in% c(1:59,305:366)        ~ "Winter",
      DOY %in% 60:181                  ~ "Spring",
      DOY %in% 182:304                 ~ "Summer",
      TRUE ~ NA_character_
    )
  )

# Histograms of meanRECO during pulses by season
plot_season_hist <- function(dat, season, fname) {
  p <- dat %>%
    filter(Season == season) %>%
    mutate(meanRECO = as.numeric(meanRECO)) %>%
    ggplot(aes(x = meanRECO)) +
    geom_histogram(color="black", fill="white", bins=30) +
    labs(title=paste0(season, " pulses"),
         x=expression(paste("Reco (", mu, "mol ", m^{-2}, " ", s^{-1}, ")")),
         y="Frequency")
  save_plot(p, fname)
}

plot_season_hist(USW9sum, "Winter", "season_winter_reco_hist.png")
plot_season_hist(USW9sum, "Spring", "season_spring_reco_hist.png")
plot_season_hist(USW9sum, "Summer", "season_summer_reco_hist.png")

# ----- Pulse composites (replicates your hand-picked windows) -------------
# Helper: compute composite mean ± SE across windows for RECO and SWC5
composite_from_windows <- function(summary_df, windows, scale_swc = 10) {
  # windows: list of list(year=YYYY, start=DOY_start, end=DOY_end, center = DOY_zero)
  pieces <- purrr::imap_dfr(windows, function(w, k) {
    dfw <- summary_df %>% filter(year == w$year, DOY >= w$start, DOY <= w$end)
    # Pulse_day relative to center (0 at center)
    dfw %>%
      mutate(Pulse_day = DOY - w$center) %>%
      select(DOY, Pulse_day, meanRECO, sdReco, meanSWC5, sdSWC5)
  })
  agg <- pieces %>%
    filter(Pulse_day >= -3, Pulse_day <= 14) %>%
    mutate(meanRECO = as.numeric(meanRECO),
           meanSWC5 = as.numeric(meanSWC5),
           sdReco   = as.numeric(sdReco),
           sdSWC5   = as.numeric(sdSWC5)) %>%
    group_by(Pulse_day) %>%
    summarise(
      n = n(),
      meanFlux = mean(meanRECO, na.rm=TRUE),
      seFlux   = sd(meanRECO, na.rm=TRUE)/sqrt(n),
      meanSWC  = mean(meanSWC5, na.rm=TRUE),
      seSWC    = sd(meanSWC5, na.rm=TRUE)/sqrt(n),
      .groups="drop"
    ) %>%
    mutate(meanSWC_scaled = meanSWC/scale_swc,
           seSWC_scaled   = seSWC/scale_swc)
  agg
}

plot_composite <- function(comp, title, scale_label) {
  ggplot(comp, aes(x=as.factor(Pulse_day)))+
    geom_point(aes(y = meanFlux), size=2) +
    geom_point(aes(y = meanSWC_scaled), color='blue', size=2) +
    geom_errorbar(aes(ymin=meanFlux - seFlux, ymax= meanFlux + seFlux),
                  width=.2, position=position_dodge(.9)) +
    geom_errorbar(aes(ymin=meanSWC_scaled - seSWC_scaled, ymax= meanSWC_scaled + seSWC_scaled),
                  width=.2, position=position_dodge(.9), color='blue') +
    labs(
      title=title,
      x='Pulse duration, days',
      y=expression(paste("Reco (", mu, "mol ", m^{-2}, " ", s^{-1}, ")"))
    ) +
    scale_y_continuous(sec.axis = sec_axis(~.*scale_label, name="SWC 5 cm (%)"))
}

# Winter windows (exclude the partial MaxWin5 as in your notes)
winter_windows <- list(
  list(year=2013, start=323, end=340, center=326),
  list(year=2015, start=316, end=333, center=319),
  list(year=2015, start=343, end=360, center=346),
  list(year=2016, start=29,  end=46,  center=32),
  # list(year=2016, start=354, end=366, center=357), # excluded
  list(year=2017, start=348, end=365, center=351),
  list(year=2019, start=320, end=337, center=323),
  list(year=2019, start=322, end=339, center=325),
  list(year=2019, start=340, end=357, center=343)
)

comp_win <- composite_from_windows(USWkg12_20_summary, winter_windows, scale_swc = 10)
p_win <- plot_composite(comp_win, "Mean Winter pulse", scale_label = 10)
save_plot(p_win, "composite_winter.png", w=7.5, h=4.5)

# Spring windows (from your code)
spring_windows <- list(
  list(year=2015, start=99,  end=116, center=102),
  list(year=2015, start=174, end=191, center=177),
  list(year=2015, start=175, end=192, center=178),
  list(year=2016, start=178, end=195, center=181),
  list(year=2017, start=173, end=190, center=176),
  list(year=2018, start=164, end=181, center=167)
)
comp_spr <- composite_from_windows(USWkg12_20_summary, spring_windows, scale_swc = 15)
p_spr <- plot_composite(comp_spr, "Mean Spring pulse", scale_label = 15)
save_plot(p_spr, "composite_spring.png", w=7.5, h=4.5)

# Summer windows (from your code; fixed typo on SWC3sd origin)
summer_windows <- list(
  list(year=2015, start=181, end=198, center=184),
  list(year=2015, start=183, end=200, center=186),
  list(year=2015, start=209, end=226, center=212),
  list(year=2016, start=196, end=213, center=199),
  list(year=2016, start=197, end=214, center=200),
  list(year=2018, start=216, end=233, center=219),
  list(year=2019, start=237, end=254, center=240)
)
comp_sum <- composite_from_windows(USWkg12_20_summary, summer_windows, scale_swc = 10)
p_sum <- plot_composite(comp_sum, "Mean Summer pulse", scale_label = 10)
save_plot(p_sum, "composite_summer.png", w=7.5, h=4.5)
