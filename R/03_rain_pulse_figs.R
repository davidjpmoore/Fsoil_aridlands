# 03_rain_pulse_figs.R ----------------------------------------------------

# find setup no matter where this script is sourced from
setup_path <- if (file.exists("R/00_setup.R")) "R/00_setup.R" else "00_setup.R"
source(setup_path)

USWkg12_20_summary <- read_csv("out/derived/USWkg12_20_summary.csv", show_col_types = FALSE) %>%
  arrange(date) %>%
  mutate(
    observation = row_number(),
    DOY = yday(date),
    bigR = as.numeric(sum_R > 5),
    bigRmm = if_else(bigR == 1, sum_R, 0)
  )

USW9sum <- USWkg12_20_summary %>% filter(bigRmm > 0) %>% arrange(date)
USW9sum <- USW9sum %>% mutate(diff = observation - lag(observation, default = first(observation)))

p1 <- ggplot(USW9sum, aes(x=bigRmm))+
  geom_histogram(color="black", fill="white", bins=30)+
  labs(title='Rain > 5 mm', y='Frequency', x='Rain, mm')
save_plot(p1, "rain_gt5_hist.png")

p2 <- ggplot(USW9sum, aes(x=diff))+
  geom_histogram(color="black", fill="white", bins=30)+
  labs(title='Periods without rain', y='Frequency', x='Days')
save_plot(p2, "dry_spell_hist.png")
