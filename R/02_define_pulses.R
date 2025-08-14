# 02_define_pulses.R ------------------------------------------------------

# find setup no matter where this script is sourced from
setup_path <- if (file.exists("R/00_setup.R")) "R/00_setup.R" else "00_setup.R"
source(setup_path)

df <- read_csv("out/derived/USWkg12_20_summary.csv", show_col_types = FALSE) %>%
  arrange(date)

# Length by rainfall class
len_from_sumR <- function(x) dplyr::case_when(
  x > 20 ~ 20L,
  x > 10 ~ 14L,
  x >  5 ~  8L,
  TRUE   ~  0L
)

wins  <- len_from_sumR(df$sum_R)
n     <- nrow(df)
dur   <- integer(n)

# take max duration across overlapping windows
for (i in which(wins > 0)) {
  j_end <- min(n, i + wins[i] - 1)
  dur[i:j_end] <- pmax(dur[i:j_end], wins[i])
}

df <- df %>%
  mutate(
    max_pulse_duration = dur,
    rain_event = as.integer(sum_R > 5),
    days_since_rain_event = {
      ctr <- integer(n())
      for (i in seq_len(n())) {
        ctr[i] <- if (sum_R[i] > 5) 0L else if (i == 1) 0L else ctr[i-1] + 1L
      }
      ctr
    }
  )

years_sum1 <- df
years_sum_Pulse0 <- df %>% filter(days_since_rain_event >= max_pulse_duration)
years_sum_Pulse1 <- df %>% filter(days_since_rain_event <  max_pulse_duration)

# Save histograms & CSVs
p_np <- ggplot(filter(years_sum_Pulse0, sum_R > 0), aes(x=sum_R)) +
  geom_histogram(color="black", fill="white", bins=30) +
  labs(title="Non-pulse time", x="Rainfall total (mm)", y="Frequency")
save_plot(p_np, "rain_hist_nonpulse.png")

p_p  <- ggplot(filter(years_sum_Pulse1, sum_R > 0), aes(x=sum_R)) +
  geom_histogram(color="black", fill="white", bins=30) +
  labs(title="Pulse time", x="Rainfall total (mm)", y="Frequency")
save_plot(p_p, "rain_hist_pulse.png")

write_csv(years_sum1,       "out/derived/years_sum1_DM.csv")
write_csv(years_sum_Pulse0, "out/derived/years_sum_Pulse0_DM.csv")
write_csv(years_sum_Pulse1, "out/derived/years_sum_Pulse1_DM.csv")
