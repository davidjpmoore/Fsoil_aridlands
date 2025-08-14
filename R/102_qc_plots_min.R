# R/102_qc_plots_min.R
setup_path <- if (file.exists("R/00_setup.R")) "R/00_setup.R" else "00_setup.R"
source(setup_path)

df <- readr::read_csv("out/derived/eddy_daily_with_pulse.csv", show_col_types = FALSE)

# 1) Histogram of daily rain (only days with rain to show distribution)
p_rain <- ggplot2::ggplot(dplyr::filter(df, sum_R > 0), aes(x = sum_R)) +
  ggplot2::geom_histogram(color = "black", fill = "white", bins = 30) +
  ggplot2::labs(title = "Daily rainfall (mm) on wet days", x = "Rain (mm)", y = "Frequency")
save_plot(p_rain, "qc_rain_hist.png")

# 2) Dry-spell histogram: gaps between rain >5 mm events
rain_days <- df %>% dplyr::filter(sum_R > 5) %>% dplyr::arrange(date)
if (nrow(rain_days) >= 2) {
  gaps <- as.numeric(diff(rain_days$date), units = "days")
  gap_df <- tibble::tibble(gap_days = gaps)
  p_gaps <- ggplot2::ggplot(gap_df, aes(x = gap_days)) +
    ggplot2::geom_histogram(color = "black", fill = "white", bins = 30) +
    ggplot2::labs(title = "Dry-spell length between >5 mm events", x = "Days", y = "Frequency")
  save_plot(p_gaps, "qc_dry_spell_hist.png")
}

# 3) Pulse vs non-pulse counts
p_flag <- ggplot2::ggplot(df, aes(x = factor(PulseFlag, levels = c(0,1), labels = c("Non-pulse","Pulse")))) +
  ggplot2::geom_bar(color = "black", fill = "white") +
  ggplot2::labs(title = "Counts of days by pulse classification", x = NULL, y = "Days")
save_plot(p_flag, "qc_pulse_counts.png")

message("✓ Saved: qc_rain_hist.png, qc_dry_spell_hist.png (if applicable), qc_pulse_counts.png")
