# =====================================================
# >>> GPP vs Rsoil chamber.R
# =====================================================
#
# 17-05-2024  (revised)
# Anastasia Makhnykina

# Portable setup import (works whether run from project root or from R/)
setup_path <- if (file.exists("R/00_setup.R")) "R/00_setup.R" else "00_setup.R"
if (file.exists(setup_path)) source(setup_path)

library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(units)
library(ggpubr)

# In this file we plot figures to show interactions between Rsoil and GPP
# --- saving helper (safe, minimal) ---
fig_dir <- "out/figs"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

save_last_plot <- function(name, width = 6, height = 5, dpi = 300) {
  p <- ggplot2::last_plot()
  if (is.null(p)) {
    message("No last plot found for ", name, " — skipping.")
    return(invisible(FALSE))
  }
  ggplot2::ggsave(file.path(fig_dir, name), plot = p, width = width, height = height, dpi = dpi)
  message("Saved: ", file.path(fig_dir, name))
  invisible(TRUE)
}


# Open updated files
summary_Cham   <- read.csv("out/derived/All_summary_chamber.csv")
Pulse_Cham     <- read.csv("out/derived/Pulse_sum_chamber.csv")
NonPulse_Cham  <- read.csv("out/derived/NonPulse_sum_chamber.csv")


# Clean data (remove non-finite rows used in regression/plotting)
df_all   <- summary_Cham   %>% filter(is.finite(meanGPP),  is.finite(meanRsoil))
df_pulse <- Pulse_Cham     %>% filter(is.finite(meanGPP),  is.finite(meanRsoil))
df_non   <- NonPulse_Cham  %>% filter(is.finite(meanGPP),  is.finite(meanRsoil))

# Plot figure for all time
df_all %>%
  ggplot(aes(x = meanGPP, y = meanRsoil)) +
  geom_point(shape = 1) +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  ggpubr::stat_regline_equation(
    aes(label = after_stat(eq.label)),
    label.x = 0, label.y = 5.8
  ) +
  ggpubr::stat_regline_equation(
    aes(label = after_stat(rr.label)),
    label.x = 0, label.y = 5.4
  ) +
  stat_smooth(method = "lm", formula = y ~ x, linewidth = 0.8) +
  ylab(~paste("Soil emission, ", mu, "mol m"^-2, " s"^-1)) +
  xlab(~paste("GPP, ", mu, "mol m"^-2, " s"^-1)) +
  ggtitle("All time") +
  ylim(0, 6)
if (nrow(df_all)) save_last_plot("gpp_vs_rsoil_all.png", 6, 5, 300)

# Plot figure for Pulse time
df_pulse %>%
  ggplot(aes(x = meanGPP, y = meanRsoil)) +
  geom_point(shape = 1) +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  ggpubr::stat_regline_equation(
    aes(label = after_stat(eq.label)),
    label.x = 0, label.y = 5.8
  ) +
  ggpubr::stat_regline_equation(
    aes(label = after_stat(rr.label)),
    label.x = 0, label.y = 5.4
  ) +
  stat_smooth(method = "lm", formula = y ~ x, linewidth = 0.8) +
  ylab(~paste("Soil emission, ", mu, "mol m"^-2, " s"^-1)) +
  xlab(~paste("GPP, ", mu, "mol m"^-2, " s"^-1)) +
  ggtitle("Pulse time") +
  ylim(0, 6)
if (nrow(df_pulse)) save_last_plot("gpp_vs_rsoil_pulse.png", 6, 5, 300)

# Plot figure for non-pulse time
df_non %>%
  ggplot(aes(x = meanGPP, y = meanRsoil)) +
  geom_point(shape = 1) +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  ggpubr::stat_regline_equation(
    aes(label = after_stat(eq.label)),
    label.x = 0, label.y = 5.8
  ) +
  ggpubr::stat_regline_equation(
    aes(label = after_stat(rr.label)),
    label.x = 0, label.y = 5.4
  ) +
  stat_smooth(method = "lm", formula = y ~ x, linewidth = 0.8) +
  ylab(~paste("Soil emission, ", mu, "mol m"^-2, " s"^-1)) +
  xlab(~paste("GPP, ", mu, "mol m"^-2, " s"^-1)) +
  ggtitle("Non-Pulse time") +
  ylim(0, 6) +
  xlim(0, 6)
if (nrow(df_non)) save_last_plot("gpp_vs_rsoil_nonpulse.png", 6, 5, 300)
