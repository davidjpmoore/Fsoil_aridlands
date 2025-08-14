# =====================================================
# >>> GPP vs Reco eddy.R
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

# In this file we focus on the interactions Between Reco and GPP fluxes
# for all time, Pulse and non-Pulse time

# Read pulse division docs
years_sum1       <- readr::read_csv("out/derived/years_sum1_DM.csv", show_col_types = FALSE)
years_sum_Pulse0 <- readr::read_csv("out/derived/years_sum_Pulse0_DM.csv", show_col_types = FALSE)
years_sum_Pulse1 <- readr::read_csv("out/derived/years_sum_Pulse1_DM.csv", show_col_types = FALSE)


# Clean data (remove non-finite rows used in regression/plotting)
df_all   <- years_sum1        %>% filter(is.finite(meanGPP),  is.finite(meanRECO))
df_pulse <- years_sum_Pulse1  %>% filter(is.finite(meanGPP),  is.finite(meanRECO))
df_non   <- years_sum_Pulse0  %>% filter(is.finite(meanGPP),  is.finite(meanRECO))

# Plot data For All time
df_all %>%
  ggplot(aes(x = meanGPP, y = meanRECO)) +
  geom_point(shape = 1) +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  # robust labels on separate layers
  ggpubr::stat_regline_equation(
    aes(label = after_stat(eq.label)),
    label.x = 0, label.y = 5.8
  ) +
  ggpubr::stat_regline_equation(
    aes(label = after_stat(rr.label)),
    label.x = 0, label.y = 5.4
  ) +
  stat_smooth(method = "lm", formula = y ~ x, linewidth = 0.8) +
  ylab(~paste("Reco, ", mu, "mol m"^-2, " s"^-1)) +
  xlab(~paste("GPP, ", mu, "mol m"^-2, " s"^-1)) +
  ggtitle("All time") +
  ylim(0, 6)

# Plot data for pulse time
df_pulse %>%
  ggplot(aes(x = meanGPP, y = meanRECO)) +
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
  ylab(~paste("Reco, ", mu, "mol m"^-2, " s"^-1)) +
  xlab(~paste("GPP, ", mu, "mol m"^-2, " s"^-1)) +
  ggtitle("Pulse time") +
  ylim(0, 6)

# Plot data for non-Pulse time
df_non %>%
  ggplot(aes(x = meanGPP, y = meanRECO)) +
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
  ylab(~paste("Reco, ", mu, "mol m"^-2, " s"^-1)) +
  xlab(~paste("GPP, ", mu, "mol m"^-2, " s"^-1)) +
  ggtitle("Non-pulse time") +
  ylim(0, 6) +
  xlim(0, 6)


# --- 05 re-render & save with guards ------------------------------------
suppressPackageStartupMessages({
  library(ggplot2)
  library(ggpubr)
  library(dplyr)
})

# Use in-memory dfs (they exist per your diag)
ys1 <- years_sum1
p1  <- years_sum_Pulse1
p0  <- years_sum_Pulse0

stopifnot(is.data.frame(ys1), is.data.frame(p1), is.data.frame(p0))

mk_plot <- function(df, title, ylim_max = 6) {
  df %>%
    filter(is.finite(meanGPP), is.finite(meanRECO)) %>%
    ggplot(aes(x = meanGPP, y = meanRECO)) +
    geom_point(shape = 1) +
    theme_bw() +
    theme(text = element_text(size = 15)) +
    # safe labelling (avoids the earlier parse error)
    ggpubr::stat_regline_equation(aes(label = after_stat(eq.label)), label.x = 0, label.y = ylim_max - 0.2) +
    ggpubr::stat_regline_equation(aes(label = after_stat(rr.label)), label.x = 0, label.y = ylim_max - 0.6) +
    stat_smooth(method = "lm", formula = y ~ x, linewidth = 0.8) +
    ylab(~paste("Reco, ", mu, "mol m"^-2, " s"^-1)) +
    xlab(~paste("GPP, ", mu, "mol m"^-2, " s"^-1)) +
    ggtitle(title) +
    ylim(0, ylim_max)
}

# Output dir
out_dir <- "out/figs"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Build & save if there are finite pairs
save_if_ok <- function(df, fname, title) {
  n_ok <- sum(is.finite(df$meanGPP) & is.finite(df$meanRECO))
  if (n_ok > 0) {
    p <- mk_plot(df, title)
    fp <- file.path(out_dir, fname)
    ggsave(fp, p, width = 6, height = 4.5, dpi = 300)
    message("Saved: ", fp, "   (n=", n_ok, ")")
    return(fp)
  } else {
    message("Skipped ", title, " — no finite (GPP, RECO) pairs.")
    return(NA_character_)
  }
}

paths <- c(
  save_if_ok(ys1, "gpp_vs_reco_all.png",   "All time"),
  save_if_ok(p1,  "gpp_vs_reco_pulse.png", "Pulse time"),
  save_if_ok(p0,  "gpp_vs_reco_nonpulse.png", "Non-pulse time")
)

# Checklist
existing <- paths[file.exists(paths)]
missing  <- setdiff(paths, existing)
cat("\n05 plot checklist\n",
    "- wrote:   ", paste(basename(existing), collapse = ", "), "\n",
    if (length(missing)) paste0("- missing: ", paste(basename(missing), collapse = ", "), "\n") else "",
    sep = "")

