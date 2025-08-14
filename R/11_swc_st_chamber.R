# =====================================================
# >>> SWC ST space chamber.R (revised: numeric coercion)
# =====================================================
#
# 27-06-2024 (revised path + robust numeric SWC/Tsoil/Rsoil)
# Anastasia Makhnykina

# Portable setup import (works whether run from project root or from R/)
setup_path <- if (file.exists("R/00_setup.R")) "R/00_setup.R" else "00_setup.R"
if (file.exists(setup_path)) source(setup_path)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
})

# --- helpers ---------------------------------------------------------------
numify <- function(df, cols) {
  # Coerce selected columns to numeric, safely
  df %>%
    mutate(across(all_of(cols), ~ suppressWarnings(as.numeric(.x))))
}

to_percent_vec <- function(x) {
  if (all(is.na(x))) return(x)
  # If values look like fractions (<= 1.5), convert to %
  if (max(x, na.rm = TRUE) <= 1.5) x * 100 else x
}

scale_size_fun <- function(vals, divisor = 2) {
  s <- vals / divisor
  s[s < 0.1 & !is.na(s)] <- 0.1
  s
}

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


# --- read derived chamber summaries ---------------------------------------
summary_Cham   <- read_csv("out/derived/All_summary_chamber.csv",   show_col_types = FALSE)
NonPulse_Cham  <- read_csv("out/derived/NonPulse_sum_chamber.csv",  show_col_types = FALSE)
Pulse_Cham     <- read_csv("out/derived/Pulse_sum_chamber.csv",     show_col_types = FALSE)

# Force key columns to numeric in case they were written with mixed types
coerce_cols <- c("meanSWC", "meanTsoil", "meanRsoil")
summary_Cham  <- numify(summary_Cham,  intersect(coerce_cols, names(summary_Cham)))
NonPulse_Cham <- numify(NonPulse_Cham, intersect(coerce_cols, names(NonPulse_Cham)))
Pulse_Cham    <- numify(Pulse_Cham,    intersect(coerce_cols, names(Pulse_Cham)))

# Make SWC percent for plotting, regardless of input scale
Pulse_Cham <- Pulse_Cham %>% mutate(SWC_per = to_percent_vec(meanSWC))
NonPulse_Cham <- NonPulse_Cham %>% mutate(SWC_per = to_percent_vec(meanSWC))

# --- plots ----------------------------------------------------------------
# Pulse
if (nrow(Pulse_Cham)) {
  with(Pulse_Cham, {
    plot(
      SWC_per, meanTsoil,
      cex = scale_size_fun(meanRsoil, divisor = 2),
      pch = 16,
      xlab = "Mean SWC 5 cm, %",
      ylab = "Mean Tsoil 5 cm, °C",
      main = "SWC and Soil T — during pulses",
      xlim = c(0, max(60, ceiling(max(SWC_per, na.rm = TRUE)))),
      ylim = c(0, max(40, ceiling(max(meanTsoil, na.rm = TRUE))))
    )
  })
} else {
  message("No rows in Pulse_Cham.")
}
# PULSE: ST vs Rsoil
save_last_plot("st_vs_rsoil_pulse.png", width = 6, height = 5)


# Non-pulse
if (nrow(NonPulse_Cham)) {
  with(NonPulse_Cham, {
    plot(
      SWC_per, meanTsoil,
      cex = scale_size_fun(meanRsoil, divisor = 2),
      pch = 16,
      xlab = "Mean SWC 5 cm, %",
      ylab = "Mean Tsoil 5 cm, °C",
      main = "SWC and Soil T — between pulses",
      xlim = c(0, max(60, ceiling(max(SWC_per, na.rm = TRUE)))),
      ylim = c(0, max(40, ceiling(max(meanTsoil, na.rm = TRUE))))
    )
  })
} else {
  message("No rows in NonPulse_Cham.")
}

# NONPULSE: SWC vs Rsoil
save_last_plot("swc_vs_rsoil_nonpulse.png", width = 6, height = 5)


# 
# # ALL: SWC vs Rsoil
# save_last_plot("swc_vs_rsoil_all.png", width = 6, height = 5)
# 
# # ALL: ST vs Rsoil
# save_last_plot("st_vs_rsoil_all.png", width = 6, height = 5)
# 
# # PULSE: SWC vs Rsoil
# save_last_plot("swc_vs_rsoil_pulse.png", width = 6, height = 5)
# 
# # NONPULSE: ST vs Rsoil
# save_last_plot("st_vs_rsoil_nonpulse.png", width = 6, height = 5)


