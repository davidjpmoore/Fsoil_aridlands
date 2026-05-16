# R/116_Fig9_model_pulse_contribution.R
# Three-panel publication figure: model-predicted pulse contribution to annual RECO
# (a) Mean model  (b) P-NP switch  (c) 15% threshold
# Grey circles = observed (reproduced from Fig 2c); black circles = model predictions
# Saves: final/figures/Fig9_ModelPulseContribution.png + _caption.txt

if (file.exists("R/000_figure_style.R")) source("R/000_figure_style.R") else source("000_figure_style.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(gridExtra)
  library(grid)
  library(scales)
})

# --- IO ---
f_pred15  <- "out/derived/RECO_predictions_15.csv"
f_predPNP <- "out/derived/RECO_predictions_PNP.csv"
f_obs     <- "out/derived/years_sum1_DM.csv"
fig_out   <- "final/figures/Fig9_ModelPulseContribution.png"
cap_out   <- "final/figures/Fig9_ModelPulseContribution_caption.txt"
dir.create("final/figures", recursive = TRUE, showWarnings = FALSE)

# --- Load predictions ---
pred15 <- read_csv(f_pred15, show_col_types = FALSE) %>%
  mutate(date = as.Date(date), year = as.integer(format(date, "%Y")))

predPNP <- read_csv(f_predPNP, show_col_types = FALSE) %>%
  mutate(date = as.Date(date), year = as.integer(format(date, "%Y")))

# --- Load observed RECO (grey reference circles — same computation as Fig 2c) ---
obs <- read_csv(f_obs, show_col_types = FALSE) %>%
  mutate(
    date      = as.Date(date),
    year      = as.integer(format(date, "%Y")),
    PulseFlag = as.integer(max_pulse_duration > 0),
    RECO_gC   = meanRECO * 86400 * 12e-6      # µmol m-2 s-1 -> g C m-2 d-1
  )

# --- Annual pulse fractions -------------------------------------------------
# All three model panels use the rainfall-event classification (PulseFlag == 1)
# for both axes, so comparisons between panels are on the same footing and
# consistent with the observed reference circles.

compute_fracs <- function(df, pred_col) {
  df %>%
    filter(year != 2012, !is.na(.data[[pred_col]])) %>%
    group_by(year) %>%
    summarise(
      pulse_day_pct  = 100 * mean(PulseFlag == 1L, na.rm = TRUE),
      pulse_reco_pct = 100 * sum(.data[[pred_col]][PulseFlag == 1L], na.rm = TRUE) /
                             sum(.data[[pred_col]],                  na.rm = TRUE),
      .groups = "drop"
    )
}

obs_fracs  <- obs %>%
  filter(year != 2012, !is.na(RECO_gC)) %>%
  group_by(year) %>%
  summarise(
    pulse_day_pct  = 100 * mean(PulseFlag == 1L, na.rm = TRUE),
    pulse_reco_pct = 100 * sum(RECO_gC[PulseFlag == 1L], na.rm = TRUE) /
                          sum(RECO_gC,                   na.rm = TRUE),
    .groups = "drop"
  )

mean_fracs <- compute_fracs(pred15,  "MeanM_15")
pnp_fracs  <- compute_fracs(predPNP, "Reco_PNP")
thr_fracs  <- compute_fracs(pred15,  "Reco_Combined")

# --- Shared axis limits (identical across all panels) ---
all_vals <- c(
  obs_fracs$pulse_day_pct,  obs_fracs$pulse_reco_pct,
  mean_fracs$pulse_reco_pct, pnp_fracs$pulse_reco_pct, thr_fracs$pulse_reco_pct
)
lim_max <- ceiling(max(all_vals, na.rm = TRUE) / 5) * 5   # round up to nearest 5 %
xlim    <- c(0, lim_max)
ylim    <- c(0, lim_max)

pct_fmt <- function(x) paste0(x, "%")

# --- Panel builder -----------------------------------------------------------
make_panel <- function(mod_fracs, obs_fracs, label, title,
                       show_y = TRUE, show_legend = FALSE) {

  # Combine observed and modelled for a single aesthetic mapping
  df_obs <- obs_fracs %>% mutate(group = "Observed")
  df_mod <- mod_fracs %>% mutate(group = "Modelled")
  df_all <- bind_rows(df_obs, df_mod) %>%
    mutate(group = factor(group, levels = c("Modelled", "Observed")))

  p <- ggplot(df_all,
              aes(x = pulse_day_pct, y = pulse_reco_pct,
                  fill = group, color = after_scale(fill))) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40",
                linewidth = 0.5) +
    geom_point(shape = 21, size = 2) +
    scale_fill_manual(
      name   = NULL,
      values = c("Modelled" = "black", "Observed" = "grey60"),
      breaks = c("Modelled", "Observed")
    ) +
    scale_x_continuous(limits = xlim, labels = pct_fmt,
                       breaks = pretty(xlim, n = 4)) +
    scale_y_continuous(limits = ylim, labels = pct_fmt,
                       breaks = pretty(ylim, n = 4)) +
    ggtitle(title) +
    labs(x = NULL, y = NULL) +
    # Panel label: bold, inside top-left corner
    annotate("text", x = -Inf, y = Inf, label = label,
             hjust = -0.3, vjust = 1.3, fontface = "bold", size = 4) +
    theme_pub() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10, face = "plain",
                                margin = margin(b = 2))
    )

  # Suppress y-axis labels on non-leftmost panels but keep tick marks
  if (!show_y) {
    p <- p + theme(
      axis.text.y  = element_blank(),
      axis.ticks.y = element_line(color = "black", linewidth = 0.4),
      plot.margin  = margin(t = 5, r = 2, b = 5, l = 2, unit = "pt")
    )
  } else {
    p <- p + theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "pt"))
  }

  if (show_legend) {
    p <- p + theme(
      legend.position      = c(0.97, 0.03),
      legend.justification = c("right", "bottom"),
      legend.background    = element_blank(),
      legend.box.background = element_blank(),
      legend.text          = element_text(size = 9),
      legend.key.size      = unit(1.0, "lines")
    )
  } else {
    p <- p + theme(legend.position = "none")
  }

  p
}

# --- Build panels ---
p_a <- make_panel(mean_fracs, obs_fracs, label = "a", title = "Mean",
                  show_y = TRUE,  show_legend = FALSE)
p_b <- make_panel(pnp_fracs,  obs_fracs, label = "b", title = "P-NP switch",
                  show_y = FALSE, show_legend = FALSE)
p_c <- make_panel(thr_fracs,  obs_fracs, label = "c", title = "15% threshold",
                  show_y = FALSE, show_legend = TRUE)

# --- Layout: shared axis labels (figure_style_defaults.md Rule 1 + 2) -------
row_grob <- arrangeGrob(p_a, p_b, p_c, ncol = 3)

ylab_grob <- textGrob(
  "Pulse fraction of annual RECO (%)",
  rot = 90, vjust = 0.5,
  gp  = gpar(fontsize = 10)
)
xlab_grob <- textGrob(
  "Pulse-day fraction of year (%)",
  vjust = 0.5,
  gp    = gpar(fontsize = 10)
)

# y-label column (narrow) | panel row
middle_row <- arrangeGrob(
  ylab_grob, row_grob,
  ncol   = 2,
  widths = unit(c(0.07, 1), "null")
)

# panel rows / shared x-label row (thin)
layout <- arrangeGrob(
  middle_row, xlab_grob,
  nrow    = 2,
  heights = unit(c(1, 0.07), "null")
)

# --- Save ---
png(fig_out,
    width  = SAVE_DOUBLE_COL_W,
    height = 3.5,
    units  = "in",
    res    = SAVE_DPI_PUB,
    bg     = BG_WHITE)
grid.draw(layout)
dev.off()
message("Saved: ", fig_out)

# --- Caption file ---
writeLines(c(
  "Fig. 9. Model-predicted disproportionate contribution of pulse days to annual",
  "ecosystem respiration (RECO) at US-Wkg (Walnut Gulch, AZ), 2013-2020.",
  "",
  "Each panel plots the fraction of annual model-predicted RECO that fell on",
  "rainfall-event pulse days (y-axis) against the fraction of days per year",
  "classified as pulse days (x-axis), following the structure of Fig. 2c.",
  "Panels compare three model variants:",
  "  (a) Mean - single parameter set fitted to all days;",
  "  (b) P-NP switch - separate parameter sets for pulse and non-pulse days,",
  "      switching on the rainfall-event criterion (max_pulse_duration > 0);",
  "  (c) 15% threshold switch - separate parameter sets for days with soil water",
  "      content at 5 cm >= or < 15%, switching on the SWC threshold.",
  "",
  "Black filled circles: model-predicted pulse fraction of annual RECO (one",
  "point per year). Grey filled circles: observed RECO from eddy-covariance",
  "(identical across all panels, reproduced from Fig. 2c for reference).",
  "The dashed 1:1 line indicates equal pulse-day and pulse-RECO fractions;",
  "points above this line indicate a disproportionately large pulse contribution.",
  "",
  "Pulse days are defined by the rainfall-event criterion throughout (daily",
  "precipitation > 5 mm triggers an 8-20 day window depending on amount;",
  "CLAUDE.md for full definition). This common definition is applied to all",
  "three panels so contributions are directly comparable. 2012 is excluded",
  "due to sparse data coverage."
), cap_out)
message("Saved caption: ", cap_out)
