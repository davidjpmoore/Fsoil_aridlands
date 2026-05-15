# R/108_Fig2_PulseContribution.R ----------------------------------------------
# Publication Figure 2: Pulse-day contribution to annual RECO — three panels.
#
# Replaces R/103_Disprop_Pulse_Impact.R for publication purposes.
# Do NOT modify 103 until the PI has reviewed this output.
#
# Panel a: stacked bar chart — number of days per year by pulse/non-pulse class
# Panel b: stacked bar chart — annual RECO (g C m⁻² yr⁻¹) by pulse/non-pulse class
# Panel c: scatter — pulse-day fraction vs pulse-RECO fraction by year, with
#   shaded reference bands and 1:1 line. Legend (Pulse / Non-pulse) inside
#   panel c top-left.
#
# Colour/fill: PAL_PULSE from 000_figure_style.R ("Non-pulse" = white,
#   "Pulse" = black). Do not redefine colours locally.
#
# Layout (figure_style_defaults.md Multi-Panel Layout Conventions):
#   - Panels do not share y-axes — normal spacing between panels.
#   - Panel labels a, b, c inside plot area via annotate() (Rule 3).
#   - Legend inside panel c top-left (Rule 4 variant).
#
# Data source: out/derived/years_sum1_DM.csv (same as R/103_Disprop_Pulse_Impact.R)
# Outputs:     final/figures/Fig2_PulseContribution.png
#              final/figures/Fig2_PulseContribution_caption.txt
#
# Run standalone from the project root:
#   source("R/108_Fig2_PulseContribution.R")
# Do NOT add to run_all.R until the PI approves the figure.
# -----------------------------------------------------------------------------

# --- 0. Style & packages -----------------------------------------------------
style_path <- if (file.exists("R/000_figure_style.R")) "R/000_figure_style.R" else "000_figure_style.R"
source(style_path)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(scales)
  library(gridExtra)
  library(grid)
})

dir.create("final/figures", recursive = TRUE, showWarnings = FALSE)

# --- 1. Read and prepare data (same as R/103_Disprop_Pulse_Impact.R) ---------
ys1 <- read_csv("out/derived/years_sum1_DM.csv", show_col_types = FALSE)

if (!("PulseFlag" %in% names(ys1))) {
  ys1 <- ys1 %>%
    mutate(PulseFlag = as.integer(days_since_rain_event < max_pulse_duration))
}

df <- ys1 %>%
  filter(year != 2012) %>%
  mutate(
    PulseClass  = factor(if_else(PulseFlag == 1L, "Pulse", "Non-pulse"),
                         levels = c("Non-pulse", "Pulse")),
    RECO_gC_day = meanRECO * 86400 * 12e-6   # µmol m⁻² s⁻¹ → g C m⁻² d⁻¹
  )

message("Years in analysis: ", paste(sort(unique(df$year)), collapse = ", "))

# --- 2. Panel a: days per year by class --------------------------------------
counts_by_year <- df %>%
  group_by(year, PulseClass) %>%
  summarise(Days = n(), .groups = "drop")

p_a <- ggplot(counts_by_year,
              aes(x = factor(year), y = Days, fill = PulseClass)) +
  geom_col(color = "black", linewidth = BAR_LINEWIDTH) +
  scale_fill_manual(values = PAL_PULSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  annotate("text", x = -Inf, y = Inf, label = "a",
           hjust = -0.5, vjust = 1.3, fontface = "bold", size = 5) +
  labs(x = NULL, y = LABEL_N_DAYS) +
  theme_pub() +
  theme(
    axis.text.x     = element_text(angle = 30, hjust = 1, vjust = 1,
                                   margin = margin(t = 6)),
    legend.position = "none"
  )

# --- 3. Panel b: annual RECO by class ----------------------------------------
reco_by_year <- df %>%
  group_by(year, PulseClass) %>%
  summarise(RECO_total = sum(RECO_gC_day, na.rm = TRUE), .groups = "drop")

p_b <- ggplot(reco_by_year,
              aes(x = factor(year), y = RECO_total, fill = PulseClass)) +
  geom_col(color = "black", linewidth = BAR_LINEWIDTH) +
  scale_fill_manual(values = PAL_PULSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  annotate("text", x = -Inf, y = Inf, label = "b",
           hjust = -0.5, vjust = 1.3, fontface = "bold", size = 5) +
  labs(x = NULL, y = LABEL_RECO_ANNUAL) +
  theme_pub() +
  theme(
    axis.text.x     = element_text(angle = 30, hjust = 1, vjust = 1,
                                   margin = margin(t = 6)),
    legend.position = "none"
  )

# --- 4. Panel c: disproportion scatter plot ----------------------------------
frac_tbl <- df %>%
  group_by(year) %>%
  summarise(
    pulse_day_frac  = mean(PulseFlag == 1L),
    pulse_reco_frac = sum(RECO_gC_day[PulseFlag == 1L], na.rm = TRUE) /
      sum(RECO_gC_day, na.rm = TRUE),
    .groups = "drop"
  )

# Dummy rows (NA coordinates) injected solely to carry the fill aesthetic
# into panel c, enabling the Pulse / Non-pulse legend without altering the
# visible data. shape = 22 (square key) matches the bar-chart convention.
legend_data <- data.frame(
  pulse_day_frac  = c(NA_real_, NA_real_),
  pulse_reco_frac = c(NA_real_, NA_real_),
  PulseClass      = factor(c("Non-pulse", "Pulse"),
                           levels = c("Non-pulse", "Pulse"))
)

p_c <- ggplot(frac_tbl,
              aes(x = pulse_day_frac, y = pulse_reco_frac)) +
  annotate("rect", xmin = 0.30, xmax = 0.50, ymin = 0, ymax = 1,
           alpha = 0.10) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.50, ymax = 0.80,
           alpha = 0.10) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(shape = PT_DISP$shape, fill = PT_DISP$fill,
             color = PT_DISP$color, size = PT_DISP$size) +
  geom_point(data    = legend_data,
             aes(x   = pulse_day_frac, y = pulse_reco_frac,
                 fill = PulseClass),
             shape = 22, size = 4, color = "black", na.rm = TRUE) +
  scale_fill_manual(
    values = PAL_PULSE,
    guide  = guide_legend(override.aes = list(shape = 22, size = 4))
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1),
                     expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1),
                     expand = expansion(mult = c(0.04, 0.04))) +
  annotate("text", x = -Inf, y = Inf, label = "c",
           hjust = -0.5, vjust = 1.3, fontface = "bold", size = 5) +
  labs(x = "Pulse-day % of year", y = LABEL_PULSE_RECO_FRAC) +
  theme_pub() +
  theme(
    legend.position      = c(0.97, 0.03),
    legend.justification = c("right", "bottom"),
    legend.background    = element_blank(),
    legend.box.background = element_blank(),
    legend.text          = element_text(size = 9),
    legend.key.size      = unit(0.9, "lines")
  )

# --- 5. Assemble and save ---------------------------------------------------
fig2_grob <- arrangeGrob(p_a, p_b, p_c, ncol = 3)

out_png <- "final/figures/Fig2_PulseContribution.png"
png(out_png,
    width  = SAVE_DOUBLE_COL_W,
    height = 3.5,
    units  = "in",
    res    = SAVE_DPI_PUB,
    bg     = "white")
grid.draw(fig2_grob)
dev.off()
message("Saved: ", out_png)

# --- 6. Caption file ---------------------------------------------------------
caption <- paste0(
  "Figure 2. Disproportionate contribution of rainfall pulse days to annual ",
  "ecosystem respiration (RECO) at US-Wkg (Walnut Gulch Kendall), 2013–2020.\n\n",

  "Panel (a): Number of days classified as pulse (black) or non-pulse (white) ",
  "for each year. Pulse days are defined as days falling within a rainfall-event ",
  "window triggered by precipitation > 5 mm d⁻¹ (window duration 8–20 days ",
  "depending on event size; see CLAUDE.md).\n\n",

  "Panel (b): Annual total RECO (g C m⁻² yr⁻¹) partitioned into pulse and ",
  "non-pulse contributions. Daily RECO (µmol m⁻² s⁻¹) was converted to ",
  "g C m⁻² d⁻¹ before summing.\n\n",

  "Panel (c): Scatter plot of pulse-day fraction of the year (x-axis) vs ",
  "pulse-day fraction of annual RECO (y-axis) for each year (2013–2020). ",
  "Points above the 1:1 dashed line indicate that pulse days contribute more ",
  "RECO than their proportional share of the year. Shaded bands mark typical ",
  "ranges across years: 30–50% of days are pulse days (vertical band) and ",
  "50–80% of annual RECO occurs during pulse days (horizontal band).\n\n",

  "2012 is excluded due to sparse eddy covariance coverage."
)

out_cap <- "final/figures/Fig2_PulseContribution_caption.txt"
writeLines(caption, out_cap)
message("Saved: ", out_cap)
