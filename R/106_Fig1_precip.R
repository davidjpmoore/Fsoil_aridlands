# R/106_Fig1_precip.R ---------------------------------------------------------
# Publication Figure 1: Precipitation characteristics — two-panel histogram.
#
# Panel a: frequency histogram of dry-spell lengths (days between consecutive
#   rain events > 5 mm).
# Panel b: frequency histogram of daily rainfall for events > 5 mm.
#
# Layout (figure_style_defaults.md Multi-Panel Layout Conventions):
#   - Shared y-axis type (Frequency): y-label on panel a only; panel b
#     suppresses y-axis title, tick labels, and tick marks (Rule 1).
#   - Panels have different x-axis variables — each panel keeps its own
#     x-axis label; no shared textGrob row needed.
#   - Panel labels a, b inside plot area via annotate() (Rule 3).
#   - No legend.
#
# Data source: out/derived/USWkg12_20_summary.csv (same as R/03_rain_pulse_figs.R)
# Outputs:     final/figures/Fig1_PrecipCharacteristics.png
#              final/figures/Fig1_PrecipCharacteristics_caption.txt
#
# Run standalone from the project root:
#   source("R/106_Fig1_precip.R")
# Do NOT add to run_all.R until the PI approves the figure.
# -----------------------------------------------------------------------------

# --- 0. Style & packages -----------------------------------------------------
style_path <- if (file.exists("R/000_figure_style.R")) "R/000_figure_style.R" else "000_figure_style.R"
source(style_path)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(gridExtra)
  library(grid)
})

dir.create("final/figures", recursive = TRUE, showWarnings = FALSE)

# --- 1. Read and prepare data (same logic as R/03_rain_pulse_figs.R) ---------
summary_df <- read_csv("out/derived/USWkg12_20_summary.csv",
                       show_col_types = FALSE) %>%
  arrange(date) %>%
  mutate(observation = row_number())

# Rain events: days with sum_R > 5 mm
# dry_spell_days: gap in row numbers between consecutive rain events (= days)
rain_events <- summary_df %>%
  filter(sum_R > 5) %>%
  mutate(dry_spell_days = observation - lag(observation,
                                            default = first(observation)))

message("Rain events (sum_R > 5 mm): ", nrow(rain_events))

# --- 2. Panel a: dry-spell lengths -------------------------------------------
p_a <- ggplot(rain_events, aes(x = dry_spell_days)) +
  geom_histogram(color = "black", fill = "white", bins = 30) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     sec.axis = dup_axis(labels = NULL, name = NULL)) +
  annotate("text", x = Inf, y = Inf, label = "a",
           hjust = 1.3, vjust = 1.3, fontface = "bold", size = 5) +
  labs(x = "Dry spell length (days)", y = "Frequency") +
  theme_pub() +
  theme(
    plot.margin      = margin(t = 5, r = 2, b = 5, l = 5, unit = "pt"),
    axis.text.y.right = element_blank()
  )

# --- 3. Panel b: rainfall amounts for events > 5 mm -------------------------
# y-axis suppressed — shared label "Frequency" lives on panel a (Rule 1).
p_b <- ggplot(rain_events, aes(x = sum_R)) +
  geom_histogram(color = "black", fill = "white", bins = 30) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     sec.axis = dup_axis(labels = NULL, name = NULL)) +
  annotate("text", x = Inf, y = Inf, label = "b",
           hjust = 1.3, vjust = 1.3, fontface = "bold", size = 5) +
  labs(x = "Rainfall (mm)", y = NULL) +
  theme_pub() +
  theme(
    axis.text.y       = element_blank(),
    axis.text.y.right = element_blank(),
    plot.margin       = margin(t = 5, r = 5, b = 5, l = 2, unit = "pt")
  )

# --- 4. Assemble and save ---------------------------------------------------
fig1_grob <- arrangeGrob(p_a, p_b, ncol = 2)

out_png <- "final/figures/Fig1_PrecipCharacteristics.png"
png(out_png,
    width  = SAVE_DOUBLE_COL_W,
    height = 3.5,
    units  = "in",
    res    = SAVE_DPI_PUB,
    bg     = "white")
grid.draw(fig1_grob)
dev.off()
message("Saved: ", out_png)

# --- 5. Caption file ---------------------------------------------------------
caption <- paste0(
  "Figure 1. Precipitation characteristics at the US-Wkg (Walnut Gulch Kendall) ",
  "arid grassland site, 2012–2020.\n\n",

  "Panel (a): Frequency distribution of dry-spell lengths, defined as the number ",
  "of days between consecutive rain events exceeding 5 mm d⁻¹. Dry spells are ",
  "calculated as the gap in days between successive qualifying rain events, ",
  "consistent with the pulse-trigger threshold used throughout the analysis.\n\n",

  "Panel (b): Frequency distribution of daily rainfall totals for all rain events ",
  "exceeding 5 mm d⁻¹ (n = 168 events). Only days above this threshold are ",
  "included; the x-axis therefore begins at 5 mm.\n\n",

  "Both histograms use 30 bins. The 5 mm threshold defines the minimum rainfall ",
  "required to initiate a respiration pulse window (see CLAUDE.md)."
)

out_cap <- "final/figures/Fig1_PrecipCharacteristics_caption.txt"
writeLines(caption, out_cap)
message("Saved: ", out_cap)
