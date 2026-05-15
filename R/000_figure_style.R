# R/000_figure_style.R
# =============================================================================
# Shared publication figure style — sourced at the top of every pub figure
# script (103, 104, 105) before any ggplot2 calls.
#
# Usage (add as the first source() call in each figure script):
#   if (file.exists("R/000_figure_style.R")) source("R/000_figure_style.R") \
#   else source("000_figure_style.R")
#
# Defines (all objects are named for direct reference in figure scripts):
#   Themes     : theme_pub() [primary], theme_in_ticks [compat alias],
#                theme_in_ticks_angle, theme_in_ticks_legend
#   Palettes   : PAL_PULSE
#   Heatmap    : COL_DELTA_LOW/MID/HIGH, scale_fill_delta_reco()
#   Points     : PT_SCATTER, PT_DISP, PT_BUBBLE, BAR_LINEWIDTH
#   Saving     : SAVE_DPI_PUB, SAVE_DPI_DIAG,
#                SAVE_SINGLE_COL_W, SAVE_DOUBLE_COL_W,
#                SAVE_DIAG_W, SAVE_DIAG_H,
#                SAVE_SINGLE_W, SAVE_SINGLE_H,
#                BG_WHITE
#   ggarrange  : GGARRANGE_LABELS, GGARRANGE_FONT_LABEL
#   Axis labels: LABEL_RECO, LABEL_RECO_ANNUAL, LABEL_DELTA_RECO,
#                LABEL_RECO_SIZE, LABEL_SWC_5CM, LABEL_TSOIL_5CM,
#                LABEL_PULSE_DAY_FRAC, LABEL_PULSE_RECO_FRAC, LABEL_N_DAYS
#
# This file has NO side-effects beyond defining objects and loading ggplot2
# and scales. It does NOT read any data files.
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})


# -----------------------------------------------------------------------------
# 1. Base theme
# -----------------------------------------------------------------------------
# Minimal publication-ready theme parameterised by base_size.
# base_size = 11 → single-column figures (~3.5 in)
# base_size = 24 → design-at-2x for double-column; scale down at submission
# No legend title by default — add locally when the title carries information.

theme_pub <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid        = element_blank(),
      panel.border      = element_rect(color = "black", linewidth = 0.5, fill = NA),
      axis.ticks        = element_line(color = "black", linewidth = 0.4),
      axis.ticks.length = unit(-3, "pt"),
      axis.text.x       = element_text(margin = margin(t = 6)),
      axis.text.y       = element_text(margin = margin(r = 6)),
      legend.title      = element_blank(),
      legend.background = element_blank(),
      strip.background  = element_blank(),
      strip.text        = element_text(face = "bold")
    )
}

# Backward-compatible alias used by scripts 103, 104, 105 (base_size = 11).
theme_in_ticks <- theme_pub()


# -----------------------------------------------------------------------------
# 2. Theme variants
# -----------------------------------------------------------------------------

# For panels with discrete year labels that need angled x-axis text
# (bar chart panels a/b in script 103)
theme_in_ticks_angle <- theme_in_ticks +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1,
                                   margin = margin(t = 6)))

# For standalone single-panel figures that expose a full legend with a title
# (bubble-only panel in script 105)
theme_in_ticks_legend <- theme_pub() +
  theme(
    legend.title      = element_text(size = 10),
    legend.key.height = unit(0.9, "lines"),
    legend.key.width  = unit(1.1, "lines")
  )


# -----------------------------------------------------------------------------
# 3. Pulse / Non-pulse colour palette
# -----------------------------------------------------------------------------
# Used via scale_fill_manual(values = PAL_PULSE) in all three figure scripts.
# Non-pulse = open (white fill, black border); Pulse = solid black.

PAL_PULSE <- c("Non-pulse" = "white", "Pulse" = "black")


# -----------------------------------------------------------------------------
# 4. Diverging heatmap colour scale (ΔRECO, script 105)
# -----------------------------------------------------------------------------
# Colour anchors — reference these directly if building a custom scale.
COL_DELTA_LOW  <- "#2b8cbe"   # blue  — pulse RECO < non-pulse at same conditions
COL_DELTA_MID  <- "white"     # zero difference
COL_DELTA_HIGH <- "#d7301f"   # red   — pulse RECO > non-pulse at same conditions

# Constructor: returns a ggplot2 scale ready to add with +
# lim   = symmetric colour limit (typically Q95 of |ΔRECO| for the dataset)
# gamma = exponent for signed-power transform applied to RECO_diff before
#         plotting; values > 1 emphasise large differences (default 1.2)
scale_fill_delta_reco <- function(lim, gamma = 1.2) {
  scale_fill_gradient2(
    name     = LABEL_DELTA_RECO,
    low      = COL_DELTA_LOW,
    mid      = COL_DELTA_MID,
    high     = COL_DELTA_HIGH,
    midpoint = 0,
    limits   = c(-(lim ^ gamma), lim ^ gamma),
    oob      = scales::squish
  )
}


# -----------------------------------------------------------------------------
# 5. Standard point geometry constants
# -----------------------------------------------------------------------------

# Scatter panels: RECO vs single driver (scripts 103 panels a/b, 104 panels a/b)
#   geom_point(shape=PT_SCATTER$shape, color=PT_SCATTER$color,
#              size=PT_SCATTER$size,   alpha=PT_SCATTER$alpha)
PT_SCATTER <- list(shape = 21, color = "black", size = 1.8, alpha = 0.85)

# Disproportion plot: fixed black fill (103 panel c)
PT_DISP <- list(shape = 21, fill = "black", color = "black", size = 2.8)

# Bubble panels: size proportional to RECO (104 panel c, 105 bubble-only)
#   geom_point(aes(size = pmax(0, pmin(meanRECO, PT_BUBBLE$reco_clamp_max)),
#                  fill = PulseClass),
#              shape=PT_BUBBLE$shape, color=PT_BUBBLE$color,
#              alpha=PT_BUBBLE$alpha, stroke=PT_BUBBLE$stroke)
#   + scale_size_continuous(range  = PT_BUBBLE$size_range,
#                            limits = c(0, PT_BUBBLE$reco_clamp_max),
#                            breaks = PT_BUBBLE$size_breaks)
PT_BUBBLE <- list(
  shape          = 21,
  color          = "black",
  alpha          = 0.55,
  stroke         = 0.25,
  reco_clamp_max = 4,
  size_range     = c(1.2, 6),
  size_breaks    = 0:4
)

# Bar chart border linewidth (103 panels a/b)
BAR_LINEWIDTH <- 0.3


# -----------------------------------------------------------------------------
# 6. ggsave output dimensions and resolution
# -----------------------------------------------------------------------------

# Resolution
SAVE_DPI_PUB  <- 300   # publication-quality PNG
SAVE_DPI_DIAG <- 150   # diagnostic / review figures — fast, adequate on screen

# Background
BG_WHITE <- "white"

# Journal column widths (Nature family as reference baseline)
SAVE_SINGLE_COL_W  <- 3.5    # single column  (~89 mm)
SAVE_DOUBLE_COL_W  <- 7.2    # double column  (~183 mm)

# Diagnostic defaults
SAVE_DIAG_W <- 7
SAVE_DIAG_H <- 5

# Single-panel publication figures (scripts 105 — both outputs share these
# dimensions intentionally for seamless slide transitions between them)
SAVE_SINGLE_W <- 6.8
SAVE_SINGLE_H <- 5.0


# -----------------------------------------------------------------------------
# 7. Multi-panel label style (ggpubr::ggarrange)
# -----------------------------------------------------------------------------
GGARRANGE_LABELS     <- c("a", "b", "c")
GGARRANGE_FONT_LABEL <- list(size = 14, face = "bold")


# -----------------------------------------------------------------------------
# 8. Axis label expressions and strings
# -----------------------------------------------------------------------------

# µmol m⁻² s⁻¹ RECO axis title (panels a/b of scripts 103/104)
LABEL_RECO <- expression(
  RECO ~ "(" * mu * "mol " * m^{-2} * " " * s^{-1} * ")"
)

# g C m⁻² yr⁻¹ annual RECO axis title (script 103 panel b)
LABEL_RECO_ANNUAL <- expression(
  "Annual RECO (g C " ~ m^{-2} ~ yr^{-1} ~ ")"
)

# ΔRECO legend title for heatmap (script 105 delta panel)
LABEL_DELTA_RECO <- expression(
  Delta * RECO ~ " (" * mu * "mol " * m^{-2} * " " * s^{-1} * ")"
)

# RECO size-legend title for bubble panels (scripts 104/105)
LABEL_RECO_SIZE <- expression(
  RECO ~ "(" * mu * "mol " * m^{-2} * " " * s^{-1} * ")"
)

# Plain-text axis labels
LABEL_SWC_5CM         <- "Soil water content (5 cm, %)"
LABEL_TSOIL_5CM       <- "Soil temperature (5 cm, °C)"
LABEL_PULSE_DAY_FRAC  <- "Pulse-day fraction of year"
LABEL_PULSE_RECO_FRAC <- "Pulse fraction of annual RECO"
LABEL_N_DAYS          <- "Number of days"
