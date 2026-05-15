# R/107_Fig3_seasonal.R -------------------------------------------------------
# Publication Figure 3: Seasonal pulse response — histogram row + normalised
# composite row.
#
# Row 1 (panels a-c): Frequency histogram of RECO on rain-event days (sum_R > 5 mm)
#   for Winter / Spring / Summer. Shared y-axis scale; y-label on panel a only;
#   shared x-axis label centered below all three panels via textGrob (not labs()).
#
# Row 2 (panels d-f): Normalised temporal composite of RECO and SWC 5 cm vs
#   days since rain event, using the same hand-picked windows as
#   R/04_seasonal_pulses.R. Each variable is divided by its day-0 value so
#   both series start at 1.0 at the moment of rain. y-label on panel d only;
#   shared x-axis label centered below all three panels via textGrob.
#   Legend inside panel f.
#
# Layout conventions (figure_style_defaults.md §Multi-Panel Layout Conventions):
#   - Panel labels inside plotting area via annotate(), not external ggpubr labels
#   - Shared y-axis: tick labels + title on leftmost panel only; tight margins
#   - Shared x-axis: ALL panels have labs(x = NULL); label sits in a textGrob
#     row below both outer panel rows via arrangeGrob() — equal panel heights
#   - Legend: inside rightmost row-2 panel (f), top-right corner, no border
#
# Inputs:  out/derived/years_sum1_DM.csv
# Outputs: final/figures/Fig3_SeasonalPulses.png
#          final/figures/Fig3_SeasonalPulses_caption.txt
#
# Run standalone from the project root:
#   source("R/107_Fig3_seasonal.R")
# Do NOT source from run_all.R until the PI approves the figure.
# -----------------------------------------------------------------------------

# --- 0. Style & packages -----------------------------------------------------
style_path <- if (file.exists("R/000_figure_style.R")) "R/000_figure_style.R" else "000_figure_style.R"
source(style_path)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(ggplot2)
  library(tidyr)
  library(purrr)
  library(gridExtra)
  library(grid)
})

dir.create("final/figures", recursive = TRUE, showWarnings = FALSE)

# --- 1. Read data -------------------------------------------------------------
df <- read_csv("out/derived/years_sum1_DM.csv", show_col_types = FALSE) %>%
  mutate(
    date     = as.Date(date),
    year     = year(date),
    DOY      = yday(date),
    meanRECO = suppressWarnings(as.numeric(meanRECO)),
    meanSWC5 = suppressWarnings(as.numeric(meanSWC5))
  ) %>%
  filter(year > 2012)  # exclude 2012 — sparse coverage (CLAUDE.md)

# Season classification (CLAUDE.md definitions)
df <- df %>%
  mutate(Season = case_when(
    DOY %in% c(1:59, 305:366) ~ "Winter",
    DOY %in% 60:181           ~ "Spring",
    DOY %in% 182:304          ~ "Summer",
    TRUE                      ~ NA_character_
  ))

message("Data loaded: ", nrow(df), " days (2013-2020).")

# --- 2. Row 1: RECO histograms on rain-event days (same logic as 04) ---------

# Rain-event days (sum_R > 5 mm), same filter as 04_seasonal_pulses.R
pulse_days <- df %>%
  filter(sum_R > 5, !is.na(Season), is.finite(meanRECO))

message("Rain-event days by season:")
print(table(pulse_days$Season))

# Shared bin breaks computed from the pooled distribution (all seasons)
# so bin edges and widths are identical across all three columns
shared_breaks <- hist(pulse_days$meanRECO, breaks = 30, plot = FALSE)$breaks

# Pre-compute per-season histogram counts
hist_counts <- function(season_label) {
  x <- pulse_days$meanRECO[pulse_days$Season == season_label]
  h <- hist(x[is.finite(x)], breaks = shared_breaks, plot = FALSE)
  data.frame(
    xmin  = h$breaks[-length(h$breaks)],
    xmax  = h$breaks[-1],
    count = h$counts
  )
}

hdat <- list(
  Winter = hist_counts("Winter"),
  Spring = hist_counts("Spring"),
  Summer = hist_counts("Summer")
)

# Shared y-limit with 8% headroom
y_max_hist <- max(sapply(hdat, function(h) max(h$count))) * 1.08

# Row-1 x-axis label — stored here; applied as a shared textGrob below all
# three panels in the assembler (§4), NOT via labs() on any individual panel.
RECO_HIST_XLAB <- expression(
  "Reco" ~ "(" * mu * "mol" ~ CO[2] ~ m^{-2} ~ s^{-1} * ")"
)

# show_y TRUE  → leftmost panel (a): y-axis label + tick labels visible
#         FALSE → panels b, c: suppress y-axis title, tick labels, tick marks
# x is always NULL — shared label lives in the textGrob row below all panels.
make_hist_panel <- function(season_label, panel_label, show_y = TRUE) {
  p <- ggplot(hdat[[season_label]],
              aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = count)) +
    geom_rect(color = "black", fill = "white", linewidth = 0.3) +
    scale_y_continuous(
      limits = c(0, y_max_hist),
      expand = expansion(mult = 0, add = 0)
    ) +
    annotate("text", x = -Inf, y = Inf, label = panel_label,
             hjust = -0.3, vjust = 1.3, fontface = "bold", size = 5) +
    labs(
      x     = NULL,
      y     = if (show_y) "Frequency" else NULL,
      title = season_label
    ) +
    theme_pub() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11))

  if (show_y) {
    p <- p + theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "pt"))
  } else {
    p <- p + theme(
      axis.text.y  = element_blank(),
      axis.ticks.y = element_line(color = "black", linewidth = 0.4),
      plot.margin  = margin(t = 5, r = 5, b = 5, l = 2, unit = "pt")
    )
  }
  p
}

p_hist_win <- make_hist_panel("Winter", "a", show_y = TRUE)
p_hist_spr <- make_hist_panel("Spring", "b", show_y = FALSE)
p_hist_sum <- make_hist_panel("Summer", "c", show_y = FALSE)

# --- 3. Row 2: normalised composites -----------------------------------------
# Hand-picked pulse windows — identical to R/04_seasonal_pulses.R.
# center = DOY of the rain event (day 0 in the composite).

winter_windows <- list(
  list(year = 2013, start = 323, end = 340, center = 326),
  list(year = 2015, start = 316, end = 333, center = 319),
  list(year = 2015, start = 343, end = 360, center = 346),
  list(year = 2016, start = 29,  end = 46,  center = 32),
  list(year = 2017, start = 348, end = 365, center = 351),
  list(year = 2019, start = 320, end = 337, center = 323),
  list(year = 2019, start = 322, end = 339, center = 325),
  list(year = 2019, start = 340, end = 357, center = 343)
)

spring_windows <- list(
  list(year = 2015, start = 99,  end = 116, center = 102),
  list(year = 2015, start = 174, end = 191, center = 177),
  list(year = 2015, start = 175, end = 192, center = 178),
  list(year = 2016, start = 178, end = 195, center = 181),
  list(year = 2017, start = 173, end = 190, center = 176),
  list(year = 2018, start = 164, end = 181, center = 167)
)

summer_windows <- list(
  list(year = 2015, start = 181, end = 198, center = 184),
  list(year = 2015, start = 183, end = 200, center = 186),
  list(year = 2015, start = 209, end = 226, center = 212),
  list(year = 2016, start = 196, end = 213, center = 199),
  list(year = 2016, start = 197, end = 214, center = 200),
  list(year = 2018, start = 216, end = 233, center = 219),
  list(year = 2019, start = 237, end = 254, center = 240)
)

# Normalise each window by its day-0 value, then average across windows.
# Windows are excluded if day-0 RECO or SWC5 is missing or non-positive.
composite_normalised <- function(summary_df, windows) {
  pieces <- map_dfr(windows, function(w) {
    dfw <- summary_df %>%
      filter(year == w$year, DOY >= w$start, DOY <= w$end) %>%
      mutate(Pulse_day = DOY - w$center)

    day0 <- filter(dfw, Pulse_day == 0)
    if (nrow(day0) == 0) return(NULL)

    reco0 <- day0$meanRECO[1]
    swc0  <- day0$meanSWC5[1]
    if (!is.finite(reco0) || !is.finite(swc0) ||
        reco0 <= 0 || swc0 <= 0) return(NULL)

    dfw %>%
      filter(Pulse_day >= -3, Pulse_day <= 14) %>%
      transmute(
        Pulse_day,
        reco_norm = meanRECO / reco0,
        swc_norm  = meanSWC5 / swc0
      )
  })

  if (nrow(pieces) == 0) {
    warning("composite_normalised: no valid windows — returning NULL")
    return(NULL)
  }

  pieces %>%
    group_by(Pulse_day) %>%
    summarise(
      n         = n(),
      mean_reco = mean(reco_norm, na.rm = TRUE),
      se_reco   = sd(reco_norm,  na.rm = TRUE) / sqrt(sum(!is.na(reco_norm))),
      mean_swc  = mean(swc_norm,  na.rm = TRUE),
      se_swc    = sd(swc_norm,   na.rm = TRUE) / sqrt(sum(!is.na(swc_norm))),
      .groups   = "drop"
    )
}

comp_win <- composite_normalised(df, winter_windows)
comp_spr <- composite_normalised(df, spring_windows)
comp_sum <- composite_normalised(df, summer_windows)

message("Composite rows — Winter: ", nrow(comp_win),
        "  Spring: ", nrow(comp_spr),
        "  Summer: ", nrow(comp_sum))

# show_y       TRUE  → leftmost panel (d): y-axis label + tick labels visible
#              FALSE → panels e, f: suppress y-axis title, tick labels, tick marks
# show_legend  TRUE  → panel f (Summer, rightmost): legend top-right, no border
# x is always NULL — shared label lives in the textGrob row below all panels.
make_comp_panel <- function(comp, panel_label, show_y = TRUE, show_legend = FALSE) {
  comp_long <- bind_rows(
    comp %>% transmute(Pulse_day, value = mean_reco, se = se_reco,
                       Variable = "Reco"),
    comp %>% transmute(Pulse_day, value = mean_swc,  se = se_swc,
                       Variable = "SWC 5 cm")
  ) %>%
    mutate(Variable = factor(Variable, levels = c("Reco", "SWC 5 cm")))

  p <- ggplot(comp_long,
              aes(x = Pulse_day, y = value, shape = Variable, fill = Variable)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey55",
               linewidth  = 0.4) +
    geom_errorbar(aes(ymin = value - se, ymax = value + se),
                  width = 0.5, color = "black", linewidth = 0.4) +
    geom_point(size = 1.8, color = "black") +
    scale_shape_manual(values = c("Reco" = 21, "SWC 5 cm" = 24)) +
    scale_fill_manual(values  = c("Reco" = "black", "SWC 5 cm" = "white")) +
    scale_x_continuous(breaks = c(-3, 0, 3, 7, 10, 14), limits = c(-4, 15)) +
    annotate("text", x = -Inf, y = Inf, label = panel_label,
             hjust = -0.3, vjust = 1.3, fontface = "bold", size = 5) +
    labs(
      x = NULL,
      y = if (show_y) "Response relative to day 0" else NULL
    ) +
    theme_pub()

  if (show_legend) {
    p <- p + theme(
      legend.position      = c(0.97, 0.97),
      legend.justification = c("right", "top"),
      legend.background    = element_blank(),
      legend.box.background = element_blank(),
      legend.text          = element_text(size = 9),
      legend.key.size      = unit(1.0, "lines")
    )
  } else {
    p <- p + theme(legend.position = "none")
  }

  if (show_y) {
    p <- p + theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "pt"))
  } else {
    p <- p + theme(
      axis.text.y  = element_blank(),
      axis.ticks.y = element_line(color = "black", linewidth = 0.4),
      plot.margin  = margin(t = 5, r = 5, b = 5, l = 2, unit = "pt")
    )
  }
  p
}

# Panel d (Winter): y-label visible; no legend
p_comp_win <- make_comp_panel(comp_win, "d", show_y = TRUE,  show_legend = FALSE)
# Panel e (Spring): suppress y-axis; no legend
p_comp_spr <- make_comp_panel(comp_spr, "e", show_y = FALSE, show_legend = FALSE)
# Panel f (Summer): suppress y-axis; legend inside top-right
p_comp_sum <- make_comp_panel(comp_sum, "f", show_y = FALSE, show_legend = TRUE)

# --- 4. Assemble -------------------------------------------------------------
# x-axis labels are textGrob rows below each panel row.
# This guarantees all three panels in each row have identical plot dimensions.
# Using labs(x = ...) on only the center panel allocates extra vertical space
# to that panel, making it shorter — the textGrob approach avoids this entirely.

xlab_row1 <- textGrob(
  RECO_HIST_XLAB,
  vjust = 0.5,
  gp    = gpar(fontsize = 10)
)
xlab_row2 <- textGrob(
  "Days since rain event",
  vjust = 0.5,
  gp    = gpar(fontsize = 10)
)

row1_grob <- arrangeGrob(p_hist_win, p_hist_spr, p_hist_sum, ncol = 3)
row2_grob <- arrangeGrob(p_comp_win, p_comp_spr, p_comp_sum, ncol = 3)

fig3_grob <- arrangeGrob(
  row1_grob, xlab_row1,
  row2_grob, xlab_row2,
  nrow    = 4,
  heights = unit(c(1, 0.07, 1, 0.07), "null")
)

# --- 5. Save -----------------------------------------------------------------
# png() + grid.draw() used instead of ggsave() because the layout object is
# a gtable grob (arrangeGrob output), not a ggplot object.
out_png <- "final/figures/Fig3_SeasonalPulses.png"
png(out_png,
    width  = SAVE_DOUBLE_COL_W,
    height = 5.5,
    units  = "in",
    res    = SAVE_DPI_PUB,
    bg     = "white")
grid.draw(fig3_grob)
dev.off()
message("Saved: ", out_png)

# --- 6. Caption file ---------------------------------------------------------
caption <- paste0(
  "Figure 3. Seasonal pulse responses in ecosystem respiration (RECO) at the ",
  "US-Wkg (Walnut Gulch Kendall) arid grassland site, 2013-2020.\n\n",

  "Row 1 (panels a-c): Frequency distributions of daily mean RECO on rain-event ",
  "days (precipitation > 5 mm d⁻¹) for Winter (panel a; DOY 305-366 and ",
  "1-59), Spring (panel b; DOY 60-181), and Summer (panel c; DOY 182-304). All ",
  "three panels share the same y-axis scale, so differences in the number of ",
  "qualifying rain events between seasons are directly comparable. The x-axis uses ",
  "consistent bin widths derived from the pooled cross-season distribution.\n\n",

  "Row 2 (panels d-f): Normalised temporal composites of RECO (● filled black ",
  "circles) and volumetric soil water content at 5 cm depth (SWC 5 cm; △ open ",
  "triangles) for the same three seasons. Each composite aggregates hand-picked ",
  "pulse windows (Winter n = 8 events; Spring n = 6; Summer n = 7) using the same ",
  "windows as the diagnostic Figure 3 time-series (R/04_seasonal_pulses.R). ",
  "Both variables are normalised by dividing each day's value by the ",
  "corresponding day-0 (rain-event day) value within each window, so both series ",
  "start at 1.0 at the moment of rain. Symbols show means across windows; error ",
  "bars show ±1 standard error. The dashed grey horizontal line marks the ",
  "day-0 reference level (ratio = 1.0). The legend (panel f) identifies the two ",
  "symbol types and applies to all three row-2 panels.\n\n",

  "Season definitions follow CLAUDE.md: Winter = DOY 305-366 and 1-59; ",
  "Spring = DOY 60-181; Summer = DOY 182-304. Year 2012 is excluded due to ",
  "sparse eddy covariance coverage. Windows spanning multiple years are ",
  "aligned to the rain-event year of each window center."
)

out_cap <- "final/figures/Fig3_SeasonalPulses_caption.txt"
writeLines(caption, out_cap)
message("Saved: ", out_cap)
