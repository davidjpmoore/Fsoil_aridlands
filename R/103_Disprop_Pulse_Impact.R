# R/103_Disprop_Pulse_Impact.R
# Three-panel figure for publication:
# (a) Days/class per year, (b) Annual RECO totals by class, (c) Disproportion plot
# Saves: out/figs/Disprop_Pulse_Impact_legend.png and .txt
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(ggplot2); library(tidyr)
  library(scales); library(ggpubr); library(ggrepel); library(grid)
})

# --- IO ---
in_csv  <- "out/derived/years_sum1_DM.csv"
fig_png <- "out/figs/pub/Disprop_Pulse_Impact_legend.png"
txt_out <- "out/figs/pub/Disprop_Pulse_Impact_legend.txt"
dir.create("out/figs/pub", recursive = TRUE, showWarnings = FALSE)

# --- Read and prep ---
ys1 <- if (exists("years_sum1")) years_sum1 else readr::read_csv(in_csv, show_col_types = FALSE)

stopifnot(all(c("date","year","meanRECO") %in% names(ys1)))
if (!("PulseFlag" %in% names(ys1))) {
  ys1 <- ys1 %>% mutate(PulseFlag = as.integer(days_since_rain_event < max_pulse_duration))
}

df <- ys1 %>%
  filter(year != 2012) %>%                                # drop sparse 2012
  mutate(
    PulseClass   = factor(if_else(PulseFlag == 1L, "Pulse", "Non-pulse"),
                          levels = c("Non-pulse","Pulse")),
    RECO_gC_day  = meanRECO * 86400 * 12e-6               # µmol m^-2 s^-1 -> g C m^-2 d^-1
  )

# --- Theme: ticks facing inward + clean BW base ---
theme_in_ticks <- theme_bw(base_size = 12) +
  theme(
    panel.grid       = element_blank(),
    panel.border     = element_rect(color = "black", size = 0.5),
    axis.ticks.length = unit(-3, "pt"),
    axis.text.x      = element_text(margin = margin(t = 6)),
    axis.text.y      = element_text(margin = margin(r = 6)),
    legend.title     = element_blank()
  )

# Variant with angled x labels for discrete years
theme_in_ticks_angle <- theme_in_ticks +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))


palette_fill <- c("Non-pulse" = "white", "Pulse" = "black")

# --- Panel (a): stacked counts of days per year ---
counts_by_year <- df %>%
  group_by(year, PulseClass) %>%
  summarise(Days = n(), .groups = "drop")

p_a <- ggplot(counts_by_year, aes(x = factor(year), y = Days, fill = PulseClass)) +
  geom_col(color = "black", linewidth = 0.3) +
  scale_fill_manual(values = palette_fill) +
  labs(x = NULL, y = "Number of days") +
  theme_in_ticks_angle

# --- Panel (b): stacked annual RECO totals (g C m^-2 yr^-1) ---
reco_by_year <- df %>%
  group_by(year, PulseClass) %>%
  summarise(RECO_total = sum(RECO_gC_day, na.rm = TRUE), .groups = "drop")

p_b <- ggplot(reco_by_year, aes(x = factor(year), y = RECO_total, fill = PulseClass)) +
  geom_col(color = "black", linewidth = 0.3) +
  scale_fill_manual(values = palette_fill) +
  labs(x = NULL, y = expression("Annual RECO (g C " ~ m^{-2} ~ yr^{-1} ~ ")")) +
  theme_in_ticks_angle

# --- Panel (c): disproportion plot (pulse-day fraction vs pulse-RECO fraction) ---
frac_tbl <- df %>%
  group_by(year) %>%
  summarise(
    pulse_day_frac  = mean(PulseFlag == 1L),
    pulse_reco_frac = sum(RECO_gC_day[PulseFlag == 1L], na.rm = TRUE) /
      sum(RECO_gC_day, na.rm = TRUE),
    .groups = "drop"
  )

p_c <- ggplot(frac_tbl, aes(x = pulse_day_frac, y = pulse_reco_frac, label = year)) +
  annotate("rect", xmin = 0.30, xmax = 0.50, ymin = 0,    ymax = 1,    alpha = 0.10) +
  annotate("rect", xmin = 0,    xmax = 1,    ymin = 0.50, ymax = 0.80, alpha = 0.10) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(shape = 21, fill = "black", color = "black", size = 2.8) +
  ggrepel::geom_text_repel(size = 3, min.segment.length = 0.05, seed = 1) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(x = "Pulse-day fraction of year", y = "Pulse fraction of annual RECO") +
  theme_in_ticks

# --- Arrange with (a)(b)(c) tags; keep legend once (right) ---
fig <- ggpubr::ggarrange(
  p_a, p_b, p_c,
  ncol = 3, nrow = 1,
  labels = c("a", "b", "c"),
  font.label = list(size = 14, face = "bold"),
  common.legend = TRUE, legend = "right", align = "hv"
)

ggsave(fig_png, fig, width = 10, height = 4.2, dpi = 300)
message("Saved figure: ", fig_png)

# --- Export figure legend text (edit as needed) ---
legend_lines <- c(
  # Keep titles out of canvases; put them here for manuscript legend.
  "Panel (a): Days classified as pulse vs non-pulse by year (2013–2020).",
  "Panel (b): Annual total ecosystem respiration (RECO; g C m^-2 yr^-1) split into pulse and non-pulse days.",
  "Panel (c): Contribution of pulse days to total annual respiration (c). RECO during pulse days is disproportionately large;",
  "points above the 1:1 line indicate that pulse days contribute more RECO than their share of days.",
  "",
  "Notes: 2012 excluded due to sparse coverage. Shaded bands mark typical ranges: 30–50% of days are pulse (vertical band)",
  "and 50–80% of annual RECO occurs during pulse days (horizontal band)."
)
writeLines(legend_lines, txt_out)
message("Saved legend text: ", txt_out)
