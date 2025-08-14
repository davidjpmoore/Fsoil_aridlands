# R/104_Temp_Moisture_Space_Figure.R
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(ggplot2); library(ggpubr); library(scales)
})

# --- IO (pub-only) ---
fig_dir <- "out/figs/pub"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
png_out <- file.path(fig_dir, "Temp_Moisture_Space_Figure.png")
txt_out <- file.path(fig_dir, "Temp_Moisture_Space_Figure.txt")

# --- Load daily eddy table & prep ---
years_sum1 <- if (exists("years_sum1")) years_sum1 else
  readr::read_csv("out/derived/years_sum1_DM.csv", show_col_types = FALSE)

if (!("PulseFlag" %in% names(years_sum1))) {
  years_sum1 <- years_sum1 %>% mutate(PulseFlag = as.integer(days_since_rain_event < max_pulse_duration))
}

df <- years_sum1 %>%
  filter(year != 2012) %>%  # drop sparse year
  mutate(PulseClass = factor(if_else(PulseFlag == 1L, "Pulse", "Non-pulse"),
                             levels = c("Non-pulse","Pulse")))

# --- Themes & palettes ---
theme_in_ticks <- theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", size = 0.5),
        axis.ticks.length = unit(-3, "pt"),
        axis.text.x = element_text(margin = margin(t = 6)),
        axis.text.y = element_text(margin = margin(r = 6)))

pal_bw <- c("Non-pulse" = "white", "Pulse" = "black")

# --- (a) RECO vs Tsoil (5 cm) ---
p_a <- ggplot(df, aes(x = meanST5, y = meanRECO, fill = PulseClass)) +
  geom_point(shape = 21, color = "black", size = 1.8, alpha = 0.85) +
  scale_fill_manual(values = pal_bw) +
  labs(x = "Soil temperature (5 cm, °C)",
       y = expression(RECO~"(" * mu * "mol " * m^{-2} * " " * s^{-1} * ")")) +
  theme_in_ticks + theme(legend.position = "none")

# --- (b) RECO vs SWC (5 cm, %) — share RECO y-axis from panel (a) ---
p_b <- ggplot(df, aes(x = meanSWC5, y = meanRECO, fill = PulseClass)) +
  geom_point(shape = 21, color = "black", size = 1.8, alpha = 0.85) +
  scale_fill_manual(values = pal_bw) +
  labs(x = "Soil water content (5 cm, %)", y = NULL) +
  theme_in_ticks + theme(legend.position = "none",
                         axis.text.y = element_blank(),
                         axis.ticks.y = element_blank(),
                         axis.title.y = element_blank())

# --- (c) Bubble: Tsoil vs SWC, size ∝ RECO (trim 1–99% ranges for clarity) ---
swc_lim <- quantile(df$meanSWC5, c(0.01, 0.99), na.rm = TRUE)
t_lim   <- quantile(df$meanST5,  c(0.01, 0.99), na.rm = TRUE)

p_c <- ggplot(df, aes(x = meanSWC5, y = meanST5)) +
  geom_point(aes(size = pmax(0, pmin(meanRECO, 4)), fill = PulseClass),
             shape = 21, color = "black", alpha = 0.55, stroke = 0.25) +
  scale_fill_manual(values = pal_bw) +
  scale_size_continuous(name = expression(RECO~"(" * mu * "mol " * m^{-2} * " " * s^{-1} * ")"),
                        range = c(1.2, 6), limits = c(0, 4), breaks = 0:4) +
  labs(x = "Soil water content (5 cm, %)", y = "Soil temperature (5 cm, °C)", fill = NULL) +
  coord_cartesian(xlim = swc_lim, ylim = t_lim, expand = TRUE) +
  theme_in_ticks +
  guides(fill = guide_legend(override.aes = list(size = 3, alpha = 1)),
         size = guide_legend(order = 1))

# --- Arrange as (a)(b)(c) with one combined legend (from panel c) ---
fig <- ggpubr::ggarrange(
  p_a + ggtitle(NULL), p_b + ggtitle(NULL), p_c + ggtitle(NULL),
  ncol = 3, nrow = 1,
  labels = c("a", "b", "c"),
  font.label = list(size = 14, face = "bold"),
  common.legend = TRUE, legend = "right", align = "hv"
)

ggsave(png_out, fig, width = 11, height = 4.4, dpi = 300)
message("Saved: ", png_out)

# --- Write legend text (panels a–c) ---
legend_lines <- c(
  "Figure: Temperature–Moisture Space (eddy daily). 2012 excluded due to sparse coverage.",
  "",
  "(a) Daily ecosystem respiration (RECO) versus soil temperature at 5 cm. Black = pulse days; white = non-pulse.",
  "(b) Daily RECO versus soil water content at 5 cm (%). Y-axis shared with panel (a); black = pulse, white = non-pulse.",
  "(c) Soil temperature (5 cm) versus SWC (5 cm, %) with point size proportional to RECO; black = pulse, white = non-pulse.",
  "",
  "Methods (summary): Daily means were computed from half-hourly flux and driver data.",
  "Pulse days were defined using rainfall-triggered windows (>5, >10, >20 mm mapped to 8, 14, 20 days)."
)
writeLines(legend_lines, txt_out)
message("Saved legend: ", txt_out)
