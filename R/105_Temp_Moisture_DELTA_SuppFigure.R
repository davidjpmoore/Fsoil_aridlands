# R/105_Temp_Moisture_DELTA_SuppFigure.R
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(ggplot2); library(scales)
})

# --- IO (pub-only) ---
fig_dir <- "out/figs/pub"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
png_out <- file.path(fig_dir, "Temp_Moisture_DELTA_SuppFigure.png")
txt_out <- file.path(fig_dir, "Temp_Moisture_DELTA_SuppFigure.txt")

# --- Load daily eddy table & prep ---
years_sum1 <- if (exists("years_sum1")) years_sum1 else
  readr::read_csv("out/derived/years_sum1_DM.csv", show_col_types = FALSE)

if (!("PulseFlag" %in% names(years_sum1))) {
  years_sum1 <- years_sum1 %>% mutate(PulseFlag = as.integer(days_since_rain_event < max_pulse_duration))
}

df <- years_sum1 %>%
  filter(year != 2012) %>%
  transmute(
    meanSWC5, meanST5, meanRECO,
    PulseClass = if_else(PulseFlag == 1L, "Pulse", "Non-pulse")
  )

# --- Tunables ---
swc_breaks <- seq(0, 45, by = 2)   # SWC in %
t_breaks   <- seq(0, 40, by = 2)   # Tsoil in °C
min_n      <- 5                    # require >= this many obs in EACH group per bin
gamma      <- 1.2                  # >1 emphasizes large diffs

# --- Bin to regular grid; compute means + counts per group ---
binned <- df %>%
  mutate(
    swc_bin = cut(meanSWC5, breaks = swc_breaks, include.lowest = TRUE, right = FALSE),
    t_bin   = cut(meanST5,  breaks = t_breaks,   include.lowest = TRUE, right = FALSE)
  ) %>%
  tidyr::drop_na(swc_bin, t_bin) %>%
  group_by(PulseClass, swc_bin, t_bin) %>%
  summarise(RECO_mean = mean(meanRECO, na.rm = TRUE),
            n = n(), .groups = "drop")

# --- Pivot wide; bulletproof names; compute Δ & tile centers ---
wide <- binned %>%
  tidyr::pivot_wider(
    names_from   = PulseClass,
    values_from  = c(RECO_mean, n),
    names_sep    = "_",
    values_fill  = NA
  ) %>%
  { colnames(.) <- make.names(colnames(.)); . } %>%   # safe names
  dplyr::rename_with(~ gsub("\\.", "_", .x)) %>%
  mutate(
    valid     = n_Pulse >= min_n & n_Non_pulse >= min_n,
    RECO_diff = RECO_mean_Pulse - RECO_mean_Non_pulse
  ) %>%
  filter(valid) %>%
  mutate(
    swc_idx = as.integer(swc_bin),
    t_idx   = as.integer(t_bin),
    SWC_mid = (swc_breaks[swc_idx] + swc_breaks[swc_idx + 1]) / 2,
    T_mid   = (t_breaks[t_idx]     + t_breaks[t_idx + 1])   / 2
  )

# --- Symmetric limits from Q95(|Δ|) + signed power shaping ---
lim_raw <- if (nrow(wide)) quantile(abs(wide$RECO_diff), 0.95, na.rm = TRUE) else 1
lim     <- max(0.5, as.numeric(lim_raw))
signed_pow <- function(x, g) sign(x) * (abs(x)^g)
wide <- wide %>% mutate(RECO_diff_shaped = signed_pow(RECO_diff, gamma))

# --- Theme ---
theme_in_ticks <- theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", size = 0.5),
        axis.ticks.length = unit(-3, "pt"),
        axis.text.x = element_text(margin = margin(t = 6)),
        axis.text.y = element_text(margin = margin(r = 6)))

# --- Plot & save ---
p_delta <- ggplot(wide, aes(SWC_mid, T_mid, fill = RECO_diff_shaped)) +
  geom_tile() +
  scale_fill_gradient2(
    name = expression(Delta*RECO~" ("*mu*"mol "*m^{-2}*" "*s^{-1}*")"),
    low = "#2b8cbe", mid = "white", high = "#d7301f", midpoint = 0,
    limits = c(-lim^gamma, lim^gamma), oob = squish
  ) +
  coord_cartesian(xlim = c(0, 45), ylim = c(0, 40), expand = FALSE) +
  labs(x = "Soil water content (5 cm, %)", y = "Soil temperature (5 cm, °C)") +
  theme_in_ticks

ggsave(png_out, p_delta, width = 6.8, height = 5.0, dpi = 300)
message("Saved: ", png_out)

# --- Write legend text (supplementary figure) ---
legend_lines <- c(
  "Supplementary Figure: ΔRECO in temperature–moisture space (Pulse minus Non-pulse). 2012 excluded.",
  "",
  "Panel: Heatmap of ΔRECO (µmol m⁻² s⁻¹) computed in matched bins of soil water content (2%-wide) and soil temperature (2°C-wide).",
  "Positive (red) indicates higher RECO on pulse days than non-pulse days at the same conditions; negative (blue) indicates the reverse.",
  "",
  "Methods (summary): Daily means were binned on a regular SWC×T grid.",
  "Bins were retained only when both groups had ≥5 observations. Color limits are symmetric about zero and set from the 95th percentile",
  "of |ΔRECO|. A mild signed power transform (γ = 1.2) was applied to emphasize larger differences and downweight near-zero values."
)
writeLines(legend_lines, txt_out)
message("Saved legend: ", txt_out)

# --- Standalone panel (c): Bubble plot with proportions matching SuppFigure ---
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(ggplot2)
})

# Paths (pub-quality output)
dir.create("out/figs/pub", recursive = TRUE, showWarnings = FALSE)
out_png <- "out/figs/pub/Temp_Moisture_Space_BubbleOnly.png"

# Load daily eddy table
years_sum1 <- readr::read_csv("out/derived/years_sum1_DM.csv", show_col_types = FALSE)

# Robust pulse flag
if (!("PulseFlag" %in% names(years_sum1))) {
  years_sum1 <- years_sum1 %>%
    mutate(PulseFlag = as.integer(days_since_rain_event < max_pulse_duration))
}

# Prep (drop 2012, classify)
df <- years_sum1 %>%
  filter(year != 2012) %>%
  mutate(
    PulseClass = factor(if_else(PulseFlag == 1L, "Pulse", "Non-pulse"),
                        levels = c("Non-pulse","Pulse"))
  )

# Inward-tick theme (with legend for presentations)
theme_in_ticks_legend <- theme_bw(base_size = 12) +
  theme(
    panel.grid        = element_blank(),
    panel.border      = element_rect(color = "black", size = 0.5),
    axis.ticks.length = unit(-3, "pt"),
    axis.text.x       = element_text(margin = margin(t = 6)),
    axis.text.y       = element_text(margin = margin(r = 6)),
    legend.title      = element_text(size = 10),
    legend.key.height = unit(0.9, "lines"),
    legend.key.width  = unit(1.1, "lines")
  )

pal_bw <- c("Non-pulse" = "white", "Pulse" = "black")

# Trim axes to 1–99% to reduce outlier influence (matches earlier behavior)
swc_lim <- quantile(df$meanSWC5, c(0.01, 0.99), na.rm = TRUE)
t_lim   <- quantile(df$meanST5,  c(0.01, 0.99), na.rm = TRUE)

# Bubble plot: Tsoil vs SWC, size ∝ RECO
p_bubble <- ggplot(df, aes(x = meanSWC5, y = meanST5)) +
  geom_point(
    aes(size = pmax(0, pmin(meanRECO, 4)), fill = PulseClass),
    shape = 21, color = "black", alpha = 0.55, stroke = 0.25
  ) +
  scale_fill_manual(values = pal_bw) +
  scale_size_continuous(
    name = expression(RECO~"(" * mu * "mol " * m^{-2} * " " * s^{-1} * ")"),
    range = c(1.2, 6), limits = c(0, 4), breaks = 0:4
  ) +
  labs(
    x = "Soil water content (5 cm, %)",
    y = "Soil temperature (5 cm, °C)",
    fill = NULL
  ) +
  coord_cartesian(xlim = swc_lim, ylim = t_lim, expand = TRUE) +
  theme_in_ticks_legend +
  guides(
    fill = guide_legend(override.aes = list(size = 3, alpha = 1)),
    size = guide_legend(order = 1)
  )

# Save with the SAME proportions as the Supplementary heatmap (for seamless slide transitions)
ggsave(out_png, p_bubble, width = 6.8, height = 5.0, dpi = 300)
message("Saved: ", out_png)

