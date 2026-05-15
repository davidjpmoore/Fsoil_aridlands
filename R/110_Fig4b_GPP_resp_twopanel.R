# R/110_Fig4b_GPP_resp_twopanel.R ---------------------------------------------
# Publication Figure 4: GPP vs respiration flux — two panels side by side.
#
# Panel a: Reco vs GPP (circles, shape 21)
# Panel b: Rsoil vs GPP (squares, shape 22)
# Both panels: black fill = pulse, white fill = non-pulse.
# Two regression lines per panel drawn in front of points:
#   pulse = solid grey, non-pulse = dashed grey.
# Shared y-axis scale; y-label and ticks on panel a only.
# Each panel carries its own x-axis label.
# Panel labels a/b inside top left. Legend inside panel a bottom right.
#
# Data sources:
#   out/derived/years_sum1_DM.csv                — daily eddy Reco + GPP + pulse flag
#   out/derived/Chamber_model_predictions_14.csv — daily Rsoil + GPP
#
# Outputs:
#   final/figures/Fig4_GPP_resp_twopanel.png
#   final/figures/Fig4_GPP_resp_twopanel_caption.txt
#
# Run standalone from project root:
#   source("R/110_Fig4b_GPP_resp_twopanel.R")
# Do NOT add to run_all.R until the PI approves the figure.
# -----------------------------------------------------------------------------

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

LABEL_RESP <- expression("Respiration flux (" * mu * "mol CO"[2] * " m"^{-2} * " s"^{-1} * ")")
LABEL_GPP  <- expression("GPP (" * mu * "mol CO"[2] * " m"^{-2} * " s"^{-1} * ")")

# --- 1. Read data -------------------------------------------------------------
ys1 <- read_csv("out/derived/years_sum1_DM.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date))
ch14 <- read_csv("out/derived/Chamber_model_predictions_14.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

# --- 2. Prepare data ----------------------------------------------------------
eddy_df <- ys1 %>%
  filter(!is.na(meanRECO), !is.na(meanGPP), meanGPP > 0) %>%
  transmute(
    date, GPP = meanGPP, flux = meanRECO,
    PulseClass = factor(
      if_else(max_pulse_duration > 0, "Pulse", "Non-pulse"),
      levels = c("Non-pulse", "Pulse")
    )
  )

chamber_df <- ch14 %>%
  left_join(select(ys1, date, max_pulse_duration), by = "date") %>%
  filter(!is.na(meanRsoil), !is.na(meanGPP), meanGPP > 0, !is.na(max_pulse_duration)) %>%
  transmute(
    date, GPP = meanGPP, flux = meanRsoil,
    PulseClass = factor(
      if_else(max_pulse_duration > 0, "Pulse", "Non-pulse"),
      levels = c("Non-pulse", "Pulse")
    )
  )

message("Eddy rows: ", nrow(eddy_df), "   Chamber rows: ", nrow(chamber_df))

# --- 3. Shared axis limits ----------------------------------------------------
y_max_all <- max(c(eddy_df$flux, chamber_df$flux), na.rm = TRUE)
y_top     <- y_max_all * 1.10
x_max_all <- max(c(eddy_df$GPP,  chamber_df$GPP),  na.rm = TRUE)

# --- 4. Per-panel R² ----------------------------------------------------------
r2_fn <- function(dat, cls) {
  sub <- filter(dat, PulseClass == cls)
  if (nrow(sub) < 3) return(NA_real_)
  summary(lm(flux ~ GPP, data = sub))$r.squared
}
r2a_p  <- r2_fn(eddy_df,    "Pulse")
r2a_np <- r2_fn(eddy_df,    "Non-pulse")
r2b_p  <- r2_fn(chamber_df, "Pulse")
r2b_np <- r2_fn(chamber_df, "Non-pulse")

# --- 5. Panel a: Reco ---------------------------------------------------------
p_a <- ggplot(eddy_df, aes(x = GPP, y = flux)) +
  # Points drawn first — regression lines appear on top
  geom_point(
    aes(fill = PulseClass),
    shape = 21, size = 1.5, stroke = 0.4, color = "black"
  ) +
  geom_smooth(
    aes(linetype = PulseClass), method = "lm", se = FALSE,
    color = "grey40", linewidth = 0.7
  ) +
  scale_fill_manual(values = c("Pulse" = "black", "Non-pulse" = "white"), name = NULL) +
  scale_linetype_manual(
    values = c("Pulse" = "solid", "Non-pulse" = "dashed"),
    guide  = "none"
  ) +
  # R² annotations — upper right
  annotate("text",
    x = x_max_all * 0.97, y = y_top * 0.97,
    label = sprintf("italic(R)^2 == %.2f ~ '(pulse)'", r2a_p),
    parse = TRUE, size = 2.5, hjust = 1
  ) +
  annotate("text",
    x = x_max_all * 0.97, y = y_top * 0.83,
    label = sprintf("italic(R)^2 == %.2f ~ '(non-pulse)'", r2a_np),
    parse = TRUE, size = 2.5, hjust = 1
  ) +
  # Panel label — top left
  annotate("text", x = -Inf, y = Inf, label = "a",
    hjust = -0.5, vjust = 1.3, fontface = "bold", size = 5) +
  coord_cartesian(ylim = c(0, y_top), xlim = c(0, x_max_all * 1.05)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  labs(x = LABEL_GPP, y = LABEL_RESP) +
  theme_pub() +
  theme(
    legend.position       = c(0.97, 0.03),
    legend.justification  = c("right", "bottom"),
    legend.background     = element_blank(),
    legend.box.background = element_blank(),
    legend.text           = element_text(size = 8),
    legend.key.size       = unit(0.85, "lines"),
    plot.margin           = margin(t = 5, r = 0, b = 5, l = 5, unit = "pt")
  )

# --- 6. Panel b: Rsoil --------------------------------------------------------
p_b <- ggplot(chamber_df, aes(x = GPP, y = flux)) +
  geom_point(
    aes(fill = PulseClass),
    shape = 22, size = 1.5, stroke = 0.4, color = "black"
  ) +
  geom_smooth(
    aes(linetype = PulseClass), method = "lm", se = FALSE,
    color = "grey40", linewidth = 0.7
  ) +
  scale_fill_manual(values = c("Pulse" = "black", "Non-pulse" = "white"), name = NULL) +
  scale_linetype_manual(
    values = c("Pulse" = "solid", "Non-pulse" = "dashed"),
    guide  = "none"
  ) +
  # R² annotations — upper right
  annotate("text",
    x = x_max_all * 0.97, y = y_top * 0.97,
    label = sprintf("italic(R)^2 == %.2f ~ '(pulse)'", r2b_p),
    parse = TRUE, size = 2.5, hjust = 1
  ) +
  annotate("text",
    x = x_max_all * 0.97, y = y_top * 0.83,
    label = sprintf("italic(R)^2 == %.2f ~ '(non-pulse)'", r2b_np),
    parse = TRUE, size = 2.5, hjust = 1
  ) +
  # Panel label — top left
  annotate("text", x = -Inf, y = Inf, label = "b",
    hjust = -0.5, vjust = 1.3, fontface = "bold", size = 5) +
  coord_cartesian(ylim = c(0, y_top), xlim = c(0, x_max_all * 1.05)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  labs(x = LABEL_GPP, y = NULL) +
  theme_pub() +
  theme(
    legend.position = "none",
    axis.text.y     = element_blank(),
    axis.ticks.y    = element_line(color = "black", linewidth = 0.4),
    plot.margin     = margin(t = 5, r = 5, b = 5, l = 0, unit = "pt")
  )

# --- 7. Assemble and save -----------------------------------------------------
fig4_grob <- arrangeGrob(p_a, p_b, ncol = 2)

out_png <- "final/figures/Fig4_GPP_resp_twopanel.png"
png(out_png,
    width  = SAVE_DOUBLE_COL_W,
    height = 3.5,
    units  = "in",
    res    = SAVE_DPI_PUB,
    bg     = BG_WHITE)
grid.draw(fig4_grob)
dev.off()
message("Saved: ", out_png)

# --- 8. Caption ---------------------------------------------------------------
caption <- paste0(
  "Figure 4. Ecosystem respiration (Reco, panel a) and soil respiration ",
  "(Rsoil, panel b) as a function of gross primary production (GPP) at ",
  "US-Wkg (Walnut Gulch Kendall). Reco: eddy covariance daily means, 2012–2020. ",
  "Rsoil: automated chamber daily means, 2017–2020.\n\n",
  "Panel (a): Reco vs GPP. Circles: daily means. Black fill: pulse days; white ",
  "fill: non-pulse days. Solid grey line: linear regression for pulse days ",
  "(R² = ", sprintf("%.2f", r2a_p), "); dashed grey line: non-pulse days ",
  "(R² = ", sprintf("%.2f", r2a_np), "). Regression lines are drawn in front of points.\n\n",
  "Panel (b): Rsoil vs GPP. Squares: daily means. Same fill and line conventions ",
  "as panel (a). Solid grey line: pulse-day regression (R² = ",
  sprintf("%.2f", r2b_p), "); dashed grey line: non-pulse-day regression ",
  "(R² = ", sprintf("%.2f", r2b_np), "). Both panels share the same y-axis scale. ",
  "Only days with GPP > 0 µmol m⁻² s⁻¹ are plotted.\n\n",
  "Pulse days are defined as days within a rainfall-event window triggered by ",
  "precipitation > 5 mm d⁻¹ (window duration 8–20 days depending on event size; ",
  "see CLAUDE.md)."
)
out_cap <- "final/figures/Fig4_GPP_resp_twopanel_caption.txt"
writeLines(caption, out_cap)
message("Saved: ", out_cap)
