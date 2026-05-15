# R/111_Fig5_env_controls.R ---------------------------------------------------
# Publication Figure 5: Environmental controls — 2×2 bubble grid.
#
# Rows:  Reco (top), Rsoil (bottom)
# Cols:  Pulse time (left), Non-pulse time (right)
# x: SWC 5 cm (%, eddy tower) — capped at 35
# y: Soil temperature 5 cm (°C, eddy tower)
# Bubble area: flux magnitude (µmol CO₂ m⁻² s⁻¹), shared scale, clamped at 4
#
# Data sources:
#   out/derived/years_sum_Pulse1_DM.csv  — Reco pulse days
#   out/derived/years_sum_Pulse0_DM.csv  — Reco non-pulse days
#   out/derived/years_sum1_DM.csv        — all eddy days (SWC5, ST5 for Rsoil join)
#   out/derived/All_summary_chamber.csv  — observed Rsoil + max_pulse_duration
#
# Outputs:
#   final/figures/Fig5_EnvControls.png
#   final/figures/Fig5_EnvControls_caption.txt
#
# Run standalone from project root:
#   source("R/111_Fig5_env_controls.R")
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

# --- 1. Read data -------------------------------------------------------------
p1 <- read_csv("out/derived/years_sum_Pulse1_DM.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date))
p0 <- read_csv("out/derived/years_sum_Pulse0_DM.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date))
ys1 <- read_csv("out/derived/years_sum1_DM.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date))
ch_all <- read_csv("out/derived/All_summary_chamber.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

# --- 2. Prepare Reco panels --------------------------------------------------
reco_p <- p1 %>%
  filter(!is.na(meanRECO), !is.na(meanSWC5), !is.na(meanST5), meanRECO > 0)
reco_np <- p0 %>%
  filter(!is.na(meanRECO), !is.na(meanSWC5), !is.na(meanST5), meanRECO > 0)

# --- 3. Prepare Rsoil panels (eddy tower SWC5 + ST5 for all panels) ----------
ch_joined <- ch_all %>%
  left_join(select(ys1, date, meanSWC5, meanST5), by = "date") %>%
  filter(!is.na(meanRsoil), !is.na(meanSWC5), !is.na(meanST5), meanRsoil > 0)

rsoil_p  <- ch_joined %>% filter(max_pulse_duration > 0)
rsoil_np <- ch_joined %>% filter(max_pulse_duration == 0)

message("Reco  pulse: ", nrow(reco_p),   "  non-pulse: ", nrow(reco_np))
message("Rsoil pulse: ", nrow(rsoil_p),  "  non-pulse: ", nrow(rsoil_np))

# --- 4. Shared size scale constants ------------------------------------------
FLUX_MAX    <- PT_BUBBLE$reco_clamp_max   # 4 µmol m⁻² s⁻¹
SIZE_MAX    <- max(PT_BUBBLE$size_range)  # 6
SIZE_BREAKS <- c(1, 2, 4)
SWC_CAP     <- 32   # x-axis upper limit (%)
TSOIL_CAP   <- 39   # y-axis upper limit (°C) — prevents text/border collision

# --- 5. Panel constructor ----------------------------------------------------
bubble_panel <- function(dat, flux_col, show_xtext, show_ytext) {
  ggplot(dat, aes(
    x    = meanSWC5,
    y    = meanST5,
    size = pmax(0, pmin(.data[[flux_col]], FLUX_MAX))
  )) +
    annotate("rect",
      xmin = 0, xmax = 15, ymin = -Inf, ymax = Inf,
      fill = "grey90", alpha = 0.8
    ) +
    geom_point(
      shape  = PT_BUBBLE$shape,
      fill   = "black",
      color  = PT_BUBBLE$color,
      alpha  = PT_BUBBLE$alpha,
      stroke = PT_BUBBLE$stroke
    ) +
    scale_size_area(
      max_size = SIZE_MAX,
      limits   = c(0, FLUX_MAX),
      breaks   = SIZE_BREAKS,
      name     = expression(mu * "mol CO"[2] * " m"^{-2} * " s"^{-1})
    ) +
    coord_cartesian(xlim = c(0, SWC_CAP), ylim = c(0, TSOIL_CAP)) +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
    labs(x = NULL, y = NULL) +
    theme_pub() +
    theme(
      legend.position = "none",
      axis.text.x = if (show_xtext) element_text(margin = margin(t = 6)) else element_blank(),
      axis.text.y = if (show_ytext) element_text(margin = margin(r = 6)) else element_blank(),
      axis.ticks.y = element_line(color = "black", linewidth = 0.4)  # keep marks always
    )
}

# --- 6. Build the four panels ------------------------------------------------
LBL <- function(label) {
  annotate("text", x = -Inf, y = Inf, label = label,
           hjust = -0.3, vjust = 1.3, fontface = "bold", size = 5)
}

p_a <- bubble_panel(reco_p,   "meanRECO",  show_xtext = FALSE, show_ytext = TRUE) +
  theme(plot.margin = margin(t = 5, r = 0, b = 0, l = 5, unit = "pt")) +
  LBL("a")

p_b <- bubble_panel(reco_np,  "meanRECO",  show_xtext = FALSE, show_ytext = FALSE) +
  theme(
    plot.margin           = margin(t = 5, r = 5, b = 0, l = 0, unit = "pt"),
    legend.position       = c(0.97, 0.97),
    legend.justification  = c("right", "top"),
    legend.background     = element_blank(),
    legend.box.background = element_blank(),
    legend.text           = element_text(size = 8),
    legend.key.size       = unit(1.0, "lines"),
    legend.title          = element_text(size = 8)
  ) +
  LBL("b")

p_c <- bubble_panel(rsoil_p,  "meanRsoil", show_xtext = TRUE,  show_ytext = TRUE) +
  theme(plot.margin = margin(t = 0, r = 0, b = 5, l = 5, unit = "pt")) +
  LBL("c")

p_d <- bubble_panel(rsoil_np, "meanRsoil", show_xtext = TRUE,  show_ytext = FALSE) +
  theme(plot.margin = margin(t = 0, r = 5, b = 5, l = 0, unit = "pt")) +
  LBL("d")

# --- 7. Assemble layout ------------------------------------------------------
gp_bold <- gpar(fontsize = 10, fontface = "bold")
gp_reg  <- gpar(fontsize = 10)

title_p   <- textGrob("Pulse time",     gp = gp_bold)
title_np  <- textGrob("Non-pulse time", gp = gp_bold)
row_reco  <- textGrob("Reco",  rot = 90, gp = gp_bold)
row_rsoil <- textGrob("Rsoil", rot = 90, gp = gp_bold)
xlab_grob <- textGrob("Soil water content (5 cm, %)", gp = gp_reg)
ylab_grob <- textGrob("Soil temperature (5 cm, °C)", rot = 90, gp = gp_reg)

# Inner grid: 3 cols (row labels | pulse panels | non-pulse panels)
#             4 rows (col titles | Reco | Rsoil | x-label)
# layout_matrix allows xlab_grob to span both panel columns so it is
# centred across the full panel row width, not just one panel.
inner <- arrangeGrob(
  grobs = list(
    nullGrob(),  # 1 — top-left corner
    title_p,     # 2 — "Pulse time"
    title_np,    # 3 — "Non-pulse time"
    row_reco,    # 4 — row label
    p_a,         # 5
    p_b,         # 6
    row_rsoil,   # 7 — row label
    p_c,         # 8
    p_d,         # 9
    nullGrob(),  # 10 — bottom-left corner
    xlab_grob    # 11 — spans panel columns 2–3
  ),
  layout_matrix = rbind(
    c(1,  2,  3),
    c(4,  5,  6),
    c(7,  8,  9),
    c(10, 11, 11)
  ),
  widths  = unit(c(0.15, 1, 1), "null"),
  heights = unit(c(0.10, 1, 1, 0.08), "null")
)

# Outer: y-axis label on far left + inner
fig5 <- arrangeGrob(
  ylab_grob, inner,
  ncol   = 2,
  widths = unit(c(0.08, 1), "null")
)

# --- 8. Save -----------------------------------------------------------------
out_png <- "final/figures/Fig5_EnvControls.png"
png(out_png,
    width  = SAVE_DOUBLE_COL_W,
    height = 5.5,
    units  = "in",
    res    = SAVE_DPI_PUB,
    bg     = BG_WHITE)
grid.draw(fig5)
dev.off()
message("Saved: ", out_png)

# --- 9. Caption --------------------------------------------------------------
n_rp  <- nrow(reco_p)
n_rnp <- nrow(reco_np)
n_sp  <- nrow(rsoil_p)
n_snp <- nrow(rsoil_np)

caption <- paste0(
  "Figure 5. Environmental controls on ecosystem respiration (Reco, top row) and ",
  "soil respiration (Rsoil, bottom row) under pulse (left column) and non-pulse ",
  "(right column) conditions at US-Wkg (Walnut Gulch Kendall).\n\n",
  "Bubble position: x-axis, soil water content at 5 cm depth measured at the eddy ",
  "covariance tower (%, capped at 32%); y-axis, soil temperature at 5 cm depth ",
  "(eddy tower, °C, capped at 39°C). Bubble area is proportional to flux magnitude ",
  "(µmol CO₂ m⁻² s⁻¹); size legend in panel (b); scale shared ",
  "across all four panels; values clamped at ", FLUX_MAX, " µmol CO₂ m⁻² s⁻¹.\n\n",
  "Panel (a): Reco, pulse days (n = ", n_rp,  " days; eddy covariance 2012–2020). ",
  "Panel (b): Reco, non-pulse days (n = ", n_rnp, " days). ",
  "Panel (c): Rsoil, pulse days (n = ", n_sp,  " days; automated chambers 2017–2020). ",
  "Panel (d): Rsoil, non-pulse days (n = ", n_snp, " days).\n\n",
  "Pulse days are defined as days within a rainfall-event window triggered by ",
  "precipitation > 5 mm d⁻¹ (window duration 8–20 days depending on event size). ",
  "Grey shading (SWC < 15%) marks the approximate threshold below which soil ",
  "moisture rarely limits respiration at this site. ",
  "Eddy tower SWC and Tsoil are used for all panels, including Rsoil rows, ",
  "to ensure a consistent environmental frame of reference. ",
  "Only days with positive flux values are shown."
)
out_cap <- "final/figures/Fig5_EnvControls_caption.txt"
writeLines(caption, out_cap)
message("Saved: ", out_cap)
