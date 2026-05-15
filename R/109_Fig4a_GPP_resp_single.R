# R/109_Fig4a_GPP_resp_single.R -----------------------------------------------
# Publication Figure 4a: GPP vs respiration flux — single panel.
#
# All four symbol types on one panel:
#   Reco circles   (shape 21): pulse = black fill, non-pulse = white fill
#   Rsoil squares  (shape 22): pulse = black fill, non-pulse = white fill
# Two regression lines: Reco vs GPP (solid), Rsoil vs GPP (dashed), all data.
#
# Data sources:
#   out/derived/years_sum1_DM.csv                — daily eddy Reco + GPP + pulse flag
#   out/derived/Chamber_model_predictions_14.csv — daily Rsoil + GPP
#
# Outputs:
#   final/figures/Fig4a_GPP_resp_single.png
#   final/figures/Fig4a_GPP_resp_single_caption.txt
#
# Run standalone from project root:
#   source("R/109_Fig4a_GPP_resp_single.R")
# Do NOT add to run_all.R until the PI approves the figure.
# -----------------------------------------------------------------------------

style_path <- if (file.exists("R/000_figure_style.R")) "R/000_figure_style.R" else "000_figure_style.R"
source(style_path)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
})

dir.create("final/figures", recursive = TRUE, showWarnings = FALSE)

LABEL_RESP <- expression("Respiration flux (" * mu * "mol CO"[2] * " m"^{-2} * " s"^{-1} * ")")
LABEL_GPP  <- expression("GPP (" * mu * "mol CO"[2] * " m"^{-2} * " s"^{-1} * ")")

# --- 1. Read data -------------------------------------------------------------
ys1 <- read_csv("out/derived/years_sum1_DM.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date))
ch14 <- read_csv("out/derived/Chamber_model_predictions_14.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

# --- 2. Prepare plotting data -------------------------------------------------
eddy_df <- ys1 %>%
  filter(!is.na(meanRECO), !is.na(meanGPP), meanGPP > 0) %>%
  transmute(
    date,
    GPP        = meanGPP,
    flux       = meanRECO,
    PulseClass = if_else(max_pulse_duration > 0, "Pulse", "Non-pulse"),
    flux_type  = "Reco"
  )

chamber_df <- ch14 %>%
  left_join(select(ys1, date, max_pulse_duration), by = "date") %>%
  filter(!is.na(meanRsoil), !is.na(meanGPP), meanGPP > 0, !is.na(max_pulse_duration)) %>%
  transmute(
    date,
    GPP        = meanGPP,
    flux       = meanRsoil,
    PulseClass = if_else(max_pulse_duration > 0, "Pulse", "Non-pulse"),
    flux_type  = "Rsoil"
  )

# Combined symbol labels — one aesthetic carries shape + fill for legend
SYM_LEVELS <- c("Reco (pulse)", "Reco (non-pulse)", "Rsoil (pulse)", "Rsoil (non-pulse)")
SYM_SHAPES <- setNames(c(21L, 21L, 22L, 22L), SYM_LEVELS)
SYM_FILLS  <- setNames(c("black", "white", "black", "white"), SYM_LEVELS)

df <- bind_rows(eddy_df, chamber_df) %>%
  mutate(
    symbol = factor(
      paste0(flux_type, " (", tolower(PulseClass), ")"),
      levels = SYM_LEVELS
    )
  )

message("Eddy rows: ", nrow(eddy_df), "   Chamber rows: ", nrow(chamber_df))

# --- 3. Regression R² ---------------------------------------------------------
r2_fn    <- function(sub) summary(lm(flux ~ GPP, data = sub))$r.squared
r2_reco  <- r2_fn(filter(df, flux_type == "Reco"))
r2_rsoil <- r2_fn(filter(df, flux_type == "Rsoil"))

x_max <- max(df$GPP,  na.rm = TRUE)
y_max <- max(df$flux, na.rm = TRUE)

# --- 4. Build plot ------------------------------------------------------------
p <- ggplot(df, aes(x = GPP, y = flux, shape = symbol, fill = symbol)) +
  # Regression lines drawn before points
  geom_smooth(
    data = filter(df, flux_type == "Reco"), aes(x = GPP, y = flux),
    method = "lm", se = FALSE, inherit.aes = FALSE,
    linetype = "solid", color = "black", linewidth = 0.55
  ) +
  geom_smooth(
    data = filter(df, flux_type == "Rsoil"), aes(x = GPP, y = flux),
    method = "lm", se = FALSE, inherit.aes = FALSE,
    linetype = "dashed", color = "black", linewidth = 0.55
  ) +
  geom_point(size = 1.5, stroke = 0.4, color = "black") +
  scale_shape_manual(values = SYM_SHAPES, name = NULL) +
  scale_fill_manual(values  = SYM_FILLS,  name = NULL) +
  guides(
    shape = guide_legend(
      override.aes = list(
        shape  = unname(SYM_SHAPES),
        fill   = unname(SYM_FILLS),
        color  = "black",
        stroke = 0.4,
        size   = 1.5
      )
    ),
    fill = "none"
  ) +
  # R² annotations — upper right, stacked below panel label
  annotate("text",
    x = x_max * 0.97, y = y_max * 0.98,
    label = sprintf("italic(R)^2 == %.2f ~ '(Reco)'", r2_reco),
    parse = TRUE, size = 2.6, hjust = 1
  ) +
  annotate("text",
    x = x_max * 0.97, y = y_max * 0.84,
    label = sprintf("italic(R)^2 == %.2f ~ '(Rsoil)'", r2_rsoil),
    parse = TRUE, size = 2.6, hjust = 1
  ) +
  # Panel label — top right
  annotate("text", x = Inf, y = Inf, label = "a",
    hjust = 1.3, vjust = 1.3, fontface = "bold", size = 5) +
  coord_cartesian(ylim = c(0, y_max * 1.10)) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  labs(x = LABEL_GPP, y = LABEL_RESP) +
  theme_pub() +
  theme(
    legend.position       = c(0.03, 0.97),
    legend.justification  = c("left", "top"),
    legend.background     = element_blank(),
    legend.box.background = element_blank(),
    legend.text           = element_text(size = 8),
    legend.key.size       = unit(0.85, "lines")
  )

# --- 5. Save ------------------------------------------------------------------
out_png <- "final/figures/Fig4a_GPP_resp_single.png"
ggsave(out_png, plot = p,
       width  = SAVE_SINGLE_COL_W,
       height = 3.5,
       units  = "in",
       dpi    = SAVE_DPI_PUB,
       bg     = BG_WHITE)
message("Saved: ", out_png)

# --- 6. Caption ---------------------------------------------------------------
caption <- paste0(
  "Figure 4a. Ecosystem respiration (Reco) and soil respiration (Rsoil) as a ",
  "function of gross primary production (GPP) at US-Wkg (Walnut Gulch Kendall). ",
  "Reco: eddy covariance daily means, 2012–2020. Rsoil: automated chamber ",
  "daily means, 2017–2020.\n\n",
  "Points show daily means. Circles: Reco (eddy covariance); squares: Rsoil ",
  "(chambers). Black fill: pulse days (within a rainfall-event window triggered ",
  "by P > 5 mm d⁻¹); white fill: non-pulse days. Solid line: linear ",
  "regression of Reco on GPP (all data, R² = ", sprintf("%.2f", r2_reco),
  "). Dashed line: linear regression of Rsoil on GPP (all data, R² = ",
  sprintf("%.2f", r2_rsoil), "). Only days with GPP > 0 µmol m⁻² ",
  "s⁻¹ are plotted."
)
out_cap <- "final/figures/Fig4a_GPP_resp_single_caption.txt"
writeLines(caption, out_cap)
message("Saved: ", out_cap)
