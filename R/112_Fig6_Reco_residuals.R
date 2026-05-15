# R/112_Fig6_Reco_residuals.R -------------------------------------------------
# Publication Figure 6: Reco model residuals — grouped boxplot.
#
# Three models compared within Pulse / Non-pulse groups:
#   Mean           — MeanM_15 from RECO_predictions_15.csv
#   P-NP switch    — Reco_PNP from RECO_predictions_PNP.csv
#   15% threshold  — Reco_Combined from RECO_predictions_15.csv
#
# Residual = predicted minus observed (meanRECO).
# Y-axis clipped to 5th–95th percentile; outliers hidden.
#
# Data sources:
#   out/derived/RECO_predictions_15.csv   — Mean + 15% threshold predictions
#   out/derived/RECO_predictions_PNP.csv  — P-NP switch predictions
#
# Outputs:
#   final/figures/Fig6_Reco_residuals.png
#   final/figures/Fig6_Reco_residuals_caption.txt
#
# Run standalone from project root:
#   source("R/112_Fig6_Reco_residuals.R")
# Do NOT add to run_all.R until the PI approves the figure.
# -----------------------------------------------------------------------------

style_path <- if (file.exists("R/000_figure_style.R")) "R/000_figure_style.R" else "000_figure_style.R"
source(style_path)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
})

dir.create("final/figures", recursive = TRUE, showWarnings = FALSE)

# --- 1. Read predictions ------------------------------------------------------
pred15  <- read_csv("out/derived/RECO_predictions_15.csv",  show_col_types = FALSE) %>%
  mutate(date = as.Date(date))
predPNP <- read_csv("out/derived/RECO_predictions_PNP.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

# --- 2. Join and compute residuals -------------------------------------------
MODEL_LEVELS <- c("Mean", "P-NP switch", "15% threshold")
PAL_MODEL    <- c("Mean" = "white", "P-NP switch" = "grey70", "15% threshold" = "grey30")

df <- pred15 %>%
  select(date, meanRECO, MeanM_15, Reco_Combined, PulseFlag) %>%
  left_join(select(predPNP, date, Reco_PNP), by = "date") %>%
  filter(!is.na(meanRECO), !is.na(MeanM_15), !is.na(Reco_Combined), !is.na(Reco_PNP)) %>%
  mutate(
    PulseClass  = factor(
      if_else(PulseFlag == 1, "Pulse time", "Non-pulse time"),
      levels = c("Non-pulse time", "Pulse time")
    ),
    Resid_Mean = MeanM_15      - meanRECO,
    Resid_PNP  = Reco_PNP      - meanRECO,
    Resid_Thr  = Reco_Combined - meanRECO
  )

message("Rows after join and NA filter: ", nrow(df))

# --- 3. Reshape to long format ------------------------------------------------
df_long <- df %>%
  select(date, PulseClass, Resid_Mean, Resid_PNP, Resid_Thr) %>%
  pivot_longer(
    cols         = starts_with("Resid_"),
    names_to     = "Model",
    names_prefix = "Resid_",
    values_to    = "Residual"
  ) %>%
  mutate(
    Model = factor(
      case_when(
        Model == "Mean" ~ "Mean",
        Model == "PNP"  ~ "P-NP switch",
        Model == "Thr"  ~ "15% threshold"
      ),
      levels = MODEL_LEVELS
    )
  )

# --- 4. Y-axis limits: 5th–95th percentile -----------------------------------
q <- quantile(df_long$Residual, probs = c(0.05, 0.95), na.rm = TRUE)
message(sprintf("Y-axis limits: %.3f to %.3f", q[1], q[2]))

# --- 5. Build plot ------------------------------------------------------------
LABEL_RESID <- expression(
  "Model residual (" * mu * "mol CO"[2] * " m"^{-2} * " s"^{-1} * ")"
)

p <- ggplot(df_long, aes(x = PulseClass, y = Residual, fill = Model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.6) +
  geom_boxplot(
    outlier.shape = NA,
    color         = "black",
    linewidth     = 0.4,
    position      = position_dodge(width = 0.8),
    width         = 0.65
  ) +
  scale_fill_manual(values = PAL_MODEL, name = NULL) +
  scale_y_continuous(
    expand   = expansion(mult = c(0.02, 0.02)),
    sec.axis = dup_axis(labels = NULL, name = NULL)
  ) +
  coord_cartesian(ylim = c(q[1], q[2])) +
  labs(x = NULL, y = LABEL_RESID) +
  annotate("text", x = -Inf, y = Inf, label = "a",
           hjust = -0.5, vjust = 1.3, fontface = "bold", size = 5) +
  theme_pub() +
  theme(
    legend.position       = c(0.03, 0.03),
    legend.justification  = c("left", "bottom"),
    legend.background     = element_blank(),
    legend.box.background = element_blank(),
    legend.text           = element_text(size = 9),
    legend.key.size       = unit(0.9, "lines"),
    axis.text.y.right     = element_blank()
  )

# --- 6. Save ------------------------------------------------------------------
out_png <- "final/figures/Fig6_Reco_residuals.png"
ggsave(out_png, plot = p,
       width  = SAVE_DOUBLE_COL_W,
       height = 4.0,
       units  = "in",
       dpi    = SAVE_DPI_PUB,
       bg     = BG_WHITE)
message("Saved: ", out_png)

# --- 7. Caption ---------------------------------------------------------------
n_np <- df_long %>% filter(PulseClass == "Non-pulse time", Model == "Mean") %>% nrow()
n_p  <- df_long %>% filter(PulseClass == "Pulse time",     Model == "Mean") %>% nrow()

caption <- paste0(
  "Figure 6. Residuals (predicted minus observed) for three Reco models under ",
  "non-pulse and pulse conditions at US-Wkg (Walnut Gulch Kendall), 2013–2020.\n\n",
  "Boxes show the interquartile range (IQR); horizontal line within each box is the ",
  "median; whiskers extend to 1.5 × IQR; outliers are omitted for clarity. ",
  "Y-axis is clipped to the 5th–95th percentile of all residuals ",
  sprintf("(%.2f to %.2f µmol CO₂ m⁻² s⁻¹).", q[1], q[2]),
  " Dashed grey line at y = 0 is the reference for unbiased prediction.\n\n",
  "Three models are compared: Mean (white boxes) — single lumped model fitted to ",
  "all days; P-NP switch (grey 70%) — separate models for pulse and non-pulse days ",
  "defined by rainfall-event windows; 15% threshold (grey 30%) — separate models ",
  "switched by soil water content (SWC ≥ 15% = pulse conditions). ",
  "Non-pulse group: n = ", n_np, " days; ",
  "Pulse group: n = ", n_p, " days. ",
  "2012 excluded (sparse coverage)."
)
out_cap <- "final/figures/Fig6_Reco_residuals_caption.txt"
writeLines(caption, out_cap)
message("Saved: ", out_cap)
