# R/113_Fig7_Rsoil_residuals.R ------------------------------------------------
# Publication Figure 7: Rsoil model residuals — grouped boxplot.
#
# Three models compared within Pulse / Non-pulse groups:
#   Mean            — Pred_All from Chamber_model_predictions_14.csv
#   P-NP switch     — Pred_PN  from Chamber_model_predictions_14.csv
#   Threshold switch — Pred_Thr from Chamber_model_predictions_14.csv
#
# Residual = predicted minus observed (meanRsoil).
# Y-axis uses the combined 5th–95th percentile across Reco (Fig 6) and Rsoil
# residuals so the two figures share a directly comparable y-axis scale.
#
# Data sources:
#   out/derived/Chamber_model_predictions_14.csv — Rsoil predictions + observed
#   out/derived/years_sum1_DM.csv                — pulse classification (max_pulse_duration)
#   out/derived/RECO_predictions_15.csv          — Reco residuals for shared scale
#   out/derived/RECO_predictions_PNP.csv         — Reco residuals for shared scale
#
# Outputs:
#   final/figures/Fig7_Rsoil_residuals.png
#   final/figures/Fig7_Rsoil_residuals_caption.txt
#
# Run standalone from project root:
#   source("R/113_Fig7_Rsoil_residuals.R")
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

# --- 1. Read Rsoil predictions ------------------------------------------------
pred14 <- read_csv("out/derived/Chamber_model_predictions_14.csv",
                   show_col_types = FALSE) %>%
  mutate(date = as.Date(date))
ys1 <- read_csv("out/derived/years_sum1_DM.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

# --- 2. Join pulse classification and compute residuals ----------------------
MODEL_LEVELS <- c("Mean", "P-NP switch", "Threshold switch")
PAL_MODEL    <- c("Mean" = "white", "P-NP switch" = "grey70",
                  "Threshold switch" = "grey30")

df <- pred14 %>%
  left_join(select(ys1, date, max_pulse_duration), by = "date") %>%
  filter(!is.na(meanRsoil), !is.na(Pred_All), !is.na(Pred_PN), !is.na(Pred_Thr)) %>%
  mutate(
    PulseClass  = factor(
      if_else(max_pulse_duration > 0, "Pulse time", "Non-pulse time"),
      levels = c("Non-pulse time", "Pulse time")
    ),
    PulseFlag   = as.integer(max_pulse_duration > 0),
    Resid_Mean  = Pred_All - meanRsoil,
    Resid_PNP   = Pred_PN  - meanRsoil,
    Resid_Thr   = Pred_Thr - meanRsoil
  )

message("Rsoil rows after filter: ", nrow(df),
        " | Pulse: ", sum(df$PulseFlag), " Non-pulse: ", sum(df$PulseFlag == 0))

# --- 3. Reshape to long format -----------------------------------------------
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
        Model == "Thr"  ~ "Threshold switch"
      ),
      levels = MODEL_LEVELS
    )
  )

# --- 4. Shared y-axis: combined 5th–95th percentile with Reco (Fig 6) --------
pred15  <- read_csv("out/derived/RECO_predictions_15.csv",  show_col_types = FALSE)
predPNP <- read_csv("out/derived/RECO_predictions_PNP.csv", show_col_types = FALSE)

reco_resids <- pred15 %>%
  left_join(select(predPNP, date, Reco_PNP), by = "date") %>%
  filter(!is.na(meanRECO), !is.na(MeanM_15), !is.na(Reco_Combined), !is.na(Reco_PNP)) %>%
  summarise(
    r1 = list(MeanM_15      - meanRECO),
    r2 = list(Reco_PNP      - meanRECO),
    r3 = list(Reco_Combined - meanRECO)
  ) %>%
  { c(unlist(.$r1), unlist(.$r2), unlist(.$r3)) }

all_resids <- c(reco_resids, df_long$Residual)
q <- quantile(all_resids, probs = c(0.05, 0.95), na.rm = TRUE)
message(sprintf("Shared y-axis limits (combined Reco+Rsoil): %.3f to %.3f", q[1], q[2]))

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
  annotate("text", x = -Inf, y = Inf, label = "b",
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
out_png <- "final/figures/Fig7_Rsoil_residuals.png"
ggsave(out_png, plot = p,
       width  = SAVE_DOUBLE_COL_W,
       height = 4.0,
       units  = "in",
       dpi    = SAVE_DPI_PUB,
       bg     = BG_WHITE)
message("Saved: ", out_png)

# --- 7. Caption ---------------------------------------------------------------
n_np  <- sum(df$PulseFlag == 0)
n_p   <- sum(df$PulseFlag == 1)
n_tot <- nrow(df)

caption <- paste0(
  "Figure 7. Residuals (predicted minus observed) for three Rsoil models under ",
  "non-pulse and pulse conditions at US-Wkg (Walnut Gulch Kendall), 2017–2020 ",
  "(n = ", n_tot, " chamber observations; Non-pulse: n = ", n_np,
  ", Pulse: n = ", n_p, ").\n\n",
  "Boxes show the interquartile range (IQR); horizontal line within each box is the ",
  "median; whiskers extend to 1.5 × IQR; outliers are omitted for clarity. ",
  "Dashed grey line at y = 0 is the reference for unbiased prediction.\n\n",
  "Three models are compared: Mean (white boxes) — single lumped model fitted to ",
  "all days (Pred_All); P-NP switch (grey 70%) — separate models for pulse and ",
  "non-pulse days defined by rainfall-event windows (Pred_PN); Threshold switch ",
  "(grey 30%) — separate models switched by an optimised SWC threshold (Pred_Thr; ",
  "optimal threshold = 0.13, identified by grid search in script 14).\n\n",
  "Y-axis scale is shared with Figure 6 (Reco residuals): limits set to the ",
  "combined 5th–95th percentile of Reco and Rsoil residuals across all three models ",
  sprintf("(%.2f to %.2f µmol CO₂ m⁻² s⁻¹), ", q[1], q[2]),
  "allowing direct visual comparison of bias magnitude between eddy covariance ",
  "(Reco, 2013–2020, n ≈ 2898 days) and chamber (Rsoil, 2017–2020, n = ",
  n_tot, " observations) model fits. ",
  "Note that the Rsoil dataset is smaller and covers a shorter period than the Reco dataset."
)
out_cap <- "final/figures/Fig7_Rsoil_residuals_caption.txt"
writeLines(caption, out_cap)
message("Saved: ", out_cap)
