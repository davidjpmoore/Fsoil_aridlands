# R/114_Fig8a_Reco_modelvobs.R ------------------------------------------------
# Publication Figure 8a: Reco observed vs predicted — scatter + cumulative bias.
#
# Panel a: scatter of observed vs predicted for three models.
# Panel b: cumulative (predicted − observed) over time for three models.
#
# Three models:
#   Mean           — MeanM_15     (RECO_predictions_15.csv)
#   P-NP switch    — Reco_PNP     (RECO_predictions_PNP.csv)
#   15% threshold  — Reco_Combined (RECO_predictions_15.csv)
#
# Data sources:
#   out/derived/RECO_predictions_15.csv
#   out/derived/RECO_predictions_PNP.csv
#
# Outputs:
#   final/figures/Fig8a_Reco_modelvobs.png
#   final/figures/Fig8a_Reco_modelvobs_caption.txt
#
# Run standalone from project root:
#   source("R/114_Fig8a_Reco_modelvobs.R")
# Do NOT add to run_all.R until the PI approves the figure.
# -----------------------------------------------------------------------------

style_path <- if (file.exists("R/000_figure_style.R")) "R/000_figure_style.R" else "000_figure_style.R"
source(style_path)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(gridExtra)
  library(grid)
})

dir.create("final/figures", recursive = TRUE, showWarnings = FALSE)

# --- 1. Read and join ---------------------------------------------------------
pred15  <- read_csv("out/derived/RECO_predictions_15.csv",  show_col_types = FALSE) %>%
  mutate(date = as.Date(date))
predPNP <- read_csv("out/derived/RECO_predictions_PNP.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

MODEL_LEVELS <- c("Mean", "P-NP switch", "15% threshold")
PAL_FILL     <- c("Mean" = "white", "P-NP switch" = "grey60", "15% threshold" = "black")
LTY_MODEL    <- c("Mean" = "solid",  "P-NP switch" = "dashed",  "15% threshold" = "dotted")

df <- pred15 %>%
  select(date, meanRECO, MeanM_15, Reco_Combined) %>%
  left_join(select(predPNP, date, Reco_PNP), by = "date") %>%
  filter(!is.na(meanRECO), !is.na(MeanM_15), !is.na(Reco_Combined), !is.na(Reco_PNP)) %>%
  arrange(date)

message("Reco rows: ", nrow(df),
        "  Date range: ", min(df$date), " to ", max(df$date))

# --- 2. Scatter data (panel a) ------------------------------------------------
df_scatter <- df %>%
  pivot_longer(
    cols      = c(MeanM_15, Reco_PNP, Reco_Combined),
    names_to  = "model_col",
    values_to = "Predicted"
  ) %>%
  mutate(
    Model = factor(
      case_when(
        model_col == "MeanM_15"      ~ "Mean",
        model_col == "Reco_PNP"      ~ "P-NP switch",
        model_col == "Reco_Combined" ~ "15% threshold"
      ),
      levels = MODEL_LEVELS
    ),
    Observed = meanRECO
  )

# --- 3. Cumulative residual data (panel b) ------------------------------------
df_cumul <- df %>%
  mutate(
    cum_Mean = cumsum(MeanM_15      - meanRECO),
    cum_PNP  = cumsum(Reco_PNP      - meanRECO),
    cum_Thr  = cumsum(Reco_Combined - meanRECO)
  ) %>%
  pivot_longer(
    cols         = starts_with("cum_"),
    names_to     = "model_col",
    names_prefix = "cum_",
    values_to    = "CumResid"
  ) %>%
  mutate(
    Model = factor(
      case_when(
        model_col == "Mean" ~ "Mean",
        model_col == "PNP"  ~ "P-NP switch",
        model_col == "Thr"  ~ "15% threshold"
      ),
      levels = MODEL_LEVELS
    )
  )

# --- 4. Axis limits -----------------------------------------------------------
obs_range <- range(df$meanRECO, na.rm = TRUE)
pad   <- diff(obs_range) * 0.03
ax_lim <- obs_range + c(-pad, pad)

# --- 5. Labels ----------------------------------------------------------------
LABEL_OBS  <- expression(
  "Observed Reco (" * mu * "mol CO"[2] * " m"^{-2} * " s"^{-1} * ")")
LABEL_PRED <- expression(
  "Predicted Reco (" * mu * "mol CO"[2] * " m"^{-2} * " s"^{-1} * ")")
LABEL_CUM  <- expression(
  "Cumulative residual (" * mu * "mol CO"[2] * " m"^{-2} * " s"^{-1} * ")")

# --- 6. Panel a: observed vs predicted scatter --------------------------------
p_a <- ggplot(df_scatter, aes(x = Observed, y = Predicted, fill = Model)) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_point(shape = 21, size = 1.2, alpha = 0.4, stroke = 0.3, color = "black") +
  scale_fill_manual(values = PAL_FILL, name = NULL) +
  coord_cartesian(xlim = ax_lim, ylim = ax_lim) +
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  labs(x = LABEL_OBS, y = LABEL_PRED) +
  annotate("text", x = Inf, y = Inf, label = "a",
           hjust = 1.3, vjust = 1.3, fontface = "bold", size = 5) +
  theme_pub() +
  theme(
    legend.position = "none",
    plot.margin     = margin(5, 5, 5, 5, "pt")
  )

# --- 7. Panel b: cumulative residual time series ------------------------------
p_b <- ggplot(df_cumul, aes(x = date, y = CumResid, linetype = Model)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.4) +
  geom_line(color = "black", linewidth = 0.6) +
  scale_linetype_manual(values = LTY_MODEL, name = NULL) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  labs(x = NULL, y = LABEL_CUM) +
  annotate("text", x = Inf, y = Inf, label = "b",
           hjust = 1.3, vjust = 1.3, fontface = "bold", size = 5) +
  theme_pub() +
  theme(
    legend.position       = c(0.03, 0.97),
    legend.justification  = c("left", "top"),
    legend.background     = element_blank(),
    legend.box.background = element_blank(),
    legend.text           = element_text(size = 9),
    legend.key.size       = unit(1.0, "lines"),
    legend.key.width      = unit(1.8, "lines"),
    plot.margin           = margin(5, 5, 5, 5, "pt")
  )

# --- 8. Assemble and save -----------------------------------------------------
fig <- arrangeGrob(p_a, p_b, ncol = 2)

out_png <- "final/figures/Fig8a_Reco_modelvobs.png"
png(out_png,
    width  = SAVE_DOUBLE_COL_W,
    height = 4.0,
    units  = "in",
    res    = SAVE_DPI_PUB,
    bg     = BG_WHITE)
grid.draw(fig)
dev.off()
message("Saved: ", out_png)

# --- 9. Caption ---------------------------------------------------------------
n_days <- nrow(df)
caption <- paste0(
  "Figure 8a. Reco model performance at US-Wkg (Walnut Gulch Kendall), 2013–2020 ",
  "(n = ", n_days, " daily observations).\n\n",
  "Panel (a): Observed vs predicted daily Reco for three models. Each point ",
  "represents one day. White circles: Mean model (single lumped fit); grey circles: ",
  "P-NP switch model (separate pulse/non-pulse fits, rainfall-event classification); ",
  "black circles: 15% threshold model (SWC-threshold switch). Dashed line is the ",
  "1:1 reference. Point alpha = 0.4 to indicate overplotting density. Axis limits ",
  "set to the range of observed values.\n\n",
  "Panel (b): Cumulative bias (running sum of predicted minus observed) over time ",
  "for each model. Line types: solid = Mean; dashed = P-NP switch; dotted = 15% ",
  "threshold. Grey horizontal line at zero is the reference for unbiased cumulative ",
  "prediction. Persistent positive values indicate systematic over-prediction; ",
  "persistent negative values indicate under-prediction."
)
out_cap <- "final/figures/Fig8a_Reco_modelvobs_caption.txt"
writeLines(caption, out_cap)
message("Saved: ", out_cap)
