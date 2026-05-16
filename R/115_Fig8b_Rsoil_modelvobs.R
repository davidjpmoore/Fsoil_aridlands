# R/115_Fig8b_Rsoil_modelvobs.R -----------------------------------------------
# Publication Figure 8b: Rsoil observed vs predicted — scatter + cumulative bias.
#
# Panel a: scatter of observed vs predicted for three models.
# Panel b: cumulative (predicted − observed) over time for three models.
#
# Three models:
#   Mean             — Pred_All (Chamber_model_predictions_14.csv)
#   P-NP switch      — Pred_PN  (Chamber_model_predictions_14.csv)
#   Threshold switch — Pred_Thr (Chamber_model_predictions_14.csv)
#
# Data source:
#   out/derived/Chamber_model_predictions_14.csv
#
# Outputs:
#   final/figures/Fig8b_Rsoil_modelvobs.png
#   final/figures/Fig8b_Rsoil_modelvobs_caption.txt
#
# Run standalone from project root:
#   source("R/115_Fig8b_Rsoil_modelvobs.R")
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

# --- 1. Read data -------------------------------------------------------------
pred14 <- read_csv("out/derived/Chamber_model_predictions_14.csv",
                   show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

MODEL_LEVELS <- c("Mean", "P-NP switch", "Threshold switch")
PAL_FILL     <- c("Mean" = "white", "P-NP switch" = "grey60", "Threshold switch" = "black")
LTY_MODEL    <- c("Mean" = "solid",  "P-NP switch" = "dashed",  "Threshold switch" = "dotted")

df <- pred14 %>%
  select(date, meanRsoil, Pred_All, Pred_PN, Pred_Thr) %>%
  filter(!is.na(meanRsoil), !is.na(Pred_All), !is.na(Pred_PN), !is.na(Pred_Thr)) %>%
  arrange(date)

message("Rsoil rows: ", nrow(df),
        "  Date range: ", min(df$date), " to ", max(df$date))

# --- 2. Scatter data (panel a) ------------------------------------------------
df_scatter <- df %>%
  pivot_longer(
    cols      = c(Pred_All, Pred_PN, Pred_Thr),
    names_to  = "model_col",
    values_to = "Predicted"
  ) %>%
  mutate(
    Model = factor(
      case_when(
        model_col == "Pred_All" ~ "Mean",
        model_col == "Pred_PN"  ~ "P-NP switch",
        model_col == "Pred_Thr" ~ "Threshold switch"
      ),
      levels = MODEL_LEVELS
    ),
    Observed = meanRsoil
  )

# --- 3. Cumulative residual data (panel b) ------------------------------------
df_cumul <- df %>%
  mutate(
    cum_Mean = cumsum(Pred_All - meanRsoil),
    cum_PNP  = cumsum(Pred_PN  - meanRsoil),
    cum_Thr  = cumsum(Pred_Thr - meanRsoil)
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
        model_col == "Thr"  ~ "Threshold switch"
      ),
      levels = MODEL_LEVELS
    )
  )

# --- 4. Axis limits -----------------------------------------------------------
obs_range <- range(df$meanRsoil, na.rm = TRUE)
pad    <- diff(obs_range) * 0.03
ax_lim <- obs_range + c(-pad, pad)

# --- 5. Labels ----------------------------------------------------------------
LABEL_OBS  <- expression(
  "Observed Rsoil (" * mu * "mol CO"[2] * " m"^{-2} * " s"^{-1} * ")")
LABEL_PRED <- expression(
  "Predicted Rsoil (" * mu * "mol CO"[2] * " m"^{-2} * " s"^{-1} * ")")
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
  annotate("text", x = -Inf, y = Inf, label = "a",
           hjust = -0.3, vjust = 1.3, fontface = "bold", size = 5) +
  guides(fill = guide_legend(
    override.aes = list(shape = 21, size = 3, alpha = 1, stroke = 0.4, color = "black")
  )) +
  theme_pub() +
  theme(
    legend.position       = c(0.03, 0.90),
    legend.justification  = c("left", "top"),
    legend.background     = element_blank(),
    legend.box.background = element_blank(),
    legend.text           = element_text(size = 9),
    legend.key.size       = unit(0.85, "lines"),
    plot.margin           = margin(5, 5, 5, 5, "pt")
  )

# --- 7. Panel b: cumulative residual time series (2017–2020) -----------------
p_b <- ggplot(df_cumul, aes(x = date, y = CumResid, linetype = Model)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.4) +
  geom_line(color = "black", linewidth = 0.6) +
  scale_linetype_manual(values = LTY_MODEL, name = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  labs(x = NULL, y = LABEL_CUM) +
  annotate("text", x = -Inf, y = Inf, label = "b",
           hjust = -0.3, vjust = 1.3, fontface = "bold", size = 5) +
  theme_pub() +
  theme(
    legend.position       = c(0.03, 0.90),
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

out_png <- "final/figures/Fig8b_Rsoil_modelvobs.png"
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
n_obs <- nrow(df)
caption <- paste0(
  "Figure 8b. Rsoil model performance at US-Wkg (Walnut Gulch Kendall), 2017–2020 ",
  "(n = ", n_obs, " chamber observations).\n\n",
  "Panel (a): Observed vs predicted daily Rsoil for three models. Each point ",
  "represents one day. White circles: Mean model (single lumped fit, Pred_All); ",
  "grey circles: P-NP switch model (separate pulse/non-pulse fits, rainfall-event ",
  "classification, Pred_PN); black circles: Threshold switch model (SWC-threshold ",
  "switch, optimal threshold = 0.13, Pred_Thr). Dashed line is the 1:1 reference. ",
  "Point alpha = 0.4 to indicate overplotting density. Axis limits set to the ",
  "range of observed Rsoil values.\n\n",
  "Panel (b): Cumulative bias (running sum of predicted minus observed) over the ",
  "2017–2020 chamber period for each model. Line types: solid = Mean; dashed = ",
  "P-NP switch; dotted = Threshold switch. Grey horizontal line at zero is the ",
  "reference for unbiased cumulative prediction.\n\n",
  "Note: Rsoil observations (n = ", n_obs, " days, 2017–2020) cover a shorter ",
  "period than the Reco eddy covariance record (n ≈ 2898 days, 2013–2020). ",
  "The chamber dataset reflects 7-port automated Rsoil measurements (LI-COR); ",
  "daily means are used throughout."
)
out_cap <- "final/figures/Fig8b_Rsoil_modelvobs_caption.txt"
writeLines(caption, out_cap)
message("Saved: ", out_cap)
