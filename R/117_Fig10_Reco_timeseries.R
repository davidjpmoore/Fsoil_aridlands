# R/117_Fig10_Reco_timeseries.R -----------------------------------------------
# Publication Figure 10: Reco observed vs three model predictions — time series.
#
# One panel: observed daily Reco (grey points) with three model lines overlaid.
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
#   final/figures/Fig10_Reco_timeseries.png
#   final/figures/Fig10_Reco_timeseries_caption.txt
#
# Run standalone from project root:
#   source("R/117_Fig10_Reco_timeseries.R")
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

# --- 1. Read and join ---------------------------------------------------------
pred15  <- read_csv("out/derived/RECO_predictions_15.csv",  show_col_types = FALSE) %>%
  mutate(date = as.Date(date))
predPNP <- read_csv("out/derived/RECO_predictions_PNP.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

df <- pred15 %>%
  select(date, meanRECO, MeanM_15, Reco_Combined) %>%
  left_join(select(predPNP, date, Reco_PNP), by = "date") %>%
  filter(
    !is.na(meanRECO), !is.na(MeanM_15), !is.na(Reco_Combined), !is.na(Reco_PNP),
    format(date, "%Y") != "2012"   # exclude sparse 2012 per publication convention
  ) %>%
  arrange(date)

message("Fig10 rows: ", nrow(df),
        "  Date range: ", min(df$date), " to ", max(df$date))

# --- 2. Long format for model lines -------------------------------------------
MODEL_LEVELS <- c("Mean", "P-NP switch", "15% threshold")
LTY_MODEL    <- c("Mean" = "solid", "P-NP switch" = "dashed", "15% threshold" = "dotted")

df_lines <- df %>%
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
    )
  )

# --- 3. Axis label ------------------------------------------------------------
LABEL_Y <- expression(
  "Reco (" * mu * "mol CO"[2] * " m"^{-2} * " s"^{-1} * ")"
)

# --- 4. Build figure ----------------------------------------------------------
p <- ggplot() +
  geom_point(
    data  = df,
    aes(x = date, y = meanRECO),
    shape = 21, fill = "grey40", color = "transparent",
    size  = 0.8, alpha = 0.4,
    show.legend = FALSE
  ) +
  geom_line(
    data = df_lines,
    aes(x = date, y = Predicted, linetype = Model),
    color = "black", linewidth = 0.6
  ) +
  scale_linetype_manual(values = LTY_MODEL, name = NULL) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand      = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.03, 0.03))) +
  labs(x = NULL, y = LABEL_Y) +
  theme_pub() +
  theme(
    legend.position       = c(0.03, 0.97),
    legend.justification  = c("left", "top"),
    legend.background     = element_blank(),
    legend.box.background = element_blank(),
    legend.text           = element_text(size = 9),
    legend.key.size       = unit(0.85, "lines"),
    legend.key.width      = unit(1.8, "lines"),
    plot.margin           = margin(5, 8, 5, 5, "pt")
  )

# --- 5. Save ------------------------------------------------------------------
out_png <- "final/figures/Fig10_Reco_timeseries.png"
ggsave(
  out_png,
  plot   = p,
  width  = SAVE_DOUBLE_COL_W,
  height = 3.5,
  units  = "in",
  dpi    = SAVE_DPI_PUB,
  bg     = BG_WHITE
)
message("Saved: ", out_png)

# --- 6. Caption ---------------------------------------------------------------
n_days  <- nrow(df)
yr_min  <- format(min(df$date), "%Y")
yr_max  <- format(max(df$date), "%Y")
caption <- paste0(
  "Figure 10. Daily ecosystem respiration (Reco) at US-Wkg (Walnut Gulch Kendall, ",
  "Arizona), ", yr_min, "–", yr_max,
  " (n = ", n_days, " daily observations).\n\n",
  "Grey filled circles show observed daily mean Reco from eddy covariance gap-filling ",
  "and flux partitioning (shape 21, fill = grey40, alpha = 0.4). Three model ",
  "predictions are overlaid as black lines: solid = Mean model (single lumped fit, ",
  "all days); dashed = P-NP switch model (separate pulse and non-pulse fits using ",
  "rainfall-event classification); dotted = 15% threshold model (SWC-threshold switch ",
  "at soil water content ≥ 15%). Year 2012 excluded due to sparse coverage."
)
out_cap <- "final/figures/Fig10_Reco_timeseries_caption.txt"
writeLines(caption, out_cap)
message("Saved: ", out_cap)
