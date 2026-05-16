# R/118_Fig10_Reco_timeseries_presentation.R ----------------------------------
# Presentation version of Figure 10: Reco time series.
# Coloured model lines, larger base font, no panel label.
# Source the publication version (117) independently — this script does NOT
# call or modify it.
#
# Output:
#   final/figures/Fig10_Reco_timeseries_presentation.png
#
# Run standalone from project root:
#   source("R/118_Fig10_Reco_timeseries_presentation.R")
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

# --- 1. Read and join (identical to publication version) ----------------------
pred15  <- read_csv("out/derived/RECO_predictions_15.csv",  show_col_types = FALSE) %>%
  mutate(date = as.Date(date))
predPNP <- read_csv("out/derived/RECO_predictions_PNP.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

df <- pred15 %>%
  select(date, meanRECO, MeanM_15, Reco_Combined) %>%
  left_join(select(predPNP, date, Reco_PNP), by = "date") %>%
  filter(
    !is.na(meanRECO), !is.na(MeanM_15), !is.na(Reco_Combined), !is.na(Reco_PNP),
    format(date, "%Y") != "2012"
  ) %>%
  arrange(date)

message("Fig10 presentation rows: ", nrow(df),
        "  Date range: ", min(df$date), " to ", max(df$date))

# --- 2. Long format for model lines -------------------------------------------
MODEL_LEVELS <- c("Mean", "P-NP switch", "15% threshold")
LTY_MODEL    <- c("Mean" = "solid",  "P-NP switch" = "dashed",  "15% threshold" = "dotted")
COL_LINE     <- c("Mean" = "#CC0000", "P-NP switch" = "#4169E1", "15% threshold" = "#00BFFF")

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

# --- 3. Axis label (identical to publication version) -------------------------
LABEL_Y <- expression(
  "Reco (" * mu * "mol CO"[2] * " m"^{-2} * " s"^{-1} * ")"
)

# --- 4. Build figure ----------------------------------------------------------
p <- ggplot() +
  geom_point(
    data  = df,
    aes(x = date, y = meanRECO),
    shape = 21, fill = "grey50", color = "transparent",
    size  = 1.5, alpha = 0.3,
    show.legend = FALSE
  ) +
  geom_line(
    data = df_lines,
    aes(x = date, y = Predicted, color = Model, linetype = Model),
    linewidth = 1.0
  ) +
  scale_color_manual(values = COL_LINE, name = NULL) +
  scale_linetype_manual(values = LTY_MODEL, name = NULL) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand      = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.03, 0.03))) +
  labs(x = NULL, y = LABEL_Y) +
  theme_pub(base_size = 14) +
  theme(
    legend.position       = c(0.03, 0.97),
    legend.justification  = c("left", "top"),
    legend.background     = element_blank(),
    legend.box.background = element_blank(),
    legend.text           = element_text(size = 12),
    legend.key.size       = unit(1.0, "lines"),
    legend.key.width      = unit(2.0, "lines"),
    plot.margin           = margin(5, 8, 5, 5, "pt")
  )

# --- 5. Save ------------------------------------------------------------------
out_png <- "final/figures/Fig10_Reco_timeseries_presentation.png"
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
