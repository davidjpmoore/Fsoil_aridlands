# 06_swc_st_space.R -------------------------------------------------------

# find setup no matter where this script is sourced from
setup_path <- if (file.exists("R/00_setup.R")) "R/00_setup.R" else "00_setup.R"
source(setup_path)

# --- saving helper (safe, minimal) ---
fig_dir <- "out/figs"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

save_last_plot <- function(name, width = 6, height = 5, dpi = 300) {
  p <- ggplot2::last_plot()
  if (is.null(p)) {
    message("No last plot found for ", name, " — skipping.")
    return(invisible(FALSE))
  }
  ggplot2::ggsave(file.path(fig_dir, name), plot = p, width = width, height = height, dpi = dpi)
  message("Saved: ", file.path(fig_dir, name))
  invisible(TRUE)
}


p0 <- read_csv("out/derived/years_sum_Pulse0_DM.csv", show_col_types = FALSE)
p1 <- read_csv("out/derived/years_sum_Pulse1_DM.csv", show_col_types = FALSE)

bubble <- function(dat, title) {
  ggplot(dat, aes(x = meanSWC5, y = meanST5, size = pmax(meanRECO, 0))) +
    geom_point(alpha=0.6) +
    scale_size_continuous(name = expression(paste("Reco (", mu,"mol ", m^{-2}," ", s^{-1},")"))) +
    coord_cartesian(xlim=c(0,45), ylim=c(0,40)) +
    labs(title=title, x="Mean SWC 5 cm (%)", y="Mean Tsoil 5 cm (°C)")
}
save_plot(bubble(p1, "SWC and Soil T — during pulses"), "bubble_pulse.png")
save_plot(bubble(p0, "SWC and Soil T — between pulses"), "bubble_nonpulse.png")
