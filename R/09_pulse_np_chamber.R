# 09_pulse_np_chamber.R ---------------------------------------------------
# (this script is intentionally minimal; 08_read_chamber.R already saves the splits)
# find setup no matter where this script is sourced from
setup_path <- if (file.exists("R/00_setup.R")) "R/00_setup.R" else "00_setup.R"
source(setup_path)
message("Chamber pulse/non-pulse summaries already created in 08_read_chamber.R")
