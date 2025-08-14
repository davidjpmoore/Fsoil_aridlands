# =====================================================
# >>> Read_chamber.R (robust + writes to out/derived)
# =====================================================
#
# 16-05-2024  (revised)
# Anastasia Makhnykina

# Portable setup import (works whether run from project root or from R/)
setup_path <- if (file.exists("R/00_setup.R")) "R/00_setup.R" else "00_setup.R"
if (file.exists(setup_path)) source(setup_path)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(rlang)
})

# ---------------------------
# Paths
# ---------------------------
raw_chamber_path <- "data/KN_soil_resp17_20_longHead.csv"
pulses_primary   <- "out/derived/years_sum1_DM.csv"
pulses_fallback  <- "data/years_sum1_DM.csv"
out_dir          <- "out/derived"

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---------------------------
# Read raw chamber file
# ---------------------------
CH_USWkg17_20 <- read.csv(
  raw_chamber_path,
  header = TRUE,
  na.strings = c("NaN", "NA", ""),
  stringsAsFactors = FALSE
)

# ---------------------------
# Robust DOY -> date conversion
# ---------------------------
CH_USWkg17_20 <- CH_USWkg17_20 %>%
  mutate(
    Year4 = ifelse(Year > 1900, as.integer(Year), as.integer(Year + 2000)),
    DOY_i = as.integer(round(DOY))
  ) %>%
  # drop impossible rows: DOY=366 in non-leap years
  filter(!(DOY_i == 366 & !lubridate::leap_year(Year4))) %>%
  mutate(
    date = as.Date(lubridate::ymd(paste0(Year4, "0101")) + days(DOY_i - 1))
  ) %>%
  select(-DOY_i)

# Save a clean copy for debugging
write.csv(CH_USWkg17_20, file.path(out_dir, "Chamber_data_clean.csv"), row.names = FALSE)

# ---------------------------
# Pulse definitions (from eddy daily summaries)
# ---------------------------
pulses_path <- if (file.exists(pulses_primary)) pulses_primary else pulses_fallback
if (!file.exists(pulses_path)) {
  stop("No pulse definition table found. Expected: ",
       pulses_primary, " or ", pulses_fallback,
       ". Did R/02_define_pulses.R run?")
}

pulsedef_raw <- readr::read_csv(pulses_path, na = c("NaN", "NA", ""), show_col_types = FALSE)

# Helper to pick the first existing column among candidates (with relaxed matching)
pick <- function(df, candidates) {
  for (nm in candidates) {
    if (nm %in% names(df)) return(df[[nm]])
  }
  norm <- function(x) gsub("[\\._ ]", "", tolower(x))
  nn <- norm(names(df))
  for (nm in candidates) {
    hit <- which(nn == norm(nm))
    if (length(hit)) return(df[[ hit[1] ]])
  }
  return(NULL)
}

pulsedefinitions <- tibble(
  date                  = as.Date(pick(pulsedef_raw, c("date","Date"))),
  days_since_rain_event = suppressWarnings(as.numeric(pick(pulsedef_raw, c("days_since_rain_event","days.since.rain.event","Days_since_rain_event")))),
  max_pulse_duration    = suppressWarnings(as.numeric(pick(pulsedef_raw, c("max_pulse_duration","max.pulse.duration","Max_pulse_duration")))),
  sum_R                 = suppressWarnings(as.numeric(pick(pulsedef_raw, c("sum_R","sum.r","Sum_R")))),
  meanSWC5              = suppressWarnings(as.numeric(pick(pulsedef_raw, c("meanSWC5","meanswc5","mean_swc5")))),
  meanST5               = suppressWarnings(as.numeric(pick(pulsedef_raw, c("meanST5","meanst5","mean_st5")))),
  meanGPP               = suppressWarnings(as.numeric(pick(pulsedef_raw, c("meanGPP","meangpp","mean_gpp")))),
  meanRECO              = suppressWarnings(as.numeric(pick(pulsedef_raw, c("meanRECO","meanreco","mean_reco"))))
)

# Validate required fields
missing_now <- names(pulsedefinitions)[vapply(pulsedefinitions, function(x) all(is.na(x)), logical(1))]
if (length(missing_now)) {
  stop("Missing required columns in pulse table (", pulses_path, "): ",
       paste(missing_now, collapse = ", "),
       "\nMake sure R/02_define_pulses.R wrote a complete years_sum1_DM.csv.")
}

# Convert units and flags
pulsedefinitions <- pulsedefinitions %>%
  mutate(
    meanSWC5 = meanSWC5 / 100,                 # % -> fraction
    pulseIND = days_since_rain_event < max_pulse_duration
  )

# ---------------------------
# Join pulse info to chamber rows
# ---------------------------
Cham_USWKG_pulsedata <- CH_USWkg17_20 %>%
  left_join(pulsedefinitions, by = "date")

Cham_USWKG_pulsedata <- Cham_USWKG_pulsedata %>%
  mutate(
    days_since_rain_event = suppressWarnings(as.numeric(days_since_rain_event)),
    max_pulse_duration    = suppressWarnings(as.numeric(max_pulse_duration)),
    pulseIND = if_else(
      !is.na(days_since_rain_event) & !is.na(max_pulse_duration) &
        (days_since_rain_event < max_pulse_duration),
      TRUE, FALSE
    )
  )



# ---------------------------
# Normalize/Coalesce driver columns (handle .x / .y suffixes)
# ---------------------------
to_num <- function(x) suppressWarnings(as.numeric(x))
grab  <- function(df, base) {
  cands <- c(base, paste0(base, c(".x", ".y")))
  vals <- lapply(cands[cands %in% names(df)], function(nm) to_num(df[[nm]]))
  if (length(vals) == 0) return(rep(NA_real_, nrow(df)))
  out <- vals[[1]]
  if (length(vals) > 1) for (k in 2:length(vals)) out <- dplyr::coalesce(out, vals[[k]])
  out
}

# right before computing mean columns
if ("Por4VWC" %in% names(Cham_USWKG_pulsedata) && !"VWC4" %in% names(Cham_USWKG_pulsedata)) {
  Cham_USWKG_pulsedata <- dplyr::rename(Cham_USWKG_pulsedata, VWC4 = Por4VWC)
}



Cham_USWKG_pulsedata <- Cham_USWKG_pulsedata %>%
  mutate(
    meanGPP  = grab(cur_data_all(), "meanGPP"),
    meanRECO = grab(cur_data_all(), "meanRECO"),
    meanSWC5 = grab(cur_data_all(), "meanSWC5"),
    meanST5  = grab(cur_data_all(), "meanST5")
  ) %>%
  select(-tidyselect::any_of(c("meanGPP.x","meanGPP.y",
                               "meanRECO.x","meanRECO.y",
                               "meanSWC5.x","meanSWC5.y",
                               "meanST5.x","meanST5.y")))

# Coverage message
n_ch <- nrow(Cham_USWKG_pulsedata)
msg  <- function(n, label) paste0(label, ": ", n, " / ", n_ch, " (", round(100*n/max(n_ch,1),1), "%)")
message(
  "Date join + driver coverage -> ",
  paste(
    msg(sum(!is.na(Cham_USWKG_pulsedata$days_since_rain_event)), "JOIN"),
    msg(sum(is.finite(Cham_USWKG_pulsedata$meanGPP)),            "GPP"),
    msg(sum(is.finite(Cham_USWKG_pulsedata$meanRECO)),           "RECO"),
    msg(sum(is.finite(Cham_USWKG_pulsedata$meanSWC5)),           "SWC5"),
    msg(sum(is.finite(Cham_USWKG_pulsedata$meanST5)),            "ST5"),
    sep = " | "
  )
)
# ---------------------------
# Rename chamber port columns if present
# ---------------------------
rename_map <- c(
  "Port.1Soil.Resp.um.co2.m2.s" = "Rsoil1",
  "Port.1VWC"                   = "VWC1",
  "Port.1Soil.Temp.deg.C"       = "Tsoil1",
  "Port.2soil.respum.co2.m2.s"  = "Rsoil2",
  "Port.2VWC"                   = "VWC2",
  "Port.2Soil.Temp.deg.C"       = "Tsoil2",
  "Port.3soil.respum.co2.m2.s"  = "Rsoil3",
  "Port.3VWC"                   = "VWC3",
  "Port.3Soil.Temp.deg.C"       = "Tsoil3",
  "Port.4Soil.Resp.um.co2.m2.s" = "Rsoil4",
  "Port4VWC"                    = "VWC4",
  "Port.4Soil.Temp.deg.C"       = "Tsoil4",
  "Port.5soil.respum.co2.m2.s"  = "Rsoil5",
  "Port.5VWC"                   = "VWC5",
  "Port.5Soil.Temp.deg.C"       = "Tsoil5",
  "Port.6soil.respum.co2.m2.s"  = "Rsoil6",
  "Port.6VWC"                   = "VWC6",
  "Port.6Soil.Temp.deg.C"       = "Tsoil6",
  "Port.7soil.respum.co2.m2.s"  = "Rsoil7",
  "Port.7VWC"                   = "VWC7",
  "Port.7Soil.Temp.deg.C"       = "Tsoil7"
)

present_old <- intersect(names(rename_map), names(Cham_USWKG_pulsedata))
if (length(present_old)) {
  mapping_new_old <- rlang::set_names(present_old, rename_map[present_old]) # new = old
  Cham_USWKG_pulsedata <- Cham_USWKG_pulsedata %>%
    dplyr::rename(!!!mapping_new_old)
}
# Also catch Por4VWC -> VWC4 (odd header variant)
if ("Por4VWC" %in% names(Cham_USWKG_pulsedata) && !"VWC4" %in% names(Cham_USWKG_pulsedata)) {
  Cham_USWKG_pulsedata <- dplyr::rename(Cham_USWKG_pulsedata, VWC4 = Por4VWC)
}

# Detect port columns that actually exist
rsoil_cols <- grep("^Rsoil\\d+$", names(Cham_USWKG_pulsedata), value = TRUE)
vwc_cols   <- grep("^VWC\\d+$",   names(Cham_USWKG_pulsedata), value = TRUE)
tsoil_cols <- grep("^Tsoil\\d+$", names(Cham_USWKG_pulsedata), value = TRUE)

# Clean negatives in Rsoil if present
if (length(rsoil_cols)) {
  for (rc in rsoil_cols) {
    Cham_USWKG_pulsedata[[rc]] <- ifelse(Cham_USWKG_pulsedata[[rc]] < 0,
                                         NA_real_, Cham_USWKG_pulsedata[[rc]])
  }
}

# ---------------------------
# Pulse / Non-Pulse splits (AFTER renames)
# ---------------------------
Cham_USWKG_PULSETIME     <- Cham_USWKG_pulsedata %>% dplyr::filter(pulseIND)
Cham_USWKG_NON_PULSETIME <- Cham_USWKG_pulsedata %>% dplyr::filter(!pulseIND)

# ---------------------------
# Mean across ports
# ---------------------------
# Safe row mean even if none of the columns exist
safe_rowmean <- function(df, cols) {
  cols <- intersect(cols, names(df))
  if (!length(cols)) return(rep(NA_real_, nrow(df)))
  rowMeans(as.data.frame(df[, cols, drop = FALSE]), na.rm = TRUE)
}

mean_across <- function(df) {
  df %>%
    mutate(
      meanRsoil = safe_rowmean(cur_data_all(), rsoil_cols),
      meanTsoil = safe_rowmean(cur_data_all(), tsoil_cols),
      meanVWC   = safe_rowmean(cur_data_all(), vwc_cols)
    )
}

Cham_USWKG_pulsedata     <- mean_across(Cham_USWKG_pulsedata)
message("All rows: ", nrow(Cham_USWKG_pulsedata),
        " | meanRsoil finite: ", sum(is.finite(Cham_USWKG_pulsedata$meanRsoil)))

Cham_USWKG_PULSETIME     <- mean_across(Cham_USWKG_PULSETIME)
message("All rows: ", nrow(Cham_USWKG_PULSETIME),
        " | meanRsoil finite: ", sum(is.finite(Cham_USWKG_PULSETIME$meanRsoil)))

Cham_USWKG_NON_PULSETIME <- mean_across(Cham_USWKG_NON_PULSETIME)
message("All rows: ", nrow(Cham_USWKG_NON_PULSETIME),
        " | meanRsoil finite: ", sum(is.finite(Cham_USWKG_NON_PULSETIME$meanRsoil)))

# ---------------------------
# Daily summaries (All / Pulse / Non-Pulse)
# ---------------------------
sum_daily <- function(df) {
  df %>%
    group_by(date) %>%
    summarise(
      meanGPP = mean(meanGPP,  na.rm = TRUE),
      meanRsoil = mean(meanRsoil, na.rm = TRUE),
      sumRain = mean(sum_R, na.rm = TRUE),
      meanTsoil = mean(meanTsoil, na.rm = TRUE),
      meanSWC = mean(meanVWC,   na.rm = TRUE),
      max_pulse_duration = mean(max_pulse_duration, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(DOY = lubridate::yday(date))
}

Sum_Chamber_all <- sum_daily(Cham_USWKG_pulsedata)
Sum_Chamber_P   <- sum_daily(Cham_USWKG_PULSETIME)
Sum_Chamber_NP  <- sum_daily(Cham_USWKG_NON_PULSETIME)

# ---------------------------
# WRITE DERIVED OUTPUTS (out/derived)
# ---------------------------
write.csv(Sum_Chamber_all, file.path(out_dir, "All_summary_chamber.csv"),   row.names = FALSE)
write.csv(Sum_Chamber_NP,  file.path(out_dir, "NonPulse_sum_chamber.csv"),  row.names = FALSE)
write.csv(Sum_Chamber_P,   file.path(out_dir, "Pulse_sum_chamber.csv"),     row.names = FALSE)

message("Wrote chamber summaries to '", out_dir, "': ",
        "All_summary_chamber.csv, NonPulse_sum_chamber.csv, Pulse_sum_chamber.csv")
