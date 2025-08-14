# R/101_ingest_daily_pulses_min.R
# Minimal, single-purpose: ingest eddy files -> daily means -> pulse flags

setup_path <- if (file.exists("R/00_setup.R")) "R/00_setup.R" else "00_setup.R"
source(setup_path)

# -------------------- 1) Input files --------------------
f_explicit <- c(
  "data/AddedPartionedCflux_US-Wkg_HH_201212312330_201812312330.csv",
  "data/Wkg_Ameriflux_2017-2020 with added partitioning/GapfilledPartitionedFluxes_US-Wkg_HH_201812312330_201912312330.csv",
  "data/Wkg_Ameriflux_2017-2020 with added partitioning/GapfilledPartitionedFluxes_US-Wkg_HH_201912312330_202012302330.csv"
)
f_glob <- Sys.glob("data/**/*US-Wkg*_HH_*.csv")
files  <- unique(c(f_explicit[file.exists(f_explicit)], f_glob))
stopifnot(length(files) > 0)

read_fx <- function(fp) readr::read_csv(fp, na = c("NaN","-9999"), show_col_types = FALSE)
raw_hh  <- purrr::map_dfr(files, read_fx)

# -------------------- 2) Safe renames (no forward references) ------------
safe_rename <- function(df, from, to) {
  if (from %in% names(df) && !(to %in% names(df))) dplyr::rename(df, !!to := dplyr::all_of(from)) else df
}

hh <- raw_hh
hh <- safe_rename(hh, "SWC_1_1_1", "SWC5")
hh <- safe_rename(hh, "SWC_1_2_1", "SWC15")
hh <- safe_rename(hh, "SWC_1_3_1", "SWC30")
hh <- safe_rename(hh, "TS_1_1_1",  "ST5")
hh <- safe_rename(hh, "TS_1_2_1",  "ST15")
hh <- safe_rename(hh, "TS_1_3_1",  "ST30")
hh <- safe_rename(hh, "TA_1_2_1",  "AT2")
hh <- safe_rename(hh, "TA_1_1_1",  "AT6")
hh <- safe_rename(hh, "RH_1_2_1",  "RH2")
hh <- safe_rename(hh, "RH_1_1_1",  "RH6")
# Precip: keep P if present; otherwise fall back to P_F if it exists
if (!("P" %in% names(hh)) && ("P_F" %in% names(hh))) {
  hh <- dplyr::rename(hh, P = P_F)
}

# -------------------- 3) Time + daily summaries --------------------------
hh <- hh %>%
  dplyr::mutate(
    TIMESTAMP_START = lubridate::ymd_hm(as.character(TIMESTAMP_START)),
    date = as.Date(TIMESTAMP_START)
  )

eddy_daily <- hh %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(
    meanAT2  = mean(AT2,  na.rm = TRUE),
    meanAT6  = mean(AT6,  na.rm = TRUE),
    sum_R    = sum(P,     na.rm = TRUE),
    meanRH2  = mean(RH2,  na.rm = TRUE),
    meanRH6  = mean(RH6,  na.rm = TRUE),
    meanSWC5 = mean(SWC5, na.rm = TRUE),
    sdSWC5   = sd(SWC5,   na.rm = TRUE),
    meanSWC15= mean(SWC15,na.rm = TRUE),
    sdSWC15  = sd(SWC15,  na.rm = TRUE),
    meanSWC30= mean(SWC30,na.rm = TRUE),
    meanST5  = mean(ST5,  na.rm = TRUE),
    meanST15 = mean(ST15, na.rm = TRUE),
    meanST30 = mean(ST30, na.rm = TRUE),
    meanNEE  = mean(NEE,  na.rm = TRUE),
    meanGPP  = mean(GPP,  na.rm = TRUE),
    meanRECO = mean(RECO, na.rm = TRUE),
    sdReco   = sd(RECO,   na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  dplyr::mutate(DOY = lubridate::yday(date),
                year = lubridate::year(date)) %>%
  dplyr::arrange(date)

readr::write_csv(eddy_daily, "out/derived/eddy_daily.csv")

# -------------------- 4) Pulse definition (your mapping) -----------------
len_from_sumR <- function(x) dplyr::case_when(
  x > 20 ~ 20L,
  x > 10 ~ 14L,
  x >  5 ~  8L,
  TRUE   ~  0L
)

df   <- dplyr::arrange(eddy_daily, date)
wins <- len_from_sumR(df$sum_R)
n    <- nrow(df)
dur  <- integer(n)

for (i in which(wins > 0)) {
  j_end <- min(n, i + wins[i] - 1)
  dur[i:j_end] <- pmax(dur[i:j_end], wins[i])
}

eddy_with_pulse <- df %>%
  dplyr::mutate(
    max_pulse_duration = dur,
    rain_event = as.integer(sum_R > 5),
    days_since_rain_event = {
      ctr <- integer(dplyr::n())
      for (i in seq_len(dplyr::n())) {
        ctr[i] <- if (sum_R[i] > 5) 0L else if (i == 1) 0L else ctr[i-1] + 1L
      }
      ctr
    },
    PulseFlag = dplyr::if_else(days_since_rain_event < max_pulse_duration, 1L, 0L)
  )

readr::write_csv(eddy_with_pulse, "out/derived/eddy_daily_with_pulse.csv")

# Convenience “DM” outputs for downstream scripts
years_sum1       <- eddy_with_pulse
years_sum_Pulse0 <- dplyr::filter(eddy_with_pulse, PulseFlag == 0L)
years_sum_Pulse1 <- dplyr::filter(eddy_with_pulse, PulseFlag == 1L)

readr::write_csv(years_sum1,       "out/derived/years_sum1_DM.csv")
readr::write_csv(years_sum_Pulse0, "out/derived/years_sum_Pulse0_DM.csv")
readr::write_csv(years_sum_Pulse1, "out/derived/years_sum_Pulse1_DM.csv")

message("✓ Wrote: out/derived/eddy_daily.csv, eddy_daily_with_pulse.csv, and DM CSVs.")
