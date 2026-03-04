# R/eddy_helpers.R
#----------------
# Helper functions for eddy-tower data processing

#' Read and combine multiple eddy data CSVs into one tibble,
#' replacing -9999 with NA and renaming raw columns to standard names.
#'
#' @param file_paths Named list of file paths to read
#' @return Combined tibble of all files with standardized column names
read_eddy_files <- function(file_paths) {
  library(readr)
  library(dplyr)
  map_dfr(file_paths, ~ read_csv(.x, na = "NaN")) %>%
    # convert -9999 to NA
    mutate(across(everything(), ~ na_if(.x, -9999))) %>%
    # standardize column names for downstream processing
    rename_eddy_vars()
}

#' Rename raw eddy column names to simplified, consistent names
#'
#' @param df Data frame with original eddy column names
#' @return Data frame with renamed columns
rename_eddy_vars <- function(df) {
  df %>% rename(
    AT2    = TA_1_2_1,
    AT6    = TA_1_1_1,
    RH2    = RH_1_2_1,
    RH6    = RH_1_1_1,
    SWC5   = SWC_1_1_1,
    SWC15  = SWC_1_2_1,
    SWC30  = SWC_1_3_1,
    ST5    = TS_1_1_1,
    ST15   = TS_1_2_1,
    ST30   = TS_1_3_1
  )
}

#' Add time-based variables to an eddy dataframe
#'
#' @param df Data frame with a timestamp column
#' @param timestamp_col Name of the timestamp column (character)
#' @param tz Time zone string for parsing
#' @return Data frame with year, month, day_of_year, hour, minute, date columns
add_time_vars <- function(df, timestamp_col = "TIMESTAMP_START", tz = "UTC") {
  library(lubridate)
  df %>%
    mutate(
      TIMESTAMP   = ymd_hm(.data[[timestamp_col]], tz = tz),
      year        = year(TIMESTAMP),
      month       = month(TIMESTAMP),
      day_of_year = yday(TIMESTAMP),
      hour        = hour(TIMESTAMP),
      minute      = minute(TIMESTAMP),
      date        = as.Date(TIMESTAMP)
    )
}

#' Summarise half-hourly eddy data to daily means/totals
#'
#' @param df Tibble with one row per half hour and variables AT2, AT6, RH2, RH6,
#'           SWC5, SWC15, SWC30, ST5, ST15, ST30, NEE, GPP, RECO, P
#' @return One-row-per-day summary tibble
daily_summary <- function(df) {
  library(dplyr)
  df %>%
    mutate(
      RainEvent_0 = as.integer(P > 0),
      RainEvent_5 = as.integer(P > 5)
    ) %>%
    group_by(date) %>%
    summarise(
      meanAT2    = mean(AT2, na.rm = TRUE),
      meanAT6    = mean(AT6, na.rm = TRUE),
      sum_R      = sum(P, na.rm = TRUE),
      rain_events= sum(RainEvent_0, na.rm = TRUE),
      meanRH2    = mean(RH2, na.rm = TRUE),
      meanRH6    = mean(RH6, na.rm = TRUE),
      meanSWC5   = mean(SWC5, na.rm = TRUE),
      meanSWC15  = mean(SWC15, na.rm = TRUE),
      meanSWC30  = mean(SWC30, na.rm = TRUE),
      meanST5    = mean(ST5, na.rm = TRUE),
      meanST15   = mean(ST15, na.rm = TRUE),
      meanST30   = mean(ST30, na.rm = TRUE),
      meanNEE    = mean(NEE, na.rm = TRUE),
      meanGPP    = mean(GPP, na.rm = TRUE),
      meanRECO   = mean(RECO, na.rm = TRUE),
      sdReco     = sd(RECO, na.rm = TRUE)
    ) %>%
    ungroup()
}

#' Classify daily data into S/M/L pulses and compute durations & days-since-rain
#'
#' @param df Daily summary tibble (must include sum_R)
#' @param pulse_thresholds Named numeric vector of rain thresholds, e.g. c(S=5, M=10, L=20)
#' @param pulse_durations  Named integer vector of pulse durations, e.g. c(S=8, M=14, L=20)
#' @return Tibble with added pulseduration_S/M/L, max_pulse_duration, days_since_rain_event
classify_pulse <- function(df, pulse_thresholds, pulse_durations) {
  library(dplyr)
  df <- df %>% arrange(date)
  # initialize duration columns
  for (lvl in names(pulse_thresholds)) {
    df[[paste0("pulseduration_", lvl)]] <- 0L
  }
  # assign pulse durations
  for (lvl in names(pulse_thresholds)) {
    th  <- pulse_thresholds[[lvl]]
    pd  <- pulse_durations[[lvl]]
    idxs <- which(df$sum_R > th)
    for (i in idxs) {
      end_i <- min(i + pd - 1, nrow(df))
      df[[paste0("pulseduration_", lvl)]][i:end_i] <- pd
    }
  }
  # compute max and days-since
  df %>% mutate(
    max_pulse_duration   = pmax(pulseduration_S, pulseduration_M, pulseduration_L, na.rm = TRUE),
    days_since_rain_event= {
      d <- integer(n())
      for (i in seq_along(d)) {
        if (df$sum_R[i] > pulse_thresholds[["S"]]) {
          d[i] <- 0L
        } else if (i == 1) {
          d[i] <- 0L
        } else {
          d[i] <- d[i - 1] + 1L
        }
      }
      d
    }
  )
}
