# 01_read_eddy.R ----------------------------------------------------------

# find setup no matter where this script is sourced from
setup_path <- if (file.exists("R/00_setup.R")) "R/00_setup.R" else "00_setup.R"
source(setup_path)

f1 <- "data/AddedPartionedCflux_US-Wkg_HH_201212312330_201812312330.csv"
f2 <- "data/Wkg_Ameriflux_2017-2020 with added partitioning/GapfilledPartitionedFluxes_US-Wkg_HH_201812312330_201912312330.csv"
f3 <- "data/Wkg_Ameriflux_2017-2020 with added partitioning/GapfilledPartitionedFluxes_US-Wkg_HH_201912312330_202012302330.csv"

read_fx <- function(fp) read_csv(fp, na = c("NaN","-9999"), show_col_types = FALSE)

USWkg12_20 <- bind_rows(read_fx(f1), read_fx(f2), read_fx(f3)) %>%
  mutate(
    T_CANOPY_2_1_1 = suppressWarnings(as.numeric(T_CANOPY_2_1_1)),
    TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)),
    date = as_date(TIMESTAMP_START),
    DOY_S = yday(date),
    RainEvent_0 = as.numeric(P > 0),
    RainEvent_5 = as.numeric(P > 5)
  ) %>%
  rename(
    SWC5=SWC_1_1_1, SWC15=SWC_1_2_1, SWC30=SWC_1_3_1,
    ST5=TS_1_1_1,  ST15=TS_1_2_1,  ST30=TS_1_3_1,
    AT2=TA_1_2_1,  AT6=TA_1_1_1,
    RH2=RH_1_2_1,  RH6=RH_1_1_1
  )

USWkg12_20_summary <- USWkg12_20 %>%
  group_by(date) %>%
  summarise(
    meanAT2 = mean(AT2, na.rm = TRUE),
    meanAT6 = mean(AT6, na.rm = TRUE),
    sum_R   = sum(P, na.rm = TRUE),
    rain_events = sum(RainEvent_0, na.rm = TRUE),
    meanRH2 = mean(RH2, na.rm = TRUE),
    meanRH6 = mean(RH6, na.rm = TRUE),
    meanSWC5 = mean(SWC5, na.rm = TRUE),
    sdSWC5   = sd(SWC5, na.rm = TRUE),
    meanSWC15 = mean(SWC15, na.rm = TRUE),
    sdSWC15   = sd(SWC15, na.rm = TRUE),
    meanSWC30 = mean(SWC30, na.rm = TRUE),
    meanST5 = mean(ST5, na.rm = TRUE),
    meanST15 = mean(ST15, na.rm = TRUE),
    meanST30 = mean(ST30, na.rm = TRUE),
    meanNEE = mean(NEE, na.rm = TRUE),
    meanGPP = mean(GPP, na.rm = TRUE),
    meanRECO = mean(RECO, na.rm = TRUE),
    sdReco = sd(RECO, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(DOY = yday(date),
         year = year(date))

write_csv(USWkg12_20_summary, "out/derived/USWkg12_20_summary.csv")
