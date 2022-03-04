library("amerifluxr")
library("pander")


## When running, replace user_id and user_email with a real AmeriFlux account
floc1_Wkg <- amf_download_base(
  user_id = "davidjpmoore",
  user_email = "davidjpmore@email.arizona.edu",
  site_id = "US-Wkg",
  data_product = "BASE-BADM",
  data_policy = "CCBY4.0",
  agree_policy = TRUE,
  intended_use = "other",
  intended_use_text = "Study of controls of soil respiration",
  verbose = TRUE,
  out_dir = "data"
)

floc2_Wkg <- system.file("extdata", "data/AMF_US-Wkg_BASE-BADM_18-5.zip", package = "amerifluxr")


base1 <- amf_read_base(
  file = "data/AMF_US-Wkg_BASE-BADM_18-5.zip",
  unzip = TRUE,
  parse_timestamp = FALSE
)
pander::pandoc.table(base1[c(1:3),])

FP_ls <- amf_variables()
pander::pandoc.table(FP_ls[c(11:20), ])

basename_decode <- amf_parse_basename(var_name = colnames(base1))
pander::pandoc.table(basename_decode[c(1, 2, 3, 4, 6, 11, 12),])

# filter data, using default physical range +/- 5% buffer
base_f <- amf_filter_base(data_in = base1)

bif <- bif[bif$SITE_ID == "US-Wkg", ]
pander::pandoc.table(bif[c(1:15), ])

bif_flux <- amf_extract_badm(bif_data = bif, select_group = "GRP_FLUX_MEASUREMENTS")
pander::pandoc.table(bif_flux)


bif_hc <- amf_extract_badm(bif_data = bif, select_group = "GRP_HEIGHTC")
pander::pandoc.table(bif_hc)


# convert HEIGHTC_DATE to POSIXlt
bif_hc$TIMESTAMP <- strptime(bif_hc$HEIGHTC_DATE, format = "%Y%m%d", tz = "GMT")

# convert HEIGHTC column to numeric
bif_hc$HEIGHTC <- as.numeric(bif_hc$HEIGHTC)

# plot time series of canopy height
plot(bif_hc$TIMESTAMP, bif_hc$HEIGHTC, xlab = "TIMESTAMP", ylab = "canopy height (m)")


# get a list of contacts
bif_contact <- amf_extract_badm(bif_data = bif, select_group = "GRP_TEAM_MEMBER")
pander::pandoc.table(bif_contact)

# get data DOI
bif_doi <- amf_extract_badm(bif_data = bif, select_group = "GRP_DOI")
pander::pandoc.table(bif_doi)

#read Kendall Grassland files
US_Wkg = read.csv("data/AMF_US-Wkg_BASE_HH_18-5.csv" ,  skip =2,  
                  comment.char = "",check.names = FALSE, quote="",
                  na.strings=c("NA","NaN", " ", "-9999") )

RsoilWkg<- read.csv("data/kendall2017.csv",  skip =0,  
             comment.char = "",check.names = FALSE, quote="",
             na.strings=c("NA","NaN", " ") )

plot(RsoilWkg$Fs_1)
plot(RsoilWkg$Fs_2)
plot(RsoilWkg$Fs_3)
plot(RsoilWkg$Fs_4)

plot(RsoilWkg$Fs_1, RsoilWkg$Fs_2)

plot(US_Wkg$LE)