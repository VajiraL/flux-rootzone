## Download data
# execute the download bash script
source("src/download_FluxDataKit.sh")

# remotes::install_local("/home/vajira/R/FluxDataKit-main.zip", dependencies = TRUE)
# set working directory to the root of the project
setwd("/home/vajira/flux-rootzone/")

## list of sites available as a csv
site_all <- read.csv("data/fdk_site_info.csv", stringsAsFactors = FALSE)

site_fullyearsequence <- read.csv("data/fdk_site_fullyearsequence.csv", stringsAsFactors = FALSE)

sites <- read.csv("data/fdk_site_info.csv", stringsAsFactors = FALSE)|>
  # filter(!(sitename %in% c("MX-Tes", "US-KS3"))) |>  # failed sites
  filter(!(igbp_land_use %in% c("CRO", "WET"))) |>
  left_join(
    fdk_site_fullyearsequence,
    by = "sitename"
  ) |>
  filter(!drop_gpp) |>  # where no full year sequence was found
  filter(nyears_gpp >= 3)

## test one site
site <- read.csv("data/fdk_csv/FLX_AU-Rob_FLUXDATAKIT_FULLSET_DD_2014_2017_2-3.csv", stringsAsFactors = FALSE)
