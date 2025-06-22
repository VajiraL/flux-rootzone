# Load required libraries
library(data.table)
library(lubridate)
library(geosphere)
library(ggplot2)
library(ggrepel)
library(SciViews)

source("R/pet_functions.R")

# 0. Read the data
site_info <- fread("data/fdk_sites_full.csv")

# 2. Process all site data
process_site_data <- function(site) {
  # Find the DD file for this site
  dd_file <- list.files(file.path("/mnt/data/vajira/fluxDataKit/fdk_csv"),
                        pattern = paste0(site, ".*DD.*\\.csv"),
                        full.names = TRUE)

  if (length(dd_file) == 0) return(NULL)

  # Read daily data
  daily_data <- fread(dd_file)

  # Add calculated fields
  daily_data[, date := as.Date(TIMESTAMP)]
  daily_data[, year := year(date)]
  daily_data[, pet := calculate_pet(method = "priestley_taylor", NETRAD, TA_F_MDS, PA)]
  daily_data[, et := LE_F_MDS / 28.4]  # Convert W/m² to mm/day
  daily_data[, precip := P_F]  # Precipitation in mm/day

  # Calculate annual sums
  # Only use the years with valid LE_CORR data.
  # The columns 'year_start_lecorr', 'year_end_lecorr' in 'site_info' gives the period.
  # Extract start and end year values
  start_year <- site_info[sitename == site][["year_start_lecorr"]][1]
  end_year <- site_info[sitename == site][["year_end_lecorr"]][1]

  # Check if both are finite numbers and create a sequence of valid years
  if (is.finite(start_year) && is.finite(end_year)) {
    valid_years <- seq.int(start_year, end_year)
    } else {
      valid_years <- integer(0)  # set to NULL if not valid
      }

# extract daily data for valid years in the list of valid_years
  if (length(valid_years)==0) {
    return(NULL)  # Skip this site if no valid years
  } else {
    daily_data <- daily_data[year %in% valid_years, ]
  }

  # Calculate annual sums
  annual_data <- daily_data[, .(
    precip = sum(precip, na.rm = TRUE),
    et = sum(et, na.rm = TRUE),
    pet = sum(pet, na.rm = TRUE)
  ), by = year]

  # Add site and Budyko ratios
  annual_data[, site := site]
  annual_data[, aridity_index := pet / precip]
  annual_data[, evaporation_ratio := et / precip]

  return(annual_data)
}

# 3. Get list of all sites and process them
all_sites <- unique(site_info$sitename)
all_annual_data_list <- lapply(all_sites, process_site_data)
all_annual_data <- rbindlist(all_annual_data_list, use.names = TRUE, fill = TRUE)

# 4. Calculate whole-period averages for each site
site_period_data <- all_annual_data[, .(
  mean_precip = mean(precip, na.rm = TRUE),
  mean_et = mean(et, na.rm = TRUE),
  mean_pet = mean(pet, na.rm = TRUE)
), by = site]

site_period_data[, aridity_index := mean_pet / mean_precip]
site_period_data[, evaporation_ratio := mean_et / mean_precip]

# 5. Plot 1: Budyko space with whole-period averages
budyko_curve <- function(ai) {
  sqrt(ai * tanh(1 / ai) * (1 - exp(-ai)))
}

plot_budyko <- ggplot() +
  stat_function(fun = budyko_curve, color = "black", size = 1) +
  geom_point(data = site_period_data,
             aes(x = aridity_index, y = evaporation_ratio, color = ln(whc)),
             size = 3 ) +
  geom_text_repel(data = site_period_data,
                  aes(x = aridity_index, y = evaporation_ratio, label = site),
                  size = 3, box.padding = 0.5) +
  labs(title = "Budyko Space (Whole Period Averages)",
       x = "Aridity Index (PET/P)",
       y = "Evaporation Ratio (ET/P)") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1.2), expand = c(0, 0)) +
  theme(legend.position = "none")

ggsave("notes/budyko_whole_period_whc.png", plot_budyko, width = 10, height = 8)

# 6. Plot 2: Budyko space with annual values and temporal trajectories
plot_budyko_annual <- ggplot() +
  stat_function(fun = budyko_curve, color = "black", size = 1) +
  geom_point(data = all_annual_data,
             aes(x = aridity_index, y = evaporation_ratio, color = year),
             alpha = 0.7, size = 2) +
  geom_path(data = all_annual_data,
            aes(x = aridity_index, y = evaporation_ratio, group = site),
            alpha = 0.3) +
  labs(title = "Annual Budyko Space with Temporal Trajectories",
       x = "Aridity Index (PET/P)",
       y = "Evaporation Ratio (ET/P)",
       color = "Year") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1.2), expand = c(0, 0)) +
  scale_color_viridis_c() +
  theme(legend.position = "right")

ggsave("analysis/budyko_annual_trajectories.png", plot_budyko_annual, width = 10, height = 8)

# 7. Save the calculated data
fwrite(all_annual_data, "analysis/annual_water_balance.csv")
fwrite(site_period_data, "analysis/site_period_water_balance.csv")

message("Budyko plots saved to:")
message("- analysis/budyko_whole_period.png")
message("- analysis/budyko_annual_trajectories.png")
message("Water balance data saved to:")
message("- analysis/annual_water_balance.csv")
message("- analysis/site_period_water_balance.csv")

site_period_data <- read.csv("analysis/site_period_water_balance.csv")
# add the Koppen climate data column from site_info to the site_period_data
site_period_data <- merge(site_period_data, site_info[, .(sitename, koeppen_code, igbp_land_use, whc)], by.x = "site", by.y = "sitename", all.x = TRUE)
