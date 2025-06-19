# Load required libraries
library(tidyverse)
library(lubridate)
library(geosphere)
library(ggrepel)

source("R/pet_functions.R")

# 0. Read the data
site_info <- read_csv("data/fdk_sites_full.csv")

# 2. Process all site data
process_site_data <- function(site) {
  # Find the DD file for this site
  dd_file <- list.files(file.path(getwd(),'data/fdk_csv'),
                        pattern = paste0(site, ".*DD.*\\.csv"),
                        full.names = TRUE)

  if (length(dd_file) == 0) return(NULL)

  # Read daily data
  daily_data <- read_csv(dd_file)

  # Calculate PET
  daily_data <- daily_data %>%
    mutate(
      date = as.Date(TIMESTAMP),
      year = year(date),
      pet = calculate_pet(method = "priestley_taylor", NETRAD, TA_F_MDS, PA),
      # unit conversions
      et = LE_F_MDS / 28.4  # Convert W m-2 to mm/day (1 W m-2 ≈ 0.035 mm/day)
    )

  # Calculate annual sums
  annual_data <- daily_data %>%
    group_by(year) %>%
    summarise(
      precip = sum(precip, na.rm = TRUE),
      et = sum(et, na.rm = TRUE),
      pet = sum(pet, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      site = site,
      aridity_index = pet / precip,  # PET/P
      evaporation_ratio = et / precip  # ET/P
    )

  return(annual_data)
}

# 3. Get list of all sites and process them
all_sites <- unique(sites_all$sitename)
all_annual_data <- map_dfr(all_sites, process_site_data, .progress = TRUE)

# 4. Calculate whole-period averages for each site
site_period_data <- all_annual_data %>%
  group_by(site) %>%
  summarise(
    mean_precip = mean(precip, na.rm = TRUE),
    mean_et = mean(et, na.rm = TRUE),
    mean_pet = mean(pet, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    aridity_index = mean_pet / mean_precip,
    evaporation_ratio = mean_et / mean_precip
  )

# 5. Plot 1: Budyko space with whole-period averages
budyko_curve <- function(ai) {
  # Budyko curve function
  sqrt(ai * tanh(1/ai) * (1 - exp(-ai)))
}

plot_budyko <- ggplot() +
  # Add Budyko curve
  stat_function(fun = budyko_curve, color = "black", size = 1) +
  # Add sites
  geom_point(data = site_period_data,
             aes(x = aridity_index, y = evaporation_ratio, color = site),
             size = 3) +
  geom_text_repel(data = site_period_data,
                  aes(x = aridity_index, y = evaporation_ratio, label = site),
                  size = 3, box.padding = 0.5) +
  # Labels and theme
  labs(title = "Budyko Space (Whole Period Averages)",
       x = "Aridity Index (PET/P)",
       y = "Evaporation Ratio (ET/P)") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1.2), expand = c(0, 0)) +
  theme(legend.position = "none")

ggsave("analysis/budyko_whole_period.png", plot_budyko, width = 10, height = 8)

# 6. Plot 2: Budyko space with annual values and temporal trajectories
plot_budyko_annual <- ggplot() +
  # Add Budyko curve
  stat_function(fun = budyko_curve, color = "black", size = 1) +
  # Add annual points with temporal gradient
  geom_point(data = all_annual_data,
             aes(x = aridity_index, y = evaporation_ratio, color = year),
             alpha = 0.7, size = 2) +
  # Connect points for each site
  geom_path(data = all_annual_data,
            aes(x = aridity_index, y = evaporation_ratio, group = site),
            alpha = 0.3) +
  # Labels and theme
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
write_csv(all_annual_data, "analysis/annual_water_balance.csv")
write_csv(site_period_data, "analysis/site_period_water_balance.csv")

message("Budyko plots saved to:")
message("- analysis/budyko_whole_period.png")
message("- analysis/budyko_annual_trajectories.png")
message("Water balance data saved to:")
message("- analysis/annual_water_balance.csv")
message("- analysis/site_period_water_balance.csv")
