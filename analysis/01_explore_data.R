# Load required libraries
library(tidyverse)
library(leaflet)
library(plotly)
library(viridis)

# 1. Read the data
site_info <- read_csv("data/fdk_site_info.csv")
site_sequence <- read_csv("data/fdk_site_fullyearsequence.csv")

# 2. Data cleaning and preparation
# Join the two datasets
site_data <- site_info %>%
  left_join(site_sequence, by = "sitename") %>%
  mutate(
    # Calculate sequence length for GPP data
    gpp_sequence_length = year_end_gpp - year_start_gpp + 1,
    # Create a factor for land cover types
    igbp_land_use = factor(igbp_land_use),
    # Create a factor for Koppen climate classes
    koeppen_code = factor(koeppen_code)
  )

# 3. Plot 1: Interactive map of site locations
map <- leaflet(site_data) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircleMarkers(
    lng = ~lon,
    lat = ~lat,
    radius = 5,
    color = "blue",
    fillOpacity = 0.8,
    popup = ~paste(
      "<b>Site:</b> ", sitename, "<br>",
      "<b>Lat:</b> ", round(lat, 2), "<br>",
      "<b>Lon:</b> ", round(lon, 2), "<br>",
      "<b>Elevation:</b> ", round(elv), " m<br>",
      "<b>Land Cover:</b> ", igbp_land_use, "<br>",
      "<b>Climate:</b> ", koeppen_code, "<br>",
      "<b>Years:</b> ", year_start, "-", year_end
    ),
    label = ~sitename
  ) %>%
  addControl(
    "Flux Site Locations",
    position = "topright"
  )

# Save the map as HTML
htmlwidgets::saveWidget(map, file = "analysis/flux_sites_map.html")

# 4. Plot 2: Bar plot of sites by IGBP land cover type
land_cover_plot <- ggplot(site_data, aes(x = fct_infreq(igbp_land_use))) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(
    title = "Number of Sites by IGBP Land Cover Type",
    x = "IGBP Land Cover Type",
    y = "Number of Sites",
    fill = "Land Cover"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("analysis/land_cover_distribution.png", land_cover_plot, width = 10, height = 6)

# 5. Plot 3: Bar plot of sites by Koppen climate class
climate_plot <- ggplot(site_data, aes(x = fct_infreq(koeppen_code), fill = koeppen_code)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  scale_fill_viridis(discrete = TRUE) +
  labs(
    title = "Number of Sites by Koppen Climate Class",
    x = "Koppen Climate Class",
    y = "Number of Sites",
    fill = "Climate Class"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("analysis/climate_distribution.png", climate_plot, width = 10, height = 6)


# 6. Plot 4: Data sequence length for each site
# Prepare data for the sequence plot
sequence_data <- site_data %>%
  filter(!is.na(year_start_gpp)) %>%
  mutate(n_years = year_end_gpp - year_start_gpp + 1) %>%
  mutate(sitename = fct_reorder(sitename, year_start_gpp)) %>%
  mutate(sitename = fct_reorder(sitename, n_years)) %>%
  rowwise() %>%
  mutate(years = list(seq(year_start_gpp, year_end_gpp))) %>%
  unnest(years) %>%
  select(sitename, years)

sequence_plot <- ggplot(sequence_data, aes(x = years, y = sitename)) +
  geom_tile(aes(fill = "Data Available"), color = "white") +
  scale_fill_manual(values = c("Data Available" = "steelblue")) +
  labs(
    title = "GPP Data Availability by Site and Year",
    x = "Year",
    y = "Site",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

ggsave("analysis/data_sequence.png", sequence_plot, width = 8, height = 24)

site_data |>
  select(
    sitename,
    year_start = year_start_gpp,
    year_end = year_end_gpp) |>
  ggplot(aes(y = sitename,
             xmin = year_start,
             xmax = year_end)) +
  geom_linerange() +
  theme(legend.title = element_blank(),
        legend.position="top") +
  labs(title = "Good data sequences",
       y = "Site")
