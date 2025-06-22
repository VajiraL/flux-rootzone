# Load required packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork) # For combining plots
library(ggthemes)  # For nice plot themes

# 1. Load and prepare data ---------------------------------------------------
# Assuming you've loaded the daily data for a site (DD files)
# Example for one site (modify path as needed)
site_data <- read_csv("D:/src/flux-rootzone/data/fdk_csv/FLX_SE-Deg_FLUXDATAKIT_FULLSET_DD_2001_2020_2-3.csv",
                     na = c("NA", "-9999"))

# Convert TIMESTAMP to Date format
site_data <- site_data %>%
  mutate(TIMESTAMP = as.Date(TIMESTAMP, format = "%m/%d/%Y"))

# 2. Data Gap Visualization -------------------------------------------------
plot_data_gaps <- function(data, var_name = "NEE_VUT_REF",
                          site_name = "SE-Deg") {
  # Create a binary variable: 1 = data present, NA = missing
  gap_data <- data %>%
    mutate(data_present = ifelse(is.na(!!sym(var_name)), NA, 1))

  ggplot(gap_data, aes(x = TIMESTAMP, y = 1)) +
    geom_tile(aes(fill = !is.na(data_present)), width = 1, height = 0.1) +
    scale_fill_manual(values = c("red", "darkgreen"),
                     labels = c("Missing", "Present"),
                     name = "Data Status") +
    labs(title = paste("Data Availability for", var_name, "at", site_name),
         x = "Date", y = "") +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank())
}

# Example usage for different variables
ustar_gap_plot <- plot_data_gaps(site_data, "USTAR")
le_gap_plot <- plot_data_gaps(site_data, "NETRAD")
precip_gap_plot <- plot_data_gaps(site_data, "SW_OUT")

# Combine plots
(ustar_gap_plot / le_gap_plot / precip_gap_plot) +
  plot_annotation(title = "Data Gap Analysis for SE-Deg")

# 3. QC Flag Analysis ------------------------------------------------------
plot_qc_flags <- function(data, var_name = "NEE_VUT_REF",
                         qc_name = "NEE_VUT_REF_QC",
                         site_name = "SE-Deg") {

  qc_data <- data %>%
    select(TIMESTAMP, value = !!sym(var_name), qc = !!sym(qc_name)) %>%
    mutate(qc_category = case_when(
      is.na(qc) ~ "Missing",
      qc > 0.75 ~ "High quality",
      qc > 0.25 ~ "Medium quality",
      TRUE ~ "Low quality"
    ))

  # Plot QC distribution
  qc_dist <- ggplot(qc_data, aes(x = qc_category, fill = qc_category)) +
    geom_bar() +
    scale_fill_brewer(palette = "Set1", name = "QC Category") +
    labs(title = paste("QC Flag Distribution for", var_name),
         x = "Quality Category", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Plot time series with QC
  ts_plot <- ggplot(qc_data, aes(x = TIMESTAMP, y = value, color = qc_category)) +
    geom_point(size = 1, alpha = 0.7) +
    scale_color_brewer(palette = "Set1", name = "QC Category") +
    labs(title = paste("Time Series with QC Flags for", var_name),
         x = "Date", y = var_name) +
    theme_minimal()

  # Combine plots
  qc_dist / ts_plot +
    plot_annotation(title = paste("QC Analysis for", var_name, "at", site_name))
}

# Example usage
plot_qc_flags(site_data, "GPP_NT_VUT_REF", "NEE_VUT_REF_QC")
plot_qc_flags(site_data, "LE_F_MDS", "LE_F_MDS_QC")

# 4. Comprehensive Data Quality Summary ------------------------------------
create_quality_summary <- function(data, site_name = "SE-Deg") {
  # Calculate missing data percentage for key variables
  key_vars <- c("GPP_NT_VUT_REF", "LE_F_MDS", "P_F", "TA_F_MDS", "VPD_F_MDS")

  missing_summary <- map_dfr(key_vars, ~ {
    tibble(
      variable = .x,
      total_days = nrow(data),
      missing_days = sum(is.na(data[[.x]])),
      missing_pct = round(mean(is.na(data[[.x]])) * 100, 1)
    )
  })

  # Calculate mean QC scores
  qc_vars <- c("NEE_VUT_REF_QC", "LE_F_MDS_QC", "TA_F_MDS_QC")

  qc_summary <- map_dfr(qc_vars, ~ {
    tibble(
      variable = str_remove(.x, "_QC"),
      mean_qc = mean(data[[.x]], na.rm = TRUE),
      good_qc_pct = round(mean(data[[.x]] == 0, na.rm = TRUE) * 100, 1)
    )
  })

  # Combine summaries
  full_summary <- missing_summary %>%
    left_join(qc_summary, by = "variable") %>%
    mutate(site = site_name) %>%
    select(site, everything())

  return(full_summary)
}

# Generate and print summary
quality_summary <- create_quality_summary(site_data)
print(quality_summary)

# 5. Visualization of Multiple Sites ---------------------------------------
# (Assuming you've loaded data for multiple sites)
# Example for 3 sites
site_list <- list(
  "US-Wkg" = read_csv("D:/src/flux-rootzone/data/fdk_csv/FLX_US-Wkg_FLUXDATAKIT_FULLSET_DD_2004_2021_2-3.csv"),
  "SE-Deg" = site_data, # from above
  "SE-Svb" = read_csv("D:/src/flux-rootzone/data/fdk_csv/FLX_SE-Svb_FLUXDATAKIT_FULLSET_DD_2014_2020_2-3.csv")
)

# Create gap analysis for all sites
gap_plots <- map(names(site_list), ~ {
  plot_data_gaps(site_list[[.x]], "NETRAD", .x)
})

# Combine plots
wrap_plots(gap_plots, ncol = 1) +
  plot_annotation(title = "GPP Data Availability Across Sites")
