# Load required library
library(ggplot2)

# --- Configuration ---
# IMPORTANT: Replace this placeholder path with the actual directory containing your daily (DD) flux data files.
# Example: flux_data_dir <- "/path/to/your/flux-rootzone/data/fluxdata/"
flux_data_dir <- "/mnt/data/vajira/fluxDataKit/fdk_csv" # <--- USER: Adjust this path as needed!

# Define key variables for which to calculate missingness
key_variables <- c(
    "P_F",              # Precipitation rate
    "TA_F_MDS",         # Near-surface air temperature
    "SW_IN_F_MDS",      # Downward shortwave radiation
    "VPD_F_MDS",        # Vapour pressure deficit
    "LE_F_MDS",         # Latent heat flux (raw)
    "LE_CORR",          # Energy-balance-corrected latent heat flux
    "GPP_NT_VUT_REF",   # Gross primary productivity
    "RECO_NT_VUT_REF",  # Ecosystem respiration
    "NETRAD"            # Net radiation
)

# --- Data Processing ---

# Initialize an empty list to store missingness percentages for each site
all_site_missingness <- list()

# Get a list of all daily (DD) flux data files in the specified directory.
# This assumes file names are in the format FLX_SITENAME_..._DD_...csv
flux_files <- list.files(path = flux_data_dir, pattern = "_DD_.*\\.csv$", full.names = TRUE)

# Check if any files were found
if (length(flux_files) == 0) {
    stop("No daily (DD) flux data files found in the specified directory: '", flux_data_dir,
         "'.\nPlease ensure the 'flux_data_dir' variable points to the correct location and contains files ending with '_DD_*.csv'.")
}

message(paste("Found", length(flux_files), "daily flux data files for processing."))

# Loop through each identified flux data file
for (file_path in flux_files) {
    # Extract the site name from the filename.
    # The regex captures the part between 'FLX_' and the next underscore or 'DD'.
    file_name <- basename(file_path)
    site_name <- sub("FLX_([A-Za-z0-9-]+)_.*", "\\1", file_name)

    message(paste("Processing site:", site_name, "(File:", file_name, ")"))

    # Use tryCatch to handle potential errors during file reading or processing
    tryCatch({
        # Read the CSV file.
        # check.names=FALSE is used to prevent R from modifying column names
        # (e.g., changing 'TA_F_MDS' to 'TA.F.MDS'), ensuring direct matching with key_variables.
        data <- read.csv(file_path, header = TRUE, check.names = FALSE)

        # Initialize a vector to store missingness percentages for the current site
        site_missing_percentages <- numeric(length(key_variables))
        names(site_missing_percentages) <- key_variables

        # Calculate missingness for each key variable
        for (var in key_variables) {
            if (var %in% colnames(data)) {
                # Calculate the percentage of NA values for the current variable
                missing_count <- sum(is.na(data[[var]]))
                total_count <- nrow(data)
                percentage <- (missing_count / total_count) * 100
                site_missing_percentages[var] <- percentage
            } else {
                # If a key variable is not found in the current file, issue a warning
                warning(paste("Variable '", var, "' not found in data for site ", site_name, ". Setting missingness to NA.", sep=""))
                site_missing_percentages[var] <- NA # Mark as NA if variable is missing from the file
            }
        }
        # Store the results for the current site in the main list
        all_site_missingness[[site_name]] <- site_missing_percentages

    }, error = function(e) {
        # Log any errors encountered during file processing
        warning(paste("Error processing file '", file_name, "': ", e$message, sep=""))
    })
}

# Convert the list of site missingness data into a data frame.
# Each row will represent a site, and each column a key variable's missingness percentage.
missingness_df <- do.call(rbind, all_site_missingness)
missingness_df <- as.data.frame(missingness_df)

# If only one site was processed, rbind might return a vector; ensure it's a data frame
if (is.null(nrow(missingness_df))) {
    missingness_df <- as.data.frame(t(missingness_df))
    colnames(missingness_df) <- key_variables
    rownames(missingness_df) <- names(all_site_missingness)
}

message("\nMissingness calculation complete. Generating plots...")

# --- Plotting Missingness Data with ggplot2 ---

# 1. Heatmap of Missing Data by Site and Variable
# Prepare data for ggplot2 heatmap (convert to long format manually)
sites <- rownames(missingness_df)
variables <- colnames(missingness_df)

# Create long format data frame manually
heatmap_data <- data.frame(
    Site = rep(sites, each = length(variables)),
    Variable = rep(variables, times = length(sites)),
    Missingness = as.vector(t(as.matrix(missingness_df)))
)

# Create heatmap
p1 <- ggplot(heatmap_data, aes(x = Variable, y = Site, fill = Missingness)) +
    geom_tile(color = "white", size = 0.2) +
    scale_fill_gradient2(low = "forestgreen", mid = "yellow", high = "red", 
                        midpoint = 50, name = "Missingness (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14, hjust = 0.5),
          legend.position = "right") +
    labs(title = "Percentage of Missing Data by Site and Variable",
         x = "", y = "")

print(p1)
# save heatmap to file
ggsave("notes/heatmap_missingness.png", plot = p1, width = 12, height = 8, dpi = 300)

# 2. Bar Plot: Average Missingness per Variable
# Calculate the mean missingness for each variable across all sites, ignoring NAs
avg_missingness_per_var <- colMeans(missingness_df, na.rm = TRUE)
# Create data frame and order by missingness
var_data <- data.frame(
    Variable = names(avg_missingness_per_var),
    Missingness = avg_missingness_per_var
)
var_data <- var_data[order(var_data$Missingness, decreasing = TRUE), ]
var_data$Variable <- factor(var_data$Variable, levels = var_data$Variable)

p2 <- ggplot(var_data, aes(x = Variable, y = Missingness)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black", size = 0.3) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 10),
          plot.title = element_text(size = 14, hjust = 0.5)) +
    labs(title = "Average Missing Data Percentage Across All Sites by Variable",
         x = "", y = "Average Missingness (%)") +
    geom_hline(yintercept = 0, color = "gray", size = 0.5)

print(p2)
# save bar plot to file
ggsave("notes/missingness_by_variable.png", plot = p2, width = 10, height = 6, dpi = 300)

# 3. Bar Plot: Average Missingness per Site
# Calculate the mean missingness for each site across all key variables, ignoring NAs
avg_missingness_per_site <- rowMeans(missingness_df, na.rm = TRUE)
# Create data frame and order by missingness
site_data <- data.frame(
    Site = names(avg_missingness_per_site),
    Missingness = avg_missingness_per_site
)
site_data <- site_data[order(site_data$Missingness, decreasing = TRUE), ]
site_data$Site <- factor(site_data$Site, levels = site_data$Site)

p3 <- ggplot(site_data, aes(x = Site, y = Missingness)) +
    geom_bar(stat = "identity", fill = "lightcoral", color = "black", size = 0.3) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
          plot.title = element_text(size = 14, hjust = 0.5)) +
    labs(title = "Average Missing Data Percentage Across Key Variables by Site",
         x = "", y = "Average Missingness (%)") +
    geom_hline(yintercept = 0, color = "gray", size = 0.5)

print(p3)
# save bar plot to file
ggsave("notes/missingness_by_site.png", plot = p3, width = 18, height = 6, dpi = 300)

# Optional: Save plots to files
# ggsave("heatmap_missingness.png", plot = p1, width = 12, height = 8, dpi = 300)
# ggsave("missingness_by_variable.png", plot = p2, width = 10, height = 6, dpi = 300)
# ggsave("missingness_by_site.png", plot = p3, width = 10, height = 6, dpi = 300)