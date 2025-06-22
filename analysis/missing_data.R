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

# --- Plotting Missingness Data ---

# Adjust plotting margins to accommodate labels, especially for vertical site names
# c(bottom, left, top, right)
par(mar = c(10, 8, 4, 2) + 0.1) # Increased bottom and left margins

# 1. Heatmap of Missing Data by Site and Variable
# Prepare the data matrix for the heatmap function.
# Ensure columns are in the predefined key_variables order.
missingness_matrix <- as.matrix(missingness_df[, key_variables])

# Define a color palette for the heatmap: green (low missingness) to red (high missingness)
color_palette <- colorRampPalette(c("forestgreen", "yellow", "orange", "red"))(100)

image(
    x = 1:ncol(missingness_matrix), # X-coordinates for variables
    y = 1:nrow(missingness_matrix), # Y-coordinates for sites
    z = t(missingness_matrix),      # Transpose matrix for correct orientation (variables on x, sites on y)
    col = color_palette,            # Apply the color palette
    axes = FALSE,                   # Suppress default axes to draw custom ones
    xlab = "", ylab = "",           # No default axis labels
    main = "Percentage of Missing Data by Site and Variable",
    cex.main = 1.2                  # Title font size
)

# Add custom X-axis labels (variables)
axis(side = 1,                 # Bottom axis
     at = 1:ncol(missingness_matrix), # Positions for labels
     labels = colnames(missingness_matrix), # Variable names
     las = 2,                    # Rotate labels vertically (2)
     cex.axis = 0.8              # Font size for axis labels
)

# Add custom Y-axis labels (sites)
axis(side = 2,                 # Left axis
     at = 1:nrow(missingness_matrix), # Positions for labels
     labels = rownames(missingness_matrix), # Site names
     las = 1,                    # Keep labels horizontal (1)
     cex.axis = 0.8              # Font size for axis labels
)

# Add a simple color scale legend outside the plot area or as text.
# A full color bar is complex in base R without external packages.
# Here, we'll just add text labels to indicate the range.
mtext("Missingness (%)", side = 4, line = 0.5, cex = 0.8, col = "gray30")
mtext("0%", side = 4, at = -0.05, line = 0.5, cex = 0.8, col = "forestgreen") # Approximation
mtext("100%", side = 4, at = 1.05, line = 0.5, cex = 0.8, col = "red") # Approximation

# save the figure as a PNG file
png("analysis/missing_data_heatmap.png", width = 1200, height = 800, res = 150)
dev.off() # Save the heatmap to a file


# Reset margins to default for any subsequent plots
# Reset plotting parameters for the next plot
par(new = FALSE)

# 2. Bar Plot: Average Missingness per Variable
# Calculate the mean missingness for each variable across all sites, ignoring NAs (for variables not present in all files)
avg_missingness_per_var <- colMeans(missingness_matrix, na.rm = TRUE)
# Order variables by average missingness (highest first) for better readability
avg_missingness_per_var <- avg_missingness_per_var[order(avg_missingness_per_var, decreasing = TRUE)]

par(mar = c(10, 5, 4, 2) + 0.1) # Adjust margins for this plot

barplot(
    height = avg_missingness_per_var,
    names.arg = names(avg_missingness_per_var),
    las = 2, # Rotate labels vertically
    ylab = "Average Missingness (%)",
    main = "Average Missing Data Percentage Across All Sites by Variable",
    col = "skyblue",
    ylim = c(0, max(avg_missingness_per_var, na.rm = TRUE) * 1.1), # Set Y-limit slightly above max
    cex.names = 0.8 # Font size for x-axis labels
)
abline(h = 0, col = "gray") # Add a baseline
#save the figure as a PNG file
png("analysis/missing_data_barplot_variables.png", width = 1200, height = 800, res = 150)
dev.off() # Save the bar plot to a file
par(new = FALSE)


# 3. Bar Plot: Average Missingness per Site (average across all key variables for each site)
# Calculate the mean missingness for each site across all key variables, ignoring NAs
avg_missingness_per_site <- rowMeans(missingness_matrix, na.rm = TRUE)
# Order sites by average missingness (highest first)
avg_missingness_per_site <- avg_missingness_per_site[order(avg_missingness_per_site, decreasing = TRUE)]

par(mar = c(10, 5, 4, 2) + 0.1) # Adjust margins for this plot

barplot(
    height = avg_missingness_per_site,
    names.arg = names(avg_missingness_per_site),
    las = 2, # Rotate labels vertically
    ylab = "Average Missingness (%)",
    main = "Average Missing Data Percentage Across Key Variables by Site",
    col = "lightcoral",
    ylim = c(0, max(avg_missingness_per_site, na.rm = TRUE) * 1.1), # Set Y-limit slightly above max
    cex.names = 0.8 # Font size for x-axis labels
)
abline(h = 0, col = "gray") # Add a baseline
# save the figure as a PNG file
png("analysis/missing_data_barplot_sites.png", width = 1200, height = 800, res = 150)
dev.off() # Save the bar plot to a file

# Reset margins to default for any subsequent plots
par(mar = c(5, 4, 4, 2) + 0.1)
