library(tidyverse)
library(earlywarnings)
library(tseries)
library(zoo)
library(ggfortify)
library(patchwork)

# 1. Data Preparation ------------------------------------------------------

# sites
path_SEDeg <- "D:/src/flux-rootzone/data/fdk_csv/FLX_SE-Deg_FLUXDATAKIT_FULLSET_DD_2001_2020_2-3.csv"
path_FIVar <- "D:/src/flux-rootzone/data/fdk_csv/FLX_FI-Var_FLUXDATAKIT_FULLSET_DD_2016_2020_2-3.csv"
#

# Load and preprocess daily data for a selected site
site_data <- read_csv(file.path(path_FIvar)) %>%
  mutate(TIMESTAMP = as.Date(TIMESTAMP, format = "%m/%d/%Y")) %>%
  # Filter for high quality data (QC=0)
  filter(NEE_VUT_REF_QC > 0.25, LE_F_MDS_QC > 0.25, !is.na(NETRAD)) %>%
  # Create derived variables
  mutate(WUE = GPP_NT_VUT_REF / LE_F_MDS, # 2454000*86400), #Convert LE to mm/day
         WUE = ifelse(is.infinite(WUE), NA, WUE)) %>%
  # Fill small gaps (<3 days) with linear interpolation
  mutate(across(c(GPP_NT_VUT_REF, LE_F_MDS, TA_F_MDS, VPD_F_MDS),
                ~na.approx(.x, maxgap = 7, na.rm = FALSE))) %>%
  # Create water balance variable
  mutate(water_balance = cumsum(P_F) - cumsum(LE_F_MDS)) # /2454000*86400)) # LE in mm/day

# 2. Basic Time Series Analysis -------------------------------------------
# Function to plot decomposed time series
plot_decomposition <- function(data, var, title = "") {
  ts_data <- ts(data[[var]], frequency = 365, start = c(year(min(data$TIMESTAMP)), yday(min(data$TIMESTAMP))))
  decomp <- stl(ts_data, s.window = "periodic")
  autoplot(decomp) + ggtitle(paste("Decomposition of", title))
}

# Example usage
decomp_1 <- plot_decomposition(site_data, "LAI", "LAI")
decomp_2 <- plot_decomposition(site_data, "NETRAD", "Net Radiation")
(decomp_1 / decomp_2)

# 3. Resilience Analysis with earlywarnings ------------------------------
# Function to compute resilience indicators
analyze_resilience <- function(data, var,
                               winsize = 0.5,  # 50% window size
                               bandwidth = 0.1, # for kernel smoothing
                               detrending = "gaussian") {

  ts_data <- na.omit(data[[var]])

  # Generic Early Warning Signals
  ew <- generic_ews(
    ts_data,
    winsize = winsize,
    detrending = detrending,
    bandwidth = bandwidth,
    logtransform = FALSE
  )

  # Plot the results
  plot(ew)

  # Return full analysis
  return(ew)
}

# Example analysis for drought response
drought_period <- site_data # %>%
#  filter(between(TIMESTAMP, as.Date('3/1/2002', format = "%m/%d/%Y"), as.Date('9/31/2002', format = "%m/%d/%Y")))

# Run resilience analysis on WUE during drought
wue_ews <- analyze_resilience(drought_period, "WUE")

# 4. Critical Transition Detection ----------------------------------------
# Function to detect potential critical transitions
detect_transitions <- function(data, var,
                               threshold = 0.05, # significance level
                               rsquared = 0.5) {

  # Calculate rolling statistics
  roll_stats <- generic_ews(
    data[[var]],
    winsize = 0.5,
    detrending = "gaussian",
    bandwidth = 0.1
  )

  # Test for significance
  bs <- surrogates(data[[var]], ns = 1000)
  bs_thresh <- quantile(bs$ar1, probs = 1 - threshold, na.rm = TRUE)

  # Plot with threshold
  ggplot(roll_stats, aes(x = timeindex, y = ar1)) +
    geom_line(color = "steelblue") +
    geom_hline(yintercept = bs_thresh, linetype = "dashed", color = "red") +
    labs(title = paste("Autocorrelation (AR1) for", var),
         x = "Time", y = "AR1 Coefficient") +
    theme_minimal()
}

# Example usage
detect_transitions(drought_period, "GPP_NT_VUT_REF")

# 5. Multivariate Resilience Analysis ------------------------------------
# Analyze cross-correlation between variables during stress
multivariate_resilience <- function(data,
                                    vars = c("GPP_NT_VUT_REF", "LE_F_MDS", "VPD_F_MDS"),
                                    max_lag = 30) {

  # Create correlation matrix
  cor_matrix <- cor(data[vars], use = "pairwise.complete.obs")

  # Cross-correlation analysis
  ccf_results <- list()
  for(i in 1:(length(vars)-1)) {
    for(j in (i+1):length(vars)) {
      ccf_temp <- ccf(data[[vars[i]]], data[[vars[j]]],
                      lag.max = max_lag, plot = FALSE)
      ccf_results[[paste(vars[i], vars[j], sep = "_")]] <- ccf_temp
    }
  }

  # Return results
  return(list(correlation_matrix = cor_matrix,
              cross_correlations = ccf_results))
}

# Run analysis
mv_analysis <- multivariate_resilience(drought_period)
print(mv_analysis$correlation_matrix)

# 6. Drought Recovery Analysis -------------------------------------------
analyze_recovery <- function(data,
                             stress_var = "VPD_F_MDS",
                             response_var = "GPP_NT_VUT_REF",
                             threshold = quantile(data[[stress_var]], 0.9, na.rm = TRUE)) {

  # Identify stress events
  stress_events <- data %>%
    mutate(stress = ifelse(!!sym(stress_var) > threshold, 1, 0),
           event_id = data.table::rleid(stress)) %>%
    filter(stress == 1) %>%
    group_by(event_id) %>%
    summarise(start = min(TIMESTAMP),
              end = max(TIMESTAMP),
              duration = as.numeric(end - start + 1))

  # Calculate recovery metrics for each event
  recovery_metrics <- map_dfr(1:nrow(stress_events), ~ {
    event <- stress_events[.x, ]
    window <- data %>%
      filter(TIMESTAMP >= event$start - 30,
             TIMESTAMP <= event$end + 60) %>%
      mutate(days_from_event = as.numeric(TIMESTAMP - event$start))

    # Baseline (pre-stress)
    baseline <- mean(window[window$days_from_event < 0, ][[response_var]], na.rm = TRUE)

    # Recovery trajectory
    window %>%
      filter(days_from_event >= 0) %>%
      group_by(week = days_from_event %/% 7) %>%
      summarise(mean_response = mean(!!sym(response_var), na.rm = TRUE),
                recovery_pct = mean_response / baseline * 100)
  })

  # Plot recovery trajectories
  ggplot(recovery_metrics, aes(x = week, y = recovery_pct, group = event_id)) +
    geom_line(alpha = 0.5) +
    stat_smooth(method = "gam", se = TRUE, color = "red") +
    labs(title = paste("Recovery of", response_var, "after", stress_var, "stress events"),
         x = "Weeks after stress event", y = "Recovery (% of baseline)") +
    theme_minimal()
}

# Example usage
analyze_recovery(site_data, "VPD_F_MDS", "GPP_NT_VUT_REF")
