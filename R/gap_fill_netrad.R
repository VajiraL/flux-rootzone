library(tidyverse)
library(lubridate)
library(mice)       # For multiple imputation
library(missForest) # Random forest imputation
library(imputeTS)   # Time series specific imputation
library(caret)      # Machine learning framework
library(xgboost)    # For XGBoost model
library(recipes)    # For feature engineering

path_SEDeg <- "D:/src/flux-rootzone/data/fdk_csv/FLX_SE-Deg_FLUXDATAKIT_FULLSET_DD_2001_2020_2-3.csv"

# 1. Load and prepare data ------------------------------------------------
se_deg <- read_csv(file.path(path_SEDeg)) %>%
  mutate(TIMESTAMP = as.Date(TIMESTAMP, format = "%m/%d/%Y")) %>%
  # Select relevant variables for NETRAD modeling
  select(TIMESTAMP, NETRAD,
         SW_IN_F_MDS, LW_IN_F_MDS, SW_OUT, TA_F_MDS,
         VPD_F_MDS, P_F, WS_F, LE_F_MDS, H_F_MDS) %>%
  # Filter to high quality periods (QC flags if available)
  filter(if_all(c(SW_IN_F_MDS, LW_IN_F_MDS), ~!is.na(.x)))

# Visualize missing data
ggplot(se_deg, aes(x = TIMESTAMP, y = NETRAD)) +
  geom_line(color = "steelblue") +
  geom_point(data = filter(se_deg, is.na(NETRAD)),
             color = "red", size = 1) +
  labs(title = "SE-Deg: NETRAD with missing values highlighted",
       x = "Date", y = "NETRAD (W/m²)") +
  theme_minimal()

# 2. Short Gap Filling (<3 days) -----------------------------------------
# Use linear interpolation for very short gaps
se_deg <- se_deg %>%
  mutate(NETRAD_interp = na_interpolation(NETRAD, maxgap = 3))

# 3. Statistical Gap Filling (Medium Gaps) -------------------------------
# Method 1: Multiple Imputation with Chained Equations (MICE)
mice_model <- mice(se_deg %>% select(-TIMESTAMP, -NETRAD_interp),
                   method = "pmm", m = 3, maxit = 10)
se_deg$NETRAD_mice <- complete(mice_model)$NETRAD

# Method 2: Random Forest Imputation
rf_imp <- missForest(se_deg %>% select(-TIMESTAMP, -NETRAD_interp, -NETRAD_mice))
se_deg$NETRAD_rf <- rf_imp$ximp$NETRAD

# 4. Data-Driven Modeling (For Large Gaps) ------------------------------
# Prepare data for machine learning
model_data <- se_deg %>%
  mutate(doy = yday(TIMESTAMP),
         year = year(TIMESTAMP)) %>%
  select(-TIMESTAMP) %>%
  # Remove rows where target is NA (these are our gaps)
  filter(!is.na(NETRAD)) %>%
  # Remove columns with too many NAs
  select(-where(~sum(is.na(.x)) > nrow(.)*0.3))

# Create recipe for feature engineering
netrad_recipe <- recipe(NETRAD ~ ., data = model_data) %>%
  step_impute_knn(all_predictors()) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal()) %>%
  step_corr(all_predictors(), threshold = 0.9)

# Train XGBoost model
set.seed(123)
xgb_model <- train(
  netrad_recipe,
  data = model_data,
  method = "xgbTree",
  trControl = trainControl(method = "cv", number = 5),
  tuneLength = 3
)

# Predict missing NETRAD values
missing_data <- se_deg %>%
  filter(is.na(NETRAD)) %>%
  mutate(doy = yday(TIMESTAMP),
         year = year(TIMESTAMP))

se_deg$NETRAD_xgb <- se_deg$NETRAD
se_deg$NETRAD_xgb[is.na(se_deg$NETRAD)] <- predict(xgb_model, newdata = missing_data)

# 5. Empirical Relationship Method ---------------------------------------
# NETRAD = SW_IN - SW_OUT + LW_IN - LW_OUT (simplified)
# Since we don't have LW_OUT, we'll estimate it using empirical relationship
lw_out_model <- lm(LW_OUT ~ TA_F_MDS + VPD_F_MDS,
                   data = your_other_site_with_complete_data) # Need reference data

se_deg <- se_deg %>%
  mutate(
    LW_OUT_est = predict(lw_out_model, newdata = se_deg),
    NETRAD_empirical = ifelse(is.na(NETRAD),
                              SW_IN_F_MDS - SW_OUT + LW_IN_F_MDS - LW_OUT_est,
                              NETRAD)
  )

# 6. Combine Methods Strategically ---------------------------------------
se_deg <- se_deg %>%
  mutate(
    NETRAD_filled = case_when(
      # Use interpolation for very short gaps
      !is.na(NETRAD_interp) ~ NETRAD_interp,
      # Use empirical for daytime (better SW relationships)
      hour(TIMESTAMP) %in% 6:18 ~ NETRAD_empirical,
      # Use XGBoost for others
      TRUE ~ NETRAD_xgb
    )
  )

# 7. Validate Results ---------------------------------------------------
# Create artificial gaps in complete periods to test methods
validation_data <- se_deg %>%
  filter(!is.na(NETRAD), year(TIMESTAMP) == 2015) %>%
  mutate(NETRAD_orig = NETRAD,
         NETRAD = ifelse(row_number() %in% sample(1:nrow(.), NA, NETRAD_orig))

         # Apply your chosen method to validation data and compare...

         # 8. Visualize Final Gap-Filled Series ---------------------------------
         ggplot(se_deg, aes(x = TIMESTAMP)) +
           geom_line(aes(y = NETRAD, color = "Original"), na.rm = TRUE) +
           geom_line(aes(y = NETRAD_filled, color = "Gap-filled"), alpha = 0.7) +
           scale_color_manual(values = c("Original" = "red", "Gap-filled" = "blue")) +
           labs(title = "SE-Deg: NETRAD gap-filling results",
                x = "Date", y = "NETRAD (W/m²)", color = "Series") +
           theme_minimal()

         # 9. Export Results -----------------------------------------------------
         write_csv(se_deg, "SE-Deg_NETRAD_gapfilled.csv")


### plot NETRAD, and  NETRAD_mice to show gapfilling results
ggplot(se_deg %>%
         filter(
           between(TIMESTAMP, as.Date('2006-09-01'), as.Date('2008-03-31'))),
       aes(x = TIMESTAMP)) +
  geom_line(aes(y = NETRAD, color = "Original"), na.rm = TRUE, size=1) +
  geom_line(aes(y = NETRAD_mice, color = "MICE Imputation"), alpha = 0.5) +
  #geom_line(aes(y = NETRAD_rf, color = "Random Forest Imputation"), alpha = 0.7) +
  #geom_line(aes(y = NETRAD_xgb, color = "XGBoost Imputation"), alpha = 0.7) +
  #geom_line(aes(y = NETRAD_filled, color = "Final Gap-filled"), alpha = 0.7) +
  scale_color_manual(values = c("Original" = "red",
                                 "MICE Imputation" = "green",
                                 "Random Forest Imputation" = "purple",
                                 "XGBoost Imputation" = "orange",
                                 "Final Gap-filled" = "blue")) +
  labs(title = "SE-Deg: NETRAD Gap-filling Results",
       x = "Date", y = "NETRAD (W/m²)", color = "Series") +
  theme_minimal()
