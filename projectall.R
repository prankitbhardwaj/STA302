# ================================
# NBA Player Performance Analysis
# ================================

# ------------------------
# 1. Load Necessary Libraries
# ------------------------

# List of required packages
required_packages <- c("MASS", "ggplot2", "car", "dplyr", "lmtest", "sandwich", "quantreg", "nlme")

# Install any missing packages
installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
}

# Load libraries
library(MASS)      # For Box-Cox transformation (not used here)
library(ggplot2)   # For enhanced plotting
library(car)       # For VIF and other regression diagnostics
library(dplyr)     # For data manipulation
library(lmtest)    # For statistical tests
library(sandwich)  # For robust standard errors
library(quantreg)  # For quantile regression
library(nlme)      # For generalized least squares

# ------------------------
# 2. Data Loading and Initial Inspection
# ------------------------

# Load the data
nba_data_raw <- read.csv(file = "nba_final_original.csv", header = TRUE)

# Inspect the first few rows
head(nba_data_raw)

# ------------------------
# 3. Data Cleaning
# ------------------------

# Step 1: Replace NA values with 0
nba_data_cleaned <- nba_data_raw %>%
  mutate_all(~ ifelse(is.na(.), 0, .))

# Step 2: Convert Position abbreviations to categorical (factor) variables
nba_data_cleaned$Position <- factor(nba_data_cleaned$Pos1, 
                                    levels = c("PG", "SG", "SF", "PF", "C"), 
                                    labels = c("PG", "SG", "SF", "PF", "C"))

# Optional: Remove 'Pos1' if it's no longer needed
nba_data_cleaned$Pos1 <- NULL

# Step 3: Rename 'eFG.' to 'EffectiveFG' for clarity
colnames(nba_data_cleaned)[colnames(nba_data_cleaned) == "eFG."] <- "EffectiveFG"

# Step 4: Convert 'EffectiveFG' and 'Salary' from strings to numeric
# Ensure that these columns are indeed character or factor before conversion
nba_data_cleaned$EffectiveFG <- as.numeric(as.character(nba_data_cleaned$EffectiveFG))
nba_data_cleaned$Salary <- as.numeric(as.character(nba_data_cleaned$Salary))

# Step 5: Select only the relevant columns for analysis
nba_data_cleaned <- nba_data_cleaned[, c("PTS", "Age", "Position", "EffectiveFG", "FGA", "Salary")]

# Step 6: Create Transformation Variables
# No Transformation: PTS used directly
# Square Root Transformation
nba_data_cleaned$sqrt_PPG <- sqrt(nba_data_cleaned$PTS)

# Log Transformation
# Adding 1 to handle any potential zero values
nba_data_cleaned$log_PPG <- log(nba_data_cleaned$PTS + 1)

# Verify the cleaned data structure
str(nba_data_cleaned)
summary(nba_data_cleaned)

# Save the cleaned data for future reference
write.csv(nba_data_cleaned, "nba_final_cleaned.csv", row.names = FALSE)

# ------------------------
# 4. Exploratory Data Analysis (EDA)
# ------------------------

# Summary Statistics
summary_stats <- summary(nba_data_cleaned)
print(summary_stats)

# Calculate Standard Deviations
sd_age <- sd(nba_data_cleaned$Age)
sd_effectivefg <- sd(nba_data_cleaned$EffectiveFG)
sd_fga <- sd(nba_data_cleaned$FGA)
sd_salary <- sd(nba_data_cleaned$Salary)
sd_pts <- sd(nba_data_cleaned$PTS)
sd_sqrt_ppg <- sd(nba_data_cleaned$sqrt_PPG)
sd_log_ppg <- sd(nba_data_cleaned$log_PPG)

# Print Standard Deviations
cat("Standard Deviation of Age:", round(sd_age, 2), "\n")
cat("Standard Deviation of EffectiveFG:", round(sd_effectivefg, 2), "\n")
cat("Standard Deviation of FGA:", round(sd_fga, 2), "\n")
cat("Standard Deviation of Salary:", round(sd_salary, 2), "\n")
cat("Standard Deviation of PTS:", round(sd_pts, 2), "\n")
cat("Standard Deviation of sqrt_PPG:", round(sd_sqrt_ppg, 2), "\n")
cat("Standard Deviation of log_PPG:", round(sd_log_ppg, 2), "\n")

# Boxplots for Each Variable
# Set up plotting area: 2 rows x 3 columns
par(mfrow = c(2, 3))

# Boxplot for Points Per Game (PPG)
boxplot(nba_data_cleaned$PTS, 
        xlab = "Points per Game", 
        main = "Boxplot of PPG", 
        horizontal = TRUE)

# Boxplot for Age
boxplot(nba_data_cleaned$Age, 
        xlab = "Age", 
        main = "Boxplot of Age", 
        horizontal = TRUE)

# Boxplot for Position
# Note: Boxplot for categorical variables converted to numeric
boxplot(as.numeric(nba_data_cleaned$Position), 
        xlab = "Position", 
        main = "Boxplot of Position", 
        horizontal = TRUE)

# Boxplot for Effective Field Goal Percentage (EFG)
boxplot(nba_data_cleaned$EffectiveFG, 
        xlab = "Effective Field Goal", 
        main = "Boxplot of EFG", 
        horizontal = TRUE)

# Boxplot for Field Goals Attempted (FGA)
boxplot(nba_data_cleaned$FGA, 
        xlab = "Field Goal Attempts", 
        main = "Boxplot of FGA", 
        horizontal = TRUE)

# Boxplot for Salary
boxplot(nba_data_cleaned$Salary, 
        xlab = "Salary", 
        main = "Boxplot of Salary", 
        horizontal = TRUE)

# Reset plotting area to default
par(mfrow = c(1, 1))

# Pairwise Scatterplots of Predictors
pairs(nba_data_cleaned[, c("Age", "EffectiveFG", "FGA", "Salary")], 
      main = "Pairwise Scatterplots of Predictors",
      pch = 19, 
      col = "blue")

# ------------------------
# 5. Multiple Linear Regression Models
# ------------------------

# Model 1: No Transformation
model_no_transform <- lm(PTS ~ Age + Position + EffectiveFG + FGA + Salary, data = nba_data_cleaned)

# Model 2: Square Root Transformation
model_sqrt <- lm(sqrt_PPG ~ Age + Position + EffectiveFG + FGA + Salary, data = nba_data_cleaned)

# Model 3: Logarithmic Transformation
model_log <- lm(log_PPG ~ Age + Position + EffectiveFG + FGA + Salary, data = nba_data_cleaned)

# ------------------------
# 6. Diagnostic Assessment of All Models
# ------------------------

# Function to plot residuals and QQ plot for a given model
plot_diagnostics <- function(model, model_name) {
  # Extract residuals and fitted values
  residuals_model <- residuals(model)
  fitted_model <- fitted(model)
  
  # Residuals vs Fitted Plot
  par(mfrow = c(1, 2))
  plot(fitted_model, residuals_model, 
       main = paste("Residuals vs Fitted Values\n(", model_name, ")", sep=""),
       xlab = "Fitted Values",
       ylab = "Residuals",
       pch = 19, col = "blue")
  abline(h = 0, col = "red", lwd = 2)
  
  # Normal Q-Q Plot
  qqnorm(residuals_model, main = paste("Normal Q-Q Plot\n(", model_name, ")", sep=""), pch = 19, col = "purple")
  qqline(residuals_model, col = "red", lwd = 2)
  par(mfrow = c(1, 1))
  
  # Residuals vs Each Predictor
  predictors <- names(model$model)[-1]  # Exclude dependent variable
  par(mfrow = c(2, 3))
  
  for (pred in predictors) {
    if (is.numeric(nba_data_cleaned[[pred]])) {
      plot(nba_data_cleaned[[pred]], residuals_model, 
           xlab = pred, 
           ylab = "Residuals", 
           main = paste("Residuals vs", pred, "\n(", model_name, ")", sep=" "),
           pch = 19, col = "blue")
      abline(h = 0, col = "red", lwd = 2)
    } else {
      boxplot(residuals_model ~ nba_data_cleaned[[pred]], 
              xlab = pred, 
              ylab = "Residuals", 
              main = paste("Residuals by", pred, "\n(", model_name, ")", sep=" "),
              col = "lightblue")
      abline(h = 0, col = "red", lwd = 2)
    }
  }
  
  # Reset plotting area to default
  par(mfrow = c(1, 1))
}

# Plot diagnostics for all models
plot_diagnostics(model_no_transform, "No Transformation")
plot_diagnostics(model_sqrt, "Square Root Transformation")
plot_diagnostics(model_log, "Logarithmic Transformation")

# ------------------------
# 7. Refining the Models by Removing Insignificant Predictors
# ------------------------

# Corrected refine_model function to exclude Intercept and keep entire categorical variables
refine_model <- function(model, model_name) {
  summary_model <- summary(model)
  
  # Identify predictors (excluding intercept)
  predictors <- names(coef(model))
  predictors <- predictors[predictors != "(Intercept)"]
  
  # Initialize list to hold insignificant numeric predictors
  insignificant_numeric_vars <- c()
  
  # Iterate through predictors to identify insignificant numeric variables
  for (pred in predictors) {
    # Check if the predictor is numeric
    if (is.numeric(nba_data_cleaned[[pred]])) {
      p_value <- coef(summary_model)[pred, "Pr(>|t|)"]
      if (is.na(p_value) || p_value > 0.05) {
        insignificant_numeric_vars <- c(insignificant_numeric_vars, pred)
      }
    }
    # If predictor is categorical (factor), do not remove it
  }
  
  if (length(insignificant_numeric_vars) > 0) {
    # Remove insignificant numeric predictors from the formula
    current_formula <- formula(model)
    current_predictors <- attr(terms(current_formula), "term.labels")
    predictors_refined <- setdiff(current_predictors, insignificant_numeric_vars)
    
    # Create a new formula with the refined predictors
    formula_refined <- as.formula(
      paste(
        as.character(current_formula)[2],
        "~",
        paste(predictors_refined, collapse = " + ")
      )
    )
    
    # Fit the refined model
    refined_model <- lm(formula_refined, data = nba_data_cleaned)
    
    cat("Refined Model (", model_name, ") by removing insignificant predictors: ", 
        paste(insignificant_numeric_vars, collapse = ", "), 
        "\n", sep="")
    
    return(refined_model)
  } else {
    cat("No insignificant numeric predictors to remove in model: ", model_name, "\n", sep="")
    return(model)
  }
}

# Refine all models
refined_model_no_transform <- refine_model(model_no_transform, "No Transformation")
refined_model_sqrt <- refine_model(model_sqrt, "Square Root Transformation")
refined_model_log <- refine_model(model_log, "Logarithmic Transformation")

# ------------------------
# 8. Diagnostic Assessment of Refined Models
# ------------------------

# Plot diagnostics for all refined models
plot_diagnostics(refined_model_no_transform, "Refined No Transformation")
plot_diagnostics(refined_model_sqrt, "Refined Square Root Transformation")
plot_diagnostics(refined_model_log, "Refined Logarithmic Transformation")

# ------------------------
# 9. Final Model Evaluation
# ------------------------

# Function to perform ANOVA and print summary for each model
evaluate_models <- function(models, model_names) {
  for (i in 1:length(models)) {
    cat("\n========================================\n")
    cat("Summary of", model_names[i], "Model:\n")
    print(summary(models[[i]]))
    
    cat("\nVariance Inflation Factors (VIF):\n")
    print(vif(models[[i]]))
    
    cat("\nBreusch-Pagan Test for Heteroscedasticity:\n")
    print(bptest(models[[i]]))
    
    cat("\nANOVA Results:\n")
    print(anova(models[[i]]))
    
    cat("========================================\n")
  }
}

# Collect refined models and their names
refined_models <- list(refined_model_no_transform, refined_model_sqrt, refined_model_log)
refined_model_names <- c("Refined No Transformation", "Refined Square Root Transformation", "Refined Logarithmic Transformation")

# Evaluate all refined models
evaluate_models(refined_models, refined_model_names)

# ------------------------
# 10. Visualization of Final Model Predictions
# ------------------------

# Add predictions from all models to the dataset
nba_data_cleaned$Predicted_PTS_NoTransform <- fitted(refined_model_no_transform)
nba_data_cleaned$Predicted_PTS_Sqrt <- (fitted(refined_model_sqrt))^2
nba_data_cleaned$Predicted_PTS_Log <- exp(fitted(refined_model_log)) - 1  # Subtracting 1 to adjust for the earlier addition

# Scatterplot of Actual vs Predicted PPG for All Models
ggplot(nba_data_cleaned) +
  geom_point(aes(x = PTS, y = Predicted_PTS_NoTransform, color = "No Transformation"), alpha = 0.6) +
  geom_point(aes(x = PTS, y = Predicted_PTS_Sqrt, color = "Square Root Transformation"), alpha = 0.6) +
  geom_point(aes(x = PTS, y = Predicted_PTS_Log, color = "Logarithmic Transformation"), alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  labs(title = "Actual vs Predicted Points Per Game (PPG)",
       x = "Actual PPG",
       y = "Predicted PPG",
       color = "Model") +
  theme_minimal()

# ------------------------
# 11. Saving Final Models and Outputs
# ------------------------

# Save the final models as RDS files
saveRDS(refined_model_no_transform, file = "final_nba_model_no_transform.rds")
saveRDS(refined_model_sqrt, file = "final_nba_model_sqrt.rds")
saveRDS(refined_model_log, file = "final_nba_model_log.rds")

# Save the dataset with all predictions
write.csv(nba_data_cleaned, "nba_final_with_predictions.csv", row.names = FALSE)

# ------------------------
# 12. Handling Influential Points (Optional)
# ------------------------

# Function to identify influential points using Cook's Distance
identify_influential_points <- function(model, model_name) {
  cooks_d <- cooks.distance(model)
  
  # Plot Cook's Distance
  plot(cooks_d, 
       main = paste("Cook's Distance for", model_name, "Model"),
       ylab = "Cook's Distance",
       type = "h",
       col = "orange")
  abline(h = 4/(nrow(nba_data_cleaned)-length(model$coefficients)-2), 
         col = "red", lty = 2)
  
  # Identify points with high Cook's Distance
  influential_points <- which(cooks_d > (4/(nrow(nba_data_cleaned)-length(model$coefficients)-2)))
  cat("Number of influential points in", model_name, "Model:", length(influential_points), "\n")
  
  return(influential_points)
}

# Identify influential points for all refined models
influential_no_transform <- identify_influential_points(refined_model_no_transform, "Refined No Transformation")
influential_sqrt <- identify_influential_points(refined_model_sqrt, "Refined Square Root Transformation")
influential_log <- identify_influential_points(refined_model_log, "Refined Logarithmic Transformation")

# ------------------------
# 13. Final Notes
# ------------------------

# Ensure that all steps are executed sequentially.
# Any modifications to the data or model should be carefully assessed.