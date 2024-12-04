
library(MASS)     
library(ggplot2)  
library(car)       
library(dplyr)     


nba_data_raw <- read.csv(file = "nba_final_original.csv", header = TRUE)


head(nba_data_raw)


nba_data_cleaned <- nba_data_raw %>%
  mutate_all(~ ifelse(is.na(.), 0, .))


nba_data_cleaned$Position <- factor(nba_data_cleaned$Pos1, 
                                    levels = c("PG", "SG", "SF", "PF", "C"), 
                                    labels = c("PG", "SG", "SF", "PF", "C"))


nba_data_cleaned$Pos1 <- NULL

colnames(nba_data_cleaned)[colnames(nba_data_cleaned) == "eFG."] <- "EffectiveFG"


nba_data_cleaned$EffectiveFG <- as.numeric(as.character(nba_data_cleaned$EffectiveFG))
nba_data_cleaned$Salary <- as.numeric(as.character(nba_data_cleaned$Salary))


nba_data_cleaned <- nba_data_cleaned[, c("PTS", "Age", "Position", "EffectiveFG", "FGA", "Salary")]

nba_data_cleaned$sqrt_PPG <- sqrt(nba_data_cleaned$PTS)

str(nba_data_cleaned)
summary(nba_data_cleaned)


write.csv(nba_data_cleaned, "nba_final_cleaned.csv", row.names = FALSE)

summary_stats <- summary(nba_data_cleaned)
print(summary_stats)

sd_age <- sd(nba_data_cleaned$Age)
sd_effectivefg <- sd(nba_data_cleaned$EffectiveFG)
sd_fga <- sd(nba_data_cleaned$FGA)
sd_salary <- sd(nba_data_cleaned$Salary)


cat("Standard Deviation of Age:", round(sd_age, 2), "\n")
cat("Standard Deviation of EffectiveFG:", round(sd_effectivefg, 2), "\n")
cat("Standard Deviation of FGA:", round(sd_fga, 2), "\n")
cat("Standard Deviation of Salary:", round(sd_salary, 2), "\n")


par(mfrow = c(2, 3))


boxplot(nba_data_cleaned$PTS, 
        xlab = "Points per Game", 
        main = "Boxplot of PPG", 
        horizontal = TRUE)


boxplot(nba_data_cleaned$Age, 
        xlab = "Age", 
        main = "Boxplot of Age", 
        horizontal = TRUE)


boxplot(nba_data_cleaned$Position, 
        xlab = "Position", 
        main = "Boxplot of Position", 
        horizontal = TRUE)


boxplot(nba_data_cleaned$EffectiveFG, 
        xlab = "Effective Field Goal", 
        main = "Boxplot of EFG", 
        horizontal = TRUE)

boxplot(nba_data_cleaned$FGA, 
        xlab = "Field Goal Attempts", 
        main = "Boxplot of FGA", 
        horizontal = TRUE)


boxplot(nba_data_cleaned$Salary, 
        xlab = "Salary", 
        main = "Boxplot of Salary", 
        horizontal = TRUE)


par(mfrow = c(1, 1))


pairs(nba_data_cleaned[, c("Age", "EffectiveFG", "FGA", "Salary")], 
      main = "Pairwise Scatterplots of Predictors",
      pch = 19, 
      col = "blue")

prelim_lm_model <- lm(sqrt_PPG ~ Age + Position + EffectiveFG + FGA + Salary, data = nba_data_cleaned)


summary(prelim_lm_model)


vif_values_prelim <- vif(prelim_lm_model)
print(vif_values_prelim)


residuals_model_prelim <- residuals(prelim_lm_model)
fitted_model_prelim <- fitted(prelim_lm_model)


par(mfrow = c(1, 2))
plot(fitted_model_prelim, residuals_model_prelim, 
     main = "Residuals vs Fitted Values (Prelim Model)",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 19, col = "darkgreen")
abline(h = 0, col = "red", lwd = 2)

qqnorm(residuals_model_prelim, main = "Normal Q-Q Plot (Prelim Model)", pch = 19, col = "purple")
qqline(residuals_model_prelim, col = "red", lwd = 2)
par(mfrow = c(1, 1))

par(mfrow = c(2, 3))

plot(nba_data_cleaned$Age, residuals_model_prelim, 
     xlab = "Age", 
     ylab = "Residuals", 
     main = "Residuals vs Age",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

boxplot(residuals_model_prelim ~ nba_data_cleaned$Position, 
        xlab = "Position", 
        ylab = "Residuals", 
        main = "Residuals by Position",
        col = "lightblue")
abline(h = 0, col = "red", lwd = 2)

plot(nba_data_cleaned$EffectiveFG, residuals_model_prelim, 
     xlab = "Effective Field Goal Percentage (eFG%)", 
     ylab = "Residuals", 
     main = "Residuals vs EFG%",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

plot(nba_data_cleaned$FGA, residuals_model_prelim, 
     xlab = "Field Goals Attempted (FGA)", 
     ylab = "Residuals", 
     main = "Residuals vs FGA",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

plot(nba_data_cleaned$Salary, residuals_model_prelim, 
     xlab = "Salary", 
     ylab = "Residuals", 
     main = "Residuals vs Salary",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

par(mfrow = c(1, 1))

refined_model <- lm(sqrt_PPG ~ Age + Position + EffectiveFG + FGA, data = nba_data_cleaned)

summary(refined_model)

vif_refined <- vif(refined_model)
print(vif_refined)

residuals_refined <- residuals(refined_model)
fitted_refined <- fitted(refined_model)

par(mfrow = c(1, 2))
plot(fitted_refined, residuals_refined, 
     main = "Residuals vs Fitted Values (Refined Model)",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 19, col = "darkgreen")
abline(h = 0, col = "red", lwd = 2)

qqnorm(residuals_refined, main = "Normal Q-Q Plot (Refined Model)", pch = 19, col = "purple")
qqline(residuals_refined, col = "red", lwd = 2)
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))

plot(nba_data_cleaned$Age, residuals_refined, 
     xlab = "Age", 
     ylab = "Residuals", 
     main = "Residuals vs Age",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

plot(nba_data_cleaned$EffectiveFG, residuals_refined, 
     xlab = "Effective Field Goal Percentage (eFG%)", 
     ylab = "Residuals", 
     main = "Residuals vs EFG%",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

plot(nba_data_cleaned$FGA, residuals_refined, 
     xlab = "Field Goals Attempted (FGA)", 
     ylab = "Residuals", 
     main = "Residuals vs FGA",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

boxplot(residuals_refined ~ nba_data_cleaned$Position, 
        xlab = "Position", 
        ylab = "Residuals", 
        main = "Residuals by Position",
        col = "lightblue")
abline(h = 0, col = "red", lwd = 2)

par(mfrow = c(1, 1))

anova_results <- anova(refined_model)
print(anova_results)

nba_data_cleaned$Predicted_sqrt_PPG <- fitted(refined_model)
nba_data_cleaned$Predicted_PPG <- (nba_data_cleaned$Predicted_sqrt_PPG)^2

ggplot(nba_data_cleaned, aes(x = PTS, y = Predicted_PPG)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Points Per Game (PPG)",
       x = "Actual PPG",
       y = "Predicted PPG") +
  theme_minimal()

saveRDS(refined_model, file = "final_nba_model.rds")

write.csv(nba_data_cleaned, "nba_final_with_predictions.csv", row.names = FALSE)
