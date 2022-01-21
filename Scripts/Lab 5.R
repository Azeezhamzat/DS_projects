# Lab 5. Categorical predictors

# Please continue in the same script you used to follow the lecture.

# Categorical Predictors with More Than Two Levels

# Go back to the 'senses' tibble, check the modalities:

unique(senses$Modality)

# Fit linear model with this five-level predictor:

sense_all <- lm(Val ~ Modality, data = senses)

# Look at estimates:

tidy(sense_all) %>% select(term:estimate) %>%
  mutate(estimate = round(estimate, 2))

# Create tibble to get predictions for:

sense_preds <- tibble(Modality =
                        sort(unique(senses$Modality)))

# Check:

sense_preds

# Append the fit:

sense_preds$fit <- round(predict(sense_all, sense_preds), 2)

# Check:

sense_preds

# Now, Let's check the residuals. Let's use the sense_all model to learn something more about residuals and the regression assumptions that relate to the residuals.

par(mfrow = c(1, 3))
# Plot 1, histogram:
hist(residuals(sense_all), col = 'skyblue2')
# Plot 2, Q-Q plot:
qqnorm(residuals(sense_all))
qqline(residuals(sense_all))
# Plot 3, residual plot:
plot(fitted(sense_all), residuals(sense_all))



