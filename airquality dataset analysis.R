# We will work on the airquality dataset, which contains daily air quality measurements in New York from May to September 1973. This dataset is particularly relevant to environmental sustainability, as it includes measurements like ozone levels, wind speed, temperature, and solar radiation.
# 
# Here’s the plan for this dataset:
#   Dataset Overview
# 
# Variables:
#   Ozone: Ozone concentration (ppb)
# Solar.R: Solar Radiation (lang)
# Wind: Wind Speed (mph)
# Temp: Temperature (degrees F)
# Month: Month (numeric, May = 5, ..., September = 9)
# Day: Day of the month

# Load the dataset
data(airquality)

# Check the structure of the dataset
str(airquality)

# Preview the first few rows
head(airquality)

# Summary statistics
summary(airquality)


# We'll check for missing values in the dataset and handle them appropriately (e.g., imputation or removal).
# Checking for missing values
colSums(is.na(airquality))

# Visualizing missing data
install.packages("VIM")
library(VIM)
aggr(airquality, col = c('navyblue', 'red'), numbers = TRUE, sortVars = TRUE, labels = names(airquality), cex.axis = 0.7, gap = 3, ylab = c("Missing Data", "Pattern"))

# Example of mean imputation
airquality$Ozone[is.na(airquality$Ozone)] <- mean(airquality$Ozone, na.rm = TRUE)
airquality$Solar.R[is.na(airquality$Solar.R)] <- mean(airquality$Solar.R, na.rm = TRUE)

# Descriptive statistics
summary(airquality)

# Additional statistics: variance, skewness, and kurtosis
library(e1071)

var(airquality$Ozone, na.rm = TRUE)
skewness(airquality$Ozone, na.rm = TRUE)
kurtosis(airquality$Ozone, na.rm = TRUE)

# Histogram and density plot for Ozone levels
ggplot(airquality, aes(x = Ozone)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(alpha = 0.3, fill = "red") +
  labs(title = "Distribution of Ozone Levels")

# Boxplot for temperature by month
ggplot(airquality, aes(x = as.factor(Month), y = Temp)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Temperature by Month", x = "Month", y = "Temperature")


# Time series plot of Ozone levels over time
ggplot(airquality, aes(x = as.Date(paste(1973, Month, Day, sep = "-")), y = Ozone)) +
  geom_line(color = "blue") +
  labs(title = "Ozone Levels Over Time", x = "Date", y = "Ozone (ppb)")


# Correlation matrix and heatmap
cor_matrix <- cor(airquality[, -c(5, 6)], use = "complete.obs")
corrplot(cor_matrix, method = "circle", title = "Correlation Heatmap")

# Pair plot for airquality dataset
ggpairs(airquality[, -c(5, 6)], title = "Pairwise Relationships in Airquality Data")

# Modeling
set.seed(123)
train_indices <- sample(1:nrow(airquality), 0.7 * nrow(airquality))
train_data <- airquality[train_indices, ]
test_data <- airquality[-train_indices, ]

# Building the linear regression model
lm_model <- lm(Ozone ~ Solar.R + Wind + Temp, data = train_data)

# Summary of the model
summary(lm_model)

# Predictions on test data
predictions <- predict(lm_model, test_data)

# Calculate RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((predictions - test_data$Ozone)^2))
rmse

# Plot actual vs predicted values
ggplot(data = test_data, aes(x = Ozone, y = predictions)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Actual vs Predicted Ozone Levels")

# Polynomial Regression:
# Explore polynomial regression to capture non-linear relationships.
poly_model <- lm(Ozone ~ poly(Temp, 2) + Wind + Solar.R, data = train_data)
summary(poly_model)

# Include interaction terms to model the combined effect of features.
interaction_model <- lm(Ozone ~ Temp * Wind + Solar.R, data = train_data)
summary(interaction_model)

# Try a more advanced machine learning model, like Random Forest, to capture complex patterns.
library(randomForest)
rf_model <- randomForest(Ozone ~ ., data = train_data)

# For models like Random Forest, examine feature importance to understand which variables contribute the most to predicting Ozone levels.
importance(rf_model)
varImpPlot(rf_model)

