# Analysis of CO2 dataset, which contains carbon dioxide uptake data from plants. 

# Dataset : CO2 (inbuilt)
# This dataset is valuable for environmental sustainability research, as it can help us understand how different conditions affect carbon dioxide absorption, an important factor in combating climate change.
# Dataset Overview
# 
# Variables:
#   Plant: The plant identifier (categorical)
# Type: The type of plant (Quebec or Mississippi)
# Treatment: Treatment applied to the plant (chilled or non-chilled)
# conc: CO2 concentration (ambient concentration in μmol/m^3)
# uptake: Carbon dioxide uptake rate (μmol/m^2 sec)
#

# Step 1: Load and Inspect the Data
data(CO2)

# Check the structure of the dataset
str(CO2)

# Preview the first few rows
head(CO2)

# Summary statistics
summary(CO2)

# Step 2: Exploratory Data Analysis (EDA)

# Checking for missing values
colSums(is.na(CO2))

# Descriptive statistics
summary(CO2)

# Additional statistics: variance, skewness, and kurtosis for uptake
library(e1071)

var(CO2$uptake)
skewness(CO2$uptake)
kurtosis(CO2$uptake)


### Univariate and Bivariate Analysis

# We will visualize the distribution of key variables like uptake and conc and explore relationships between them.

library(ggplot2)

# Histogram and density plot for uptake
ggplot(CO2, aes(x = uptake)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(alpha = 0.3, fill = "red") +
  labs(title = "Distribution of CO2 Uptake")

# Scatter plot of uptake vs. CO2 concentration (conc)
ggplot(CO2, aes(x = conc, y = uptake)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "CO2 Uptake vs. CO2 Concentration", x = "CO2 Concentration (μmol/m^3)", y = "CO2 Uptake (μmol/m^2 sec)")

#Grouped Analysis

#Explore how factors like Type (Quebec vs. Mississippi) and Treatment (chilled vs. non-chilled) affect CO2 uptake.

# Boxplot of uptake by plant type
ggplot(CO2, aes(x = Type, y = uptake)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "CO2 Uptake by Plant Type", x = "Plant Type", y = "CO2 Uptake (μmol/m^2 sec)")

# Boxplot of uptake by treatment
ggplot(CO2, aes(x = Treatment, y = uptake)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "CO2 Uptake by Treatment", x = "Treatment", y = "CO2 Uptake (μmol/m^2 sec)")


# Step 3: Advanced Visualizations
# 1. Interaction Plots

# We’ll explore how the interaction between factors like Type, Treatment, and conc affects CO2 uptake.

# Interaction plot of CO2 uptake by Type and conc
ggplot(CO2, aes(x = conc, y = uptake, color = Type)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Interaction of CO2 Concentration and Plant Type on CO2 Uptake", x = "CO2 Concentration", y = "CO2 Uptake")

# 2. Faceted Plots

# Faceted plots allow us to visualize data for different groups, such as different plant types or treatments.

# Faceted plot for CO2 uptake by plant type and treatment
ggplot(CO2, aes(x = conc, y = uptake)) +
  geom_point(size = 3, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  facet_grid(Type ~ Treatment) +
  labs(title = "CO2 Uptake by Type and Treatment", x = "CO2 Concentration", y = "CO2 Uptake")

# Step 4: Modeling

# We’ll explore regression models to predict uptake based on factors like conc, Type, and Treatment. This will include linear regression, interaction models, and more advanced models.
# 1. Linear Regression Model

# We’ll start with a simple linear regression model to predict uptake based on conc.

# Simple linear regression: uptake vs conc
lm_model <- lm(uptake ~ conc, data = CO2)

# Summary of the model
summary(lm_model)

# 
# 2. Multiple Linear Regression
# 
# Next, we’ll extend the model to include categorical variables like Type and Treatment.

# Multiple linear regression with interaction terms
multi_lm_model <- lm(uptake ~ conc * Type * Treatment, data = CO2)

# Summary of the model
summary(multi_lm_model)

# 3. Model Evaluation

# We’ll evaluate the model using metrics like RMSE and R-squared.

# Predictions and RMSE
predictions <- predict(multi_lm_model, CO2)
rmse <- sqrt(mean((predictions - CO2$uptake)^2))
rmse

# R-squared
rsquared <- summary(multi_lm_model)$r.squared
rsquared


# 4. Random Forest Model
# We can also try a Random Forest model, which can handle non-linear relationships and interactions between variables.
install.packages("randomForest")
library(randomForest)

# Random forest model to predict uptake
rf_model <- randomForest(uptake ~ ., data = CO2)

# Summary of the model
print(rf_model)


# Step 5: Model Interpretability

# Feature Importance:
  # For models like Random Forest, we’ll examine feature importance to understand which variables are most influential in predicting uptake.
varImpPlot(rf_model)

# 
# Partial Dependence Plots:
#   Visualize the effect of individual features on CO2 uptake.
install.packages("pdp")
library(pdp)

# Partial dependence plot for CO2 concentration (conc)
pdp_conc <- partial(rf_model, pred.var = "conc")
plotPartial(pdp_conc)

