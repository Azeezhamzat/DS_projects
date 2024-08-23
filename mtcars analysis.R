# we will analyze the mtcars dataset, which contains data on various car models. It includes variables such as miles per gallon (mpg), engine displacement, horsepower, weight, and others.
# 
# This dataset is often used to explore the relationships between various features of cars and performance metrics like fuel efficiency.
# Dataset Overview
# 
# Variables:
#   mpg: Miles per gallon (fuel efficiency)
# cyl: Number of cylinders
# disp: Engine displacement (in cubic inches)
# hp: Horsepower
# drat: Rear axle ratio
# wt: Weight of the car (1000 lbs)
# qsec: 1/4 mile time (time to complete a quarter-mile in seconds)
# vs: Engine shape (0 = V-shaped, 1 = straight)
# am: Transmission type (0 = automatic, 1 = manual)
# gear: Number of forward gears
# carb: Number of carburetors
# 
# Step 1: Load and Inspect the Data
# 
# We’ll begin by loading the mtcars dataset and performing basic inspection tasks.
# 


# Load the dataset
data(mtcars)

# Check the structure of the dataset
str(mtcars)

# Preview the first few rows
head(mtcars)

# Summary statistics
summary(mtcars)

# Step 2: Exploratory Data Analysis (EDA)
# 1. Checking for Missing Values

# We'll check for any missing values in the dataset.


# Checking for missing values
colSums(is.na(mtcars))

# Since the mtcars dataset does not contain any missing values, we can proceed directly to EDA.
# 2. Descriptive Statistics
# 
# We’ll calculate descriptive statistics for all the variables to understand their distribution.



# Descriptive statistics
summary(mtcars)

# Additional statistics: variance, skewness, and kurtosis
library(e1071)

var(mtcars$mpg)
skewness(mtcars$mpg)
kurtosis(mtcars$mpg)

# 3. Univariate and Bivariate Analysis
# 
# We will visualize the distribution of key variables like mpg, hp, and wt and explore relationships between them.


library(ggplot2)

# Histogram and density plot for mpg
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(alpha = 0.3, fill = "red") +
  labs(title = "Distribution of Miles Per Gallon (mpg)")

# Scatter plot of mpg vs. weight (wt)
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "MPG vs. Weight", x = "Weight (1000 lbs)", y = "Miles per Gallon")

# 4. Correlation Analysis
# 
# We will calculate and visualize the correlation between variables to identify relationships.


library(corrplot)

# Correlation matrix and heatmap
cor_matrix <- cor(mtcars)
corrplot(cor_matrix, method = "circle", title = "Correlation Heatmap")

    Observations: We can identify strong correlations between variables like wt (weight) and mpg, hp (horsepower) and mpg, and so on. This will guide our feature selection for modeling.

# Step 3: Advanced Visualizations
# 1. Pair Plots
# 
# Visualize relationships between all numerical variables with pair plots.


library(GGally)

# Pair plot for the mtcars dataset
ggpairs(mtcars, title = "Pairwise Relationships in MTCARS Data")

# 2. Boxplots
# 
# We’ll visualize how categorical variables like cyl (cylinders) and am (transmission type) affect mpg.


# Boxplot of mpg by number of cylinders
ggplot(mtcars, aes(x = as.factor(cyl), y = mpg)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "MPG by Number of Cylinders", x = "Number of Cylinders", y = "Miles per Gallon")

# Boxplot of mpg by transmission type
ggplot(mtcars, aes(x = as.factor(am), y = mpg)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "MPG by Transmission Type", x = "Transmission Type (0 = Automatic, 1 = Manual)", y = "Miles per Gallon")

# 3. Interaction Plots
# 
# Explore interactions between different variables, such as the interaction between weight (wt) and horsepower (hp) and their effect on mpg.


# Interaction plot for weight and horsepower on mpg
ggplot(mtcars, aes(x = wt, y = mpg, color = as.factor(hp))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Interaction of Weight and Horsepower on MPG", x = "Weight (1000 lbs)", y = "Miles per Gallon")

# Step 4: Modeling
# 
# We’ll explore various regression models to predict mpg based on other features. This will include linear regression, multiple regression, and more advanced models like Random Forest.
# 1. Linear Regression Model
# 
# We’ll start with a simple linear regression model to predict mpg based on wt.


# Simple linear regression: mpg vs wt
lm_model <- lm(mpg ~ wt, data = mtcars)

# Summary of the model
summary(lm_model)

# 2. Multiple Linear Regression
# 
# Next, we’ll extend the model to include multiple predictors, such as wt, hp, and cyl.


# Multiple linear regression
multi_lm_model <- lm(mpg ~ wt + hp + cyl, data = mtcars)

# Summary of the model
summary(multi_lm_model)

# 3. Model Evaluation
# 
# Evaluate the performance of the model using metrics like RMSE (Root Mean Squared Error) and R-squared.


# Predictions and RMSE
predictions <- predict(multi_lm_model, mtcars)
rmse <- sqrt(mean((predictions - mtcars$mpg)^2))
rmse

# R-squared
rsquared <- summary(multi_lm_model)$r.squared
rsquared

# 4. Random Forest Model
# 
# We’ll try a more advanced model like Random Forest, which can handle non-linear relationships and interactions between variables.


library(randomForest)

# Random forest model to predict mpg
rf_model <- randomForest(mpg ~ ., data = mtcars)

# Summary of the model
print(rf_model)

# 5. Cross-Validation and Hyperparameter Tuning
# 
# To ensure the model’s robustness, we’ll apply k-fold cross-validation and hyperparameter tuning.


library(caret)

# 10-fold cross-validation for Random Forest
train_control <- trainControl(method = "cv", number = 10)
rf_cv_model <- train(mpg ~ ., data = mtcars, method = "rf", trControl = train_control)

# Best model
rf_cv_model$bestTune

# Step 5: Model Interpretability
# 
#     Feature Importance:
#     For models like Random Forest, we’ll examine feature importance to understand which variables are most influential in predicting mpg.

    
varImpPlot(rf_model)

# Partial Dependence Plots:
# We can also explore partial dependence plots to visualize how changes in individual features affect predictions.


library(pdp)

# Partial dependence plot for weight
pdp_wt <- partial(rf_model, pred.var = "wt")
plotPartial(pdp_wt)
