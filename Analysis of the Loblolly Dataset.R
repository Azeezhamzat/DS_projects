# Load the Loblolly dataset
data(Loblolly)

# Inspect the structure of the dataset
str(Loblolly)

# View the first few rows of the dataset
head(Loblolly)

# Summary statistics for the dataset
summary(Loblolly)

# Check for missing values
colSums(is.na(Loblolly))

# Descriptive statistics
library(dplyr)

loblolly_stats <- Loblolly %>%
  group_by(age) %>%
  summarise(
    mean_height = mean(height),
    median_height = median(height),
    sd_height = sd(height),
    min_height = min(height),
    max_height = max(height)
  )

print(loblolly_stats)


# Scatter plot of Age vs. Height
ggplot(Loblolly, aes(x = age, y = height)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Scatter Plot of Age vs. Height", x = "Age (Years)", y = "Height (Feet)")

# Histogram and density plot for Height
ggplot(Loblolly, aes(x = height)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "green", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Distribution of Tree Height", x = "Height (Feet)", y = "Density")


# Box plot of Height by Age
ggplot(Loblolly, aes(x = as.factor(age), y = height)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Box Plot of Tree Height by Age", x = "Age (Years)", y = "Height (Feet)")


# Fit linear model
lm_model_loblolly <- lm(height ~ age, data = Loblolly)

# Plot with regression line
ggplot(Loblolly, aes(x = age, y = height)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Regression Line for Age vs. Height", x = "Age (Years)", y = "Height (Feet)") +
  theme_minimal()

# Display model summary
summary(lm_model_loblolly)


# Residual diagnostics
par(mfrow = c(2, 2))
plot(lm_model_loblolly)



