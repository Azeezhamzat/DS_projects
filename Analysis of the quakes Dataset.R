# Load the quakes dataset
data(quakes)

# Inspect the structure of the dataset
str(quakes)

# View the first few rows of the dataset
head(quakes)

# Summary statistics for the dataset
summary(quakes)

# Check for missing values
colSums(is.na(quakes))

# Descriptive statistics
library(dplyr)

quakes_stats <- quakes %>%
  summarise(
    mean_lat = mean(lat),
    median_lat = median(lat),
    sd_lat = sd(lat),
    min_lat = min(lat),
    max_lat = max(lat),
    mean_long = mean(long),
    median_long = median(long),
    sd_long = sd(long),
    min_long = min(long),
    max_long = max(long),
    mean_depth = mean(depth),
    median_depth = median(depth),
    sd_depth = sd(depth),
    min_depth = min(depth),
    max_depth = max(depth),
    mean_magnitude = mean(mag),
    median_magnitude = median(mag),
    sd_magnitude = sd(mag),
    min_magnitude = min(mag),
    max_magnitude = max(mag)
  )

print(quakes_stats)


# Pairs plot
pairs(quakes, main = "Pairwise Scatter Plots of Quakes Dataset")

# Correlation matrix
cor_matrix_quakes <- cor(quakes)
print(cor_matrix_quakes)

library(ggplot2)

# Histogram and density plots for Latitude
ggplot(quakes, aes(x = lat)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "blue", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Distribution of Latitude", x = "Latitude", y = "Density")

# Histogram and density plots for Longitude
ggplot(quakes, aes(x = long)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "green", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Distribution of Longitude", x = "Longitude", y = "Density")

# Histogram and density plots for Depth
ggplot(quakes, aes(x = depth)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, fill = "orange", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Distribution of Depth", x = "Depth", y = "Density")

# Histogram and density plots for Magnitude
ggplot(quakes, aes(x = mag)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "purple", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Distribution of Magnitude", x = "Magnitude", y = "Density")

library(scatterplot3d)

# 3D Scatter Plot for Latitude, Longitude, and Depth
scatterplot3d(quakes$lat, quakes$long, quakes$depth, 
              pch = 16, color = "blue", angle = 45, 
              main = "3D Scatter Plot of Latitude, Longitude, and Depth",
              xlab = "Latitude", ylab = "Longitude", zlab = "Depth")

# Heatmap of correlation matrix
library(reshape2)

cor_data_quakes <- melt(cor_matrix_quakes)
ggplot(cor_data_quakes, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() + 
  labs(title = "Correlation Heatmap", x = "Variable", y = "Variable") +
  coord_fixed()

# Linear model
lm_model_quakes <- lm(mag ~ depth, data = quakes)
summary(lm_model_quakes)

# Residual diagnostics
par(mfrow = c(2, 2))
plot(lm_model_quakes)

