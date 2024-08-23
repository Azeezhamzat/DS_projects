# Load the trees dataset
data(trees)

# Inspect the structure of the dataset
str(trees)

# View the first few rows of the dataset
head(trees)

# Summary statistics for the dataset
summary(trees)

# Check for missing values
colSums(is.na(trees))


# Descriptive statistics
library(dplyr)

trees_stats <- trees %>%
  summarise(
    mean_Girth = mean(Girth),
    median_Girth = median(Girth),
    sd_Girth = sd(Girth),
    min_Girth = min(Girth),
    max_Girth = max(Girth),
    mean_Height = mean(Height),
    median_Height = median(Height),
    sd_Height = sd(Height),
    min_Height = min(Height),
    max_Height = max(Height),
    mean_Volume = mean(Volume),
    median_Volume = median(Volume),
    sd_Volume = sd(Volume),
    min_Volume = min(Volume),
    max_Volume = max(Volume)
  )

print(trees_stats)

# Pairs plot
pairs(trees, main = "Pairwise Scatter Plots of Trees Dataset")

# Correlation matrix
cor_matrix <- cor(trees)
print(cor_matrix)

library(ggplot2)

# Histogram and density plots for Girth
ggplot(trees, aes(x = Girth)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Distribution of Girth", x = "Girth", y = "Density")

# Histogram and density plots for Height
ggplot(trees, aes(x = Height)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, fill = "green", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Distribution of Height", x = "Height", y = "Density")

# Histogram and density plots for Volume
ggplot(trees, aes(x = Volume)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, fill = "orange", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Distribution of Volume", x = "Volume", y = "Density")


library(scatterplot3d)

# 3D Scatter Plot
scatterplot3d(trees$Girth, trees$Height, trees$Volume, 
              pch = 16, color = "blue", angle = 45, 
              main = "3D Scatter Plot of Girth, Height, and Volume",
              xlab = "Girth", ylab = "Height", zlab = "Volume")


# Heatmap of correlation matrix
library(reshape2)

cor_data <- melt(cor_matrix)
ggplot(cor_data, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() + 
  labs(title = "Correlation Heatmap", x = "Variable", y = "Variable") +
  coord_fixed()


# Linear model
lm_model <- lm(Volume ~ Girth + Height, data = trees)
summary(lm_model)

# Residual diagnostics
par(mfrow = c(2, 2))
plot(lm_model)



