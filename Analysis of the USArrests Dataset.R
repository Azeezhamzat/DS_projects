# Load the USArrests dataset
data(USArrests)

# Inspect the structure of the dataset
str(USArrests)

# View the first few rows of the dataset
head(USArrests)

# Summary statistics for the dataset
summary(USArrests)

# Check for missing values
colSums(is.na(USArrests))

# Descriptive statistics
library(dplyr)

usarrests_stats <- USArrests %>%
  summarise(
    mean_Murder = mean(Murder),
    median_Murder = median(Murder),
    sd_Murder = sd(Murder),
    min_Murder = min(Murder),
    max_Murder = max(Murder),
    mean_Assault = mean(Assault),
    median_Assault = median(Assault),
    sd_Assault = sd(Assault),
    min_Assault = min(Assault),
    max_Assault = max(Assault),
    mean_UrbanPop = mean(UrbanPop),
    median_UrbanPop = median(UrbanPop),
    sd_UrbanPop = sd(UrbanPop),
    min_UrbanPop = min(UrbanPop),
    max_UrbanPop = max(UrbanPop),
    mean_Rape = mean(Rape),
    median_Rape = median(Rape),
    sd_Rape = sd(Rape),
    min_Rape = min(Rape),
    max_Rape = max(Rape)
  )

print(usarrests_stats)

# Pairs plot
pairs(USArrests, main = "Pairwise Scatter Plots of USArrests Dataset")

# Correlation matrix
cor_matrix_usarrests <- cor(USArrests)
print(cor_matrix_usarrests)

library(ggplot2)

# Histogram and density plots for Murder
ggplot(USArrests, aes(x = Murder)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Distribution of Murder", x = "Murder", y = "Density")

# Histogram and density plots for Assault
ggplot(USArrests, aes(x = Assault)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, fill = "green", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Distribution of Assault", x = "Assault", y = "Density")

# Histogram and density plots for UrbanPop
ggplot(USArrests, aes(x = UrbanPop)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "orange", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Distribution of UrbanPop", x = "UrbanPop", y = "Density")

# Histogram and density plots for Rape
ggplot(USArrests, aes(x = Rape)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "purple", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Distribution of Rape", x = "Rape", y = "Density")


library(scatterplot3d)

# 3D Scatter Plot for Murder, Assault, and Rape
scatterplot3d(USArrests$Murder, USArrests$Assault, USArrests$Rape, 
              pch = 16, color = "blue", angle = 45, 
              main = "3D Scatter Plot of Murder, Assault, and Rape",
              xlab = "Murder", ylab = "Assault", zlab = "Rape")

# Heatmap of correlation matrix
library(reshape2)

cor_data_usarrests <- melt(cor_matrix_usarrests)
ggplot(cor_data_usarrests, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() + 
  labs(title = "Correlation Heatmap", x = "Variable", y = "Variable") +
  coord_fixed()

# Linear model
lm_model_usarrests <- lm(Rape ~ Murder + Assault + UrbanPop, data = USArrests)
summary(lm_model_usarrests)

# Residual diagnostics
par(mfrow = c(2, 2))
plot(lm_model_usarrests)


