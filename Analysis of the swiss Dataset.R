# Load the swiss dataset
data(swiss)

# Inspect the structure of the dataset
str(swiss)

# View the first few rows of the dataset
head(swiss)

# Summary statistics for the dataset
summary(swiss)

# Check for missing values
colSums(is.na(swiss))


# Descriptive statistics
library(dplyr)

swiss_stats <- swiss %>%
  summarise(
    mean_Fertility = mean(Fertility),
    median_Fertility = median(Fertility),
    sd_Fertility = sd(Fertility),
    min_Fertility = min(Fertility),
    max_Fertility = max(Fertility),
    mean_Agriculture = mean(Agriculture),
    median_Agriculture = median(Agriculture),
    sd_Agriculture = sd(Agriculture),
    min_Agriculture = min(Agriculture),
    max_Agriculture = max(Agriculture),
    mean_Examination = mean(Examination),
    median_Examination = median(Examination),
    sd_Examination = sd(Examination),
    min_Examination = min(Examination),
    max_Examination = max(Examination),
    mean_Education = mean(Education),
    median_Education = median(Education),
    sd_Education = sd(Education),
    min_Education = min(Education),
    max_Education = max(Education),
    mean_ Catholic = mean(Catholic),
    median_Catholic = median(Catholic),
    sd_Catholic = sd(Catholic),
    min_Catholic = min(Catholic),
    max_Catholic = max(Catholic)
  )

print(swiss_stats)



# Pairs plot
pairs(swiss, main = "Pairwise Scatter Plots of Swiss Dataset")

# Correlation matrix
cor_matrix_swiss <- cor(swiss)
print(cor_matrix_swiss)

library(ggplot2)

# Histogram and density plots for Fertility
ggplot(swiss, aes(x = Fertility)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Distribution of Fertility", x = "Fertility", y = "Density")

# Histogram and density plots for Agriculture
ggplot(swiss, aes(x = Agriculture)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, fill = "green", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Distribution of Agriculture", x = "Agriculture", y = "Density")

# Histogram and density plots for Examination
ggplot(swiss, aes(x = Examination)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "orange", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Distribution of Examination", x = "Examination", y = "Density")

# Histogram and density plots for Education
ggplot(swiss, aes(x = Education)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, fill = "purple", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Distribution of Education", x = "Education", y = "Density")

# Histogram and density plots for Catholic
ggplot(swiss, aes(x = Catholic)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "brown", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Distribution of Catholic", x = "Catholic", y = "Density")

library(scatterplot3d)

# 3D Scatter Plot for Fertility, Agriculture, and Education
scatterplot3d(swiss$Fertility, swiss$Agriculture, swiss$Education, 
              pch = 16, color = "blue", angle = 45, 
              main = "3D Scatter Plot of Fertility, Agriculture, and Education",
              xlab = "Fertility", ylab = "Agriculture", zlab = "Education")

# Heatmap of correlation matrix
library(reshape2)

cor_data_swiss <- melt(cor_matrix_swiss)
ggplot(cor_data_swiss, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() + 
  labs(title = "Correlation Heatmap", x = "Variable", y = "Variable") +
  coord_fixed()



# Linear model
lm_model_swiss <- lm(Fertility ~ Agriculture + Examination + Education + Catholic, data = swiss)
summary(lm_model_swiss)


# Residual diagnostics
par(mfrow = c(2, 2))
plot(lm_model_swiss)


