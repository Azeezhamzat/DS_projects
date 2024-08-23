# The PlantGrowth dataset in R is a classic dataset that contains the results of an experiment where the effect of different treatments on the growth of plants was studied. It has 30 observations across two variables:

# weight: The weight of the plants.
# group: The group/treatment to which the plant belongs. There are three groups:
#   ctrl: Control group.
# trt1: Treatment 1.
# trt2: Treatment 2.


# Load the PlantGrowth dataset
data(PlantGrowth)

# Inspect the structure of the dataset
str(PlantGrowth)

# View the first few rows of the dataset
head(PlantGrowth)

# Summary statistics for the dataset
summary(PlantGrowth)

# Check for missing values
colSums(is.na(PlantGrowth))


# Descriptive statistics by group
library(dplyr)

group_stats <- PlantGrowth %>%
  group_by(group) %>%
  summarise(
    count = n(),
    mean = mean(weight),
    median = median(weight),
    sd = sd(weight),
    min = min(weight),
    max = max(weight)
  )

print(group_stats)


library(ggplot2)

# Histogram of plant weights by group
ggplot(PlantGrowth, aes(x = weight, fill = group)) +
  geom_histogram(binwidth = 0.2, color = "black", alpha = 0.7) +
  facet_wrap(~ group) +
  labs(title = "Distribution of Plant Weights by Group", x = "Weight", y = "Frequency")

# Boxplot of plant weights by group
ggplot(PlantGrowth, aes(x = group, y = weight, fill = group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Plant Weights by Group", x = "Group", y = "Weight")

# Interpretation:
# 
# Median: The median plant weight is highest in trt1 and lowest in the control group (ctrl), indicating a potential effect of the treatment on plant growth.
# IQR: The interquartile ranges are similar across all groups, suggesting consistent variability within each group.
# Outliers: There are a few outliers in the trt1 group, which might indicate individual plants that responded differently to the treatment.
# Conclusion: The boxplot suggests that trt1 may have increased plant growth compared to the control, but further statistical testing (like ANOVA) is necessary to confirm this.


# Bar plot with error bars
ggplot(PlantGrowth, aes(x = group, y = weight, fill = group)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge", color = "black") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Mean Plant Weights by Group with 95% Confidence Intervals", x = "Group", y = "Mean Weight")


# One-way ANOVA
anova_result <- aov(weight ~ group, data = PlantGrowth)
summary(anova_result)


# Tukey's HSD test
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)


# Check for normality using Shapiro-Wilk test
shapiro_test <- shapiro.test(residuals(anova_result))
print(shapiro_test)

# Check for homogeneity of variance using Levene's Test
library(car)
levene_test <- leveneTest(weight ~ group, data = PlantGrowth)
print(levene_test)


# Violin plot of plant weights by group
ggplot(PlantGrowth, aes(x = group, y = weight, fill = group)) +
  geom_violin(trim = FALSE) +
  labs(title = "Violin Plot of Plant Weights by Group", x = "Group", y = "Weight")


# Plot of confidence intervals for mean differences
plot(tukey_result, las = 1)


# Linear model
lm_model <- lm(weight ~ group, data = PlantGrowth)
summary(lm_model)


# Residual diagnostics
par(mfrow = c(2, 2))
plot(lm_model)


