# --------------------------------------------------------
# Lab 2 A.
###########################################################
# Adapted from Bodo Winter. Statistics for linguists. CH.2.
###########################################################
# Load in data:

icon <- read_csv('perry_winter_2017_iconicity.csv')
mod <- read_csv('lynott_connell_2009_modality.csv')

# Check iconicity tibble:

icon

# Reduce tibble to relevant columns via select():

icon <- select(icon, Word, POS, Iconicity)
icon

# Check range of the iconicity measure:

range(icon$Iconicity)

## Draw a histogram:

ggplot(icon, aes(x = Iconicity)) +
  geom_histogram(fill = 'peachpuff3') +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  theme_minimal()

# Check the contents of the mod tibble:

mod

# To display all columns:

mod %>% print(width = Inf)

# To display all rows:

mod %>% print(n = Inf)

# Take relevant subset:

mod <- select(mod, Word, DominantModality:Smell)

# Check:

mod

# Rename 'DominantModality' column to 'Modality':

mod <- rename(mod, Modality = DominantModality)

# Check random rows to get a feel for the tibble:

sample_n(mod, 4)

# Merge the 'mod' tibble into the 'icon' tibble:

both <- left_join(icon, mod)

# Take only the three major content classes:

both <- filter(both,
               POS %in% c('Adjective', 'Verb', 'Noun'))

# Make a boxplot of iconicity as a function of modality:

ggplot(both,
       aes(x = Modality, y = Iconicity, fill = Modality)) +
  geom_boxplot() + theme_minimal()

# Pipeline that excludes NAs:

both %>% filter(!is.na(Modality)) %>%
  ggplot(aes(x = Modality, y = Iconicity,
             fill = Modality)) +
  geom_boxplot() + theme_minimal()

# Let's count modalities:

both %>% count(Modality)

## Let's make a bar plot:

both %>% count(Modality) %>%
  filter(!is.na(Modality)) %>%
  ggplot(aes(x = Modality, y = n, fill = Modality)) +
  geom_bar(stat = 'identity') + theme_minimal()

# Same bar plot without creating an intervening tibble ...
# ... of counts by hand. This time, geom_bar() does ...
# ... the counting:

both %>% filter(!is.na(Modality)) %>%
  ggplot(aes(Modality, fill = Modality)) +
  geom_bar(stat = 'count') + theme_minimal()


# --------------------------------------------------------
# Descriptive statistics in R:

# Generate 50 random uniformly distributed numbers:

x <- runif(50)

# Check:

x

# Specify min and max:

x <- runif(50, min = 2, max = 6)

# Check:

head(x)

# Create a quick-and-dirty base R plot:

hist(x, col = 'steelblue')

# Generate random normally distributed numbers & plot 'em:

x <- rnorm(50)
hist(x, col = 'steelblue')
abline(v = mean(x), lty = 2, lwd = 2)

# Create random data with mean = 5 and SD = 2:

x <- rnorm(50, mean = 5, sd = 2)

# Check mean and SD:

mean(x)
sd(x)

# Check the percentiles:

quantile(x)

# Check the values that span the 68% interval:

quantile(x, 0.16)
quantile(x, 0.84)

# This should correspond to +/- 1 SD around the mean:

mean(x) - sd(x)
mean(x) + sd(x)

# The values of the 95% interval:

quantile(x, 0.025)
quantile(x, 0.975)

# This should correspond to +/- 2 SD around the mean:

mean(x) - 2 * sd(x)
mean(x) + 2 * sd(x)

# Execture repeatedly to get a feel for the normal:

hist(rnorm(n = 20))

#--------------------------#

# Load tidyverse and Warriner et al. (2013) data:

library(tidyverse)

war <- read_csv('warriner_2013_emotional_valence.csv')

# Check:

war

# Check valence measure range:

range(war$Val)

# Check the least and most positive wors:

filter(war, Val == min(Val) | Val == max(Val))

# Same thing, but more compact:

filter(war, Val %in% range(Val))

# Check tibble in ascending order:

arrange(war, Val)

# And descending order:

arrange(war, desc(Val))

# Mean and SD:

mean(war$Val)
sd(war$Val)

# 68% rule:

mean(war$Val) + sd(war$Val)
mean(war$Val) - sd(war$Val)

# Confirm:

quantile(war$Val, c(0.16, 0.84))

# Median:

median(war$Val)

# Which is the same as the 50th percentile:

quantile(war$Val, 0.5)



