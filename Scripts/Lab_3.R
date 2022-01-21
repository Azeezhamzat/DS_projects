# --------------------------------------------------------
# 3. A simple linear model in R:

# Continue from where you left off in the reading, using the same dataframe.

# --------------------------------------------------------
# 3.1. Linear models with tidyverse functions

# Put data into tibble:

mydf <- tibble(x, y)

# Re-fit model:

xmdl <- lm(y ~ x, data = mydf)

# Load broom package for tidy linear model output:

library(broom)

# Summary:

tidy(xmdl)

# Extract coefficient estimates:

tidy(xmdl)$estimate

# Check overall model performance:

glance(xmdl)

# Make a ggplot of this data:

mydf %>% ggplot(aes(x = x, y = y)) +
  geom_point() + geom_smooth(method = 'lm') +
  theme_minimal()



# --------------------------------------------------------
# 3.2. Model formula notation: Intercept placeholders

# These two formulas produce the same outcome:

xmdl <- lm(y ~ x, data = mydf)
xmdl <- lm(y ~ 1 + x, data = mydf)

# The '1' (one) acts as intercept placeholder...
# ... so the second formula is more explicit.

# Fitting an intercept-only model:

xmdl_null <- lm(y ~ 1, data = mydf)

# Look at the intercept estimate:

coef(xmdl_null)

# Compare against the mean:

mean(y)   # same



