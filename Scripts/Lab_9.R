# Lab 9. Inferential Statistics. Bodo Winter.

# Load tidyverse and broom package:

library(tidyverse)
library(broom)

# Another example: the absolute valence of taste and smell words

senses <- read_csv('winter_2016_senses_valence.csv')

# Create an absolute valence measure ('folded valence'):

senses <- mutate(senses,
                 Val_z = scale(Val),
                 AbsVal = abs(Val_z))

# Fit a model:

abs_mdl <- lm(AbsVal ~ Modality, data = senses)

# Look at overall modality effect:

anova(abs_mdl)

# Chemical senses vector:

chems <- c('Taste', 'Smell')

# Create a chemical senses vs. other variable:

senses <- mutate(senses,
                 ChemVsRest = ifelse(Modality %in% chems,
                                     'Chem', 'Other'))

# Quick sanity check of the new column:

with(senses, table(Modality, ChemVsRest))

# Perform linear model:

abs_mdl <- lm(AbsVal ~ ChemVsRest, data = senses)

# Check output:

tidy(abs_mdl)



# --------------------------------------------------------
# Communicating uncertainty around predictions: categorical data

# Get predictions:

newpreds <- tibble(Modality =
                     sort(unique(senses$Modality)))

# Check:

newpreds

# Append fitted values to tibble:

fits <- predict(senses_mdl, newpreds)

# Check:

fits

# Add standard errors of predictions:

SEs <- predict(senses_mdl, newpreds,
               se.fit = TRUE)$se.fit

# Check:

SEs

# Put this into a tibble to compute 95% CIs:

CI_tib <- tibble(fits, SEs)

CI_tib

# Compute CIs:

CI_tib <- mutate(CI_tib,
                 LB = fits - 1.96 * SEs, # lower bound
                 UB = fits + 1.96 * SEs) # upper bound

# Check:

CI_tib

# Comparison:

sense_preds <- predict(senses_mdl, newpreds, interval = 'confidence')

sense_preds

# Append modality labels:

sense_preds <- cbind(newpreds, sense_preds)

sense_preds

# Make a plot of the predictions:

sense_preds %>%
  ggplot(aes(x = Modality, y = fit)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr)) +
  theme_minimal()

# Extract ascending order:

sense_order <- arrange(sense_preds, fit)$Modality

# Set factor to this order:

sense_preds <- mutate(sense_preds,
                      Modality = factor(Modality,
                                        levels = sense_order))

# Better plot:

sense_preds %>%
  ggplot(aes(x = Modality, y = fit)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = lwr, ymax = upr),
                size = 1, width = 0.5) +
  ylab('Predicted emotional valence\n') +
  theme_minimal() +
  theme(axis.text.x =
          element_text(face = 'bold', size = 15),
        axis.text.y =
          element_text(face = 'bold', size = 15),
        axis.title =
          element_text(face = 'bold', size = 20))



# --------------------------------------------------------
# Communicating uncertainty for continuous data

# Load data:

ELP <- read_csv('ELP_frequency.csv')

# Log-transform:

ELP <- mutate(ELP,
              Log10Freq = log10(Freq))

# Check:

ELP

# Fit RT by log frequency model:

ELP_mdl <- lm(RT ~ Log10Freq, ELP)

# Create tibble with new data:

newdata <- tibble(Log10Freq = seq(0, 5, 0.01))

# Compute 95% confidence interval:

preds <- predict(ELP_mdl, newdata,
                 interval = 'confidence')

# Check:

head(preds)

# Glue together:

preds <- cbind(newdata, preds)

head(preds)

# Plot:

preds %>% ggplot(aes(x = Log10Freq, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              fill = 'grey', alpha = 0.5) +
  geom_line() +
  geom_text(data = ELP, aes(y = RT, label = Word)) +
  theme_minimal()







