# Lab 7: Interactions
--------------------------------------------------------
  # Categorical * categorical interactions
  # In this experiment Winter and Matlock (2013) wanted to know whether participants would place the cities closer to each other on the map when a given text emphasises the similarity between cities.
  # In addition to this manipulation of what they call 'semantic similarity', they added a 'phonological similarity' condition. In the phonologically similar condition, the names of the two cities were Swaneplam and Scaneplave; in the phonologically different condition, the names of the two cities sounded more different from each other: Swaneplam and Mouchdalt.
  # Given these research questions, you want to model the distance between the two cities as a function of phonological similarity and semantic similarity. Moreover, it is plausible that these two predictors interact.
  
  # Load data:
  
  sim <- read_csv('winter_matlock_2013_similarity.csv')
  
  # Check:
  
  sim
  
  # Count the number of data points per condition:
  
  sim %>% count(Phon, Sem)
  
  # Check whether there are NA's:
  
  sum(is.na(sim$Distance))
  
  # Get rid of the NA:
  
  sim <- filter(sim, !is.na(Distance))
  
  # Achieves the same job:
  
  sim <- filter(sim, complete.cases(Distance))
  
  # Check that there is indeed less in this tibble:
  
  nrow(sim)
  
  # Compute the range:
  
  range(sim$Distance)
  
  # Fit model without interaction:
  
  sim_mdl <- lm(Distance ~ Phon + Sem, data = sim)
  
  # Look at terms:
  
  tidy(sim_mdl) %>% select(term, estimate)
  
  # Fit model with interaction:
  
  sim_mdl_int <- lm(Distance ~ Phon * Sem, data = sim)
  
  # Look at coefficients:
  
  tidy(sim_mdl_int) %>% select(term, estimate)
  
  # Create predictions for all combinations:
  
  Phon <- rep(c('Different', 'Similar'), each = 2)
  Sem <- rep(c('Different', 'Similar'), times = 2)
  
  Phon
  Sem
  
  newdata <- tibble(Phon, Sem)
  newdata
  
  newdata$fit <- predict(sim_mdl_int, newdata)
  
  newdata
  
  # Compare to descriptive averages:
  
  newdata %>% group_by(Sem) %>% summarize(distM = mean(fit))
  
  # Transform to factors and add sum coding scheme:
  
  sim <- mutate(sim,
                Phon_sum = factor(Phon),
                Sem_sum = factor(Sem))
  contrasts(sim$Phon_sum) <- contr.sum(2)
  contrasts(sim$Sem_sum) <- contr.sum(2)
  
  # Refit model:
  
  sum_mdl <- lm(Distance ~ Phon_sum * Sem_sum, data = sim)
  
  # Look at output:
  
  tidy(sum_mdl) %>% select(term, estimate)
  
  # Example:
  
  sumcodes <- tibble(Phon, Sem)
  sumcodes$PhonSum <- c(1, 1, -1, -1)
  sumcodes$SemSum <- c(1, -1, 1, -1)
  
  
  
  # --------------------------------------------------------
  # Continuous * continuous interactions
  
  # The final case to consider is an interaction between two continuous predictors. In 2018, Sidhu and Pexman looked at the effect of sensory experience on iconicity. However, they additionally considered the role of what psycholinguists call 'semantic neighborhood density'. This term describes the idea that there are certain regions in your mental lexicon that are quite 'crowded' or 'dense', with lots of words that are connected to each other by virtue of having similar meanings. It has been proposed that iconicity may lead to confusion, because, with many iconic forms, similar meanings will also sound similar (Gasser, 2004; Christiansen & Chater, 2016). Sidhu and Pexman (2018) reasoned that language should be biased against iconicity specifically in semantically dense neighborhoods, where there is more room for confusion. In sparse neighborhoods, iconicity is not as dangerous, as there is less opportunity to confuse concepts.
  
  # Load Sidhu & Pexman (2017) data:
  
  lonely <- read_csv('sidhu&pexman_2017_iconicity.csv')
  
  # Check:
  
  lonely
  
  # Get rid of negative iconicity values:
  
  lonely <- filter(lonely, Iconicity >= 0)
  
  # Fit model with interaction:
  
  lonely_mdl <- lm(Iconicity ~ SER * ARC, data = lonely)
  
  # Look at terms:
  
  tidy(lonely_mdl) %>% select(term, estimate)
  
  # Standardize predictors:
  
  lonely <- mutate(lonely,
                   SER_z = (SER - mean(SER)) / sd(SER),
                   ARC_z = (ARC - mean(ARC)) / sd (ARC))
  
  # Check:
  
  lonely
  
  # Fit model with standardized predictors:
  
  lonely_mdl <- lm(Iconicity ~ SER_z * ARC_z, data = lonely)
  
  # Interpret coefficients:
  
  tidy(lonely_mdl) %>% select(term, estimate)
  
  
  