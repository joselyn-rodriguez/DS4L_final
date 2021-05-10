library(brms)
library(tidyverse)

########################################################################################################################
# This file is for running the bayesian models and saving the models so I don't have to run them each time             #
########################################################################################################################

# need to load in data again
test <- read_csv(here::here("data_tidy/data_test_tidy.csv")) %>%
  mutate(block = factor(block, levels=c("pre", "post")),   # re-factor to set reference levels
         condition = factor(condition, levels = c("tent-biasing", "dent-biasing")),
         resp_t = as.numeric(resp_t))  %>%
  mutate(vot = vot - mean(vot))  # center vot


#### Bayesian Modeling 

# intercept model
bayes_analysis_int <- brm(resp_t ~ 1, data = test, family = "bernoulli"(link = "logit"), file = "bayes_analysis_int")

# adding in one random intercept
bayes_analysis_1 <- brm(resp_t ~ block * condition * vot +
                          (1 | workerId),
                        data = test, family = "bernoulli"(link = "logit"), file = "bayes_analysis_1", 
                        prior = c(prior("normal(0,0.5)", class = "b"), # setting priors (remember -4 to 4)
                                  prior("normal(0,1", class = "Intercept"))) # setting priors (-4 to 4)

# adding in another random intercept
bayes_analysis_2 <- brm(resp_t ~ block * condition * vot +
                          (1 | workerId) +
                          (1 | stimulus),
                        save_all_pars = T, 
                        data = test, family = "bernoulli"(link = "logit"), file = "bayes_analysis_2",
                        prior = c(prior("normal(0,0.5)", class = "b"), # setting priors (-4 to 4)
                                  prior("normal(0,1", class = "Intercept")), # setting priors (-4 to 4)
                        cores = parallel::detectCores()) # this will make use of multiple cores on your computer. If you don't want to do this, just delete this line

# finally, random intercepts and random slope for block
bayes_analysis_full <- brm(resp_t ~ block * condition * vot +
                             (block | workerId) +
                             (1 | stimulus),
                           data = test, family = "bernoulli"(link = "logit"), 
                           prior = c(prior("normal(0,0.5)", class = "b"), # setting priors (-4 to 4)
                                     prior("normal(0,1", class = "Intercept")), # setting priors (-4 to 4)
                           cores = parallel::detectCores(), # this will make use of multiple cores on your computer. If you don't want to do this, just delete this line
                           save_all_pars = T,
                           file = "bayes_analysis_full") # otherwise it takes a million years


