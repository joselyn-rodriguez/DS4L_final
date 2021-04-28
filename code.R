library(tidyverse)
library(brms)
library(tidybayes)
library(lme4)
library(lmerTest)
library(psych)
library(bayesplot)
library(ProbBayes)


# 1. Re-level variables. [✓]
# - the base factor should be /t/-biasing
# - make sure VOT is centered correctly

#2. Calculate Rope [✓]

#3. Plot posterior estimates []
# - and do other posterior checks

#4. Actually set your priors [✓]



# load some data that i'll need real quick

temp <- read_csv("data/batch1-test.csv") 

# releveld w/pre:tent-biasing as the intercept
test <- temp %>% 
  mutate(block = factor(block, levels=c("pre", "post")),
         resp_t = as.numeric(resp_t))  %>%
  mutate(condition = factor(condition, levels = c("tent-biasing", "dent-biasing"))) %>% 
  mutate(vot = (vot - mean(vot))) # centering

test %>% 
  group_by(workerId) %>% 
  summarize(average = mean(resp_t), sd = sd(resp_t))

pre_test <- test %>% 
  filter(block == "pre")
nrow(pre_test)

post_test <- test %>% 
  filter(block == "post")
nrow(post_test)

exposure <- read_csv("data/batch1-exposure.csv")

#### visualize pre-post tests

ggplot(data = test,
       aes(x=vot, y=resp_t, color = condition)) +
  # uncomment to see the actual data points instead of just the mean
  geom_point(position=position_jitter(h = 0.05), alpha=0.1) +
  geom_line(stat="summary", fun = mean) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap( . ~ block) +
  ylab("Proportion of /t/ response") + xlab("VOT (ms)") +
  labs(title = "*Pre- and Post Continuum 'Tent' Responses*", tag = "Figure 1") +
  mdthemes::md_theme_bw() +
  theme(text=element_text(size=11, family = "Times"))



#### Begin Frequentist Modeling

# intercept model 
freq_analysis_int <- glm(resp_t ~ 1, data = test, family = "binomial"(link = "logit"))
summary(freq_analysis_int)

# including interaction and one random effect
freq_analysis_1 <- glmer(resp_t ~ block * condition * vot +
                           (1 | workerId),
                         data = test, family = "binomial"(link = "logit"))
# adding one more random effect
freq_analysis_2 <- glmer(resp_t ~ block * condition * vot +
                           (1 | workerId) +
                           (1 | stimulus),
                         data = test, family = "binomial"(link = "logit"), 
                         control=glmerControl(optimizer="bobyqa",
                                              optCtrl=list(maxfun=2e5)))

# adding in a random slope and lets goooo
freq_analysis_full <- glmer(resp_t ~ block * condition * vot +
                              (block | workerId) +
                              (1 | stimulus),
                            data = test, family = "binomial"(link = "logit"), 
                            control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5)))

#### Model comparison
freq_anova <-  anova(freq_analysis_1, freq_analysis_2 , freq_analysis_full, test = "Chisq") # well it looks like including stimulus is better

#### frequentist summary
# Let's take a look at the summary for the full model 
summary(freq_analysis_full)


#### Bayesian Modeling 

# intercept model
bayes_analysis_int <- brm(resp_t ~ 1, data = test, family = "bernoulli"(link = "logit"))
summary(bayes_analysis_int)

# adding in one random intercept
bayes_analysis_1 <- brm(resp_t ~ block * condition * vot +
                                  (1 | workerId),
                                  data = test, family = "bernoulli"(link = "logit"))
summary(bayes_analysis_1)

# adding in another random intercept
bayes_analysis_2 <- brm(resp_t ~ block * condition * vot +
                                  (1 | workerId) +
                                  (1 | stimulus),
                                  data = test, family = "bernoulli"(link = "logit"),
                                  cores = getOption("mc.cores", 4))
summary(bayes_analysis_2)

# finally, random intercepts and random slope for block
bayes_analysis_full <- brm(resp_t ~ block * condition * vot +
                                  (block | workerId) +
                                  (1 | stimulus),
                                  data = test, family = "bernoulli"(link = "logit"), 
                                  prior = c(prior("normal(0,0.5)", class = "b"), # setting priors (remember -4 to 4)
                                            prior("normal(0,1", class = "Intercept")), # setting priors (remember -4 to 4)
                                  cores = getOption("mc.cores", 4)) # otherwise it takes a million years

bayes_summary <- summary(bayes_analysis_full)


#### Model comparison 
waic(bayes_analysis_2 , bayes_analysis_full) 
bayes_factor(bayes_analysis_2, bayes_analysis_full)


### Check priors 
prior_summary(bayes_analysis_full)

#### get your posteriors
posterior_table <- as_tibble(bayes_analysis_full)

#### calculating ROPE
ROPE <- rope(bayes_analysis_full,
             range = "default", 
             ci = 0.95, 
             ci_method = "HDI", # or ETI - forgot what the difference is 
             effects = "all",
             components = "all") # possible multi-collinearity between b_blockpost.conditiondentMbiasing.vot and b_conditiondentMbiasing.vot (r = 0.75). This might lead to inappropriate results.


# ROPE calculated as: [-0.18, 0.18]
posterior_table %>% 
  select(c("b_Intercept":"b_blockpost:conditiondentMbiasing:vot")) %>% 
  pivot_longer(everything(), names_to = "parameter", values_to = "estimate") %>% 
  ggplot(., aes(x = estimate, y = parameter, fill = stat(abs(x) < .18))) +
    stat_halfeye() + 
    geom_vline(xintercept = c(ROPE$ROPE_low, ROPE$ROPE_high), linetype = "dashed", color = "red") +
    labs(title = "*Parameter estimates for Bayesian Model*", tag = "Figure 2") +
    mdthemes::md_theme_bw() +
    theme(text=element_text(size=11, family = "Times")) + 
    scale_fill_manual(values = c("gray80", "skyblue"))

# not relevant for our current purposes
# mcmc_hist(posterior_table, pars = c("b_Intercept", "b_blockpost:conditiondentMbiasing"), 
#              size = 1.5, alpha = 0.5)


###### ONGOIG IN PROGRESS ######

# this is where  i'm trying to figure out how to plot the posterior samples

#### Begin posterior checks
get_variables(bayes_analysis_full)



# plot posterior samples against data
test %>%
  add_fitted_draws(bayes_analysis_full, n = 100) %>%
  ggplot(aes(x = vot, y = resp_t, color = condition)) +
  facet_grid(~block) + 
  geom_line(aes(y = .value, alpha = .1)) +
  scale_color_brewer(palette = "Dark2")


samples <- posterior_table %>% 
  mutate_at(
    .vars = vars(matches("Intercept\\]")), 
    .funs = ~ . + posterior_table$b_Intercept
  ) %>% 
  mutate_at(
    .vars = vars(matches("continuum_\\]")), 
    .funs = ~ . + posterior_table$b_vot
  ) %>% 
  select(starts_with("r_workerId")) %>% 
  pivot_longer(cols = everything(), names_to = c("Subject", ".value"), 
    names_sep = ",")



# attempt at a caterpillar plot
cat_plot <- bayes_analysis_full %>% 
  plot(
    combo = c("hist", "trace"), widths = c(1, 1.5),
    theme = theme_bw(base_size = 16)
  )

# posterior predictive checks
pp_check_plot <- pp_check(bayes_analysis_full, nsamples = 1e2) + theme_bw(base_size = 20)









