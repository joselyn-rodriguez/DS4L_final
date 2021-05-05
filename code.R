library(tidyverse)
library(brms)
library(tidybayes)
library(lme4)
library(lmerTest)
library(psych)
library(bayesplot)
library(ProbBayes)


# 1. Re-level variables. [✓]
# - the reference level should be /t/-biasing
# - make sure VOT is centered correctly

#2. Calculate Rope [✓]

#3. Plot posterior estimates []
# - and do other posterior checks

#4. Actually set your priors [✓]

#5. Save brmsfit to file for faster access [✓]


#### Load data and clean up a bit more
test <- read_csv(here::here("data_tidy/data_test.csv")) %>%
            mutate(block = factor(block, levels=c("pre", "post")),   # re-factor to set reference levels
                   condition = factor(condition, levels = c("tent-biasing", "dent-biasing")),
                   resp_t = as.numeric(resp_t))  %>%
            mutate(vot = vot - mean(vot))  # center vot
            
# questionnaire data
questionnaire <- read_csv(here::here("data_tidy/questionnaire.csv"))

#### Load models I'd saved ahead of time
readRDS("bayes_analysis_int.rds")
readRDS("bayes_analysis_1.rds")
readRDS("bayes_analysis_2.rds")
readRDS("bayes_analysis_full.rds")


#### Descriptives
test %>% 
  group_by(workerId) %>% 
  summarize(average = mean(resp_t), sd = sd(resp_t)) 


#### Double checking number of observations
obs_test <- length(unique(test$workerId))
obs_quest <- length(unique(test$workerId))

#### Getting demographic information
avg_age = mean(questionnaire$age)                             # this is the average age of the participants
med_age = median(questionnaire$age)                           # median age of participants
sd_age = sd(questionnaire$age)                                # sd of age
num_fem = count(questionnaire %>% filter(sex == "Female"))    # number of 'female' responses. keep in mind Na is an option
num_male = count(questionnaire %>% filter(sex == "Male"))     # number of 'male' responses
hisp = count(questionnaire %>% filter(ethnicity == "Hisp"))   # number of responses to "yes" for hispanic ethnicity
eng = count(questionnaire %>% filter(parent == "yes"))        # number of participants whose parents spoken english @home
audio = table(questionnaire$audioequip)                       # the kind of headphones people used 


#### Visualize pre-post tests

test_plot <- ggplot(data = test, aes(x=vot, y=resp_t, color = condition)) +
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

# including interaction and one random effect
freq_analysis_1 <- glmer(resp_t ~ block * condition * vot +
                           (1 | workerId),
                           data = test, family = "binomial"(link = "logit"))

# adding one more random effect
freq_analysis_2 <- glmer(resp_t ~ block * condition * vot +
                           (1 | workerId) +
                           (1 | stimulus),
                         data = test, family = "binomial"(link = "logit"), 
                         control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# adding in a random slope and lets goooo
freq_analysis_full <- glmer(resp_t ~ block * condition * vot +
                              (block | workerId) +
                              (1 | stimulus),
                            data = test, family = "binomial"(link = "logit"), 
                            control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))) # using an optimizer to help it converge
#### Frequentist Model comparison
freq_anova <-  anova(freq_analysis_1, freq_analysis_2 , freq_analysis_full, test = "Chisq") # it looks like including stimulus is better

#### frequentist summary
# Let's take a look at the summary for the full model 
freq_sum <- summary(freq_analysis_full)


# plot of conditional means
freq_cond_plot <- ggplot(data = test, aes(x=vot, y=resp_t, color = condition)) +
  geom_point(position=position_jitter(h = 0.05), alpha=0.1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap( . ~ block) +
  ylab("Proportion of /t/ response") + xlab("VOT (ms)") +
  labs(title = "*Pre- and Post Continuum 'Tent' Responses*", tag = "Figure 1") +
  mdthemes::md_theme_bw() +
  theme(text=element_text(size=11, family = "Times"))

  
#### Bayesian Modeling 

# intercept model
bayes_analysis_int <- brm(resp_t ~ 1, data = test, family = "bernoulli"(link = "logit"), file = "bayes_analysis_int")

# adding in one random intercept
bayes_analysis_1 <- brm(resp_t ~ block * condition * vot +
                                  (1 | workerId),
                                  data = test, family = "bernoulli"(link = "logit"), file = "bayes_analysis_1")

# adding in another random intercept
bayes_analysis_2 <- brm(resp_t ~ block * condition * vot +
                                  (1 | workerId) +
                                  (1 | stimulus),
                                  save_all_pars = T, 
                                  data = test, family = "bernoulli"(link = "logit"), file = "bayes_analysis_2",
                                  cores = getOption("mc.cores", 4))

# finally, random intercepts and random slope for block
bayes_analysis_full <- brm(resp_t ~ block * condition * vot +
                                  (block | workerId) +
                                  (1 | stimulus),
                                  data = test, family = "bernoulli"(link = "logit"), 
                                  prior = c(prior("normal(0,0.5)", class = "b"), # setting priors (remember -4 to 4)
                                            prior("normal(0,1", class = "Intercept")), # setting priors (remember -4 to 4)
                                  cores = getOption("mc.cores", 4),
                                  save_all_pars = T,
                                  file = "bayes_analysis_full") # otherwise it takes a million years

bayes_sum <- summary(bayes_analysis_full)


#### Bayesian Model comparison 
comparison_waic <- waic(bayes_analysis_2, bayes_analysis_full, compare = T) # you can see that the full model is slightly better (~ -8)
comparison_bf <- bayes_factor(bayes_analysis_2, bayes_analysis_full)        # bayes factor for the model 


# Check priors 
prior_sum <- prior_summary(bayes_analysis_full)


#### Bayesian posterior checks
# get your posteriors
posterior_table <- as_tibble(bayes_analysis_full)

# calculating ROPE w/95% credible intervals
ROPE <- rope(bayes_analysis_full,
             range = "default", 
             ci = 0.95, 
             ci_method = "HDI",
             effects = "all",
             components = "all") # possible multi-collinearity between b_blockpost.conditiondentMbiasing.vot and b_conditiondentMbiasing.vot (r = 0.75). This might lead to inappropriate results.


# posterior estimate graph with all coef estimates
full_posterior_est <- posterior_table %>% 
  select(c("b_Intercept":"b_blockpost:conditiondentMbiasing:vot")) %>% 
  pivot_longer(everything(), names_to = "parameter", values_to = "estimate") %>% 
  ggplot(., aes(x = estimate, y = parameter, fill = stat(abs(x) < .18))) + # ROPE calculated as: [-0.18, 0.18]
  stat_halfeye() + 
  geom_vline(xintercept = c(ROPE$ROPE_low, ROPE$ROPE_high), linetype = "dashed", color = "red") +
  labs(title = "*Parameter estimates for Bayesian Model*", tag = "Figure 2") +
  mdthemes::md_theme_bw() +
  theme(text=element_text(size=11, family = "Times")) + 
  scale_fill_manual(values = c("gray80", "skyblue"))

# posterior estimate graph with only variables mostly outside ROPE
sub_posterior_est <- posterior_table %>% 
  select("b_Intercept","b_blockpost:conditiondentMbiasing") %>% 
  pivot_longer(everything(), names_to = "parameter", values_to = "estimate") %>% 
  ggplot(., aes(x = estimate, y = parameter, fill = stat(abs(x) < .18))) +
  stat_halfeye() +  
  geom_vline(xintercept = c(ROPE$ROPE_low, ROPE$ROPE_high), linetype = "dashed", color = "skyblue") +
  labs(title = "*Parameter estimates for Bayesian Model*", tag = "Figure 2") +
  mdthemes::md_theme_bw() +
  theme(text=element_text(size=11, family = "Times")) + 
  scale_fill_manual(values = c("gray80", "skyblue"))


###### ONGOING IN PROGRESS ######

# this is where  i'm trying to figure out how to plot the posterior samples

#### Begin posterior checks
bayes_vars <- get_variables(bayes_analysis_full)


# plot posterior samples against data
# test %>%
#   add_fitted_draws(bayes_analysis_full, n = 100) %>%
#   ggplot(aes(x = vot, y = resp_t, color = condition)) +
#   facet_grid(~block) + 
#   geom_line(aes(y = .value, alpha = .1)) +
#   scale_color_brewer(palette = "Dark2")
# 
# 
# samples <- posterior_table %>% 
#   mutate_at(
#     .vars = vars(matches("Intercept\\]")), 
#     .funs = ~ . + posterior_table$b_Intercept
#   ) %>% 
#   mutate_at(
#     .vars = vars(matches("continuum_\\]")), 
#     .funs = ~ . + posterior_table$b_vot
#   ) %>% 
#   select(starts_with("r_workerId")) %>% 
#   pivot_longer(cols = everything(), names_to = c("Subject", ".value"), 
#     names_sep = ",")
# 
# 
# 
# # attempt at a caterpillar plot
# cat_plot <- bayes_analysis_full %>% 
#   plot(
#     combo = c("hist", "trace"), widths = c(1, 1.5),
#     theme = theme_bw(base_size = 16)
#   )
# 
# # posterior predictive checks
# pp_check_plot <- pp_check(bayes_analysis_full, nsamples = 1e2) + theme_bw(base_size = 20)
# 
# # Using this package from tidybayes may be getting me what i'm looking for
# conditions <- make_conditions(bayes_analysis_full, c("block", "condition"))
# 
# conditional_plots <- conditional_effects(bayes_analysis_full,
#                                          effects = "vot:block",
#                                          conditions = conditions,
#                                          method  = "predict",
#                                          categorical = T)
# 
# test <- conditional_effects(bayes_analysis_full)






