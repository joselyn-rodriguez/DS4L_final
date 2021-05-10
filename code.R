library(tidyverse)
library(brms)
library(tidybayes)
library(lme4)
library(lmerTest)
library(psych)
library(bayesplot)
library(ProbBayes)

########################################################################
# This file is for the bulk of the code used in the paper              #
# It includes data visualization, model tests, and comparisons.        #
########################################################################


#### TODO
# 1. Re-level variables. [✓]
# - the reference level should be /t/-biasing
# - make sure VOT is centered correctly

#2. Calculate Rope [✓]

#3. Plot posterior estimates [✓]
# - and do other posterior checks

#4. Actually set your priors [✓]

#5. Save brmsfit to file for faster access [✓]

#6. Finish writeup []


#### Load data and clean up a bit more
test <- read_csv(here::here("data_tidy/data_test_tidy.csv")) %>%
            mutate(block = factor(block, levels=c("pre", "post")),   # re-factor to set reference levels
                   condition = factor(condition, levels = c("tent-biasing", "dent-biasing")),
                   resp_t = as.numeric(resp_t))  %>%
            mutate(vot = vot - mean(vot))  # center vot


           
# questionnaire data
questionnaire <- read_csv(here::here("data_tidy/questionnaire_tidy.csv"))

#### Load models I'd saved ahead of time - you'll need to run the models.R file first to get these files
bayes_analysis_int <- readRDS(here::here("bayes_analysis_int.rds"))
bayes_analysis_1 <- readRDS(here::here("bayes_analysis_1.rds"))
bayes_analysis_2 <- readRDS(here::here("bayes_analysis_2.rds"))
bayes_analysis_full <- readRDS(here::here("bayes_analysis_full.rds"))


#### Descriptives
test %>% 
  group_by(new_ID) %>% 
  summarize(average = mean(resp_t), sd = sd(resp_t)) 


#### Double checking number of observations
obs_test <- length(unique(test$new_ID))
obs_quest <- length(unique(test$new_ID))

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


###############################################################
#                  Frequentist modeling                       #
###############################################################

# intercept model 
freq_analysis_int <- glm(resp_t ~ 1, data = test, family = "binomial"(link = "logit"))

# including interaction and one random effect
freq_analysis_1 <- glmer(resp_t ~ block * condition * vot +
                           (1 | new_ID),
                           data = test, family = "binomial"(link = "logit"))

# adding one more random effect
freq_analysis_2 <- glmer(resp_t ~ block * condition * vot +
                           (1 | new_ID) +
                           (1 | stimulus),
                         data = test, family = "binomial"(link = "logit"), 
                         control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# adding in a random slope and lets goooo
freq_analysis_full <- glmer(resp_t ~ block * condition * vot +
                              (block | new_ID) +
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


###############################################################
#                     Bayesian modeling                       #
###############################################################


# NOTE: the bayesian models were moved to a separate file because they take approximately 5.6 billion years to run. Instead,
# I load the models into this file so that you only have to run the models once then save them and reload
# Although these do also take a while to run, so I mean there's that. 

#### Bayesian model summary
bayes_sum <- summary(bayes_analysis_full)


#### Bayesian Model comparison 
comparison_waic <- waic(bayes_analysis_2, bayes_analysis_full, compare = T) # you can see that the full model is slightly better (~ -8)
# comparison_bf <- bayes_factor(bayes_analysis_2, bayes_analysis_full)        # bayes factor for the model 


# Check priors 
prior_sum <- prior_summary(bayes_analysis_full)


#### Bayesian posterior checks
# get your posteriors
posterior_table <- as_tibble(bayes_analysis_full)

posterior_check <- pp_check(bayes_analysis_full)

# calculating ROPE w/95% credible intervals
ROPE <- bayestestR::rope(bayes_analysis_full,
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
  geom_vline(xintercept = c(ROPE$ROPE_low, ROPE$ROPE_high), linetype = "dashed", color = "skyblue") +
  labs(title = "*Parameter estimates for Bayesian Model*") +
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
  labs(title = "*Parameter estimates for Bayesian Model*") +
  mdthemes::md_theme_bw() +
  theme(text=element_text(size=11, family = "Times")) + 
  scale_fill_manual(values = c("gray80", "skyblue"))


# getting conditions for conditional mean 
conditions <- make_conditions(bayes_analysis_full, "block") 

# spaghetti plot of the expectations of the posterior predictive distribution 
conditional_plot <- plot(conditional_effects(bayes_analysis_full,
                                             effects = "vot:condition",
                                             conditions = conditions,
                                             spaghetti = T,
                                             method  = "posterior_epred",
                                             nsamples = 500), 
                         main = "conditional effects of /t/-response", 
                         sub = "Figure 4")








