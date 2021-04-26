library("tidyverse")
library("lme4")
library("patchwork")
library("lmerTest")


# 1.
#
# Get to know the sleepstudy dataset 
#
# How many rows? Columns? Make a plot. 
#
dim(sleepstudy)
glimpse(sleepstudy)
str(sleepstudy)
head(sleepstudy)

sleepstudy %>% 
  ggplot(., aes(x = Days, y = Reaction)) + 
    geom_point()


# 2. 
#
# Fit a complete pooling model (lm)
# Give a rough interpretation of the model summary
#
complete_pool_full <- lm(Reaction ~ Days, data = sleepstudy)
summary(complete_pool_full)



# 3. 
#
# Fit no pooling models (hint: ?lmList)
# What would be a good visualization to understand individual differences?
#
no_pool_full <- lmList(Reaction ~ Days | Subject, sleepstudy)
summary(no_pool_full)
coef(no_pool_full)


subj_slope <- tibble(
  Intercept = ranef(no_pool_full)$`(Intercept)`, 
  Slope = ranef(no_pool_full)$Days) %>% 
  mutate(Subject = rownames(coef(no_pool_full)), 
         Subject = forcats::fct_reorder(Subject, Slope, min)) %>% 
  ggplot(., aes(x = Slope, y = Subject)) + 
    geom_vline(xintercept = 0, color = "white", size = 3) + 
    geom_point()

subj_int <- tibble(
  Intercept = ranef(no_pool_full)$`(Intercept)`, 
  Slope = ranef(no_pool_full)$Days) %>% 
  mutate(Subject = rownames(coef(no_pool_full)), 
         Subject = forcats::fct_reorder(Subject, Intercept, min)) %>% 
  ggplot(., aes(x = Intercept, y = Subject)) + 
    geom_vline(xintercept = 0, color = "white", size = 3) + 
    geom_point()

subj_int + subj_slope

coef(no_pool_full) %>% 
  as_tibble() %>% 
  mutate(subject = rownames(coef(no_pool_full)), 
         Intercept = `(Intercept)`, Slope = Days) %>% 
  ggplot(., aes(x = Slope, y = Intercept, label = subject)) + 
    geom_point() + 
    ggrepel::geom_text_repel() + 
    ds4ling::ds4ling_bw_theme()

tibble(
  Intercept = ranef(no_pool_full)$`(Intercept)`, 
  Slope = ranef(no_pool_full)$Days) %>% 
  mutate(subject = rownames(coef(no_pool_full))) %>% 
  ggplot(., aes(x = Slope, y = Intercept, label = subject)) + 
    geom_hline(yintercept = 0, lty = 3) + 
    geom_vline(xintercept = 0, lty = 3) + 
    geom_point() + 
    ggrepel::geom_text_repel() + 
    ds4ling::ds4ling_bw_theme()




# 4. 
#
# Fit a partial pooling models
#

# 4a. Fit an intercept only model with by-subject random intercepts
# Describe the summary output, make a visualization if you can think 
# of a good one. 
pp_mod_null <- lmer(Reaction ~ 1 + (1 | Subject), data = sleepstudy)
summary(pp_mod_null)

coef(pp_mod_null)
ranef(pp_mod_null)
fixef(pp_mod_null)

coef(pp_mod_null)$Subject %>% 
  as_tibble() %>% 
  transmute(Intercepts = `(Intercept)`, 
            Subjects = rownames(coef(pp_mod_null)$Subject), 
            Model = "Null") %>% 
  ggplot(., aes(x = Model, y = Intercepts)) + 
    geom_point() + 
    geom_point(color = "darkred", size = 5, alpha = 0.7,  
      data = tibble(Model = "Null", Intercepts = fixef(pp_mod_null)))



# 4b. Include a fixed effect for "Days"
# Try to plot this model with individual regression lines for the grouping 
# variable (Subjects) AND the population-level regression line (fixed effect).
pp_mod_days <- lmer(Reaction ~ 1 + Days + (1 | Subject), data = sleepstudy)
summary(pp_mod_days)

sleepstudy %>% 
  ggplot(., aes(x = Days, y = Reaction)) + 
    geom_point() + 
    geom_abline(
      data = coef(pp_mod_days)$Subject %>% 
        as_tibble() %>% 
        rename(Reaction = `(Intercept)`), 
      aes(slope = Days, intercept = Reaction)) + 
    geom_abline(
      slope = fixef(pp_mod_days)[2], 
      intercept = fixef(pp_mod_days)[1], 
      color = "darkred", size = 2)






# 4c. Include a random slope for "Days"
# Compare model estimates (visually) and do nested model comparisons
# Try to plot this model with individual regression lines for the grouping 
# variable (Subjects) AND the population-level regression line (fixed effect).
pp_mod_days <- lmer(Reaction ~ 1 + Days + (1 | Subject), data = sleepstudy)
pp_mod_full <- lmer(Reaction ~ 1 + Days + (1 + Days | Subject), data = sleepstudy)

anova(pp_mod_days, pp_mod_full, test = "Chisq")
summary(pp_mod_full)

sleepstudy %>% 
  ggplot(., aes(x = Days, y = Reaction)) + 
    geom_point() + 
    geom_abline(
      data = coef(pp_mod_full)$Subject %>% 
        as_tibble() %>% 
        rename(Reaction = `(Intercept)`), 
      aes(slope = Days, intercept = Reaction)) + 
    geom_abline(
      slope = fixef(pp_mod_full)[2], 
      intercept = fixef(pp_mod_full)[1], 
      color = "darkred", size = 2)





# 5. For more practice: 
# glimpse(ChickWeight)
# glimpse(Orange)




library(academicWriteR)
# https://www.jvcasillas.com/academicWriteR/
