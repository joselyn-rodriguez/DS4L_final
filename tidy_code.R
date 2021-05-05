library(tidyverse)


#### Code for tidying the raw data sets 
questionnaire <- read_csv(here::here("data/batch1.csv"), col_types = cols(sender = col_character(), 
                                                              workerId = col_character(), audioequip = col_character(), 
                                                              sex = col_character(), ethnicity = col_character(), 
                                                              race = col_character(), raceother = col_character(), 
                                                              born = col_character(), parent = col_character(), 
                                                              lang = col_character(), comments = col_character(), 
                                                              age = col_number())) %>% 
  select("sender", "workerId", "audioequip":"comments") %>% 
  filter(sender == "Quesionnaire Form")


# block_id, trial, resp are all created variables
data_full <- read_csv(here::here("data/batch1.csv"), col_types = cols(sender = col_character(), 
                                                          workerId = col_character(), audioequip = col_character(), 
                                                          sex = col_character(), ethnicity = col_character(), 
                                                          race = col_character(), raceother = col_character(), 
                                                          born = col_character(), parent = col_character(), 
                                                          lang = col_character(), comments = col_character(), 
                                                          age = col_number())) %>% 
  select("workerId","condition","stimulus","response","sender","sender_id","block", "duration", "item", "item_block", "time") %>% 
  filter(sender == "Sounds" | sender == "Response") %>% 
  mutate(block_id = substr(sender_id,1,2),
         resp_t = case_when(substr(response, 1,1) == "t" ~ "TRUE",
                            substr(response, 1,1) == "d" ~ "FALSE"),
         vot = substr(stimulus, 11, 12))


# this is the data for just the pre- and post-tests 
data_test <- data_full %>% 
  select("workerId","condition","stimulus","response","sender","sender_id","duration", "block_id", "block", "vot", "resp_t") %>% 
  mutate(block = case_when(block_id=="6_"~ "pre",
                           block_id=="10"~ "post")) %>% 
  filter(block == "pre" | block == "post") %>% 
  filter(sender == "Sounds")



write_csv(data_full, "data_tidy/data_full.csv")
write_csv(data_test, "data_tidy/data_test.csv")
write_csv(questionnaire, "data_tidy/questionnaire.csv")





#### Old way of tidying data

# temp <- read_csv(here::here("data/batch1-test.csv"))
# 
# # tidy  data some more and re-level
# test <- temp %>% 
#   mutate(block = factor(block, levels=c("pre", "post")),
#          resp_t = as.numeric(resp_t))  %>%
#   mutate(condition = factor(condition, levels = c("tent-biasing", "dent-biasing"))) %>% 
#   mutate(vot = (vot - mean(vot))) # centering
# 
# print(unique(test$workerId))
# print(unique(questionnaire$workerId))
# 
# # oh no, the number of subjects doesn't match lol
# questionnaire <- read_csv(here::here("data/batch1.csv"), col_types = cols(sender = col_character(), 
#                                                                           workerId = col_character(), audioequip = col_character(), 
#                                                                           sex = col_character(), ethnicity = col_character(), 
#                                                                           race = col_character(), raceother = col_character(), 
#                                                                           born = col_character(), parent = col_character(), 
#                                                                           lang = col_character(), comments = col_character(), 
#                                                                           age = col_number())) %>% 
#   select("sender", "workerId", "audioequip":"comments") %>% 
#   filter(sender == "Quesionnaire Form")
# 
