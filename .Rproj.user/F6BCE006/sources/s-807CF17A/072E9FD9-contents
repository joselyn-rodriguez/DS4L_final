library(tidyverse)

## r cleaning data

# I'm sorry this is a mess but so was this csv :(
questionnaire <- read_csv("data/batch1.csv", col_types = cols(sender = col_character(), 
                                                              workerId = col_character(), audioequip = col_character(), 
                                                              sex = col_character(), ethnicity = col_character(), 
                                                              race = col_character(), raceother = col_character(), 
                                                              born = col_character(), parent = col_character(), 
                                                              lang = col_character(), comments = col_character(), 
                                                              age = col_number())) %>% 
  select("sender", "workerId", "audioequip":"comments") %>% 
  filter(sender == "Quesionnaire Form")


# block_id, trial, resp are all created variables
data_full <- read_csv("data/batch1.csv", col_types = cols(sender = col_character(), 
                                                          workerId = col_character(), audioequip = col_character(), 
                                                          sex = col_character(), ethnicity = col_character(), 
                                                          race = col_character(), raceother = col_character(), 
                                                          born = col_character(), parent = col_character(), 
                                                          lang = col_character(), comments = col_character(), 
                                                          age = col_number())) %>% 
  select("workerId","condition","stimulus","response","sender","sender_id","block", "duration", "item", "item_block", "time") %>% 
  filter(sender == "Sounds" | sender == "Response") %>% 
  mutate(block_id = substr(sender_id,1,1),
         resp_t = case_when(substr(response, 1,1) == "t" ~ 1,
                            substr(response, 1,1) == "d" ~ 0),
         vot = substr(stimulus, 11, 12))

# this is the data for just the pre- and post-tests 
data_test <- data_full %>% 
  select("workerId","condition","stimulus","response","sender","sender_id", "resp_t", "vot", "duration", "block_id") %>% 
  mutate(block = case_when(block_id=="6"~ "pre",
                           block_id=="1"~ "post")) %>% 
  filter(block == "pre" | block == "post") %>% 
  filter(sender == "Sounds") 

data_exposure <- data_full %>% 
  select("workerId","condition","response","sender","block_id","vot", "resp_t", "duration", "item","time") %>% 
  filter(block_id == "8") %>% 
  filter(sender == "Response") %>% 
  mutate(bias = case_when(str_detect(item, "tent")~ "tent",
                          str_detect(item, "dent")~ "dent"))

# write output to csvs
write_csv(data_full, "data/data_full.csv")
write_csv(data_test, "data/data_test.csv")
write_csv(data_exposure, "data/data_exposure.csv")
write_csv(questionnaire, "data/questionnaire.csv")

