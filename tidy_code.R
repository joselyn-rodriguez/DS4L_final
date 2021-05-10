library(tidyverse)

########################################################################################################################
# This file is for cleaning and saving tidied data                                                                     #
# The data was first pulled out from the csvs into one giant file - the data_full then further separated based on need #
########################################################################################################################



#### Removing workerId information 

# as long as I include all this code about removing worker Ids, it won't run for you because you don't have the ids so I've 
# commented it out, but this is how I did it (thank you, https://stackoverflow.com/questions/61638927/r-dplyr-replace-unique-values-from-one-data-frame-with-unique-values-from-other)
 
# set.seed(2)
# worker_id_list <- unique(data_full$workerId)
# id_list <-  sample(1000:4000, 60, replace = FALSE)
# num <-  1
# id_lookup = tibble(workerId = unique(data_full$workerId), new_ID = unique(id_list))
# 
# data_raw <- read_csv(here::here("data/batch1.csv"), col_types = cols(sender = col_character(),
#                                                                      workerId = col_character(), audioequip = col_character(),
#                                                                      sex = col_character(), ethnicity = col_character(),
#                                                                      race = col_character(), raceother = col_character(),
#                                                                      born = col_character(), parent = col_character(),
#                                                                      lang = col_character(), comments = col_character(),
#                                                                      age = col_number())) %>%
#                         left_join(.,id_lookup, by = "workerId") %>%     # since none of the files include the workerIds anymore, this will not run
#                         select(-c(workerId, session, meta, url))

#### Code for tidying the raw data sets 

# block_id, trial, resp are all created variables
data_full <- read_csv(here::here("data/data_deidentified.csv")) %>% 
  select("new_ID","condition","stimulus","response","sender","sender_id","block", "duration", "item", "item_block", "time") %>% 
  filter(sender == "Sounds" | sender == "Response") %>% 
  mutate(block_id = substr(sender_id,1,2),
         resp_t = case_when(substr(response, 1,1) == "t" ~ "TRUE",
                            substr(response, 1,1) == "d" ~ "FALSE"),
         vot = substr(stimulus, 11, 12)) 



questionnaire <- read_csv(here::here("data/data_deidentified.csv"), col_types = cols(sender = col_character(), 
                                                                                     new_ID = col_character(), audioequip = col_character(), 
                                                                                     sex = col_character(), ethnicity = col_character(), 
                                                                                     race = col_character(), raceother = col_character(), 
                                                                                     born = col_character(), parent = col_character(), 
                                                                                     lang = col_character(), comments = col_character(), 
                                                                                     age = col_number())) %>% 
  select("sender", "new_ID", "audioequip":"comments") %>% 
  filter(sender == "Quesionnaire Form")


# this is the data for just the pre- and post-tests 
data_test <- data_full %>% 
  select("new_ID","condition","stimulus","response","sender","sender_id","duration", "block_id", "block", "vot", "resp_t") %>% 
  mutate(block = case_when(block_id=="6_"~ "pre",    # separating out pre and post data
                           block_id=="10"~ "post")) %>% 
  filter(block == "pre" | block == "post") %>% 
  filter(sender == "Sounds")


# uncomment if you want to re-save the data but it's available already as csvs
# write_csv(data_full, "data_tidy/data_full_tidy.csv")
# write_csv(data_test, "data_tidy/data_test_tidy.csv")
# write_csv(questionnaire, "data_tidy/questionnaire_tidy.csv")
# write_csv(data_raw, "data/data_deidentified.csv")

