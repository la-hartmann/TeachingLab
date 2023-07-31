library(googledrive)
library(googlesheets4)
library(tidyverse)

fall_freire <- read_sheet("https://docs.google.com/spreadsheets/d/1K7s8WhhStppogW9Ig4DWFj3POGi8-eZIUtG-EWhR45w/edit#gid=0", sheet = 1)
fall_d11 <- read_sheet("https://docs.google.com/spreadsheets/d/1iKPFqDnGzxnxPaa0EpJPJdEg8YDbxCvmIg2gNjH2E-Y/edit#gid=1145074088", sheet = 1, 
                       skip = 1)
fall_allOther <- read_sheet("https://docs.google.com/spreadsheets/d/1WFwDKQ74ibnLVHtt1p1-xcxVpivXT7wT1opmw17ktws/edit#gid=0", sheet = 1)
fall_massacheussets <- read_sheet("https://docs.google.com/spreadsheets/d/1VxVwFU_BQ3p2eybGSGOBiVDoDV7sRYcD9CNwMldZ0cw/edit#gid=0", sheet = 1)
needsAssessment_allOther <- read_sheet("https://docs.google.com/spreadsheets/d/1xlP7wRgBwcwhhLjypVnR6X-XmwJMgGHBtPtrCqPqQ1U/edit#gid=724018753", sheet = 1)
needsAssessment_delaware <- read_sheet("https://docs.google.com/spreadsheets/d/1XWteLXnOnFN2ulvB5P0RpmXT6uYuEc43LWHhlsZ2kss/edit#gid=0", sheet = 1)
spring_allOther <- read_sheet("https://docs.google.com/spreadsheets/d/1j-qLYFM9fU0ox3N-3sX5dHSJOIayTluWQFk2h_mfZTU/edit#gid=0", sheet = 1)
fall_edSurvey <- read_sheet("https://docs.google.com/spreadsheets/d/1956U1s3n31fD5o0FbNjnfqt0OjqnQGvZVrhTpnE_shg/edit#gid=0", sheet = 1)

spring_d11 <- read_sheet("https://docs.google.com/spreadsheets/d/1VyHisJabXasKIt-5vXqzGoZJbckpezodHZteCHrQuzM/edit#gid=1145200258",
                         sheet = 1, skip = 1)

#### Initial look at data indicates that start date, end date, ip address, and id are best joins

# Fall Freire
# Does this really matter??? 7 observations is kind of worthless
# Here is the general data pattern
# 0. Made some copies in google sheets and used those to do some data manipulation
# 1. Remove additional row
# 2. Select out unnecessary columns
# 3. Mutate in new identifier columns
# 4. Rename columns to standardize
# 5. Mutate strings for standardization

colnames(fall_freire)[1] <- "Respondent ID"
fall_freire <- fall_freire %>% 
  slice(-1) %>%
  select(-c(2,4,5)) %>%
  mutate(season = "Fall",
         survey_group = "Fall Freire") %>%
  mutate(across(everything(), ~ as.character(.)))
  # rename_with( ~ str_remove_all(.x, "...[:digit:]"))

# Fall d11
## Replacement vector for uppercase second words
## This replaces capital untrue or true with lowercase when there is a space before it
truth_replace <- c("(?<= )Untrue" = "untrue",
                   "(?<= )True" = "true")

colnames(fall_d11)[1] <- "Survey ID"

fall_d11 <- fall_d11 %>%
  select(-4) %>%
  mutate(survey_group = "Fall D11",
         season = "Fall") %>%
  rename(Role = `How would you define your PRIMARY role at your current school? - Selected Choice`,
         School = `School Name`) %>%
  mutate(across(c(5:9), ~ str_to_title(.)),
         across(c(5:9), ~ str_replace_all(.x, truth_replace))) %>%
  mutate(across(everything(), ~ as.character(.)))

colnames(spring_d11)[1] <- "Survey ID"

spring_d11 <- spring_d11 %>%
  select(-4) %>%
  mutate(survey_group = "Spring D11",
         season = "Spring") %>%
  rename(Role = `How would you define your PRIMARY role at your current school? - Selected Choice`) %>%
  mutate(across(c(5:9), ~ str_to_title(.)),
         across(c(5:9), ~ str_replace_all(.x, truth_replace))) %>%
  mutate(across(everything(), ~ as.character(.)))
# Fall Massachuessets
## Vector replacement list
curriculum_replacement <- c("Mathematics (Illustrative Mathematics)" = "Illustrative Mathematics",
                            "ELA/ Literacy (EL Education)" = "EL")

colnames(fall_massacheussets)[1] <- "Respondent ID"
fall_massacheussets <- fall_massacheussets %>% 
  slice(-1) %>%
  select(-c(2, `Custom Data 1`, 10)) %>% # Select out collector ID, weird survey id, and NA custom data column
  mutate(survey_group = "Fall Massacheussets",
         season = "Fall") %>%
  rename(Curriculum = `Which subject area and curriculum do you work with Teaching Lab on this year?`) %>%
  mutate(Curriculum = str_replace_all(Curriculum, curriculum_replacement)) %>%
  mutate(across(everything(), ~ as.character(.)))

# Fall all other
# Duplicate: Bharbour@k12.wv.us, Rohrig

colnames(fall_allOther)[1] <- "Respondent ID"
fall_allOther <- fall_allOther %>% 
  slice(-1) %>%
  select(-c(2)) %>%
  mutate(survey_group = "Fall All Other",
         season = "Fall") %>%
  rename(Site = `Custom Data 1`, `Role` = `Custom Data 3`, `Grades` = `Custom Data 4`, `School Name` = `Custom Data 2`) %>%
  mutate(across(everything(), ~ as.character(.)))

# Spring all other

colnames(spring_allOther)[1] <- "Respondent ID"
spring_allOther <- spring_allOther %>%
  slice(-1) %>%
  select(c(-2, `Custom Data 3`)) %>% # Select out weird dates and collector id
  rename(Site = `Custom Data 1`, `Role` = `Custom Data 5`, `Grades` = `Custom Data 6`, Curriculum = `Custom Data 2`,
         `School Name` = `Custom Data 4`) %>%
  rename_with( ~ str_remove_all(.x, "...[:digit:]")) %>%
  mutate(season = "Spring",
         survey_group = "Spring All Other",
         Curriculum = str_replace_all(Curriculum, "Expeditionary Learning (EL)", "EL")) %>%
  mutate(across(everything(), ~ as.character(.)))

# Fall ed survey

# colnames(fall_edSurvey)[1]
## Remove weird string details with number_ format
## Replacement vector
curriculum_replacement_2 <- c("GB" = "Guidebooks",
                              "IM" = "Illustrative Mathematics",
                              "EL" = "EL",
                              "State-level work" = "State-level")
### NOT SURE WHAT CKLA IS

fall_edSurvey <- fall_edSurvey %>%
  select(-`MS10-Code`) %>%
  mutate(across(c(6:14), ~ str_remove_all(.x, "[:digit:]_"))) %>%
  mutate(survey_group = "Fall Ed Survey",
         season = "Fall",
         Curriculum = str_replace_all(Curriculum, curriculum_replacement_2)) %>%
  mutate(across(everything(), ~ as.character(.)))

## Joining custom data columns surveys
# Sort the data from least to most na column ->
fall_join <- fall_allOther %>%
  # full_join(fall_edSurvey) %>%
  full_join(fall_massacheussets)
  # full_join(fall_freire)

d11_join <- fall_d11 %>%
  full_join(spring_d11)

# # IP ADDRESS JOIN TEST
# test <- fall_edSurvey %>%
#   full_join(fall_allOther, by = c("IP Address", "Site" = "Custom Data 1", "Role" = "Custom Data 3", "Grades" = "Custom Data 4"))

# fall_spring_join <- fall_join %>%
#   full_join(spring_allOther)

### ALL DATAFRAMES TOGETHER
# length(na.omit(fall_spring_join$`IP Address`)) - length(unique(na.omit(fall_spring_join$`IP Address`))) # Difference of 656 means 328 possible merges
# length(fall_spring_join$`Email Address`) - length(unique(fall_spring_join$`Email Address`)) # Difference of 750 means 750 possible merges

test_join <- fall_allOther %>%
  full_join(spring_allOther)

# length(test_join$`IP Address`) - length(unique(test_join$`IP Address`)) # Difference of 281 means 281 possible merges
# length(test_join$`Email Address`) - length(unique(test_join$`Email Address`)) # Difference of 119 means 119 possible merges
# 
# length(d11_join$`Survey ID`) - length(unique(d11_join$`Survey ID`)) # 43 matches

fall_emails <- test_join %>% filter(season == "Fall") %>% select(`Email Address`) %>% as_vector()
spring_emails <- test_join %>% filter(season == "Spring") %>% select(`Email Address`) %>% as_vector()

sum(fall_emails %in% spring_emails, na.rm = T) # 118 MATCHES

d11_fall_ids <- d11_join %>% filter(season == "Fall") %>% select(`Survey ID`) %>% as_vector()
d11_spring_ids <- d11_join %>% filter(season == "Spring") %>% select(`Survey ID`) %>% as_vector()
sum(d11_fall_ids %in% d11_spring_ids, na.rm = T) # 25 matches
#### 143 MATCHES TOTAL

##### IP ADDRESS STUFF
fall_ip <- test_join %>% filter(season == "Fall") %>% select(`IP Address`, `Email Address`) %>% as_vector()
spring_ip <- test_join %>% filter(season == "Spring") %>% select(`IP Address`, `Email Address`) %>% as_vector()

fall_ip <- test_join %>% filter(season == "Fall") %>% select(`IP Address`, `Email Address`) %>% rownames_to_column()
spring_ip <- test_join %>% filter(season == "Spring") %>% select(`IP Address`, `Email Address`) %>% rownames_to_column()
fall_spring_ip <- bind_cols(fall_ip, spring_ip)
sum(fall_ip %in% spring_ip, na.rm = T)




# SUMMARY TABLE



