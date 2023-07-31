library(tidyverse)
library(here)
# Read in data
moodle_csv <- read_csv(here("Data/Moodle Manual Downloads/Results.csv"))
moodle_csv2 <- read_csv(here("Data/Moodle Manual Downloads/ResultsApril.csv"))
moodle_csv3 <- read_csv(here("Data/Moodle Manual Downloads/Results (9).csv"))
moodle_csv4 <- read_csv(here("Data/Moodle Manual Downloads/Results (10).csv"))
moodle_csv5 <- read_csv(here("Data/Moodle Manual Downloads/Results (12).csv"))
moodle_csv6 <- read_csv(here("Data/Moodle Manual Downloads/Results (13).csv"))
moodle_csv7 <- read_csv(here("Data/Moodle Manual Downloads/Results (15).csv"))

# numbers <- c("5","4","3","2","1")
# likerts <- c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree")

moodle_data_fix <- function(df) {
  # Separating out so we can bind rows
  new_questions <<- df %>%
    select(response10, response2, response11, response12, response13) %>%
    select(`How, if in any way, this course helped you prepare for school opening after COVID-19?` = response2,
           `The independent online work activities were well-designed to help me meet the learning targets.` = response10,
           `The Zoom meeting activities were well-designed to help me meet the learning targets.` = response11,
           `I felt a sense of community with the other participants in this course even though we were meeting virtually.` = response12,
           `This course helped me navigate remote and/or hybrid learning during COVID-19.` = response13) %>% 
    mutate(across(!c(1), ~ str_replace_all(.x, "5", "Strongly agree"))) %>%
    mutate(across(!c(1), ~ str_replace_all(.x, "4", "Agree"))) %>%
    mutate(across(!c(1), ~ str_replace_all(.x, "3", "Neither agree nor disagree"))) %>%
    mutate(across(!c(1), ~ str_replace_all(.x, "2", "Disagree"))) %>%
    mutate(across(!c(1), ~ str_replace_all(.x, "1", "Strongly disagree")))
  
  # Renaming all the data and selecting out question columns
  moodle_rename <- df %>%
    rename(`Date for the session` = date,
           `District, Parish, Or Network` = partner,
           `Name Of Your Facilitator` = teacher,
           `Professional Training Session` = course,
           `What is the learning from this professional learning that you are most excited about trying out?` = response1,
           `Overall, what went well in this professional learning?` = `response3`,
           `Which activities best supported your learning?` = `response4`,
           `What could have improved your experience?` = `response5`,
           `Why did you choose this rating?` = `response6`,
           `Do you have additional comments?` = `response7`,
           `% Satisfied With The Overall Quality Of Today's Professional Learning Session` = `response9`,
           `% Who Say Today's Topic Was Relevant For My Role` = `response10`,
           `How Likely Are You To Apply This Learning To Your Practice In The Next 4-6 Weeks?` = `response14`,
           `How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?` = `response15`,
           `S/He Facilitated The Content Clearly` = `response16`,
           `S/He Effectively Built A Community Of Learners` = `response17`) %>%
    select(-c(portfolio, question1, question2, question3, question4, question5, question6, question7, question8,
              question9, question11, question12, question13, question14, question15, question16, question17,
              response2, response11, response12, response13)) %>%
    mutate(Portfolio = case_when(!str_detect(`Professional Training Session`, c("EL|IM|Guidebooks|GuideBooks")) == T ~ "State-Level",
                                 str_detect(`Professional Training Session`, "IM") == T ~ "Illustrative Mathematics",
                                 str_detect(`Professional Training Session`, "Guidebooks|GuideBooks") == T ~ "Guidebooks",
                                 str_detect(`Professional Training Session`, "EL") == T ~ "EL"))
  
  cols_agree <- c("% Who Say Today's Topic Was Relevant For My Role",
                  "How Likely Are You To Apply This Learning To Your Practice In The Next 4-6 Weeks?",
                  "S/He Facilitated The Content Clearly",
                  "S/He Effectively Built A Community Of Learners",
                  "% Satisfied With The Overall Quality Of Today's Professional Learning Session")
  
  factor_cols <- map_df(moodle_rename[cols_agree], ~ as.factor(.x))
  
  refactor_cols <- factor_cols %>% 
    mutate_all( ~ str_replace_all(., "5", "Strongly agree")) %>%
    mutate_all( ~ str_replace_all(., "4", "Agree")) %>%
    mutate_all( ~ str_replace_all(., "3", "Neither agree nor disagree")) %>%
    mutate_all( ~ str_replace_all(., "2", "Disagree")) %>%
    mutate_all( ~ str_replace_all(., "1", "Strongly disagree"))
  
  moodle_reformat <- moodle_rename %>%
    select(-c("% Who Say Today's Topic Was Relevant For My Role",
              "How Likely Are You To Apply This Learning To Your Practice In The Next 4-6 Weeks?",
              "S/He Facilitated The Content Clearly",
              "S/He Effectively Built A Community Of Learners",
              "% Satisfied With The Overall Quality Of Today's Professional Learning Session")) %>%
    bind_cols(refactor_cols, new_questions) %>%
    # Change to character here for joining purposes in shiny app
    mutate(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?` = as.character(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`),
           `How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?` = str_replace_all(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`, "11", "10")) %>%
    dplyr::filter(`What could have improved your experience?` != lag(`What could have improved your experience?`),
                  `Which activities best supported your learning?` != lag(`Which activities best supported your learning?`)) %>%
    mutate(`Name Of Your Facilitator` = str_remove_all(`Name Of Your Facilitator`, " -"),
           `Name Of Your Facilitator` = str_replace_all(`Name Of Your Facilitator`, "meredith starks", "Meredith Starks"),
           `Name Of Your Facilitator` = str_replace_all(`Name Of Your Facilitator`, "Andrea Fitz(?!g)", "Andrea Fitzgerald"),
           `Name Of Your Facilitator` = str_replace_all(`Name Of Your Facilitator`, "Kyra Caldwell Templeton, Ph.D.", "Kyra Templeton")) %>%
    filter(`Name Of Your Facilitator` != "Test Lead FacilitatorAnother Lead Facilitator") %>%
    # Reformat facilitator names
    # Regex several last names for not followed by space to have multiple spaces like data already has, so that the names can be split 
    # then split, unnest for duplication and filter out empties, then trim blank space at end of strings
    mutate(`Name Of Your Facilitator` = case_when(str_detect(`Name Of Your Facilitator`, "Williams(?! )") == T ~ 
                                                    str_replace(`Name Of Your Facilitator`, "Williams", "Williams  "),
                                                  str_detect(`Name Of Your Facilitator`, "Denning(?! )") == T ~ 
                                                    str_replace(`Name Of Your Facilitator`, "Denning", "Denning  "),
                                                  str_detect(`Name Of Your Facilitator`, "Weldon(?! )") == T ~ 
                                                    str_replace(`Name Of Your Facilitator`, "Weldon", "Weldon  "),
                                                  str_detect(`Name Of Your Facilitator`, "Anderson(?! )") == T ~ 
                                                    str_replace(`Name Of Your Facilitator`, "Anderson", "Anderson  "),
                                                  str_detect(`Name Of Your Facilitator`, "Fears(?! )") == T ~ 
                                                    str_replace(`Name Of Your Facilitator`, "Fears", "Fears  "),
                                                  str_detect(`Name Of Your Facilitator`, "Rushton(?! )") == T ~ 
                                                    str_replace(`Name Of Your Facilitator`, "Rushton", "Rushton  "),
                                                  str_detect(`Name Of Your Facilitator`, "Seeger(?! )") == T ~ 
                                                    str_replace(`Name Of Your Facilitator`, "Seeger", "Seeger  "),
                                                  str_detect(`Name Of Your Facilitator`, "Hoesen(?! )") == T ~ 
                                                    str_replace(`Name Of Your Facilitator`, "Hoesen", "Hoesen  "),
                                                  str_detect(`Name Of Your Facilitator`, "Fitzgerald(?! )") == T ~ 
                                                    str_replace(`Name Of Your Facilitator`, "Fitzgerald", "Fitzgerald  "),
                                                  str_detect(`Name Of Your Facilitator`, "Herring(?! )") == T ~ 
                                                    str_replace(`Name Of Your Facilitator`, "Herring", "Herring  "),
                                                  str_detect(`Name Of Your Facilitator`, "Taylor(?! )") == T ~ 
                                                    str_replace(`Name Of Your Facilitator`, "Taylor", "Taylor  "),
                                                  str_detect(`Name Of Your Facilitator`, "Silverthorne(?! )") == T ~ 
                                                    str_replace(`Name Of Your Facilitator`, "Silverthorne", "Silverthorne  "),
                                                  str_detect(`Name Of Your Facilitator`, "Walls(?! )") == T ~ 
                                                    str_replace(`Name Of Your Facilitator`, "Walls", "Walls  "),
                                                  str_detect(`Name Of Your Facilitator`, "Endicott(?! )") == T ~ 
                                                    str_replace(`Name Of Your Facilitator`, "Endicott", "Endicott  "),
                                                  TRUE ~ as.character(`Name Of Your Facilitator`))) %>%
    mutate(`Name Of Your Facilitator` = case_when(str_detect(`Name Of Your Facilitator`, "(?<=[:alpha:])Christi") == T ~ 
                                                    str_replace(`Name Of Your Facilitator`, "Christi", "  Christi"),
                                                  str_detect(`Name Of Your Facilitator`, "(?<=[:alpha:])Erin") == T ~ 
                                                    str_replace(`Name Of Your Facilitator`, "Erin", "  Erin"),
                                                  str_detect(`Name Of Your Facilitator`, "Silverthorne(?! )") == T ~ 
                                                    str_replace(`Name Of Your Facilitator`, "Silverthorne", "Silverthorne  "),
                                                  str_detect(`Name Of Your Facilitator`, "Rushton(?! )") == T ~ 
                                                    str_replace(`Name Of Your Facilitator`, "Rushton", "Rushton  "),
                                                  str_detect(`Name Of Your Facilitator`, "Satyal(?! )") == T ~ 
                                                    str_replace(`Name Of Your Facilitator`, "Satyal", "Satyal  "),
                                                  str_detect(`Name Of Your Facilitator`, "Taylor(?! )") == T ~ 
                                                    str_replace(`Name Of Your Facilitator`, "Taylor", "Taylor  "),
                                                  TRUE ~ as.character(`Name Of Your Facilitator`))) %>%
    mutate(`Name Of Your Facilitator` = str_trim(`Name Of Your Facilitator`),
           `Name Of Your Facilitator` = case_when(str_detect(`Name Of Your Facilitator`, "  ") == T ~ strsplit(`Name Of Your Facilitator`, "  "),
                                                  TRUE ~ as.list(`Name Of Your Facilitator`))) %>%
    unnest(`Name Of Your Facilitator`) %>%
    filter(`Name Of Your Facilitator` != "")
}


first_time_df <- moodle_data_fix(moodle_csv)
# Add april data, and a bit of march
moodle_april <- moodle_data_fix(moodle_csv2 %>% 
                           dplyr::filter(date > max(moodle_csv$date)))

# Add second half of april data, and a tiny bit of May
moodle_april_may <- moodle_data_fix(moodle_csv3 %>%
                                      dplyr::filter(date > max(moodle_csv2$date)))

# Add first half of May data
moodle_april_may <- moodle_data_fix(moodle_csv4 %>%
                                      dplyr::filter(date > max(moodle_csv3$date)))
# Second half of May data
moodle_may <- moodle_data_fix(moodle_csv5 %>%
                                dplyr::filter(date > max(moodle_csv4$date)))
# Early June Data
moodle_june <- moodle_data_fix(moodle_csv6 %>%
                                dplyr::filter(date > max(moodle_csv5$date)))

# Mid June Data
moodle_june <- moodle_data_fix(moodle_csv7 %>%
                                 dplyr::filter(date > max(moodle_csv6$date)))



# All Moodle Data
moodle_reformat <- bind_rows(moodle_june, moodle_may, moodle_april_may, moodle_april, first_time_df) %>%
  mutate(`District, Parish, Or Network` = case_when(str_detect(`Professional Training Session`, "Cleveland|Calcasieu") == T ~ 
                                                      "Cleveland Metropolitan School District, OH",
                                                    str_detect(`Professional Training Session`, "Coupee") == T ~ 
                                                      "Providence Public Schools, RI",
                                                    str_detect(`Professional Training Session`, "Lafayette") == T ~ 
                                                      "Legacy Early College, SC",
                                                    str_detect(`Professional Training Session`, "PS121") == T ~ 
                                                      "NYC District 11, PS 121",
                                                    str_detect(`Professional Training Session`, "Kankakee") == T ~ 
                                                      "Louisville School District - Jacob Elementary, KY",
                                                    str_detect(`Professional Training Session`, "Tangi") == T ~ 
                                                      "Washington Parish, LA",
                                                    T ~ as.character(`District, Parish, Or Network`)))


# All New Questions Separately
new_questions <- read_rds(here("Data/Dashboard Data/moodle_new_questions.rds")) %>%
  bind_rows(new_questions)

write_rds(moodle_reformat, here("Data/Dashboard Data/moodle_export_reformat.rds"))
write_rds(new_questions, here("Data/Dashboard Data/moodle_new_questions.rds"))

### DATA ISSUES - 3 DATES WRONG - FIXED
### Missing columns - questions dropped?
### Changed questions phrasing
### No grade bands
### Lots of duplicates for no apparent reason




