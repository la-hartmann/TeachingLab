# library(tidyverse)
# library(googlesheets4)
# library(here)
# 
# # Read in data
# 
# big_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/1xfhI6jwUpNdAg4cE3BVrBckvhgV7pgD4ryS4kQDI4S0/edit#gid=704521182",
#                         skip = 1,
#                         sheet = "Form Responses")
# delaware_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/1voIeS_I2GGLRv89z8eFHy21MzPTiLyCbTjMDhQhVqxc/edit#gid=1850278171",
#                              sheet = "Form Responses 1")
# 
# ## Cleaning, basically renaming
# big_sheet_join <- big_sheet %>%
#   rename(Date = `Select the date for the Zoom meeting for this course.`,
#          `Please select the focus of the session you attended today.` = `Portfolio 1`,
#          `S/he facilitated the content clearly #1` = `S/he facilitated the content clearly....16`,
#          `S/he facilitated the content clearly #2` = `S/he facilitated the content clearly....20`,
#          `S/he effectively built a community of learners #1` = `S/he effectively built a community of learners....17`,
#          `S/he effectively built a community of learners #2` = `S/he effectively built a community of learners....21`) %>%
#   mutate(`How, if in any way, this course helped you navigate remote and/or hybrid learning during COVID-19?` = as.character(`How, if in any way, this course helped you navigate remote and/or hybrid learning during COVID-19?`),
#          `Why did you choose this rating?` = as.character(`Why did you choose this rating?`))
# delaware_sheet_join <- delaware_sheet %>%
#   select(-Score) %>%
#   rename(Date = `Select the date for this training`,
#          `I am satisfied with the overall quality of this course.` = `I am satisfied with the overall quality of today’s professional learning session.`,
#          `The topics for this course were relevant for my role.` = `Today’s topic was relevant for my role.`,
#          `The Zoom course activities were well-designed to help me meet the learning targets.` = `The Zoom meeting activities were well-designed to help me meet the learning targets.`,
#          `This course helped me navigate remote and/or hybrid learning during COVID-19.` = `This session helped me navigate remote and/or hybrid learning during COVID-19.`,
#          `Which activities best supported your learning?` = `Which activities best supported your learning today?`,
#          `Do you have additional comments about this course?` = `Do you have additional comments about today?`,
#          `Do you give us permission to include your feedback in promotional materials?` = `Do you give us permission to include your feedback about this course in promotional materials? Only your role and site will be included.`,
#          `How likely are you to recommend this professional learning to a colleague or friend?` = `How likely are you to recommend this professional learning session to a colleague or friend?`,
#          `I felt a sense of community with the other participants in this course even though we were virtual.` = `I felt a sense of community with the other participants in this course even though we were meeting virtually.`,
#          `Overall, what went well in this course?` = `Overall, what went well in today’s professional learning session?`,
#          `S/he effectively built a community of learners #1` = `S/he effectively built a community of learners....15`,
#          `S/he effectively built a community of learners #2` = `S/he effectively built a community of learners....19`,
#          `S/he effectively built a community of learners #3` = `S/he effectively built a community of learners....23`,
#          `S/he facilitated the content clearly #1` = `S/he facilitated the content clearly....14`,
#          `S/he facilitated the content clearly #2` = `S/he facilitated the content clearly....18`,
#          `S/he facilitated the content clearly #3` = `S/he facilitated the content clearly....22`,
#          `Select your course.` = `Please select the training session you attended today.`,
#          `What could have improved your experience?` = `What could have improved your experience today?`) %>%
#   mutate(`Select your site (district, parish, network, or school).` = "Delaware Professional Learning")
# 
# sheets_data <- full_join(big_sheet_join, delaware_sheet_join)
# 
# community_content_second <- sheets_data %>%
#   filter(`Did you have a second facilitator?` == "Yes") %>%
#   dplyr::select(!c(`S/he facilitated the content clearly #1`, 
#                    `S/he effectively built a community of learners #1`,
#                    `S/he facilitated the content clearly #3`, 
#                    `S/he effectively built a community of learners #3`,
#                    `Select the name of your first facilitator.`,
#                    `Select the name of your third facilitator.`)) %>%
#   rename(`S/He Facilitated The Content Clearly` = `S/he facilitated the content clearly #2`,
#          `S/He Effectively Built A Community Of Learners` = `S/he effectively built a community of learners #2`,
#          `Name Of Your Facilitator` = `Select the name of your second facilitator.`)
# 
# community_content_third <- sheets_data %>%
#   filter(`Did you have a third facilitator?` == "Yes") %>%
#   dplyr::select(!c(`S/he facilitated the content clearly #1`, 
#                    `S/he effectively built a community of learners #1`,
#                    `S/he facilitated the content clearly #2`, 
#                    `S/he effectively built a community of learners #2`,
#                    `Select the name of your second facilitator.`,
#                    `Select the name of your first facilitator.`)) %>%
#   rename(`S/He Facilitated The Content Clearly` = `S/he facilitated the content clearly #3`,
#          `S/He Effectively Built A Community Of Learners` = `S/he effectively built a community of learners #3`,
#          `Name Of Your Facilitator` = `Select the name of your third facilitator.`)
# 
# sheets_data_merge <- sheets_data %>%
#   rename(`S/He Facilitated The Content Clearly` = `S/he facilitated the content clearly #1`,
#          `S/He Effectively Built A Community Of Learners` = `S/he effectively built a community of learners #1`,
#          `Name Of Your Facilitator` = `Select the name of your first facilitator.`) %>%
#   select(-c(`Select the name of your second facilitator.`,
#             `S/he facilitated the content clearly #2`, `S/he effectively built a community of learners #2`, `Did you have a third facilitator?`,
#             `Select the name of your third facilitator.`, `S/he facilitated the content clearly #3`, `S/he effectively built a community of learners #3`)) %>%
#   mutate(Portfolio = case_when(str_detect(`Please select the focus of the session you attended today.`, "IM|Math") == T ~ "Illustrative Mathematics",
#                                str_detect(`Please select the focus of the session you attended today.`, "EL") == T ~ "EL",
#                                str_detect(`Please select the focus of the session you attended today.`, "Guidebooks|GuideBooks") == T ~ "Guidebooks",
#                                !str_detect(`Please select the focus of the session you attended today.`, c("EL|IM|Guidebooks|GuideBooks")) == T ~ "State-Level"),
#          `Select your course.` = paste0(`Portfolio`, " ", `Select your course.`)) %>%
#   bind_rows(community_content_second, community_content_third) %>%
#   select(-c(`Please select the focus of the session you attended today.`, `Timestamp`, `Select the best description for your role.`,
#          `If you would like to speak to our team further about your experience at today’s training, please share your email address and we will reach out to you shortly.`,
#          `Do you give us permission to include your feedback in promotional materials?`,
#          `Did you have a third facilitator?`)) %>%
#   rename(`Date for the session` = `Date`,
#          `Professional Training Session` = `Select your course.`,
#          `District, Parish, Or Network` = `Select your site (district, parish, network, or school).`,
#          `% Satisfied With The Overall Quality Of Today's Professional Learning Session` = `I am satisfied with the overall quality of this course.`,
#          `% Who Say Today's Topic Was Relevant For My Role` = `The topics for this course were relevant for my role.`,
#          `The Zoom meeting activities were well-designed to help me meet the learning targets.` = `The Zoom course activities were well-designed to help me meet the learning targets.`,
#          `What is the learning from this professional learning that you are most excited about trying out?` = `What is the learning from this course that you are most excited about trying out?`,
#          `How Likely Are You To Apply This Learning To Your Practice In The Next 4-6 Weeks?` = `How likely are you to apply this learning to your practice in the next 4-6 weeks?`,
#          `Did You Have A Second Facilitator?` = `Did you have a second facilitator?`,
#          `Overall, what went well in this professional learning?` = `Overall, what went well in this course?`,
#          `Which activities best supported your learning?` = `Which activities best supported your learning?`,
#          `What could have improved your experience?` = `What could have improved your experience?`,
#          `How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?` = `How likely are you to recommend this professional learning to a colleague or friend?`,
#          `Why did you choose this rating?` = `Why did you choose this rating?`,
#          `Do you have additional comments?` = `Do you have additional comments about this course?`,
#          `How, if in any way, this course helped you prepare for school opening after COVID-19?` = `How, if in any way, this course helped you navigate remote and/or hybrid learning during COVID-19?`,
#          `The independent online work activities were well-designed to help me meet the learning targets.` = `The independent online work activities were well-designed to help me meet the learning targets.`,
#          `The Zoom meeting activities were well-designed to help me meet the learning targets.` = `The Zoom course activities were well-designed to help me meet the learning targets.`,
#          `I felt a sense of community with the other participants in this course even though we were meeting virtually.` = `I felt a sense of community with the other participants in this course even though we were virtual.`,
#          `This course helped me navigate remote and/or hybrid learning during COVID-19.` = `This course helped me navigate remote and/or hybrid learning during COVID-19.`) %>%
#   mutate(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?` = as.numeric(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`))
# 
# sheets_data_merge_final <- sheets_data_merge %>%
#   mutate(`Date for the session` = str_replace(`Date for the session`, "0021-04-21", "2021-04-21")) %>%
#   mutate(across(c(4, 5, 6, 7, 8, 9, 12, 14, 15), ~ str_replace_all(.x, "5", "Strongly agree"))) %>%
#   mutate(across(c(4, 5, 6, 7, 8, 9, 12, 14, 15), ~ str_replace_all(.x, "4", "Agree"))) %>%
#   mutate(across(c(4, 5, 6, 7, 8, 9, 12, 14, 15), ~ str_replace_all(.x, "3", "Neither agree nor disagree"))) %>%
#   mutate(across(c(4, 5, 6, 7, 8, 9, 12, 14, 15), ~ str_replace_all(.x, "2", "Disagree"))) %>%
#   mutate(across(c(4, 5, 6, 7, 8, 9, 12, 14, 15), ~ str_replace_all(.x, "1", "Strongly disagree")))
# 
# 
# write_rds(sheets_data_merge_final, here("Data-Clean/Data-move/Dashboard Data/sheets_data_merge.rds"))
# 
# write_rds(sheets_data_merge_final, here("dashboards/ParticipantFeedback/Data/sheets_data_merge.rds"))
# 
# 
# 
# 
# 
# 
# 
# 
