# library(tidyverse)
# library(googlesheets4)
# library(here)
# 
# # Read in google data
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
#          `What could have improved your experience?` = `What could have improved your experience today?`#,
#          # `What is the learning from this course that you are most excited about trying out?` = `What is the learning from today that you are most excited about trying out?`
#          ) %>%
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
#             `If you would like to speak to our team further about your experience at today’s training, please share your email address and we will reach out to you shortly.`,
#             # `Select the grade-band(s) you focused on today.`, 
#             `Do you give us permission to include your feedback in promotional materials?`,
#             `Did you have a third facilitator?`)) %>%
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
#   mutate(across(c(4, 5, 6, 7, 8, 9, 12, 14, 15), ~ str_replace_all(.x, "5", "Strongly agree"))) %>%
#   mutate(across(c(4, 5, 6, 7, 8, 9, 12, 14, 15), ~ str_replace_all(.x, "4", "Agree"))) %>%
#   mutate(across(c(4, 5, 6, 7, 8, 9, 12, 14, 15), ~ str_replace_all(.x, "3", "Neither agree nor disagree"))) %>%
#   mutate(across(c(4, 5, 6, 7, 8, 9, 12, 14, 15), ~ str_replace_all(.x, "2", "Disagree"))) %>%
#   mutate(across(c(4, 5, 6, 7, 8, 9, 12, 14, 15), ~ str_replace_all(.x, "1", "Strongly disagree")))
# 
# 
# write_rds(sheets_data_merge_final, here("data/merged/sheets_data_merge.rds"))
# 
# # write_rds(sheets_data_merge_final, here("ParticipantFeedback/Data/sheets_data_merge.rds"))
# 
# ## Other data
# 
# teaching_df_readin <- read_rds(here("Data-Clean/Data-move/Dashboard Data/original_df.rds")) # Read in the data
# # teaching_df <- read_rds("~/Teaching Lab/Coding/TeachingLab/PieCharter/Data/original_df.rds")
# # Relevant columns
# oldcols <- c(
#   "Professional training session",
#   "Select your site (district, parish, or network).",
#   # "Select the best description for your role.",
#   "Select the grade-band(s) you focused on.",
#   "I am satisfied with the overall quality of today's professional learning session.",
#   "Today's topic was relevant for my role.",
#   "The activities of today's session were well-designed to help me learn.",
#   "How likely are you to apply this learning to your practice in the next 4-6 weeks?",
#   "Select the name of your first facilitator.",
#   "S/he facilitated the content clearly....12",
#   "S/he effectively built a community of learners....13",
#   "Did you have a second facilitator?",
#   "Select the name of your second facilitator.",
#   "S/he facilitated the content clearly....16",
#   "S/he effectively built a community of learners....17",
#   "How likely are you to recommend this professional learning to a colleague or friend?"
# ) # Original column names
# 
# newcols <- str_to_title(c(
#   "Professional training session",
#   "District, parish, or network",
#   # "What is the best description for your role?",
#   # "What grade band(s) do you focus on?",
#   "% satisfied with the overall quality of today's professional learning session",
#   "% Who say today's topic was relevant for my role",
#   "% Who say activities of today's session were well-designed to help me learn",
#   "How likely are you to apply this learning to your practice in the next 4-6 weeks?",
#   "Name of your first facilitator",
#   "S/he facilitated the content clearly (first facilitator)",
#   "S/he effectively built a community of learners (first facilitator)",
#   "Did you have a second facilitator?",
#   "Name of your second facilitator.",
#   "S/he facilitated the content clearly (second facilitator)",
#   "S/he effectively built a community of learners (second facilitator)",
#   "How likely are you to recommend this professional learning to a colleague or friend?"
# )) # New column names
# 
# # Small data clean
# teaching_df_readin <- teaching_df_readin %>%
#   select(
#     # -`Select the grade-band(s) you focused on.`,
#          -`Select the best description for your role.`) %>%
#   rename_with(~ newcols[which(oldcols == .x)], .cols = oldcols) %>%
#   mutate(`Date for the session` = lubridate::ymd(`Date for the session`)) %>%
#   mutate(Portfolio = case_when(!str_detect(`Professional Training Session`, c("EL|IM|Guidebooks|GuideBooks")) == T ~ "State-Level",
#                                str_detect(`Professional Training Session`, "IM") == T ~ "Illustrative Mathematics",
#                                str_detect(`Professional Training Session`, "Guidebooks|GuideBooks") == T ~ "Guidebooks",
#                                str_detect(`Professional Training Session`, "EL") == T ~ "EL"),
#          `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "Grades 6-8;Grades 9-12", "Grades 6-8, Grades 9-12"),
#          `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "All grades K-12", "All Grades"),
#          `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "Grades 9-12, All grades K-12", "All Grades"),
#          `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "Grades K-2, Grades 3-5, Grades 6-8, Grades 9-12, All grades K-12", "All Grades"),
#          `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "Grades K-2, Grades 3-5, Grades 6-8, Grades 9-12", "All Grades"),
#          `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "All Grades, All Grades", "All Grades"))
# 
# # teaching_df <- teaching_df_readin
# 
# # Making the reviews of multiple facilitators in a session into one
# # Split the data
# community_content_second <- teaching_df_readin %>%
#   filter(`Did You Have A Second Facilitator?` == "Yes") %>%
#   dplyr::select(!c(`S/He Facilitated The Content Clearly (First Facilitator)`, 
#                    `S/He Effectively Built A Community Of Learners (First Facilitator)`,
#                    `Name Of Your First Facilitator`)) %>%
#   rename(`S/He Facilitated The Content Clearly` = `S/He Facilitated The Content Clearly (Second Facilitator)`,
#          `S/He Effectively Built A Community Of Learners` = `S/He Effectively Built A Community Of Learners (Second Facilitator)`,
#          `Name Of Your First Facilitator` = `Name Of Your Second Facilitator.`)
# 
# # Name replacement vector
# name_replace <- c("Octavia" = "Octavia Nixon", "Vaishali" = "Vaishali Joshi", "Ryan C" = "Ryan Colon",
#                   "Holli" = "Holli Fears", "Addie" = "Addie Kelley", "Adrienne" = "Adrienne Williams",
#                   "Anita" = "Anita Walls", "Brad" = "Brad Haggerty", "Christi" = "Christi Herring",
#                   "Erin" = "Erin Abraham", "Evan" = "Evan Rushton", "Jalinda" = "Jalinda Soto",
#                   "John" = "John Silverthorne", "Justin" = "Justin Endicott", "Katie" = "Katie Endicott",
#                   "Lauren" = "Lauren Myer", "Lindsay" = "Lindsay Tomlinson", "Lindsey" = "Lindsey Tomlinson",
#                   "Liza" = "Liza Zarifi", "Mandi" = "Mandi Van Dellen", "Mandy" = "Mandy Flora", "Meredith" = "Meredith Starks",
#                   "Rod" = "Rod Naquin", "Sarah" = "Sarah Tierney", "Sheena" = "Sheena Lights", "Ryan S" = "Ryan Mateo Sharnbroich",
#                   "Spencer" = "Spencer Russell", "Stacy" = "Stacy Weldon", "Stephanie" = "Stephanie Carpenter",
#                   "Tamala" = "Tamala Wiley", "Tara" = "Tara McDonald", "Tia" = "Tiayana Marks", "Zoe" = "Zoe Rind",
#                   "Fitz" = "Andrea Fitzgerald")
# 
# # Bind it to original dataframe
# teaching_df_merge <- teaching_df_readin %>%
#   dplyr::select(-c(`S/He Facilitated The Content Clearly (Second Facilitator)`,
#                    `S/He Effectively Built A Community Of Learners (Second Facilitator)`,
#                    `Name Of Your Second Facilitator.`,
#                    -`Did You Have A Second Facilitator?`)) %>%
#   rename(`S/He Effectively Built A Community Of Learners` = `S/He Effectively Built A Community Of Learners (First Facilitator)`,
#          `S/He Facilitated The Content Clearly` = `S/He Facilitated The Content Clearly (First Facilitator)`) %>%
#   bind_rows(community_content_second) %>%
#   rename(`Name Of Your Facilitator` = `Name Of Your First Facilitator`) %>%
#   mutate(`Name Of Your Facilitator` = str_replace_all(`Name Of Your Facilitator`, name_replace))
# 
# # Moodle data merge
# moodle_data <- read_rds(here("Data/moodle_export_reformat.rds")) %>%
#   mutate(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?` = as.numeric(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`))
# # Sheets Data Merge
# sheets_data <- read_rds(here("data/merged/sheets_data_merge.rds")) %>%
#   mutate(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?` = as.numeric(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`),
#          `Date for the session` = as.Date(`Date for the session`))
# 
# teaching_df <- full_join(teaching_df_merge, moodle_data) %>%
#   full_join(sheets_data) %>%
#   mutate(across(c(4:18, 20:24), ~ replace_na(., "No Response"))) %>%
#   mutate(across(c(4:18, 20:24), ~ replace_na(., "No Response"))) %>%
#   mutate(across(c(4:18, 20:24), ~ str_replace_all(.x, "NULL", "No Response")))
# 
# 
# write_rds(teaching_df, here("Data/dashboard_data.rds"))
# 
# write_rds(teaching_df, here("ParticipantFeedback/Data/dashboard_data.rds"))
# 
