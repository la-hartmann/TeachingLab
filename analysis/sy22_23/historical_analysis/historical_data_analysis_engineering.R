library(TeachingLab)
library(tidyverse)


course_survey_21_22 <- get_course_survey(year = "21_22", update = FALSE) # DO NOT UPDATE
course_survey_22_23 <- get_course_survey(year = "22_23", update = TRUE) # DO NOT UPDATE
session_survey_21_22 <- get_session_survey(year = "21_22", update = FALSE) # DO NOT UPDATE
session_survey_22_23 <- get_session_survey(year = "22_23", update = TRUE) # DO NOT UPDATE

renamed_course_survey_21_22 <- course_survey_21_22 |>
  rename(
    RecordedDate = date_created,
    site = `Select your site (district, parish, network, or school).`,
    role = `Select your role.`,
    content_area = `Select the content area for today's professional learning session.`,
    course = `Select your course.`,
    overall_went_well = `Overall, what went well in this course?`,
    been_better_course = `Overall, what could have been better in this course?`,
    learning_excited_try = `What is the learning from this course that you are most excited about trying out?`,
    best_activities = `Which activities best supported your learning in this course?`,
    course_add_comments = `Feel free to leave us any additional comments, concerns, or questions.`,
    course_feedback_14 = `How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`,
    course_feedback_4 = `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`,
    course_feedback_3 = `How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.`,
    course_feedback_7 = `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course.`,
    course_feedback_9 = `How much do you agree with the following statements about this course? - The strategies I’ve learned in this course will improve my instruction.`,
    course_feedback_10 = `How much do you agree with the following statements about this course? - The strategies I’ve learned in this course will improve my coaching or supervision of teachers.`,
    course_feedback_11 = `How much do you agree with the following statements about this course? - The strategies I’ve learned in the course are easy to implement.`,
    course_feedback_12 = `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice.`,
    course_feedback_13 = `How much do you agree with the following statements about this course? - This course has supported me in being responsive to students' backgrounds, cultures, and points of view.`,
    nps = `On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`
  ) |>
  mutate(role = as.character(role),
         role = str_replace_all(role, c("School-based coach" = "Coach",
                                        "District/network/state-level professional or administrator" = "District leader or administrator",
                                        "School-based administrator or supervisor" = "School leader or administrator")),
         site = str_replace_all(site, c(c("Amistad Dual Language, NY" = "NY_NYC AMS Study Schools Bronx_ Channel View School for Research, Amistad Dual Language, and Fannie Lou Hamer",
                                          "Calcasieu Parish, LA" = "LA_Calcasieu Parish",
                                          "CityYear, NY" = "US_City Year",
                                          "Cleveland Metropolitan School District, OH" = "OH_Cleveland Metro School District",
                                          "Delaware Department of Education, DE" = "DE_DE DoE",
                                          "Jefferson Davis Parish, LA" = "LA_Jefferson Davis Parish",
                                          "Kankakee School District, IL" = "IL_Kankakee District 111",
                                          "Louisville School District - Jacob Elementary, KY" = "KY_Louisville School District_Jacob Elem.",
                                          "Massachusetts Dept of Elementary & Secondary Education" = "MA_DESE",
                                          "McNairy County, TN" = "TN_McNairy County Schools",
                                          "New Mexico Public Education Department, NM" = "NM_NM PED",
                                          "NYC District 10 - PS 386" = "NY_D10",
                                          "NYC District 11 - District-wide, NY" = "NY_D11",
                                          "NYC District 12 - ESMT-IS 190, NY" = "NY_EMST_IS190",
                                          "NYC District 12 - MS 286 Fannie Lou Hamer" = "NY_D11",
                                          "NYC District 27 - District-wide, NY" = "NY_D27",
                                          "NYC District 6 - MS311, NY" = "NY_D6",
                                          "NYC District 9 - District-wide, NY" = "NY_D9",
                                          "Open Enrollment, National" = "US_Open Enrollment",
                                          "Orleans Central Supervisory Union, VT" = "VT_Orleans Central Supervisory Union",
                                          "Pointe Coupee Parish, LA" = "LA_Pointe Coupee Parish",
                                          "Rochester City School District - District-wide" = "NY_Rochester City School District",
                                          "San Diego Unified School District, CA" = "CA_San Diego",
                                          "West Contra Costa USD, CA" = "CA_West Contra Costa_WCCUSD",
                                          "Wisconsin Department of Education, WI" = "WI_WI DPI"))))

write_csv(renamed_course_survey_21_22, "~/Downloads/course_survey_19_22.csv")

all_time_course_survey <- renamed_course_survey_21_22 |>
  bind_rows(get_course_survey())
write_rds(all_time_course_survey, "~/Downloads/course_survey.rds")

renamed_session_survey_21_22 <- session_survey_21_22 |>
  rename(
    facilitator1 = `Facilitator`,
    RecordedDate = `Date`,
    site = `Select your site (district, parish, network, or school).`,
    role = `Select your role.`,
    content_area = `Select the content area for today’s professional learning session.`,
    course = `Select your course.`,
    fac_feedback_1 = `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.`,
    fac_feedback_2 = `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.`,
    fac_feedback_3 = `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.`,
    fac_feedback_4 = `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.`,
    fac_feedback_5 = `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.`,
    fac_add1 = `Facilitation_Feedback`,
    went_well_today = `What went well in today’s session?`,
    been_better_today = `What could have been better about today’s session?`
  ) |>
  mutate(role = as.character(role),
         content_area = as.character(content_area),
         course = as.character(course))

# write_csv(renamed_session_survey_21_22, "~/Downloads/session_survey_21_22.csv")

all_time_session_survey <- renamed_session_survey_21_22 |>
  bind_rows(get_session_survey())
write_rds(all_time_session_survey, "~/Downloads/session_survey.rds")

# course_survey_21_22 |>
#   filter(date_created >= as.Date("2021-07-01")) |>
#   group_by(`How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`) |>
#   count(sort = T) |>
#   drop_na() |>
#   ungroup() |>
#   summarise(percent = sum(`How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.` %in% c("(4) Agree", "(5) Strongly agree")) / sum(`How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`))

# dashboard_data <- readr::read_csv("~/Downloads/dashboard_data.csv")
# colnames(dashboard_data) |> as_tibble() |> gt::gt()

# session_survey <- dashboard_data |>
#   select(RecordedDate = `Date for the session`,
#          site = `District, Parish, Or Network`,
#          facilitator = `Name Of Your Facilitator`,
#          overall_quality = `% Satisfied With The Overall Quality Of Today's Professional Learning Session`,
#          relevant_for_role = `% Who Say Today's Topic Was Relevant For My Role`,
#          help_me_learn = `% Who Say Activities Of Today's Session Were Well-Designed To Help Me Learn`,
#          excited_trying = `What is the learning from this professional learning that you are most excited about trying out?`,
#          apply_four_six_weeks = `How Likely Are You To Apply This Learning To Your Practice In The Next 4-6 Weeks?`,
#          fac_feedback_2 = `S/He Facilitated The Content Clearly`,
#          fac_feedback_3 = `S/He Effectively Built A Community Of Learners`,
#          second_fac = `Did You Have A Second Facilitator?`,
#          been_better_today = `What could have improved your experience?`)
# 
# course_survey <- dashboard_data |>
#   select(RecordedDate = `Date for the session`,
#          `Overall, what went well in this professional learning?`,
#          `Which activities best supported your learning?`,
#          `How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`,
#          `Why did you choose this rating?`,
#          `Do you have additional comments?`,
#          `How, if in any way, this course helped you prepare for school opening after COVID-19?`,
#          `The independent online work activities were well-designed to help me meet the learning targets.`,
#          `The Zoom meeting activities were well-designed to help me meet the learning targets.`,
#          `I felt a sense of community with the other participants in this course even though we were meeting virtually.`,
#          `This course helped me navigate remote and/or hybrid learning during COVID-19.`)
