library(googlesheets4)
library(gt)
library(tidvyerse)
library(TeachingLab)

course_survey_22_23 <- get_course_survey(year = "22_23", update = FALSE) # DO NOT UPDATE
course_survey_21_22 <- get_course_survey(year = "21_22", update = FALSE) # DO NOT UPDATE
course_survey_20_21 <- read_sheet("1xfhI6jwUpNdAg4cE3BVrBckvhgV7pgD4ryS4kQDI4S0",
                                  sheet = "Form Responses",
                                  skip = 1)

session_survey_22_23 <- get_session_survey(year = "22_23", update = FALSE) # DO NOT UPDATE
session_survey_21_22 <- get_session_survey(year = "21_22", update = FALSE) # DO NOT UPDATE
session_survey_20_21 <- read_sheet("1xfhI6jwUpNdAg4cE3BVrBckvhgV7pgD4ryS4kQDI4S0",
                                   sheet = "Form Responses",
                                   skip = 1)
session_survey_18_19 <- read_sheet("1liqddOExAuoV0xNzMDSi5WtV_8lYYH87",
                                   sheet = "Guskey 1 raw data",
                                   skip = 1)


#### COURSE SURVEY ####
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
all_time_course_survey <- renamed_course_survey_21_22 |>
  bind_rows(course_survey_22_23)

all_time_course_survey |>
  mutate(month = floor_date(RecordedDate, "month")) |>
  group_by(month) |>
  summarise(nps = mean(nps, na.rm = T)) |>
  ggplot2::ggplot(aes(x = month, y = nps)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 10)) +
  labs(x = NULL, y = "NPS", title = "Teaching Lab NPS Scores on a Rolling Monthly Average") +
  theme_tl()

pos <- function(x) {
  x <- x[!is.na(x)]
  100 * (sum(str_detect(x, "4|5"))/(sum(str_detect(x, "1|2|3")) + sum(str_detect(x, "4|5"))))
}

n_count <- function(x) {
  sum(!is.na(x))
}

all_time_course_survey |>
  mutate(month = floor_date(RecordedDate, "month")) |>
  select(month, contains("course_feedback")) |>
  drop_na(month) |>
  group_by(month) |>
  summarise(across(everything(), list(n = n_count, pos = pos))) |>
  gt::gt() |>
  data_color(columns = contains("pos"),
             method = "numeric",
             palette = "ggsci::blue_material") |>
  data_color(columns = contains("n"),
             method = "numeric",
             palette = "ggsci::red_material") |>
  fmt_percent(columns = contains("pos"),
              scale_values = F) |>
  sub_missing(missing_text = "No data") |>
  gt_theme_tl()
  
#### END OF COURSE SURVEY SECTION ####

### SESSION SURVEY SECTION ###
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

all_time_session_survey <- renamed_session_survey_21_22 |>
  bind_rows(session_survey_22_23)

all_time_session_survey |>
  mutate(month = floor_date(RecordedDate, "month")) |>
  select(month, contains("fac_feedback")) |>
  drop_na(month) |>
  group_by(month) |>
  summarise(across(everything(), list(n = n_count, pos = pos))) |>
  gt::gt() |>
  data_color(columns = contains("pos"),
             method = "numeric",
             palette = "ggsci::blue_material") |>
  data_color(columns = contains("n"),
             method = "numeric",
             palette = "ggsci::red_material") |>
  fmt_percent(columns = contains("pos"),
              scale_values = F) |>
  sub_missing(missing_text = "No data") |>
  gt_theme_tl()

### EDUCATOR SURVEY SECTION ###

educator_survey_22_23 <- TeachingLab::get_diagnostic_survey(year = "22_23")
followup_educator_22_23 <- TeachingLab::get_followup_educator(year = "22_23")
educator_survey_21_22 <- TeachingLab::get_diagnostic_survey(year = "21_22")
followup_educator_21_22 <- TeachingLab::get_diagnostic_survey(year = "21_22")
# https://drive.google.com/drive/folders/1QodYDcVFywpPN9SyDHvcrXtQiIiN_yAF
educator_survey_20_21 <- TeachingLab::get_diagnostic_survey(year = "20_21")
followup_educator_20_21 <- TeachingLab::get_followup_educator(year = "20_21")
# https://drive.google.com/drive/folders/1Hsc2yqNKVTWwBUTIh0JDJLgmgXKAgIdm
# Key is here: https://docs.google.com/spreadsheets/d/183DRvEYJ7Ks9lQB1zSPXpZtTSa-pse7T/edit#gid=1355456152 #
educator_followup_survey_19_20 <- TeachingLab::get_diagnostic_survey(year = "19_20")
  
### END OF EDUCATOR SURVEY SECTION ###

### IPG FORMS SECTION ###

### END OF IPG FORMS SECTION ###

### KNOWLEDGE ASSESSMENTS SECTION ###

### END OF KNOWLEDGE ASSESSMENTS SECTION ###

