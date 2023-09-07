library(qualtRics)
library(TeachingLab)
library(tidyverse)


session_survey <- fetch_survey(surveyID = "SV_djt8w6zgigaNq0C", 
                               verbose = TRUE)

##### Create dataframe for selection of either first or second facilitator #####
session_survey_adjusted <- session_survey |>
  ### Change Q7 and Q11 to take into account (other) written responses ###
  ### Create id column ###
  dplyr::mutate(Q7 = coalesce(Q7, Q7_64_TEXT),
         Q11 = coalesce(Q11, Q11_64_TEXT),
         id = paste0(tolower(Initials), DOB),
         Race = coalesce(Race_1, Race_2, Race_3, Race_4, Race_5, Race_6, Race_7)) |>
  ### Remove incomplete and test responses ###
  dplyr::filter(Finished == TRUE & 
                  id %!in% c("tst1000", "tst0000") & 
                  last_session_or_not == "No - this was the final session for this PL course or coaching.")

##### First facilitator only selections #####
first_fac_session_survey <- session_survey_adjusted |>
  ### Select useful columns for end of session survey data analysis ###
  select(Date = EndDate,
         id,
         Role,
         Gender,
         Ethnicity,
         Race,
         `Content area`,
         Course,
         virtual_hybrid_in_person = Q41,
         Site,
         `Facilitator` = Q7,
         additional_feedback_1 = Q9,
         `They demonstrated deep knowledge of the content they facilitated` = Q8_1,
         `They facilitated the content clearly` = Q8_2,
         `They effectively built a safe learning community` = Q8_3,
         `They were fully prepared for the session` = Q8_4,
         `They responded to the group’s needs` = Q8_5,
         `What is one thing from today's learning that you plan to take back to your classroom?` = Q14,
         `What went well in today’s session?` = Q15,
         `What could have been better about today’s session?` = Q16)

##### Second facilitator only selections #####
second_fac_session_survey <- session_survey_adjusted |>
  ### Filter for only those who said they had a second facilitator ###
  filter(Q10 == "Yes") |>
  ### Relevant to data analysis column selection only for second facilitator options
  select(Date = EndDate,
         id,
         Role,
         Gender,
         Ethnicity,
         Race,
         `Content area`,
         Course,
         virtual_hybrid_in_person = Q41,
         Site,
         `Facilitator` = Q11,
         additional_feedback = Q13,
         `They demonstrated deep knowledge of the content they facilitated` = Q12_1,
         `They facilitated the content clearly` = Q12_2,
         `They effectively built a safe learning community` = Q12_3,
         `They were fully prepared for the session` = Q12_4,
         `They responded to the group’s needs` = Q12_5)

final_session_survey_df <- first_fac_session_survey |>
  dplyr::bind_rows(second_fac_session_survey)

readr::write_rds(final_session_survey_df, here::here("data/sy22_23/end_of_session_survey_22_23.rds"))

final_course_survey_df <- participant_feedback |>
  dplyr::filter(last_session_or_not == "Yes - there will be more sessions for this PL course or coaching cycle.") |>
  dplyr::mutate(
    id = paste0(tolower(Initials), DOB),
    Race = coalesce(Race_1, Race_2, Race_3, Race_4, Race_5, Race_6, Race_7)
  ) |>
  dplyr::select(Date = EndDate,
                id,
                Role,
                Gender,
                Ethnicity,
                Race,
                `Content area`,
                Course,
                virtual_hybrid_in_person = Q41,
                Site,
                `I was fully present "minds-on" during these PL sessions.` = course_feedback_2,
                `The activities were well-designed to help me meet the learning targets.` = Q33_3,
                `I am satisfied with how the course was facilitated.` = Q33_4,
                `This PL was a good use of my time.` = Q33_5,
                `I talk to other teachers about the things I learned in this PL.` = Q33_6,
                `I felt a sense of community with the other participants in this course.` = Q33_7,
                `This course was relevant to my instructional practices.` = Q33_8,
                `The strategies I’ve learned in this course will improve my instruction.` = Q33_9,
                `The strategies I’ve learned in this course will improve my coaching or supervision of teachers.` = course_feedback_10,
                `The strategies I’ve learned in the course are easy to implement.` = course_feedback_11,
                `I have applied or will apply what I have learned in this course to my practice.` = course_feedback_12,
                `This course has supported me in being responsive to students' backgrounds, cultures, and points of view.` = course_feedback_13,
                `I am satisfied with the overall quality of this course.` = course_feedback_14,
                nps = Q35,
                `Overall, what went well in this course?` = Q38,
                `Overall, what could have been better in this course?` = Q39,
                `What is the learning from this course that you are most excited about trying out?` = Q36,
                `Which activities best supported your learning in this course?` = Q37,
                `Feel free to leave us any additional comments, concerns, or questions.` = Q40) |>
  readr::write_rds(here::here("data/sy22_23/end_of_course_survey_22_23.rds"))

partners <- "IL_Kankakee District 111"



rmarkdown::render(params = list(partner = partners),
                  input = here::here("analysis/sy22_23/Reports/end_of_session_report.Rmd"),
                  output_dir = here::here("analysis/sy22_23/Reports/reports_generated"))

