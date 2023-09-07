library(qualtRics)
library(tidyverse)

participant_feedback <- qualtRics::fetch_survey(
  surveyID = "SV_djt8w6zgigaNq0C",
  verbose = FALSE
)

delaware_data <- participant_feedback |>
  dplyr::filter(site == "DE_DE Department of Education" & Finished == TRUE)

delaware_data |>
  dplyr::select(Progress,
                Finished,
                RecordedDate,
                last_session_or_not,
                initials,
                selected_date,
                site,
                role,
                gender,
                race_1,
                race_2,
                race_3,
                race_4,
                race_5,
                race_6,
                race_7,
                ethnicity,
                contains("grade_level"),
                course,
                location,
                coach,
                coach_other,
                coach_ongoing_feed_1,
                coach_ongoing_feed_2,
                coach_ongoing_feed_3,
                coach_ongoing_feed_4,
                coach_ongoing_feed_5,
                coach_additonal_feed,
                coach_gone_well,
                been_better_coach,
                facilitator1,
                second_fac_q,
                facilitator2,
                fac_feedback_1,
                fac_feedback_2,
                fac_feedback_3,
                fac_feedback_4,
                fac_feedback_5,
                fac_add1,
                fac_add_2,
                went_well_today,
                take_back_class,
                been_better_today,
                contains("course_feedback"),
                nps,
                overall_went_well,
                been_better_course,
                learning_excited_try,
                best_activities,
                course_add_comments,
                contains("coach_end_feed"),
                coach_nps,
                coach_learn_excited,
                coach_activities_sup,
                coach_went_well,
                coach_been_better,
                coach_add_comment) |>
  googlesheets4::write_sheet("https://docs.google.com/spreadsheets/d/1p2HNUWkwRjjOLT_lM4q4DEVHPtgzIZTha4ZHatwgxw4/edit#gid=638757300",
                             sheet = "Sheet2")

