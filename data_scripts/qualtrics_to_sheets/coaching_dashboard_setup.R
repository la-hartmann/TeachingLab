library(dplyr)
library(googlesheets4)
library(qualtRics)

participant_feedback <- fetch_survey(
  surveyID = "SV_djt8w6zgigaNq0C",
  force_request = TRUE
)

original_sheet <- googlesheets4::read_sheet("1fvy2NTZXs3zuIi9BE1XZP_0MZW7_7I_r8lrbjaqbtnU")

max_date <- max(original_sheet$Date, na.rm = T)

coach_data_2 <- participant_feedback |>
  dplyr::filter(course == "Coaching" &
                  Finished == TRUE &
                  RecordedDate > max_date &
                  (last_session_or_not == "Yes - there will be more sessions for this PL course or coaching cycle." |
                     last_session_or_not == "This was a 1-day PL course.") &
                  !is.na(coach_2)) |>
  dplyr::select(facilitator = coach_2,
                `They demonstrate deep knowledge of the content on which they coach` = coach_ongoing_feed_2_1,
                `Their coaching is clear` = coach_ongoing_feed_2_2,
                `They seem fully prepared for the coaching sessions` = coach_ongoing_feed_2_3,
                `They effectively build a safe learning environment` = coach_ongoing_feed_2_4,
                `They make necessary adjustments based on my needs` = coach_ongoing_feed_2_5,
                `Additional feedback` = coach_add_feed_2,
                `What has gone well in your coaching sessions?` = coach_gone_well_2,
                `What could have been better about your coaching sessions?` = been_better_coach_2,
                course,
                site,
                Date = RecordedDate,
                `Content Area` = content_area
  )

all_coach_data <- participant_feedback |>
  dplyr::filter(course == "Coaching" &
    Finished == TRUE &
    RecordedDate > max_date &
    (last_session_or_not == "Yes - there will be more sessions for this PL course or coaching cycle." |
      last_session_or_not == "This was a 1-day PL course.")) |>
  dplyr::select(facilitator,
    `They demonstrate deep knowledge of the content on which they coach` = coach_ongoing_feed_1,
    `Their coaching is clear` = coach_ongoing_feed_2,
    `They seem fully prepared for the coaching sessions` = coach_ongoing_feed_3,
    `They effectively build a safe learning environment` = coach_ongoing_feed_4,
    `They make necessary adjustments based on my needs` = coach_ongoing_feed_5,
    `Additional feedback` = coach_add_feed_1,
    `What has gone well in your coaching sessions?` = coach_gone_well_1,
    `What could have been better about your coaching sessions?` = been_better_coach_1,
    course,
    site,
    Date = RecordedDate,
    `Content Area` = content_area
  ) |>
  bind_rows(coach_data_2)

all_coach_data |>
  googlesheets4::sheet_append(
    ss = "https://docs.google.com/spreadsheets/d/1fvy2NTZXs3zuIi9BE1XZP_0MZW7_7I_r8lrbjaqbtnU/edit#gid=0",
    sheet = "Sheet3"
  )
