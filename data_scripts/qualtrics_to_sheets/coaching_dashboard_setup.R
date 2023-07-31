library(dplyr)
library(googlesheets4)
library(qualtRics)

participant_feedback <- fetch_survey(
  surveyID = "SV_djt8w6zgigaNq0C"
)

original_sheet <- googlesheets4::read_sheet("1fvy2NTZXs3zuIi9BE1XZP_0MZW7_7I_r8lrbjaqbtnU")

max_date <- max(original_sheet$Date, na.rm = T)

participant_feedback |>
  dplyr::filter(course == "Coaching" & Finished == TRUE & RecordedDate > max_date &
    last_session_or_not == "Yes - there will be more sessions for this PL course or coaching support.") |>
  dplyr::select(coach,
    `They demonstrate deep knowledge of the content on which they coach` = coach_ongoing_feed_1,
    `Their coaching is clear` = coach_ongoing_feed_2,
    `They seem fully prepared for the coaching sessions` = coach_ongoing_feed_3,
    `They effectively build a safe learning environment` = coach_ongoing_feed_4,
    `They make necessary adjustments based on my needs` = coach_ongoing_feed_5,
    `Additional feedback` = coach_additonal_feed,
    `What has gone well in your coaching sessions?` = coach_gone_well,
    `What could have been better about your coaching sessions?` = been_better_coach,
    course,
    site,
    Date = RecordedDate,
    `Content Area` = content_area
  ) |>
  googlesheets4::sheet_append(
    ss = "https://docs.google.com/spreadsheets/d/1fvy2NTZXs3zuIi9BE1XZP_0MZW7_7I_r8lrbjaqbtnU/edit#gid=0",
    sheet = "Sheet3"
  )
