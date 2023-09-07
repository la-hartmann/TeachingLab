library(dplyr)
library(googlesheets4)
library(qualtRics)
library(TeachingLab)

participant_feedback <- qualtRics::fetch_survey(surveyID = "SV_djt8w6zgigaNq0C", 
                                                verbose = TRUE,
                                                force_request = TRUE,
                                                add_column_map = TRUE) |>
  dplyr::mutate(id = paste0(tolower(Initials), DOB)) |>
  dplyr::filter(id %!in% c("tst1000", "tst0000"))

san_diego_responses_session <- subset(participant_feedback, Site == "CA_San Diego") |>
  dplyr::filter(last_session_or_not == "No - today was not the final session.") |>
  dplyr::select(Date = EndDate, 
                `Select the name of your facilitator` = Q7,
                `What did you find useful in our professional learning?` = Q56, 
                `What will you try as a result of this learning?` = Q58, 
                `Is there anything else you would like the San Diego Enhanced Mathematics leadership team to know?` = Q59,
                `How much do you agree with the following statements about this facilitator today? - They demonstrated deep knowledge of the content they facilitated.` = Q8_1,
                `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.` = Q8_2,
                `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.` = Q8_3,
                `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.` = Q8_4,
                `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.` = Q8_5,
                `What additional feedback do you have about their facilitation skills, if any?` = Q9,
                # `What went well in today’s session?` = Q15, 
                `What could have been better about today’s session?` = Q16)

san_diego_responses_session |>
  googlesheets4::write_sheet("https://docs.google.com/spreadsheets/d/1LWaHmoEezhv9aJkGcBBVrA_PBeDojOClL4_tONM9Vis/edit#gid=858147958",
                             sheet = "end_of_session")

san_diego_responses_course <- subset(participant_feedback, Site == "CA_San Diego") |>
  dplyr::filter(last_session_or_not == "Yes - today was the final session OR this was a one-day PL course.") |>
  dplyr::select(Date = EndDate, 
                `What did you find useful in our professional learning?` = Q60,
                `What will you try as a result of this learning?` = Q61,
                `Is there anything else you would like the San Diego Enhanced Mathematics leadership team to know?` = Q63,
                `How much do you agree with the following statements about this course? - I was fully present "minds-on" during these PL sessions.` = course_feedback_2, 
                `How much do you agree with the following statements about this course? - The activities were well-designed to help me meet the learning targets.` = Q33_3, 
                `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.` = Q33_4, 
                `How much do you agree with the following statements about this course? - This PL was a good use of my time.` = Q33_5, 
                `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course.` = Q33_7, 
                `How much do you agree with the following statements about this course? - This course was relevant to my instructional practices.` = Q33_8, 
                `How much do you agree with the following statements about this course? - The strategies I’ve learned in this course will improve my instruction.` = Q33_9, 
                `How much do you agree with the following statements about this course? - I have applied or will apply what I have learned in this course to my practice.` = course_feedback_12, 
                `How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.` = course_feedback_14,
                `Overall, what went well in this course?` = Q38,
                `Overall, what could have been better in this course?` = Q39,
                `What is the learning from this course that you are most excited about trying out?` = Q36,
                `Which activities best supported your learning in this course?` = Q37,
                `Feel free to leave us any additional comments, concerns, or questions.` = Q40)

san_diego_responses_course |>
  googlesheets4::write_sheet("https://docs.google.com/spreadsheets/d/1mxktY0fR0MIzRLSq8H7z9DxPquOSRY23CRpkcsap6Vc/edit#gid=843377505",
                             sheet = "san_diego_course")
