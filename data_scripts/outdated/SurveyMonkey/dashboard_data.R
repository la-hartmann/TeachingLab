####### This script gets all data for the end of course survey, end of session survey, and #######
####### writes them to the relevant dashboards, as well as the personalized facilitator dashboard. #######

##### Course Survey #####

# rename_old_df <- c(
#   "Do you have additional comments\\?" = "Feel free to leave us any additional comments, concerns, or questions\\.",
#   "Overall, what went well in this professional learning\\?" = "Overall, what went well in this course\\?",
#   "Which activities best supported your learning\\?" = "Which activities best supported your learning in this course\\?",
#   "What could have improved your experience\\?" = "Overall, what could have been better in this course\\?",
#   "Professional Training Session" = "Select your course\\.",
#   "District, Parish, Or Network" = "Select your site \\(district, parish, network, or school\\)\\.",
#   "% Satisfied With The Overall Quality Of Today's Professional Learning Session" = "How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course\\.",
#   "% Who Say Activities Of Today's Session Were Well-Designed To Help Me Learn" = "How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets\\.",
#   "What is the learning from this professional learning that you are most excited about trying out\\?" = "What is the learning from this course that you are most excited about trying out\\?",
#   "How Likely Are You To Apply This Learning To Your Practice In The Next 4-6 Weeks\\?" = "How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks\\.",
#   "How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend\\?" = "On a scale of 0-10, how likely are you to recommend this course to a colleague or friend\\?",
#   "Date for the session" = "Select the date for this session. - \n    Date / Time\n",
#   "Portfolio" = "Select the content area for today's professional learning session\\."
# )
#
# old_df <- readr::read_rds("data-clean/data-move/dashboard_data/dashboard_data.rds") %>%
#   dplyr::mutate(Portfolio = stringr::str_replace_all(Portfolio, c(
#     "EL" = "ELA",
#     "Guidebooks" = "ELA",
#     "Illustrative Mathematics" = "Math"
#   ))) %>%
#   dplyr::rename_with(~ stringr::str_replace_all(.x, rename_old_df)) %>%
#   dplyr::select(
#     "Feel free to leave us any additional comments, concerns, or questions.",
#     "Overall, what went well in this course?",
#     "Which activities best supported your learning in this course?",
#     "Overall, what could have been better in this course?",
#     "Select your course.",
#     "Select your site (district, parish, network, or school).",
#     "How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.",
#     "How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.",
#     "What is the learning from this course that you are most excited about trying out?",
#     "How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks.",
#     "On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?",
#     "Select the date for this session. - \n    Date / Time\n",
#     "Select the content area for today's professional learning session."
#   )

# readr::write_rds(old_df, here::here("data/old_course_survey_reformatted.rds"))

course_survey <- TeachingLab::get_course_survey(update = TRUE)

################################################################################################################################################################

##### Session Survey #####
  
session_survey <- TeachingLab::get_session_survey(update = T)

################################################################################################################################################################

##### Facilitator Dashboard #####

options(sm_oauth_token = "6zpcKriMLjBWVEHno8VWb4Uvclqotpq0H53HudGcfcyLc6aW0vxfm-M3e.REqngqrQ7vw1HPB92gxQprqcGH7IFXI1u64xNU.PLchF79sIqyhoTsuHyTAchN2yfLvBvU")

fake_fac <- readr::read_rds("data/fake_facilitator.rds")
fake_dunc <- readr::read_rds("data/fake_duncan.rds")

facilitator_session_survey <- session_survey |>
  # FOR A FAKE TEMPORARY FACILITATOR DATA BEFORE VERIFICATION
  dplyr::bind_rows(fake_fac, fake_dunc)

readr::write_rds(facilitator_session_survey, here::here("data/sy21_22/session_facilitator_surveymonkey.rds"))
readr::write_rds(facilitator_session_survey, here::here("dashboards/PersonalFacilitator/data/session_facilitator_surveymonkey.rds"))

################################################################################################################################################################

options(rsconnect.force.update.apps = TRUE)
### Deploy Course Survey ###
rsconnect::deployApp(
  appDir = here::here("dashboards/CourseSurvey"),
  account = "teachinglabhq",
  server = "shinyapps.io",
  appName = "CourseSurvey",
  appId = 4505718,
  launch.browser = function(url) {
    message("Deployment completed: ", url)
  },
  lint = FALSE,
  metadata = list(
    asMultiple = FALSE,
    asStatic = FALSE
  ),
  logLevel = "verbose",
  forceUpdate = TRUE
)


### Deploy Session Survey ###
rsconnect::deployApp(
  appDir = here::here("dashboards/SessionSurvey"),
  account = "teachinglabhq",
  server = "shinyapps.io",
  appName = "SessionSurvey",
  appId = 4505754,
  launch.browser = function(url) {
    message("Deployment completed: ", url)
  },
  lint = TRUE,
  metadata = list(
    asMultiple = FALSE,
    asStatic = FALSE
  ),
  forceUpdate = TRUE,
  logLevel = "verbose"
)
# 
# ### Deploy Personal Facilitator Survey ###
rsconnect::deployApp(
  appDir = here::here("dashboards/PersonalFacilitator"),
  account = "teachinglabhq",
  server = "shinyapps.io",
  appName = "PersonalFacilitator",
  appId = 4489188,
  launch.browser = function(url) {
    message("Deployment completed: ", url)
  },
  lint = FALSE,
  metadata = list(
    asMultiple = FALSE,
    asStatic = FALSE,
    ignoredFiles = "data/.DS_Store"
  ),
  logLevel = "verbose"
)

# rstudioapi::navigateToFile(here::here("dashboards/CourseSurvey/app.R"))
# rstudioapi::navigateToFile(here::here("dashboards/SessionSurvey/app.R"))
# rstudioapi::navigateToFile(here::here("dashboards/PersonalFacilitator/app.R"))
