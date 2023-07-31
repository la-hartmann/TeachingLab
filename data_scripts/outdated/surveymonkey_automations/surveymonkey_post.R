library(surveymonkey)
library(tidyverse)
library(googlesheets4)

### Set oauth token for SurveyMonkey ###
options(sm_oauth_token = "LTRq.89-huFrldt4aN90RM62PzhHb62O5j9qyWt6SgwXiKRkTqlBmc.622s4kvJ43TOAX4SUeBkmqd-feekNRmfOZDTeYjfxQOqRUHkLhbl5ElQs0gd568.l1LGHEyGX")

### Get list of surveys needed for changing ###
surveys <- surveymonkey::browse_surveys()
survey_urls_ids <- surveys %>%
  filter(title %in% c("Teaching Lab Diagnostic Educator Survey: SY2021-22",
                      "SY21-22 End-of-Session Participant Feedback Survey",
                      "SY21-22 End-of-Course Participant Feedback Survey",
                      "ELA EL: Bootcamp - ALL Block (3-5)",
                      "ELA Foundational Skills: Cycle of Inquiry 2: Using Data to Inform Foundational Skills Instruction",
                      "ELA General: Cycle of Inquiry - Complex Text",
                      "ELA General: Cycle of Inquiry - Speaking & Listening",
                      "ELA: Bootcamp - Foundational Skills Bootcamp Skills (K-2)",
                      "ELA: Bootcamp - General",
                      "ELA: CRSE PLC",
                      "ELA: Cycle of Inquiry - Curriculum Flex Foundational Skills",
                      "ELA: Guidebooks Cycle of Inquiry 1",
                      "ELA: Guidebooks Cycle of Inquiry 2",
                      "ELA: Guidebooks Diverse Learners Bootcamp - Leader",
                      "ELA: Guidebooks Diverse Learners Bootcamp - Teacher",
                      "ELA: Guidebooks Diverse Learners Bootcamp Writing",
                      "ELA: Guidebooks Diverse Learners Cycle of Inquiry - Fluency",
                      "ELA: Guidebooks Diverse Learners Cycle of Inquiry - Vocabulary",
                      "ELA: HQIM & Enrichment",
                      "ELA: School Leader Coaching Series",
                      "School Leaders: ELA",
                      "Math: Accelerating Learning",
                      "Math: Cycle of Inquiry III - Facilitating Student Discourse",
                      "Math: Bootcamp",
                      # "Math: Cycle of Inquiry II - Making Math Visible", ### ISSUE: Survey is bugged for some reason 
                      "Math: Cycle of Inquiry I - Eliciting Student Thinking",
                      "Math: Cycle of Inquiry V- Sequencing and Connecting Representations",
                      "Math: Supporting Math Intervention",
                      "Cycle of Inquiry VI- Summarizing the Mathematics",
                      "Math: Accelerating Learning - EIC",
                      "Math: Bootcamp - Curriculum Flexible",
                      "Math: Cycle of Inquiry I - Eliciting Student Thinking - Curriculum Flexible",
                      "Math: Bootcamp - EIC"
                      )) %>%
  select(url, id)

survey_url_questions <- paste0(survey_urls_ids, )

### Get list of sites to change option in SurveyMonkey ###
sites <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/11jlo9UeWxZGwunhDb24hZBwAKc5b8ZKM9AYNWZaUyZY/edit#gid=0",
                                             skip = 1) %>%
  dplyr::select(3) %>%
  set_names("site")

### Set authentication header ###
h <- httr::add_headers(Authorization = paste('bearer', getOption('sm_oauth_token')),
                       'Content-Type'='application/json')

### Function needs to loop over each survey, then find correct question, then write over it ###
### Another option is make ALL of the data 

diagnostic <- fetch_survey_obj(306944493)

### Delete question if length is greater than SurveyMonkey automation list ###
new_question_choices <- diagnostic$pages[[2]]$questions[[3]]$answers$choices[1:length(sites$site)]

for (i in seq(new_question_choices)) {
  new_question_choices[[i]]$text <- sites$site[i]
}

b <- rjson::toJSON(new_question_choices)

httr::POST(url = diagnostic$pages[[2]]$questions[[3]]$href,
           config = h,
           body = b)

### Post new question choices ###


# check <- httr::GET(url = "https://api.surveymonkey.net/v3/surveys/308115193/pages/171452872/questions/679384221", 
#           config = h,
#           httr::user_agent("http://github.com/tntp/surveymonkey"))
# 
# parsed_content <- httr::content(check, as = "parsed")
