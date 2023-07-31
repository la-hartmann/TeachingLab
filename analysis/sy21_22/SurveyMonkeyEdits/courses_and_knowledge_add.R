library(surveymonkey)
library(tidyverse)
library(googlesheets4)

options(sm_oauth_token = "LTRq.89-huFrldt4aN90RM62PzhHb62O5j9qyWt6SgwXiKRkTqlBmc.622s4kvJ43TOAX4SUeBkmqd-feekNRmfOZDTeYjfxQOqRUHkLhbl5ElQs0gd568.l1LGHEyGX")

## Get all courses and content areas
courses_content <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1cELGsuEjXsb1e5oRdtdYof-Ul7jfdBXMOTYbIYd7nS4/edit#gid=0") %>%
  dplyr::select(1, 2) %>%
  tidyr::drop_na(1)

surveys <- readr::read_rds("data/sy21_22/course_knowledge_surveys.rds")
## Survey selector ##
# surveys <- surveymonkey::browse_surveys() %>%
#   dplyr::filter(title %in% c("SY21-22 End-of-Session Participant Feedback Survey",
#                       "SY21-22 End-of-Course Participant Feedback Survey")) %>%
#   dplyr::select(id) %>%
#   purrr::as_vector()
# surveys %>% readr::write_rds("data/sy21_22/course_knowledge_surveys.rds")

fetch_survey_obj(surveys[1]) -> test


edit_survey <- function(id, oauth_token = getOption('sm_oauth_token')) {
  # Make url for request
  u <- paste0("https://api.surveymonkey.net/v3/surveys/", id, "/surveys_write/")
  
  # Checks to ensure oauth_token is legit
  if (!is.null(oauth_token)) {
    token <- paste('bearer', oauth_token)
  } else {
    stop("Must specify 'oauth_token', Try using smlogin() first.")
  }
  
  out <- httr::POST(u,
                   config = h,
                   query = b,
                   httr::user_agent("http://github.com/tntp/surveymonkey")
  )
  
  message(paste0("you have ", out$headers$`x-ratelimit-app-global-day-remaining`, " requests left today before you hit the limit"))
  httr::stop_for_status(out)
  
  parsed_content <- httr::content(out, as = 'parsed')
  
}

h <- httr::add_headers(Authorization = paste('bearer', getOption('sm_oauth_token')),
                  'Content-Type'='application/json')

httr::GET(url = "https://api.surveymonkey.net/v3/surveys/308115193/pages/171452872/questions/679384221", 
          config = h,
          httr::user_agent("http://github.com/tntp/surveymonkey")) -> check

parsed_content <- httr::content(check, as = "parsed")

parsed_content[["answers"]][["choices"]][[1]][["text"]] <- "Something that shouldn't be here"
parsed_content %>% rjson::toJSON()

(httr::PUT(url = "https://api.surveymonkey.net/v3/surveys/308115193/pages/171452872/questions/679384221",
            config = h,
            body = rjson::toJSON(parsed_content),
            httr::user_agent("http://github.com/tntp/surveymonkey")) -> check_error)


change_courses <- function(survey) {
  
}

map(surveys, ~ editsurvey(id = .x))
