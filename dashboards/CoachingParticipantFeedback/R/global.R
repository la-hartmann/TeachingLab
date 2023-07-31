#### Coaching Participant Feedback Dashboard ####

### Libraries Load ###
library(dplyr)
library(ggplot2)
library(ggtext)
library(googleAuthR)
library(gt)
library(forcats)
library(purrr)
library(showtext)
library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shiny.semantic)
library(shiny.router)
library(tidyr)
library(tlShiny)

### Graphics ###
# options(shiny.useragg = T)
### Fonts ###
###### CSV is From Here: https://groups.google.com/a/teachinglab.org/g/employees/members ######
## Get list of current employees from CSV ##
# approved_emails_list <- readr::read_csv("dashboards/SessionSurvey/data/employees.csv", skip = 1) %>%
#   dplyr::select(1) %>%
#   purrr::as_vector() %>%
#   # Add extra people as requested
#   append(c("dgreenberg@education-first.com",
#            "sbriggs@education-first.com",
#            "duncan.gates123@gmail.com",
#            "kristen.taylor@teachinglab.org",
#            "ryan.stewart@gmail.com"))
## Write approved list to dashboard data folder ##
# readr::write_rds(approved_emails_list, "dashboards/CoachingParticipantFeedback/data/employees.rds")

######## THIS WORKS BY RUNNING THE ABOVE CODE LOCALLY #######
approved_emails_list <- readr::read_rds("data/employees.rds")

################################################################################################

coaching_participant_feedback <- readr::read_rds("data/coaching_participant_feedback.rds")
