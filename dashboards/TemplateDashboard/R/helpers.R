options(googleAuthR.webapp.client_id = "342318881032-ui98nm42rcujf7v5cugcv38fmns8hjvg.apps.googleusercontent.com")
# options(googleAnalyticsR.webapp.client_secret = "gSfM4MILy7BUD17oM7XQWfxY")

options("googleAuthR.redirect" = "https://teachinglabhq.shinyapps.io/SessionSurvey/")

options(shiny.port = 7325)
library(shiny)
library(shiny.router)
library(shiny.semantic)
suppressPackageStartupMessages(library(tidyverse))
library(surveymonkey)
library(shinycssloaders)
library(lubridate)
library(scales)
library(TeachingLab)
library(ggtext)
library(showtext)
font_add(family = "Calibri", regular = "www/Calibri.ttf")
library(gt)
library(glue)
library(shinyWidgets)
library(bookdown)
library(tidytext)
library(Cairo)
library(grDevices)
library(googleAuthR)
library(shinyjs)
library(rmarkdown)

options(spinner.color = "#04ABEB")

info_page <- div(class = "ui container",
                 div(class = "ui center aligned header",
                     h2("Please watch the video below to learn how to use this dashboard, or provide"),
                     h2("feedback at the following ", tags$a(tags$b("link."), href = "https://www.surveymonkey.com/r/DashboardImprovements")),
                     div(class="ui center aligned", style = "text-align: center;",
                         br(),
                         HTML('<iframe src="https://www.youtube.com/embed/bjZVeSNtQdI" width="90%" height="500" frameborder="1"></iframe>')
                     )),
                 div(class = "ui two column stackable grid container",
                     div(class = "eight wide column",
                         h2("Images", align = "center"),
                         img(src="imgs/CourseSummaryLafayette.png", height = "5%"),
                         br(),
                         br(),
                         br(),
                         br(),
                         img(src="imgs/quote_vizzes.png", height = "5%"),
                         br(),
                         br(),
                         br(),
                         br(),
                         img(src="imgs/StaffandCulture.png", height = "12%"),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         h2("FAQ/Upcoming Features:")
                     ),
                     div(class = "eight wide column",
                         div(class="ui center aligned big header",
                             h2("Session Survey Dashboard Information")),
                         p("This app was created to visualize participant perception data of ",
                           a(tags$b("Teaching Lab"), href = "https://www.teachinglab.org"),
                           " professional learning sessions, specifically related to facilitation. The dashboard automatically pulls from ",
                           a("SurveyMonkey", href = "https://www.surveymonkey.com/r/TLendofsession"), " responses and integrates ",
                           "the data in two different tabs: the first tab visualizes the percent that agree/strongly agree with questions, and",
                           "the second tab pulls quotes from the open-ended feedback questions. The sidebar that appears on the left hand side of",
                           "each tab allows one to filter for the optimal data."),
                         p("The first image on the left depicts the \"Strongly Agree/Agree\" tab which shows",
                           "the percent of people who have responded either Strongly Agree, Agree, Neither Agree or Disagree, Disagree, or Strongly Disagree",
                           "in response to questions about the quality of facilitation."),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         p("The second image on the left here demonstrates the quote visualization that is available for the questions:"),
                         tags$li("\"What additional feedback do you have about their facilitation skills?\""),
                         tags$li("\"What went well in today’s session?\""),
                         tags$li("\"What could have been better about today’s session?\""),
                         p("are displayed. The tables sample 10 responses from each question based on the applied filters, and the",
                           " refresh button above extracts 10 new responses when you click it."),
                         br(),
                         p("The highlighting simply indicates the 3 most frequent words in the particular extracted data",
                           "which can be used to highlight certain features or ignored altogether."),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         p("Lastly there are downloadable reports that include all of the above features, as well as summary statements",
                           "about the percent of participants that agree/strongly agree with the feedback on facilitation."),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         h3("Upcoming features"),
                         tags$li("Cross-tab communication (always saving filters)"),
                         tags$li("Advanced reporting features (downloaded reports with machine learning recognized positive features)"),
                         tags$li("Downloadable tables in quotes page"),
                         h3("FAQ"),
                         tags$li("How can I download x from the dashboard?", tags$i("Everything is downloadable from the dashboard currently except for the tables under qualitative responses. See the video to learn how, for qualitative responses at the moment I recommend screenshotting, or downloading the reports and using the tables from there.")),
                         tags$li("What is the highlighting in the qualititative feedback tables?", tags$i("Highlighting uses a text pattern recognition algorithm to try to find frequent features in the text and then highlight the particular words it discerns."))
                     )
                 )
)

check_email_domain <- function(email, domain) {
  grepl(paste0("@",domain,"$"), email, ignore.case = TRUE)
}

# From Here: https://groups.google.com/a/teachinglab.org/g/employees/members
# approved_emails_list <- readr::read_csv(here::here("dashboards/SessionSurvey/Data/employees.csv"), skip = 1) %>%
#   dplyr::select(1) %>%
#   purrr::as_vector()
# approved_emails_list <- readr::read_csv("dashboards/SessionSurvey/Data/employees.csv", skip = 1) %>%
#   dplyr::select(1) %>%
#   purrr::as_vector() %>%
#   append("kristen.taylor@teachinglab.org")
# readr::write_rds(approved_emails_list, "dashboards/SessionSurvey/Data/employees.rds")
approved_emails_list <- readr::read_rds("data/employees.rds")

check_email_approved <- function(email, approved_emails_list) {
  email %in% approved_emails_list
}

