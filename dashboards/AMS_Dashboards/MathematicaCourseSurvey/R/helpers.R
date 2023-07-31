#### Mathematica End of Course Survey Dashboard ####
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shiny.router))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(surveymonkey))
suppressPackageStartupMessages(library(ggtext))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(TeachingLab))
suppressPackageStartupMessages(library(bookdown))
suppressPackageStartupMessages(library(tidytext))
suppressPackageStartupMessages(library(shinycssloaders))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(showtext))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(gt))
suppressPackageStartupMessages(library(ggfx))
suppressPackageStartupMessages(library(ggforce))
suppressPackageStartupMessages(library(shiny.semantic))
suppressPackageStartupMessages(library(glue))
suppressPackageStartupMessages(library(Cairo))
suppressPackageStartupMessages(library(grDevices))
library(shinyjs)
library(rmarkdown)
library(shinymanager)

credentials <- data.frame(
  user = c("ijun", "1"),
  password = c("supersecretpassword12", "1"),
  # password will automatically be hashed
  admin = c(TRUE),
  stringsAsFactors = FALSE
)

font_add(family = "Calibri", regular = "www/Calibri.ttf")
font_add(family = "Calibri Bold", regular = "www/Calibri Bold.ttf")

options(shiny.port = 7325)
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
                         img(src="imgs/CourseSummaryLafayette.png", height = "3.5%"),
                         br(),
                         br(),
                         br(),
                         br(),
                         img(src="imgs/NPS.png", height = "5.5%"),
                         br(),
                         br(),
                         br(),
                         br(),
                         img(src="imgs/QuotesLafayette.png", height = "16.3%"),
                         br(),
                         br(),
                         br(),
                         br(),
                         img(src="imgs/StaffandCulture.png", height = "12%")
                     ),
                     div(class = "eight wide column",
                         div(class="ui center aligned big header",
                             h2("Teaching Lab Course Survey Dashboard")),
                         p("This app was created for the purposes of illustrating and ennumerating ",
                           a(tags$b("Teaching Lab"), href = "https://www.teachinglab.org"),
                           " data for understanding and improving our impact. The dashboard automatically pulls from",
                           a("SurveyMonkey", href = "https://www.surveymonkey.com/r/TLendofcourse"), "responses and integrates ",
                           "the data in three different tabs; one to show answers to the percent that agree/strongly agree with",
                           "questions, one to show nps, and one to show quotes."),
                         p("Feel free to watch the video above to understand the full functionality ",
                           "of this application. The images on the left show the functionalities of each tab."),
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
                         p("Lastly there are downloadable reports that include all of the above features, as well as summary statements",
                           "about the percent of participants that agree/strongly agree with the feedback on facilitation.")
                     )
                 )
)

# Verifying from @teachinglab.org
check_email_domain <- function(email, domain) {
  grepl(paste0("@",domain,"$"), email, ignore.case = TRUE)
}