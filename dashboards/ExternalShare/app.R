library(shiny)
library(htmltools)
library(fontawesome)
library(tidyverse)
library(extrafont)
library(here)
library(shinydashboard)
library(shinydashboardPlus)


# Read in data sources ------------------------------------------------
teaching_df <- read_csv(here("Data/dashboard_data.csv"))

# Deal with font issue ----------------------------------------------------
# dir.create("~/.fonts")
# file.copy("www/Arial Narrow.ttf", "~/.fonts")
# system("fc-cache -f ~/.fonts")

# Create user interface -------------------------------
ui <- navbarPage(
  inverse = F,
  windowTitle = "Teaching Lab Data Share",
  position = "fixed-top",
  header = tags$style(
      ".navbar-right {
                       float: right !important;
                       }",
      "body {padding-top: 75px;}"),
  footer = dashboardFooter(
    left = "© Teaching Lab 2021"
  ),
  id = "teachinglab_data_share",
  # App Title
  tags$div(tags$img(src = "teachinglab_logo.png", width = 135, height = 40, style = "float:left; margin-left: 5px; margin-right: 5px; margin-top: -10px")),

  # Teacher Post-Survey ------------------------------------------------------
  tabPanel(
    "All Linked Data",
    shiny::column(12, align = "center",
           shiny::selectInput("filters_all", choices = c("One Thing", "Other Thing"), label = h3("Select a Dataset")),
           downloadButton("all_data", "Download", icon = shiny.semantic::icon("download"))
    )
  ),
  # Teacher Post-Survey ------------------------------------------------------
  tabPanel(
    "Mindsets",
    shiny::column(12, align = "center",
                  shiny::selectInput("filters_all", choices = c("One Thing", "Other Thing"), label = h3("Select a Dataset")),
    downloadButton("download_mindsets", "Download", icon = shiny.semantic::icon("download"))
    )
  ),
  # Teacher Pre-Survey -------------------------------------------------------
  tabPanel(
    "Knowledge",
    shiny::column(12, align = "center",
                  shiny::selectInput("filters_all", choices = c("One Thing", "Other Thing"), label = h3("Select a Dataset")),
    downloadButton("download_knowledge", "Download", icon = shiny.semantic::icon("download"))
    )
  ),
  # Administrator Surveys --------------------------------------------------------
  tabPanel(
    "Practice",
    shiny::column(12, align = "center",
                  shiny::selectInput("filters_all", choices = c("One Thing", "Other Thing"), label = h3("Select a Dataset")),
    downloadButton("download_practice", "Download", icon = shiny.semantic::icon("download"))
    )
  ),
  # Student Work Samples --------------------------------------------------------
  tabPanel(
    "Student Work Samples",
    shiny::column(12, align = "center",
                  shiny::selectInput("filters_all", choices = c("One Thing", "Other Thing"), label = h3("Select a Dataset")),
    downloadButton("download_student_work", "Download", icon = shiny.semantic::icon("download"))
    )
  ),
  # Student Surveys --------------------------------------------------------
  tabPanel(
    "Student Surveys",
    shiny::column(12, align = "center",
                  shiny::selectInput("filters_all", choices = c("One Thing", "Other Thing"), label = h3("Select a Dataset")),
    downloadButton("download_student_survey", "Download", icon = shiny.semantic::icon("download"))
    )
  ),
  # Student Assessments --------------------------------------------------------
  tabPanel(
    "Student Assessments",
    shiny::column(12, align = "center",
                  shiny::selectInput("filters_all", choices = c("One Thing", "Other Thing"), label = h3("Select a Dataset")),
    downloadButton("download_student_assessment", "Download", icon = shiny.semantic::icon("download"))
    )
  ),
  # Teacher Evaluation --------------------------------------------------------
  tabPanel(
    "Teacher Evaluation",
    shiny::column(12, align = "center",
                  shiny::selectInput("filters_all", choices = c("One Thing", "Other Thing"), label = h3("Select a Dataset")),
    downloadButton("download_teacher", "Download", icon = shiny.semantic::icon("download"))
    )
  ),
  # Potential Downloadable Report ------------------------------------------------
  tabPanel("Report",
    icon = shiny.semantic::icon("bars"),
    includeHTML(here("R/2019-2020Report.html"))
  )
)

# Server logic -----------------------------------

server <- function(input, output, session) {
  # MINDSETS DATA -----------------------------------
  output$download_mindsets <- downloadHandler(
    filename = function () {
        "teachinglab_professional_learning_post.csv"
        },
    content = function(file) {
      write_csv(teaching_df, file)
    }
  )
  # KNOWLEDGE DATA -----------------------------------
  output$download_knowledge <- downloadHandler(
    filename = function() {
        "teachinglab_professional_learning_pre.csv"
        },
    content = function(file) {
      write_csv(teaching_df, file)
    }
  )
  # PRACTICE DATA -----------------------------------
  output$download_practice <- downloadHandler(
    filename = function() {
        "teachinglab_professional_learning_admin.csv"
        },
    content = function(file) {
      write_csv(teaching_df, file)
    }
  )
  # STUDENT WORK SAMPLE DATA -----------------------------------
  output$download_student_work <- downloadHandler(
    filename = function () {
        "teachinglab_professional_learning_student.csv"
        },
    content = function(file) {
      write_csv(teaching_df, file)
    }
  )
  # STUDENT SURVEY DATA -----------------------------------
  output$download_student_survey <- downloadHandler(
    filename = function () {
      "teachinglab_professional_learning_post.csv"
    },
    content = function(file) {
      write_csv(teaching_df, file)
    }
  )
  # STUDENT ASSESSMENTS DATA -----------------------------------
  output$download_student_assessment <- downloadHandler(
    filename = function() {
      "teachinglab_professional_learning_pre.csv"
    },
    content = function(file) {
      write_csv(teaching_df, file)
    }
  )
  # TEACHER EVALUATION DATA -----------------------------------
  output$download_teacher <- downloadHandler(
    filename = function() {
      "teachinglab_professional_learning_admin.csv"
    },
    content = function(file) {
      write_csv(teaching_df, file)
    }
  )
  # ALL DATA -----------------------------------
  output$download_all <- downloadHandler(
    filename = function () {
      "teachinglab_professional_learning_student.csv"
    },
    content = function(file) {
      write_csv(teaching_df, file)
    }
  )
}

# Run app -------------------------------
shinyApp(ui = ui, server = server)
