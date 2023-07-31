library(shiny)
library(shinymanager)
library(htmltools)
library(fontawesome)
library(tidyverse)
library(extrafont)
library(here)
library(shinydashboard)
library(shinydashboardPlus)
library(rmdformats)
library(fresh)
library(gt)
library(TeachingLab)

# Read in data sources ------------------------------------------------
# teaching_df <- read_csv(here("Data/Dashboard Data/dashboard_data.csv"))
Sites <- read_rds(here("Data/SY20-21/Sites.rds"))

# Deal with font issue ----------------------------------------------------
# dir.create("~/.fonts")
# file.copy("www/Arial Narrow.ttf", "~/.fonts")
# system("fc-cache -f ~/.fonts")

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

# Custom theme

# #5a859d

mytheme <- create_theme(
    theme = "readable",
    bs_vars_navbar(
        default_bg = "#ebebeb",
        default_color = "black",
        default_link_color = "black",
        default_link_active_color = "#04ABEB"
    ),
    bs_vars_color(
        gray_base = "#354e5c",
        brand_primary = "#04ABEB",
        brand_success = "#c9d175",
        brand_info = "#758bd1",
        brand_warning = "#d1ab75",
        brand_danger = "#d175b8"
    ),
    bs_vars_state(
        success_text = "#FFF",
        success_bg = "#c9d175",
        success_border = "#c9d175",
        info_text = "#FFF",
        info_bg = "#04ABEB",
        info_border = "#04ABEB",
        danger_text = "#FFF",
        danger_bg = "#d175b8",
        danger_border = "#d175b8"
    ),
    bs_vars_wells(
        bg = "#FFF",
        border = "#04ABEB"
    ),
    output_file = NULL
)


# data.frame with credentials info
credentials <- data.frame(
    user = c("1", "test", "teaching", "freire"),
    password = c("1", "test", "lab", "charter"),
    stringsAsFactors = FALSE
)

ui <- secure_app(head_auth = tags$script(inactivity),
                 navbarPage(
                     collapsible = T,
                     fluid = T,
                     position = "static-top",
                     shinyjs::useShinyjs(),
                     inverse = F,
                     selected = "All Linked Data",
                     header = tagList(
                         fresh::use_theme(mytheme)
                     ),
                     windowTitle = "Teaching Lab Data Share",
                     # footer = tags$footer("© Teaching Lab 2021", style = "position:fixed;
                     #                                                      bottom:0;
                     #                                                      right:0;
                     #                                                      left:0;
                     #                                                      padding:10px;
                     #                                                      box-sizing:border-box;
                     #                                                      color: black;"),
                     id = "navbar",
                     
                     
                     # Set background color
                     # shinyWidgets::setBackgroundColor("#484a4a"),
                     shinyWidgets::setBackgroundImage(
                         src = "website_background.png"
                     ),
                     
                     # App Title
                     title = tags$div(tags$img(src = "teachinglab_logo.png", width = 165, height = 50, style = "float:left; margin-left: 5px; margin-right: 5px; margin-top: -7px")),
                     
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
                     # tabPanel(
                     #     "Knowledge",
                     #     shiny::column(12, align = "center",
                     #                   shiny::selectInput("filters_all", choices = c("One Thing", "Other Thing"), label = h3("Select a Dataset")),
                     #                   downloadButton("download_knowledge", "Download", icon = shiny.semantic::icon("download"))
                     #     )
                     # ),
                     # Administrator Surveys --------------------------------------------------------
                     # tabPanel(
                     #     "Practice",
                     #     shiny::column(12, align = "center",
                     #                   shiny::selectInput("filters_all", choices = c("One Thing", "Other Thing"), label = h3("Select a Dataset")),
                     #                   downloadButton("download_practice", "Download", icon = shiny.semantic::icon("download"))
                     #     )
                     # ),
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
                     tabPanel("Render a Report",
                              shiny::column(12, align = "center",
                                  selectInput("partner", "Select a Partner:", Sites),
                                  selectInput("year", "Select a Year:", choices = c("SY20-21", "SY19-20")),
                                  actionButton("rmd", "Create Report", icon = shiny.semantic::icon("bars"))
                              ),
                              uiOutput("report")
                     )
                 )
                 )

server <- function(input, output, session) {
    
    # Hide the buttons for report generating on click
    # Create rmd with report
    observeEvent(input$rmd, {
        shinyjs::hide("rmd", anim = T)
        shinyjs::hide("year", anim = T)
        shinyjs::hide("partner", anim = T)
        shinyjs::hide("navbar", selector = "#navbar li a[data-value=mytab2]")
        
        output$report <- renderUI({
            params <- list(partner = input$partner, year = input$year)
            
            htmltools::includeHTML(rmarkdown::render(input = "Rmd/filter_based_report.Rmd"))
        })
    })
    
    result_auth <- secure_server(check_credentials = check_credentials(credentials))
    
    output$res_auth <- renderPrint({
        reactiveValuesToList(result_auth)
    })
    
    # classic app
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


shinyApp(ui = ui, server = server)