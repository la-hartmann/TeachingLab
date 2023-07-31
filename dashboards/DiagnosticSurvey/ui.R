#### UI for Diagnostic Dashboard ####
### Libraries Load ###

library(shiny)
library(bslib)
library(thematic)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(forcats)
library(readr)
library(stringr)
library(gt)
library(glue)
library(TeachingLab)
library(ggtext)
library(patchwork)
library(showtext)
### Graphics ###
options(shiny.useragg = T)
### Fonts ###
thematic::thematic_shiny(font = font_spec(families = c("Calibri", "Roboto")),
                         inherit = T)
font_add("Calibri", "www/Calibri.ttf")
font_add("Calibri Bold", "www/Calibri Bold.ttf")
theme_set(theme_tl(markdown = F)) # Have to set markdown as T in individual aesthetics for some reason
showtext_auto()

### boot dash layout funs --------------------------------------------------- ###


boot_side_layout <- function(...) {
  div(class = "d-flex wrapper", ...)
}

boot_sidebar <- function(...) {
  div(
    class = "bg-light border-right sidebar-wrapper",
    div(class = "list-group list-group-flush", ...)
  )
}

boot_main <- function(...) {
  div(
    class = "page-content-wrapper",
    div(class = "container-fluid", ...)
  )
}

### css --------------------------------------------------------------------- ###

css_def <- sass::sass_file("www/styles.scss")

#### All taken from https://github.com/rstudio/bslib/issues/76 ####

# Define UI for application that draws a histogram
page_navbar(

    # Application title and background color
    title = tags$b("Diagnostic Survey Dashboard", style = "padding-left:20px;"),
    # Window title
    window_title = "Diagnostic Survey",
    # Theme settings
    theme = bslib::bs_theme(base_font = c("Calibri", "sans-serif"), primary = "#04abeb") %>%
        bslib::bs_add_rules(css_def),

    nav("Results", boot_side_layout(
        boot_sidebar(
          selectizeInput("grouping",
                         label = "Select a question grouping",
                         choices = c("Teacher - Curricula Use" = "teacher_curricula_use", 
                                     "Teacher - Mindsets" = "teacher_mindsets", 
                                     "Teacher - CRSE practices" = "teacher_crse_practices",
                                     "Teachers - School Environment" = "teacher_school_environment", 
                                     "Administrator - Mindsets" = "administrator_mindsets",
                                     "Administrators - Support of CRT Practices" = "administrator_support_crt",
                                     "Administrators - Observational Practices" = "administrator_observational_practices"),
                         multiple = F),
          selectizeInput("comparison", 
                         label = "Select a basis of Comparison",
                         choices = c("Lab Leader", "Race", "Role (Teacher, etc.)", "Subject Area (Math/ELA)"),
                         multiple = T,
                         options = list(plugins= list('remove_button'))),
          selectizeInput("site", 
                         label = "Select Sites to Include",
                         choices = diagnostic$your_site_district_parish_network_or_school_br_br %>% unique() %>% sort(),
                         multiple = T,
                         options = list(plugins= list('remove_button'))),
          selectizeInput("grade", 
                         label = "Select Grades to Include",
                         choices = c("K", 1:12),
                         multiple = T,
                         options = list(plugins= list('remove_button'))),
          dateRangeInput(inputId = "date_range",
                         label = "Select a Date Range",
                         start = min(diagnostic$date_created, na.rm = T),
                         end = max(diagnostic$date_created, na.rm = T)),
          tags$html("*Note that for all of the above filters, the default (no selection) will select all results.")
        ),
        boot_main(
            fluidRow(
                column(12, plotOutput(outputId = "main_plot", width = "auto"))
            )
        )
    )),
    nav("Summary Statistics", 
        gt_output("diagnostic_text"),
        plotOutput("diagnostic_correct")),
    nav_item(
        tags$a(icon("youtube"), "Tutorial", href = "https://youtube.com", target = "_blank")
    ),
    nav_spacer(),
    nav_menu(
        "Other links", align = "right",
        nav_item(
            tags$a(icon("chart-bar"), "Session Survey Dashboard", href = "https://teachinglabhq.shinyapps.io/SessionSurvey/", target = "_blank")
        ),
        nav_item(
            tags$a(icon("table"), "Course Survey Dashboard", href = "https://teachinglabhq.shinyapps.io/CourseSurvey/", target = "_blank")
        )
    ),
    
    footer = div(
        style = "width:100%; margin: 0 auto; text-align: center; padding: 5px; bottom: 0; position:fixed;",
        "© Teaching Lab, 2021",
    )
)
