library(shiny)
library(shiny.semantic)
library(semantic.dashboard) # <-- Change this line to: library(semantic.dashboard)
library(sysfonts)
library(tidyverse)
library(TeachingLab)
library(tidytext)
library(bookdown)
library(grid)
library(shinybusy)
font_add(family = "Calibri", regular = "www/Calibri.ttf")

round_even <- function(x) {
  2 * ceiling(x/2)
}

# write_rds(discrete_data, here::here("dashboards/RaceandCulture/data/discrete_data.rds"))
data <- read_rds("data/full_data.rds")

likert_data <- read_rds("data/likert_data.rds")
numeric_data <- read_rds("data/numeric_data.rds")
discrete_data <- read_rds("data/discrete_data.rds")
gwc_1 <- read_rds("data/gwc_plot1.rds")
gwc_2 <- read_rds("data/gwc_plot2.rds")

ui <- dashboardPage(
  theme = "cosmo",
  suppress_bootstrap = F,
  # tags$head(tags$link(rel="shortcut icon", href="www/favicon.ico")),
  dashboardHeader(
    title = h2("Race and Culture", style = "font-family:'Calibri'"),
    logo_path = "imgs/teaching_lab_logo.png",
    logo_align = "center",
    titleWidth = "wide",
    color = "black",
    inverted = F
  ),
  dashboardSidebar(
    size = "wide",
    sidebarMenu(
      menuItem(
        tabName = "instruction_tab",
        icon = shiny::icon("align-left"),
        uiOutput("text_appear")
      ),
      menuItem(
        tabName = "question_type_tab", icon = shiny::icon("home"),
        selectInput("question_type",
          label = h3("Select a Culture Question to Evaluate"),
          choices = c("Background", "General Working Conditions", "Management/Leadership", "Employee Engagement", "Equity & Inclusion",
                      "Social Learning Groups")
          # choices = c("Likert Scale Questions", "Numeric Rating Questions", "Discrete Questions")
        )
      ),
      menuItem(
        tabName = "question_tab", icon = shiny::icon("question"),
        uiOutput("question_select")
      )
    )
  ),
  dashboardBody(
    add_busy_spinner(spin = "fading-circle", color = "#04abeb"),
    uiOutput("ui_plot")
  )
)

server <- function(input, output) {

  # Render Question Type Sub-Item
  output$question_select <- renderUI({
    if (input$question_type == "General Working Conditions") {
      shiny::selectizeInput("question",
        label = h3("Add a Question to Evaluate"),
        choices = unique(likert_data$Question) %>% sort() %>% purrr::prepend("All Questions"),
        multiple = T,
        selected = "All Questions",
        options = list(plugins = list("remove_button"))
      )
    } else if (input$question_type == "Numeric Rating Questions") {
      shiny::selectizeInput("question",
        label = h3("Select a Question to Evaluate"),
        choices = unique(numeric_data$Question) %>% sort(),
        # multiple = T,
        selected = "How likely would you be to recommend working at Teaching Lab to a friend or colleague" # ,
        # options = list(plugins = list("remove_button"))
      )
    } else if (input$question_type == "Discrete Questions") {
      shiny::selectizeInput("question",
        label = h3("Select a Question to Evaluate"),
        choices = unique(discrete_data$Question) %>% sort(),
        # multiple = T,
        selected = "identify" # ,
        # options = list(plugins = list("remove_button"))
      )
    }
  })

  output$text_appear <- renderUI({
    if (input$question_type == "Likert Scale Questions") {
      h3("Add elements to the chart on the right after selecting the type of question by removing all questions and selecting specific questions of interest.")
    } else if (input$question_type == "Numeric Rating Questions") {
      h3("Select a question to see the results over time in the main body.")
    } else if (input$question_type == "Discrete Questions") {
      h3("Select a question to see the results over time in the main body.")
    }
  })

  # Plot reactive height
  plot_height <- reactive({
    height <- if (round_even(length(input$question)) == 0) { # Case handling for no selection
      "auto"
    } else if (round_even(length(input$question)) >= 4) { # For extending facet wrap in likert questions
      height1 <- round_even(length(input$question)) * 200
      paste0(height1, "px")
    } else if (input$question == "All Questions") { # For all questions
      "6400px"
    } else if (length(input$question) == 1 & input$question != "All Questions") { # For numeric and discrete questions
      "800px"
    }
  })

  output$plot1 <- renderPlot({
    if (input$question_type == "Likert Scale Questions") {
      req(input$question)
      likert_data %>%
        {
          if (input$question != "All Questions") dplyr::filter(., 
                                                               str_detect(Question, 
                                                                          paste(input$question, collapse = "|")
                                                                          )
                                                               ) else .
        } %>%
        mutate(Question = str_wrap(Question, width = 60)) %>%
        ggplot(aes(x = time, y = Percent, fill = time)) +
        geom_col() +
        geom_text(aes(label = paste0(round(Percent), "%")), vjust = -0.5) +
        facet_wrap(~Question, ncol = 2) +
        labs(x = "", title = "Percent that Agree/Strongly Agree with the Following Statements", x = "") +
        scale_fill_tl(n = 4) +
        scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
        theme_tl() +
        theme(
          strip.text = element_text(hjust = 0.5, face = "bold", family = "Calibri", size = 18),
          panel.spacing = unit(2, "lines"),
          plot.title = element_text(face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)
        )
    } else if (input$question_type == "Numeric Rating Questions") {
      
    } else if (input$question_type == "Discrete Questions") {
      req(input$question)
      discrete_data_plot <- discrete_data %>%
        filter(Question == input$question3) %>%
        mutate(time = str_wrap(time, width = 60))
      ggplot(discrete_data_plot, aes(x = Rating, y = Percent, fill = Rating)) +
        geom_col() +
        geom_text(aes(label = paste0(round(Percent), "%")), hjust = -0.5) +
        scale_fill_manual(values = tl_palette(n = length(unique(discrete_data$Rating)), theme = "dark", color = "green")) +
        scale_color_manual(values = tl_palette(n = length(unique(discrete_data$Rating)), theme = "dark", color = "green")) +
        scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
        facet_wrap(~ factor(time, levels = c("March 2019", "July 2019", "February 2020", "November 2020"))) +
        ggtitle(input$question) +
        coord_flip() +
        theme_tl() +
        theme(
          strip.text = element_text(hjust = 0.5, face = "bold", family = "Calibri", size = 18),
          panel.spacing = unit(2, "lines"),
          plot.title = element_text(face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)
        )
    }
  })
  
  output$plot2 <- renderPlot({
    if (input$question_type == "General Working Conditions") {
      req(input$question)
      numeric_data_plot <- numeric_data %>%
        mutate(time = str_wrap(time, width = 60))
      print(numeric_data_plot)
      numeric_data_plot %>%
        ggplot(aes(x = Rating, y = Percent, fill = Rating)) +
        geom_col() +
        geom_text(aes(label = paste0(round(Percent), "%")), vjust = -0.5) +
        ggtitle(input$question) +
        scale_fill_manual(values = c(
          "1" = "#040404", "2" = "#03161E", "3" = "#032938", "4" = "#023C52", "5" = "#024E6C", "6" = "#016187",
          "7" = "#0174A1", "8" = "#0086BB", "9" = "#0099D5", "10" = "#00ACF0"
        )) +
        scale_color_manual(values = c(
          "1" = "#040404", "2" = "#03161E", "3" = "#032938", "4" = "#023C52", "5" = "#024E6C", "6" = "#016187",
          "7" = "#0174A1", "8" = "#0086BB", "9" = "#0099D5", "10" = "#00ACF0"
        )) +
        scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
        facet_wrap(~ factor(time, levels = c("March 2019", "July 2019", "February 2020", "November 2020"))) +
        theme_tl() +
        theme(
          strip.text = element_text(hjust = 0.5, face = "bold", family = "Calibri", size = 18),
          panel.spacing = unit(2, "lines"),
          plot.title = element_text(face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)
        )
    }
  })
  
  output$ui_plot <- renderUI({
    plotOutput("plot1", height = plot_height(), width = "1200px")
  })
  
  output$ui_plot <- renderUI({
    plotOutput("plot2")
  })
}

shinyApp(ui, server)
