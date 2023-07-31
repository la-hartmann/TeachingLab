# Teaching Lab Shiny Application
suppressPackageStartupMessages(library(tidyverse)) # General data cleaning, graphics, etc.
suppressPackageStartupMessages(library(ggforce)) # Not sure
suppressPackageStartupMessages(library(lubridate)) # Dates and times
suppressPackageStartupMessages(library(here)) # Library calling
suppressPackageStartupMessages(library(scales)) # For graphing scales
suppressPackageStartupMessages(library(shiny)) # Required for app
suppressPackageStartupMessages(library(shinyWidgets)) # Making widgets sometimes, most called by semantic
suppressPackageStartupMessages(library(DT)) # Data tables
suppressPackageStartupMessages(library(grDevices)) # Color palettes
suppressPackageStartupMessages(library(shinyjs)) # Clickability
suppressPackageStartupMessages(library(semantic.dashboard)) # Custom themeing
suppressPackageStartupMessages(library(shiny.semantic)) # Custom themeing
# suppressPackageStartupMessages(library(Cairo))
suppressPackageStartupMessages(library(showtext)) # Rendering fonts
suppressPackageStartupMessages(library(waffle)) # Waffle charts
suppressPackageStartupMessages(library(treemap)) # Tree maps
# suppressPackageStartupMessages(library(wordcloud2)) # Wordclouds if I decide to add them
suppressPackageStartupMessages(library(tidytext)) # Sentiment modelling
suppressPackageStartupMessages(library(gt)) # Table making
suppressPackageStartupMessages(library(bslib)) # Not sure
suppressPackageStartupMessages(library(shinycssloaders))
# font_add("Open Sans", here("www/OpenSans-Regular.ttf"))
# font_add("Open Sans ExtraBold", here("www/OpenSans-ExtraBold.ttf"))
font_add_google(name = "Open Sans", family = "Open Sans") # Adds font for ggplot
showtext_auto()
showtext_opts(dpi = 150) # Changes ggplot font size
options(semantic.themes = T) # Use semantic themes
# library(shinyalert)
# extrafont::ttf_import(pattern = "OpenSans-Regular.ttf", paths = here("ParticipantFeedback/www"))
# extrafont::loadfonts()

#### FOR SHINY SERVER ONLY NEEDED TO BE RUN ONCE
# dir.create('~/.fonts')
# file.copy("www/Open Sans-Regular.ttf",
#           "~/.fonts")
# file.copy("www/Open Sans-ExtraBold.ttf",
#           "~/.fonts")
# file.copy("www/Open Sans-Bold.ttf",
#           "~/.fonts")
# system('fc-cache -f ~/.fonts')
####

# extrafont::font_import(pattern = "OpenSans-Regular", prompt = F)
# extrafont::ttf_import(pattern = "OpenSans-ExtraBold.ttf", paths = here("ParticipantFeedback/www"))

teaching_df <- read_rds(here("Data/dashboard_data.rds")) # See R/InternalDashboardDataSetup.R for information on data manipulation

factor_cols <- c("% Satisfied With The Overall Quality Of Today's Professional Learning Session", 
                 "% Who Say Today's Topic Was Relevant For My Role", "% Who Say Activities Of Today's Session Were Well-Designed To Help Me Learn", 
                 "How Likely Are You To Apply This Learning To Your Practice In The Next 4-6 Weeks?", 
                 "S/He Facilitated The Content Clearly", 
                 "S/He Effectively Built A Community Of Learners", 
                 "The independent online work activities were well-designed to help me meet the learning targets.", 
                 "The Zoom meeting activities were well-designed to help me meet the learning targets.", 
                 "I felt a sense of community with the other participants in this course even though we were meeting virtually.", 
                 "This course helped me navigate remote and/or hybrid learning during COVID-19.")

# Teaching Lab theme
# theme_tl <- function(){
#   
#   font <- "Open Sans" # Assign font up front
#   
#   theme_bw() %+replace%
#     theme(
#       legend.position = "none",
#       legend.title = element_blank(),
#       plot.title = element_text(hjust = 0.5, font),
#       plot.subtitle = element_text(hjust = 0.5, font),
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor.y = element_blank(),
#       axis.text.x = element_text(color = "#324D56", font),
#       axis.title.y = element_text(font),
#       axis.title.x = element_text(font),
#       axis.text.y = element_text(color = "#324D56", font),
#       legend.text = element_text(font),
#       text = element_text(font))
# }
# 
# # Filtering columns for gt table
# cols_filtered <- c("Clear\nFacilitation #1",
#                    "Community of\nLearners #1", 
#                    "Clear\nFacilitation #2",
#                    "Community\nof Learners #2",
#                    "Satisfied with\nOverall Quality",
#                    "Relevance to\nRole",
#                    "Helped Me\n Learn",
#                    "Apply Next 4-\n6 Weeks")
# 
# 
# # Labels for ggplot gt table
# x_labels <- c("Strongly\nDisagree", "Disagree", "Neither Agree or Disagree", "Agree", "Strongly\nAgree")
# 
# # Function for creating a plot
# plot_group <- function(name, df) {
#   plot_object <- ggplot(data = df, aes(x = factor(question), y = likert_numeric, 
#                                        fill = factor(pos, levels = c("positive", "negative")))) +
#     geom_col(color = "black") +
#     geom_text(aes(y = 0, label = ifelse(is.na(likert_numeric), "No Data Available", "")), family = "Oswald", color = "black") +
#     scale_fill_manual(values = c(positive = "#04ABEB", negative = "black")) +
#     labs(y = "Rating", x = "Question") +
#     scale_y_continuous(labels = x_labels, breaks = c(-2,-1,0,1,2), limits = c(-2,2)) +
#     theme_tl() +
#     coord_flip() +
#     theme(axis.title.y = element_text(angle = 90))
#   return(plot_object)
# }
# 
# # Combine ggplot within table
# fmt_ggplot <- fmt_gg <- function(
#   data,
#   columns,
#   rows = NULL,
#   height = 100,
#   aspect_ratio = 1.0
# ) {
#   rows <- rlang::enquo(rows)
#   
#   fmt(
#     data = data,
#     columns = columns,
#     rows = !!rows,
#     fns = list(
#       html = function(x) {
#         map(
#           x,
#           ggplot_image,
#           height = height,
#           aspect_ratio = aspect_ratio
#         )
#       }
#     )
#   )
# }

# colordates <- unique(c(na.omit(teaching_df$`Date for the session`))) # Need to figure out how to highlight available dates


# sidebarPanel2 <- function (..., out = NULL, width = 4)
# {
#     div(class = paste0("col-sm-", width),
#         tags$form(class = "well", ...),
#         out
#     )
# }


# Define UI for application that draws
ui <- 
  # navbarPage(
  # id = "nav_bar",
  # tabPanel("MAIN", value = "viz_panel",
  dashboard_page(
    
  title = "Teaching Lab PL Internal Dashboard",

  theme = "solar",

  suppress_bootstrap = F,

  # tabset(
  #   tabs = list(
  #     list(menu = "First Tab", content = "Text works well", id = "first_tab"),
  #     list(menu = "Second Tab", content = uiOutput("viz_panel"), id = "second_tab"),
  #     list(menu = "Third Tab", content = plotOutput("piePlot"))
  #   ),
  #   active = "second_tab",
  #   id = "exampletabset"
  # ),

  # Application title
  dashboardHeader(
    title = h2("Internal Dashboard", style = "font-family:'Open Sans Bold'"),
    logo_path = "teaching_lab_logo.png",
    logo_align = "center",
    titleWidth = "wide",
    color = "black",
    inverted = F
  ),
  # Sidebar with a series of selections for variables
  dashboardSidebar(
    side = "left", size = "wide", closable = T,
    sidebar_menu(
      # tags$head(tags$style("input.date {font-family: Open Sans;}")), # Trying to change widget font
      tags$head(includeCSS("www/styles.css")), # Open Sans CSS
      menu_item("",
        icon = shiny::icon("chart-pie"),
        conditionalPanel(condition = "input.viz_type != 'Text Visualization'",
            selectInput("data",
              c("All", 
                "How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?",
                "% Satisfied With The Overall Quality Of Today's Professional Learning Session", 
                "% Who Say Today's Topic Was Relevant For My Role", "% Who Say Activities Of Today's Session Were Well-Designed To Help Me Learn", 
                "How Likely Are You To Apply This Learning To Your Practice In The Next 4-6 Weeks?", 
                "S/He Facilitated The Content Clearly", 
                "S/He Effectively Built A Community Of Learners", 
                "The independent online work activities were well-designed to help me meet the learning targets.", 
                "The Zoom meeting activities were well-designed to help me meet the learning targets.", 
                "I felt a sense of community with the other participants in this course even though we were meeting virtually.", 
                "This course helped me navigate remote and/or hybrid learning during COVID-19."),
              selected = NULL,
              label = h3("Select Variable of Interest:", style = "font-family:'Open Sans ExtraBold';"),
              width = 400
              )
          ),
        conditionalPanel(condition = "input.viz_type == 'Text Visualization'",
                          selectInput("text_filters",
                                      choices = c("Random", "Areas of Strength", "Areas for Improvement"),
                                      selected = NULL, 
                                      label = h3("Select a Method of Text Filtration", style = "font-family:'Open Sans ExtraBold';"),
                                      width = 400)),
        # ), 
        br(),
        selectInput("viz_type",
                    choices = c("Column Chart",
                                "Donut Chart",
                                "Pie Chart",
                                "Tree Map",
                                "Waffle Chart",
                                "Text Visualization"),
                    selected = "Column Chart",
                    label = h3("Select a Type of Visualization", style = "font-family:'Open Sans ExtraBold';"),
                    width = 400),
        br(),
        div(class = "ui form",
          multiple_radio("color",
                       choices = c("Blue", "Green", "Orange", "Purple"),
                       selected = "Blue",
                       position = "grouped",
                       type = "toggle",
                       label = h3("Select a Color Palette:", style = "font-family:'Open Sans ExtraBold';")
        )
        ),
        conditionalPanel(condition = "input.viz_type == 'Text Visualization'",
                         textInput("string_filters",
                                   label = h3("Filter for a Specific Word/Phrase", style = "font-family:'Open Sans ExtraBold';"),
                                   value = "",
                                     width = 400))
        ),
      menu_item("",
        icon = shiny::icon("calendar"),
        # textOutput("textData"),
        dateRangeInput("date",
          label = h3("Calendar Select:", style = "font-family:'Open Sans ExtraBold';"),
          start = min(as.Date(teaching_df$`Date for the session`), na.rm = T),
          end = max(as.Date(teaching_df$`Date for the session`), na.rm = T),
          min = min(as.Date(teaching_df$`Date for the session`), na.rm = T),
          max = max(as.Date(teaching_df$`Date for the session`), na.rm = T),
          format = "yyyy-mm-dd",
          width = "100%",
          tags$head(includeCSS("www/styles.css"))
        ),
        br(),
        shiny::sliderInput("date2",
          label = h3("Date Slider:", style = "font-family:'Open Sans ExtraBold';margin-bottom:6px;"),
          value = c(
            min(as.Date(teaching_df$`Date for the session`), na.rm = T),
            max(as.Date(teaching_df$`Date for the session`), na.rm = T)
          ),
          min = min(as.Date(teaching_df$`Date for the session`), na.rm = T),
          max = max(as.Date(teaching_df$`Date for the session`), na.rm = T),
          timeFormat = "%b %d, %Y"
        ),
        br()
      ),
      menu_item("",
                icon = shiny::icon("globe"),
                selectizeInput("facilitator",
                               choices = unique(teaching_df$`Name Of Your Facilitator`) %>% 
                                 sort() %>%
                                 purrr::prepend("All"),
                               selected = "All",
                               label = h3("Select a Facilitator:", style = "font-family:'Open Sans ExtraBold';"),
                               width = 400
                )
      ),
      menu_item("",
        icon = shiny::icon("globe"),
        selectizeInput("portfolio",
          choices = unique(teaching_df$Portfolio) %>% purrr::prepend("All") %>% sort(),
          selected = "All",
          label = h3("Select a Portfolio:", style = "font-family:'Open Sans ExtraBold';"),
          width = 400,
          size = 200
        )
      ),
      menu_item("",
        icon = shiny::icon("globe"),
        selectizeInput("course",
          choices = list("All",
                         "EL" = teaching_df %>% filter(Portfolio == "EL") %>% select(`Professional Training Session`) %>% unique() %>% arrange() %>% pull(),
                         "Guidebooks" = teaching_df %>% filter(Portfolio == "Guidebooks") %>% select(`Professional Training Session`) %>% unique() %>% arrange() %>% pull(),
                         "IM" = teaching_df %>% filter(Portfolio == "Illustrative Mathematics") %>% select(`Professional Training Session`) %>% unique() %>% arrange() %>% pull(),
                         "State-Level" = teaching_df %>% filter(Portfolio == "State-Level") %>% select(`Professional Training Session`) %>% unique() %>% arrange() %>% pull()),
            # unique(teaching_df$`Professional Training Session`) %>% sort() %>% purrr::prepend("All"),
          selected = "All",
          label = h3("Select a Course:", style = "font-family:'Open Sans ExtraBold';"),
          width = 400
        )
      ),
      menu_item("",
        icon = shiny::icon("globe"),
        selectizeInput("client",
          choices = unique(teaching_df$`District, Parish, Or Network`) %>% sort() %>% purrr::prepend("All"),
          selected = "All",
          label = h3("Select a Client:", style = "font-family:'Open Sans ExtraBold';"),
          width = 400
        )
      ),
      menu_item("",
                icon = shiny::icon("globe"),
                tags$a(href = "https://www.youtube.com/watch?v=vfqhLSImpLs", "Dashboard Tutorial"),
                tags$a(href = "https://docs.google.com/forms/d/e/1FAIpQLSey0OlydXDvgZfpieYfsLG9kc4sn8zUOzuxP2gxT3O0ZcGdRA/viewform?usp=sf_link", "Feedback Form", style = "margin-left:75px;color:#ff6961;")
      )
    )
  ),

  # Show a plot the pie charts in one row, tables in another
  dashboardBody(
    tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")), # Favicon add
    fluidRow(
      conditionalPanel(condition = "input.viz_type == 'Text Visualization'",
                     gt_output("gt_text"))
      ),
    fluidRow(
      # Left-most plot
      column(
        8,
        style='margin-top:-30px', # Shifts plots up 
        conditionalPanel(condition = "input.viz_type != 'Text Visualization'",
                          withSpinner(plotOutput("piePlotNA", width = "100%"), type = 3, color.background = "white")
                         )
      ),
      # Right-most plot
      column(8, 
             style='margin-top:-30px', # Shifts plots up
             conditionalPanel(condition = "input.viz_type != 'Text Visualization'",
              withSpinner(plotOutput("piePlot", width = "100%"), type = 3, color.background = "white")
              )
             )
    ),
    fluidRow(
      # Left-most table
      column(
        8,
        style='padding-left:5px; padding-right:5px; padding-top:38px; padding-bottom:5px', # Add padding to the top of table and left-right
        conditionalPanel(condition = "input.viz_type != 'Text Visualization'",
          withSpinner(DTOutput("tableData2"), type = 3, color.background = "white")
        )
      ),
      # Right-most table
      column(8, 
             style='padding-left:5px; padding-right:5px; padding-top:38px; padding-bottom:5px', # Add padding to the top of table and left-right
             conditionalPanel(condition = "input.viz_type != 'Text Visualization'",
              withSpinner(DTOutput("tableData"), type = 3, color.background = "white")
              )
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive dropdowns
  ## Portfolio Reactive
  # output$portfolio <- renderUI({
  #   selectizeInput("portfolio",
  #                  choices = c("All", unique(teaching_df$`Portfolio`)),
  #                  selected = "All",
  #                  # options = list(
  #                  #   placeholder = 'Please select an option below',
  #                  #   onInitialize = I('function() { this.setValue(""); }')
  #                  # ),
  #                  label = h3("Select a Portfolio:", style = "font-family:'Open Sans ExtraBold';"),
  #                  width = 400
  #   )
  # })
  ## Course reactive
  # output$course <- renderUI({
  #   if (input$portfolio == "All") {
  #     selectizeInput("course",
  #                    choices = list("All",
  #                                   "EL" = teaching_df %>% filter(Portfolio == "EL") %>% select(`Professional Training Session`) %>% unique() %>% arrange() %>% pull(),
  #                                   "Guidebooks" = teaching_df %>% filter(Portfolio == "Guidebooks") %>% select(`Professional Training Session`) %>% unique() %>% arrange() %>% pull(),
  #                                   "IM" = teaching_df %>% filter(Portfolio == "Illustrative Mathematics") %>% select(`Professional Training Session`) %>% unique() %>% arrange() %>% pull(),
  #                                   "State-Level" = teaching_df %>% filter(Portfolio == "State-Level") %>% select(`Professional Training Session`) %>% unique() %>% arrange() %>% pull()),
  #                    # unique(teaching_df$`Professional Training Session`) %>% sort() %>% purrr::prepend("All"),
  #                    selected = "All",
  #                    multiple = T,
  #                    label = h3("Select a Course:", style = "font-family:'Open Sans ExtraBold';"),
  #                    width = 400
  #     )
  #   } else if (input$portfolio == "EL") {
  #     selectizeInput("course",
  #                    choices = teaching_df %>% filter(Portfolio == "EL") %>% select(`Professional Training Session`) %>% 
  #                      unique() %>% prepend("All"),
  #                    # unique(teaching_df$`Professional Training Session`) %>% sort() %>% purrr::prepend("All"),
  #                    selected = "All",
  #                    label = h3("Select a Course:", style = "font-family:'Open Sans ExtraBold';"),
  #                    width = 400
  #     )
  #   } else if (input$portfolio == "Guidebooks") {
  #     selectizeInput("course",
  #                    choices = teaching_df %>% filter(Portfolio == "Guidebooks") %>% select(`Professional Training Session`) %>% 
  #                      unique() %>% prepend("All"),
  #                    # unique(teaching_df$`Professional Training Session`) %>% sort() %>% purrr::prepend("All"),
  #                    selected = "All",
  #                    label = h3("Select a Course:", style = "font-family:'Open Sans ExtraBold';"),
  #                    width = 400
  #     )
  #   } else if (input$portfolio == "IM") {
  #     selectizeInput("course",
  #                    choices = teaching_df %>% filter(Portfolio == "IM") %>% select(`Professional Training Session`) %>% 
  #                      unique() %>% prepend("All"),
  #                    # unique(teaching_df$`Professional Training Session`) %>% sort() %>% purrr::prepend("All"),
  #                    selected = "All",
  #                    label = h3("Select a Course:", style = "font-family:'Open Sans ExtraBold';"),
  #                    width = 400
  #     )
  #   } else if (input$portfolio == "State-Level") {
  #     selectizeInput("course",
  #                    choices = teaching_df %>% filter(Portfolio == "State-Level") %>% select(`Professional Training Session`) %>% 
  #                      unique() %>% prepend("All"),
  #                    # unique(teaching_df$`Professional Training Session`) %>% sort() %>% purrr::prepend("All"),
  #                    selected = "All",
  #                    label = h3("Select a Course:", style = "font-family:'Open Sans ExtraBold';"),
  #                    width = 400
  #     )
  #   }
  # })
  ## Client Reactive
  # output$client <- renderUI({
  #   
  # })
  
  # # Client reactive
  # output$client <- renderUI({
  #   selectizeInput("client",
  #                  choices = c("All", unique(
  #                    teaching_df$`District, Parish, Or Network`[teaching_df$Portfolio %in% str_remove(input$course, "All")])), # Somehow filter for in portfolio post unique
  #                  selected = "All",
  #                  # options = list(
  #                  #   placeholder = 'Please select an option below',
  #                  #   onInitialize = I('function() { this.setValue(""); }')
  #                  # ),
  #                  label = h3("Select a Client:", style = "font-family:'Open Sans ExtraBold';"),
  #                  width = 400
  #   )
  # })
  # Grade band reactive
  
  # Facilitator reactive
  
  # Prevent errors in if statements, should work but doesn't
  # outputOptions(output, "course", suspendWhenHidden = FALSE)
  
  # Make dates stay the same
  ## Avoid chain reaction
  reactdelay <- 1
  change_slider <- reactiveVal(Sys.time())
  change_daterange <- reactiveVal(Sys.time())

  # Check date input
  observeEvent(input$date2, {
    # req(input$date)
    if (difftime(Sys.time(), change_slider()) > reactdelay) {
      change_daterange(Sys.time())
      updateDateRangeInput(session,
        "date",
        start = input$date2[1],
        end = input$date2[2]
      )
    }
  })

  observeEvent(input$date, {
    if (difftime(Sys.time(), change_daterange()) > reactdelay) {
      change_slider(Sys.time())
      shiny::updateSliderInput(session,
        "date2",
        value = c(
          input$date[1],
          input$date[2]
        ),
        timeFormat = "%b, %d, %Y"
      ) # Needs fixing
    }
  })
  
  
  
  col <- reactive({
                 if(input$color == "Blue") {
                   col <- grDevices::colorRampPalette(c("#040404", "#04ABEB"))
                 } else if (input$color == "Green") {
                   col <- grDevices::colorRampPalette(c("#040404", "#43c6b9"))
                 } else if (input$color == "Orange") {
                   col <- grDevices::colorRampPalette(c("#040404", "#ff7b43"))
                 } else if (input$color == "Purple") {
                   col <- grDevices::colorRampPalette(c("#040404", "#d17df7"))
                 } else {
                   col <- grDevices::colorRampPalette(c("#040404", "#04ABEB"))
                 }
               })
  
  options(spinner.color = "#04ABEB")
  # Update spinner color
  # eventReactive(input$color, {
    # options(spinner.color = col()(2)[2])
  #   })
  
  # Data for second pie chart and table (rightmost)
  mydata <- reactive({
    if (input$data != "All") {
      # Filter for portfolio
      teaching_df <- if (input$portfolio == "All") {
        teaching_df
      } else {
        teaching_df %>% 
          dplyr::filter(input$portfolio == Portfolio)
      }
      # Filter for course
      teaching_df <- if (input$course == "All") {
        teaching_df
      } else {
        teaching_df %>% dplyr::filter(`Professional Training Session` == input$course)
      }
      # Filter for client
      teaching_df <- if (input$client == "All") {
        teaching_df
      } else {
        teaching_df %>% dplyr::filter(`District, Parish, Or Network` == input$client)
      }
      # Filter for facilitator
      teaching_df <- if (input$facilitator == "All") {
        teaching_df
      } else {
        teaching_df %>% dplyr::filter(`Name Of Your Facilitator` == input$facilitator)
      }
      
      teaching_df <- if(input$data == "How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?") {
        # For likeliness to recommend to colleague or friend
        teaching_df %>%
          dplyr::filter(between(`Date for the session`, input$date[1], input$date[2])) %>%
          mutate(`Date for the session` = paste0(lubridate::month(`Date for the session`, label = T, abbr = F), ", ", year(`Date for the session`))) %>%
          group_by(`Date for the session`) %>%
          summarise(Percent = round(((length(which(as.numeric(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`) %in% c(9, 10)))/
                                        length(as.numeric(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`))) -
                                       (length(which(as.numeric(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`) %in% c(0:6)))/
                                          length(as.numeric(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`))))*100, 2)) %>%
          drop_na()
      } else {
        teaching_df %>%
          dplyr::filter(between(`Date for the session`, input$date[1], input$date[2])) %>%
          group_by(get(input$data), `Date for the session`) %>%
          summarise(`Number Agree/Disagree` = n()) %>%
          drop_na() %>%
          mutate(Rating = case_when(`get(input$data)` %in% c("Agree", "Strongly agree") ~ "Agree/Strongly Agree",
                                    `get(input$data)` %in% c("Neither agree nor disagree", "Disagree", "Strongly disagree") ~ "Neither/Disagree/Strongly Disagree"),
                 `Date for the session` = paste0(lubridate::month(`Date for the session`, label = T, abbr = F), ", ", year(`Date for the session`))) %>%
          group_by(`Date for the session`) %>%
          drop_na() %>%
          mutate(Percent = `Number Agree/Disagree`/sum(`Number Agree/Disagree`)*100) %>%
          filter(Rating == "Agree/Strongly Agree") %>%
          group_by(`Date for the session`, Rating) %>%
          summarise(Percent = round(sum(Percent), 2))
      }
    }
    # cat(str(teaching_df))
  })
  
  # Data for first pie chart and table (leftmost)
  mydata2 <- reactive({
    if (input$data != "All") {
      # Filter for portfolio
      teaching_df <- if (input$portfolio == "All") {
        teaching_df
      } else {
        teaching_df %>% 
          dplyr::filter(input$portfolio == Portfolio)
      }
      # Filter for course
      teaching_df <- if (input$course == "All") {
        teaching_df
      } else {
        teaching_df %>% dplyr::filter(`Professional Training Session` == input$course)
      }
      # Filter for client
      teaching_df <- if (input$client == "All") {
        teaching_df
      } else {
        teaching_df %>% dplyr::filter(`District, Parish, Or Network` == input$client)
      }
      # Filter for facilitator
      teaching_df <- if (input$facilitator == "All") {
        teaching_df
      } else {
        teaching_df %>% dplyr::filter(`Name Of Your Facilitator` == input$facilitator)
      }
  
      teaching_df %>%
        dplyr::filter(between(`Date for the session`, input$date[1], input$date[2])) %>%
        dplyr::group_by(get(input$data)) %>% # Group by input variable
        dplyr::summarise(n = n()) %>% # Get count of variable
        ungroup() %>% # Ungroup
        dplyr::filter(`get(input$data)` != "No Response") %>% # Filter out non-responses
        dplyr::mutate(percent = round(100 * (n / sum(n)), 2)) %>% # Make a percent column without non-responses
        dplyr::relocate(percent, .before = n)
    }
  })
  
  all_data <- reactive({
    # Filter for portfolio
    teaching_df <- if (input$portfolio == "All") {
      teaching_df
    } else {
      teaching_df %>% 
        dplyr::filter(input$portfolio == Portfolio)
    }
    # Filter for course
    teaching_df <- if (input$course == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`Professional Training Session` == input$course)
    }
    # Filter for client
    teaching_df <- if (input$client == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`District, Parish, Or Network` == input$client)
    }
    # Filter for facilitator
    teaching_df <- if (input$facilitator == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`Name Of Your Facilitator` == input$facilitator)
    }
    
    teaching_df %>%
      select(`Date for the session`, `% Satisfied With The Overall Quality Of Today's Professional Learning Session`, 
             `% Who Say Today's Topic Was Relevant For My Role`, 
             `% Who Say Activities Of Today's Session Were Well-Designed To Help Me Learn`, 
             `How Likely Are You To Apply This Learning To Your Practice In The Next 4-6 Weeks?`, 
             `S/He Facilitated The Content Clearly`, 
             `S/He Effectively Built A Community Of Learners`,
             `The independent online work activities were well-designed to help me meet the learning targets.`, 
             `The Zoom meeting activities were well-designed to help me meet the learning targets.`, 
             `I felt a sense of community with the other participants in this course even though we were meeting virtually.`, 
             `This course helped me navigate remote and/or hybrid learning during COVID-19.`) %>%
      dplyr::filter(between(`Date for the session`, input$date[1], input$date[2])) %>%
      # Rename with line breaks every 24 characters
      rename_with( ~ gsub("(.{25,}?)\\s", "\\1\n", .x)) %>%
      pivot_longer(!`Date for the session`, names_to = "question", values_to = "answer") %>%
      dplyr::filter(answer != "No Response") %>% # Filter out non-responses
      dplyr::group_by(answer, question) %>% # Group by input variable
      dplyr::summarise(n = n()) %>% # Get count of variable
      dplyr::group_by(question) %>%
      dplyr::mutate(percent = round(100 * (n / sum(n)), 2)) %>% # Make a percent column without non-responses
      dplyr::relocate(percent, .before = n)
  })
  
  all_data_ts <- reactive({
    # Filter for portfolio
    teaching_df <- if (input$portfolio == "All") {
      teaching_df
    } else {
      teaching_df %>% 
        dplyr::filter(input$portfolio == Portfolio)
    }
    # Filter for course
    teaching_df <- if (input$course == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`Professional Training Session` == input$course)
    }
    # Filter for client
    teaching_df <- if (input$client == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`District, Parish, Or Network` == input$client)
    }
    # Filter for facilitator
    teaching_df <- if (input$facilitator == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`Name Of Your Facilitator` == input$facilitator)
    }
    
    teaching_df %>%
      select(`Date for the session`, `% Satisfied With The Overall Quality Of Today's Professional Learning Session`, 
             `% Who Say Today's Topic Was Relevant For My Role`, 
             `% Who Say Activities Of Today's Session Were Well-Designed To Help Me Learn`, 
             `How Likely Are You To Apply This Learning To Your Practice In The Next 4-6 Weeks?`, 
             `S/He Facilitated The Content Clearly`, 
             `S/He Effectively Built A Community Of Learners`,
             `The independent online work activities were well-designed to help me meet the learning targets.`, 
             `The Zoom meeting activities were well-designed to help me meet the learning targets.`, 
             `I felt a sense of community with the other participants in this course even though we were meeting virtually.`, 
             `This course helped me navigate remote and/or hybrid learning during COVID-19.`) %>%
      dplyr::filter(between(`Date for the session`, input$date[1], input$date[2])) %>%
      # Rename with line breaks every 24 characters
      rename_with( ~ gsub("(.{25,}?)\\s", "\\1\n", .x)) %>%
      pivot_longer(!`Date for the session`, names_to = "question", values_to = "answer") %>%
      dplyr::filter(`answer` != "No Response") %>% # Filter out non-responses
      dplyr::group_by(question, `Date for the session`) %>% # Group by input variable
      mutate(`Number Agree/Disagree` = n()) %>%
      mutate(Rating = case_when(answer %in% c("Agree", "Strongly agree") ~ "Agree/Strongly Agree",
                                answer %in% c("Neither agree nor disagree", "Disagree", "Strongly disagree") ~ "Neither/Disagree/Strongly Disagree"),
             `Date for the session` = paste0(lubridate::month(`Date for the session`, label = T, abbr = F), ", ", year(`Date for the session`))) %>%
      ungroup() %>%
      group_by(`Date for the session`, question) %>%
      mutate(Percent = `Number Agree/Disagree`/sum(`Number Agree/Disagree`)*100) %>%
      filter(Rating == "Agree/Strongly Agree") %>%
      group_by(`Date for the session`, Rating, question) %>%
      summarise(Percent = round(sum(Percent), 2))
  })
  
  # Text Dataframe
  text_viz_data <- reactive({
    # Filter for date and make rating numeric
    teaching_df <- teaching_df %>%
      dplyr::filter(between(`Date for the session`, input$date[1], input$date[2])) %>%
      dplyr::filter(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?` != "No Response") %>%
      mutate(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?` = as.numeric(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`))
    # Filter for portfolio
    teaching_df <- if (input$portfolio == "All") {
      teaching_df
    } else {
      teaching_df %>% 
        dplyr::filter(input$portfolio == Portfolio)
    }
    # Filter for course
    teaching_df <- if (input$course == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`Professional Training Session` == input$course)
    }
    # Filter for client
    teaching_df <- if (input$client == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`District, Parish, Or Network` == input$client)
    }
    # Filter for facilitator
    teaching_df <- if (input$facilitator == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`Name Of Your Facilitator` == input$facilitator)
    }
    # Filter for positive
    teaching_df <- if(input$text_filters == "Areas of Strength") {
      teaching_df %>% dplyr::filter(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?` > 8)
    } else {
      teaching_df
    }
    # Filter for negative
    teaching_df <- if(input$text_filters == "Areas for Improvement") {
      teaching_df %>% dplyr::filter(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?` <= 6)
    } else {
      teaching_df
    }
    
    teaching_df <- teaching_df %>%
      select(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`,
             `Why did you choose this rating?`,
             `What could have improved your experience?`,
             `Professional Training Session`,
             `Date for the session`,
             # `Name Of Your Facilitator`,
             `How, if in any way, this course helped you prepare for school opening after COVID-19?`,
             `Overall, what went well in this professional learning?`,
             `Which activities best supported your learning?`) %>%
      drop_na(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`) %>%
      mutate(Date = format(`Date for the session`, '%b, %d %Y')) %>%
      select(-`Date for the session`) %>%
      dplyr::filter(if_any(c(2,3,5,6,7), ~ str_detect(., input$string_filters)))
  })

  # options(shiny.usecairo = T) # I don't think this does anything, need to read about it
# A time series visualization right side
  output$piePlot <- renderPlot(
    {
      g <- if(input$data == "How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?"){
        mydata() %>%
          mutate(`Date for the session` = paste0(`Date for the session`, ", 01")) %>%
          ggplot(aes(x = myd(`Date for the session`), y = Percent)) +
          geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1.1) +
          geom_segment(aes(x = myd(`Date for the session`), xend = myd(`Date for the session`), y = 0, yend = Percent), 
                       color = "gray70", size = 2) +
          geom_point(color = rev(col()(2)[2]), size = 5) +
          geom_text(aes(label = Percent), color = "black", family = "Open Sans", fontface = "bold",
                    size = 3, nudge_y = if_else(mydata()$Percent > 0, 7, -7)) +
          scale_x_date(date_breaks = "1 month", date_labels = "%b, %y", expand = c(0.03, 0)) +
          scale_y_continuous(breaks = pretty_breaks(n = 20), limits = c(-100, 100), expand = c(0, 0)) +
          # scale_fill_manual(values = c(rev(col()(2)))) +
          labs(x = "Date", title = "Monthly NPS",
               y = "How Likely Are You To Recommend This\nProfessional Learning To A Colleague Or Friend?") +
          theme_bw() + # BW Panel panel elements
          theme(
            legend.position = "none",
            # legend.text = element_text(family = "Open Sans"),
            text = element_text(family = "Open Sans", face = "bold"),
            # panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.title.x = element_blank(),
            plot.title = element_text(hjust = 0.5, family = "Open Sans", face = "bold"),
            axis.line = element_line(size = 1.5)
          )
      } else if (input$data == "All") {
        all_data_ts() %>%
          mutate(`Date for the session` = paste0(`Date for the session`, ", 01")) %>%
          # mutate(`Date for the session` = myd(`Date for the session`)) %>%
          # group_by(`Date for the session`) %>%
          ggplot(aes(x = myd(`Date for the session`), y = Percent)) +
          geom_area(color = "gray50", aes(fill = Rating), alpha = 0.6) +
          # geom_rect(aes(xmin = `Date for the session`, xmax = lead(`Date for the session`),
          #                                 ymin = 0, ymax = Percent), alpha = 0.6, fill = "blue") +
          # geom_rect(color = "transparent", aes(xmin = `Date for the session`, xmax = lead(`Date for the session`),
          #               ymin = Percent, ymax = 100), alpha = 0.85, fill = "gray") +
          geom_ribbon(color = "transparent", aes(ymin = Percent, ymax = 100,
                                                 fill = "Neither Agree nor Disagree/Disagree/Strongly Disagree"), alpha = 0.85) +
          geom_line(size = 1.25, alpha = 0.9) +
          geom_point(size = 1, alpha = 0.9) +
          facet_wrap( ~ fct_reorder(question, .x = `Percent`, .fun = mean, .desc = T)) +
          coord_cartesian() +
          # Doesn't work right now
          scale_x_date(date_breaks = if_else((max(teaching_df$`Date for the session`, na.rm = T) - min(teaching_df$`Date for the session`, na.rm = T)) > 365,
                                             "3 month",
                                             "1 month"), 
                       date_labels = if_else((max(teaching_df$`Date for the session`, na.rm = T) - min(teaching_df$`Date for the session`, na.rm = T)) > 365,
                                             "%m/%y",
                                             "%b, %y"), 
                       expand = c(0, 0)) +
          scale_y_continuous(breaks = pretty_breaks(n = 5), limits = c(0, 100), 
                             labels = scales::percent_format(scale = 1), expand = c(0, 0)) +
          scale_fill_manual(values = c(rev(col()(2)))) +
          labs(x = "Date", title = "Monthly Percentage that Agree/Strongly Agree") +
          theme_bw() + # BW Panel panel elements
          theme(
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(family = "Open Sans"),
            text = element_text(family = "Open Sans", face = "bold"),
            # panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.text.x = element_text(size = 6),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            # axis.title.y = element_text(margin = margin(l = -10)),
            plot.title = element_text(hjust = 0.5, family = "Open Sans"),
            axis.line = element_line(size = 1.5)
          )
      } else {
        mydata() %>%
          mutate(`Date for the session` = paste0(`Date for the session`, ", 01")) %>%
          ggplot(aes(x = myd(`Date for the session`), y = Percent)) +
          geom_area(color = "gray50", aes(fill = Rating), alpha = 0.6) +
          geom_ribbon(color = "transparent", aes(ymin = Percent, ymax = 100,
                                                 fill = "Neither Agree nor Disagree/Disagree/Strongly Disagree"), alpha = 0.85) +
          geom_line(size = 2, alpha = 0.9) +
          geom_point(size = 1, alpha = 0.9) +
          coord_cartesian() +
          scale_x_date(date_breaks = "1 month", date_labels = "%b, %y", expand = c(0, 0)) +
          scale_y_continuous(breaks = pretty_breaks(n = 10), limits = c(0, 100), 
                             labels = scales::percent_format(scale = 1), expand = c(0, 0)) +
          scale_fill_manual(values = c(rev(col()(2)))) +
          labs(x = "Date", title = "Monthly Percentage that Agree/Strongly Agree",
               y = paste(gsub("(.{40,}?)\\s", "\\1\n", str_to_title(as.character(input$data))))) +
          theme_bw() + # BW Panel panel elements
          theme(
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(family = "Open Sans"),
            text = element_text(family = "Open Sans", face = "bold"),
            # panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.text.x = element_text(size = 8),
            axis.title.x = element_blank(),
            plot.title = element_text(hjust = 0.5, family = "Open Sans"),
            axis.line = element_line(size = 1.5)
          )
      }
      cowplot::ggdraw(g) #+
      # theme(plot.background = element_rect(fill = "#355c7d", color = NA))
    },
    height = 450,
    width = "auto",
    res = 80
  )
# Visualization type left side
  output$piePlotNA <- renderPlot(
    {
      if (input$viz_type == "Pie Chart" & input$data != "All") {
        g2 <- if (input$viz_type == "Pie Chart" & input$data == "How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?") {
          ggplot2::ggplot(mydata2(), aes(x = 0, y = n, fill = reorder(`get(input$data)`, n))) + # Pie chart input, ordered by n
          labs(
            fill = "Type", x = NULL, y = NULL,
            title = paste(str_to_title(as.character(input$data))),
            subtitle = paste0("Average NPS from ", input$date[1], " to ", input$date[2], ": ", 
                              sum(mydata2() %>% 
                                    filter(`get(input$data)` == 10 | `get(input$data)` == 9) %>%
                                    select(percent)) - 
                                sum(mydata2() %>%
                                      filter(`get(input$data)` == 0:6) %>%
                                      select(percent)))
          ) +
          geom_bar(stat = "identity", width = 0.5, color = "gray10", size = ifelse(nrow(mydata2()) > 10,
            0.1,
            0.6
          )) + # Using bar columns put in polar coordinates later
          geom_text(aes(
            label = ifelse(percent > 5,
              paste0(percent, "%\n", ifelse(
                str_length(`get(input$data)`) > 10,
                gsub("(.{9,}?)\\s", "\\1\n\\2", `get(input$data)`),
                `get(input$data)`
              )),
              paste("")
            ),
            x = 0.4, # Distance outwards from pie chart
            color = reorder(`get(input$data)`, n)
          ),
          position = position_stack(vjust = 0.5), # position stack normally stacks bars, but here it keeps the text in the right place once put in polar
          size = 3,
          fontface = "bold",
          family = "Open Sans"
          ) + # Add bold text with percentage and variable label
          scale_x_continuous(expand = c(0, 0)) + # Change x so expand is not default and adds no padding so the bars will produce a circle
          coord_polar(theta = "y", direction = -1) + # Make bars polar
          scale_fill_manual(values = c(col()(nrow(mydata2())))) + # custom colors
          scale_color_manual(values = c(col()(nrow(mydata2())))) +
          theme_void() +
          theme(
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, family = "Open Sans", face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, family = "Open Sans")
          )
        } else {
          ggplot2::ggplot(mydata2(), aes(x = 0, y = n, fill = reorder(`get(input$data)`, n))) + # Pie chart input, ordered by n
            labs(
              fill = "Type", x = NULL, y = NULL,
              title = paste(str_to_title(as.character(input$data))),
              subtitle = paste0(sum(mydata2() %>% 
                                                 dplyr::filter(`get(input$data)` == "Agree" | `get(input$data)` == "Strongly agree") %>%
                                                 select(percent)) ,"% of people rated agree or strongly agree")
            ) +
            geom_bar(stat = "identity", width = 0.5, color = "gray10", size = ifelse(nrow(mydata2()) > 10,
                                                                                     0.1,
                                                                                     0.6
            )) + # Using bar columns put in polar coordinates later
            geom_text(aes(
              label = ifelse(percent > 5,
                             paste0(percent, "%\n", ifelse(
                               str_length(`get(input$data)`) > 10,
                               gsub("(.{9,}?)\\s", "\\1\n\\2", `get(input$data)`),
                               `get(input$data)`
                             )),
                             paste("")
              ),
              x = 0.4, # Distance outwards from pie chart
              color = reorder(`get(input$data)`, n)
            ),
            position = position_stack(vjust = 0.5), # position stack normally stacks bars, but here it keeps the text in the right place once put in polar
            size = 3,
            fontface = "bold",
            family = "Open Sans"
            ) + # Add bold text with percentage and variable label
            scale_x_continuous(expand = c(0, 0)) + # Change x so expand is not default and adds no padding so the bars will produce a circle
            coord_polar(theta = "y", direction = -1) + # Make bars polar
            scale_fill_manual(values = c(col()(nrow(mydata2())))) + # custom colors
            scale_color_manual(values = c(col()(nrow(mydata2())))) +
            theme_void() +
            theme(
              legend.position = "none",
              plot.title = element_text(hjust = 0.5, family = "Open Sans", face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, family = "Open Sans")
            )
        }
        cowplot::ggdraw(g2)
      } else if (input$data == "All" & input$viz_type == "Pie Chart") {
        g2 <- all_data() %>%
          mutate(answer = str_replace_all(answer, "Neither agree nor disagree", "Neither"),
                 answer = str_replace_all(answer, "Strongly agree", "Strongly\nagree")) %>%
          ggplot2::ggplot(aes(x = 0, y = n, fill = reorder(percent, n))) + # Pie chart input, ordered by n
          labs(
            fill = "Type", x = NULL, y = NULL
          ) +
          facet_wrap( ~ question, nrow = 2) +
          geom_bar(stat = "identity", width = 0.5, color = "gray10", size = 0.6, position = "fill") + # Using bar columns put in polar coordinates later
          geom_text(aes(
            label = ifelse(percent > 10,
                           paste0(percent, "%\n", answer),
                           paste("")
            ),
            x = 0.4075, # Distance outwards from pie chart
            color = reorder(percent, n)
          ),
          position = position_fill(vjust = 0.5), # position stack normally stacks bars, but here it keeps the text in the right place once put in polar
          size = 2.25,
          fontface = "bold",
          family = "Open Sans"
          ) + # Add bold text with percentage and variable label
          scale_x_continuous(expand = c(0, 0)) + # Change x so expand is not default and adds no padding so the bars will produce a circle
          coord_polar(theta = "y", direction = -1) + # Make bars polar
          scale_fill_manual(values = c(col()(nrow(all_data())))) + # custom colors
          scale_color_manual(values = c(col()(nrow(all_data())))) +
          theme_void() +
          theme(
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, family = "Open Sans", face = "bold"),
            strip.text = element_text(face = "bold", family = "Open Sans")
          )
        cowplot::ggdraw(g2)
        } else if (input$data == "All" & input$viz_type == "Donut Chart") {
        g2 <- all_data() %>%
          mutate(answer = str_replace_all(answer, "Neither agree nor disagree", "Neither")) %>%
          group_by(as.factor(question)) %>%
          arrange(desc(percent)) %>%
          ungroup() %>%
          drop_na() %>%
          dplyr::mutate(answer = str_replace_all(answer, c("Strongly agree" = "Strongly\nagree", "Strongly disagree" = "Strongly\ndisagree"))) %>%
          dplyr::mutate(answer = factor(answer, levels = c("Strongly\nagree", "Agree", "Neither", "Disagree", "Strongly\ndisagree"))) %>%
          ggplot2::ggplot(aes(x = 2, y = percent, fill = fct_reorder(answer, percent))) + # Pie chart input, ordered by n
          labs(
            fill = "Type", x = NULL, y = NULL
          ) +
          geom_bar(stat = "identity", width = 0.75, color = "gray10", size = 0.5, position = "fill") +
          geom_text(aes(
            label = ifelse(percent > 10,
                           paste0(percent, "%\n", answer),
                           paste("")
            ),
            x = 2#, # Distance outwards from pie chart
            # color = reorder(percent, n)
          ),
          position = position_fill(vjust = 0.5), # position stack normally stacks bars, but here it keeps the text in the right place once put in polar
          size = 2.25,
          color = "black",
          fontface = "bold",
          family = "Open Sans"
          ) + # Add bold text with percentage and variable label
          facet_wrap( ~ question, nrow = 2) +
          scale_x_discrete(expand = c(0, 1)) + # Change x so expand is not default and adds no padding so the bars will produce a circle not a donut
          coord_polar(theta = "y", direction = -1) + # Make bars polar
          scale_fill_manual(values = c("Strongly\nagree" = col()(5)[5], "Agree" = col()(5)[4], "Neither" = col()(5)[3], 
                                       "Disagree" = col()(5)[2], "Strongly\ndisagree" = col()(5)[1])) + # custom colors
          scale_color_manual(values = c(col()(nrow(all_data())))) +
          theme_void() +
          theme(
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, family = "Open Sans", face = "bold"),
            text = element_text(family = "Open Sans", face = "bold"),
            strip.text = element_text(face = "bold", family = "Open Sans")
          )
        # g2 <- ggplot(data = tibble(x = runif(100), y = runif(100)), aes(x = x, y = y)) +
        #   geom_point()
        cowplot::ggdraw(g2)
      } else if (input$viz_type == "Donut Chart" & input$data != "All") {
        g2 <- if (input$viz_type == "Donut Chart" & input$data == "How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?") {
          ggplot2::ggplot(mydata2(), aes(x = 0, y = n, fill = reorder(`get(input$data)`, n))) + # Pie chart input, ordered by n
          labs(
            fill = "Type", x = NULL, y = NULL,
            title = paste(str_to_title(as.character(input$data))),
            subtitle = paste0("Average NPS from ", input$date[1], " to ", input$date[2], ": ", 
                              sum(mydata2() %>% 
                                    filter(`get(input$data)` == 10 | `get(input$data)` == 9) %>%
                                    select(percent)) - 
                                sum(mydata2() %>%
                                      filter(`get(input$data)` == 0:6) %>%
                                      select(percent)))
          ) +
          geom_bar(stat = "identity", width = 1.5, color = "gray10", size = ifelse(nrow(mydata2()) > 10,
                                                                                   0.1,
                                                                                   0.6
          )) + # Using bar columns put in polar coordinates later
          geom_text(aes(
            label = ifelse(percent > 5,
                           paste0(percent, "%\n", ifelse(
                             str_length(`get(input$data)`) > 10,
                             gsub("(.{9,}?)\\s", "\\1\n\\2", `get(input$data)`),
                             `get(input$data)`
                           )),
                           paste("")
            ),
            x = 2, # Distance outwards from pie chart
            color = reorder(`get(input$data)`, n)
          ),
          position = position_stack(vjust = 0.5), # position stack normally stacks bars, but here it keeps the text in the right place once put in polar
          size = 3,
          fontface = "bold",
          family = "Open Sans"
          ) + # Add bold text with percentage and variable label
          scale_x_continuous(expand = c(0, 1)) + # Change x so expand is not default and adds no padding so the bars will produce a circle not a donut
          coord_polar(theta = "y", direction = -1) + # Make bars polar
          scale_fill_manual(values = c(col()(nrow(mydata2())))) + # custom colors
          scale_color_manual(values = c(col()(nrow(mydata2())))) +
          theme_void() +
          theme(
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, family = "Open Sans", face = "bold"),
            text = element_text(family = "Open Sans"),
            plot.subtitle = element_text(hjust = 0.5, family = "Open Sans", face = "plain")
          )
        } else {
          ggplot2::ggplot(mydata2(), aes(x = 0, y = n, fill = reorder(`get(input$data)`, n))) + # Pie chart input, ordered by n
            labs(
              fill = "Type", x = NULL, y = NULL,
              title = paste(str_to_title(as.character(input$data))),
              subtitle = paste0(sum(mydata2() %>% 
                                      dplyr::filter(`get(input$data)` == "Agree" | `get(input$data)` == "Strongly agree") %>%
                                      select(percent)) ,"% of people rated agree or strongly agree")
            ) +
            geom_bar(stat = "identity", width = 1.5, color = "gray10", size = ifelse(nrow(mydata2()) > 10,
                                                                                     0.1,
                                                                                     0.6
            )) + # Using bar columns put in polar coordinates later
            geom_text(aes(
              label = ifelse(percent > 5,
                             paste0(percent, "%\n", ifelse(
                               str_length(`get(input$data)`) > 10,
                               gsub("(.{9,}?)\\s", "\\1\n\\2", `get(input$data)`),
                               `get(input$data)`
                             )),
                             paste("")
              ),
              x = 2, # Distance outwards from pie chart
              color = reorder(`get(input$data)`, n)
            ),
            position = position_stack(vjust = 0.5), # position stack normally stacks bars, but here it keeps the text in the right place once put in polar
            size = 3,
            fontface = "bold",
            family = "Open Sans"
            ) + # Add bold text with percentage and variable label
            scale_x_continuous(expand = c(0, 1)) + # Change x so expand is not default and adds no padding so the bars will produce a circle not a donut
            coord_polar(theta = "y", direction = -1) + # Make bars polar
            scale_fill_manual(values = c(col()(nrow(mydata2())))) + # custom colors
            scale_color_manual(values = c(col()(nrow(mydata2())))) +
            theme_void() +
            theme(
              legend.position = "none",
              plot.title = element_text(hjust = 0.5, family = "Open Sans", face = "bold"),
              text = element_text(family = "Open Sans"),
              plot.subtitle = element_text(hjust = 0.5, family = "Open Sans", face = "plain")
            )
        }
        cowplot::ggdraw(g2)
      } else if (input$viz_type == "Column Chart" & input$data != "All") {
        g2 <- if (input$viz_type == "Column Chart" & input$data == "How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?") {
          mydata2() %>%
            dplyr::arrange(desc(`get(input$data)`)) %>%
            ggplot2::ggplot(aes(x = 0, y = n, fill = reorder(`get(input$data)`, n))) + 
            labs(
              fill = "Type", x = NULL, y = NULL,
              title = paste(str_to_title(as.character(input$data))),
              subtitle = paste0("Average NPS from ", input$date[1], " to ", input$date[2], ": ",
                                sum(mydata2() %>%
                                      filter(`get(input$data)` == 10 | `get(input$data)` == 9) %>%
                                      select(percent)) -
                                  sum(mydata2() %>%
                                        filter(`get(input$data)` == 0:6) %>%
                                        select(percent)))
            ) +
            geom_bar(stat = "identity", width = 0.1, color = "gray10"
            ) + 
            geom_text(aes(x = 0.125, label = "some text that won't show up"), color = "transparent") + # This adjusts the column size
            geom_text(aes(x = -0.125, label = "some text that won't show up"), color = "transparent") + # This adjusts the column size
            geom_text(aes(
              label = ifelse(percent > 5,
                             paste0(percent, "% (", n, ")\n", ifelse(
                               str_length(`get(input$data)`) > 10,
                               gsub("(.{9,}?)\\s", "\\1\n\\2", `get(input$data)`),
                               `get(input$data)`
                             )),
                             paste("")
              ),
              x = 0.075, # Distance outwards from bar
              color = reorder(`get(input$data)`, n)
              # angle = 90
            ),
            position = position_stack(vjust = 0.5), # position stack normally stacks bars, but here it keeps the text in the right place once put in polar
            size = 3,
            fontface = "bold",
            family = "Open Sans",
            # color = "white"
            ) + # Add bold text with percentage and variable label
            scale_x_continuous() + 
            coord_flip() +
            scale_fill_manual(values = c(col()(nrow(mydata2())))) + # custom colors
            scale_color_manual(values = c(col()(nrow(mydata2())))) +
            theme_void() +
            # ggpubr::theme_transparent() +
            theme(
              legend.position = "none",
              plot.title = element_text(hjust = 0.5, family = "Open Sans", face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, family = "Open Sans")
            )
        } else { 
          ggplot2::ggplot(mydata2(), aes(x = 0, y = n, fill = reorder(`get(input$data)`, n))) + # Pie chart input, ordered by n
          labs(
            fill = "Type", x = NULL, y = NULL,
            title = paste(str_to_title(as.character(input$data))),
            subtitle = paste0(sum(mydata2() %>% 
                                    dplyr::filter(`get(input$data)` == "Agree" | `get(input$data)` == "Strongly agree") %>%
                                    select(percent)), "% of people rated agree or strongly agree")
          ) +
          geom_bar(stat = "identity", width = 0.1, color = "gray10"#, 
          #          size = ifelse(nrow(mydata2()) > 10,
          #                                                                          0.1,
          #                                                                          0.6
          # )
          ) + # Using bar columns put in polar coordinates later
          geom_text(aes(x = 0.125, label = "some text that won't show up"), color = "transparent") + # This adjusts the column size
          geom_text(aes(x = -0.125, label = "some text that won't show up"), color = "transparent") + # This adjusts the column size
          geom_text(aes(
            label = ifelse(percent > 5,
                           paste0(percent, "% (", n, ")\n", ifelse(
                             str_length(`get(input$data)`) > 10,
                             gsub("(.{9,}?)\\s", "\\1\n\\2", `get(input$data)`),
                             `get(input$data)`
                           )),
                           paste("")
            ),
            x = 0.075, # Distance outwards from bar
            color = reorder(`get(input$data)`, n)
            # angle = 90
          ),
          position = position_stack(vjust = 0.5), # position stack normally stacks bars, but here it keeps the text in the right place once put in polar
          size = 3,
          fontface = "bold",
          family = "Open Sans",
          # color = "white"
          ) + # Add bold text with percentage and variable label
          scale_x_continuous() + 
          coord_flip() +
          scale_fill_manual(values = c(col()(nrow(mydata2())))) + # custom colors
          scale_color_manual(values = c(col()(nrow(mydata2())))) +
          theme_void() +
          # ggpubr::theme_transparent() +
          theme(
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, family = "Open Sans", face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, family = "Open Sans")
          )
        }
        cowplot::ggdraw(g2)
      } else if (input$data == "All" & input$viz_type == "Column Chart") {
        g2 <- all_data() %>%
          mutate(answer = str_replace_all(answer, "Neither agree nor disagree", "Neither")) %>%
          group_by(as.factor(question)) %>%
          arrange(desc(percent)) %>%
          ungroup() %>%
          drop_na() %>%
          dplyr::mutate(answer = factor(answer, levels = c("Strongly agree", "Agree", "Neither", "Disagree", "Strongly disagree"))) %>%
          ggplot2::ggplot(aes(x = fct_reorder(question, percent, .desc = T), y = percent, fill = fct_reorder(answer, percent))) + # Pie chart input, ordered by n
          labs(
            fill = "Type", x = NULL, y = NULL
          ) +
          geom_bar(stat = "identity", width = 0.5, color = "gray10", position = position_fill()) +
          # geom_text(aes(x = 0.125, label = "some text that won't show up"), color = "transparent", position = "fill") + # This adjusts the column size
          # geom_text(aes(x = -0.125, label = "some text that won't show up"), color = "transparent", position = "fill") + # This adjusts the column size
          geom_text(aes(
            label = ifelse(percent > 10,
                           paste0(percent, "%\n", answer),
                           paste("")
            ),
            # x = if_else(percent > 10,
            #             0.085,
            #             -0.085), # Distance outwards from pie chart
            # color = reorder(percent, n)
          ),
          position = position_fill(vjust = 0.5), # position stack normally stacks bars, but here it keeps the text in the right place once put in polar
          size = 2.3,
          fontface = "bold",
          color = "black",
          family = "Open Sans"
          ) + # Add bold text with percentage and variable label
          # facet_wrap( ~ question) +
          coord_flip() +
          # scale_y_discrete() +
          # scale_x_continuous() + # Change x so expand is not default and adds no padding so the bars will produce a circle
          scale_fill_manual(values = c("Strongly agree" = col()(5)[5], "Agree" = col()(5)[4], "Neither" = col()(5)[3], 
                                       "Disagree" = col()(5)[2], "Strongly disagree" = col()(5)[1])) + # custom colors
          scale_color_manual(values = c(col()(5))) +
          theme_bw() +
          theme(
            legend.position = "none",
            text = element_text(face = "bold", family = "Open Sans"),
            panel.border = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.length.y = unit(0.45, "cm"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank()
          )
        cowplot::ggdraw(g2)
        } else if (input$viz_type == "Tree Map" & input$data != "All") {
        mydata2() %>%
          mutate(label = paste0(percent, "% (", n, ")\n", `get(input$data)`)) %>%
          treemap::treemap(
          index = c("label"),
          vSize = "n",
          type = "index",
          palette = c(col()(nrow(mydata2()))),
          title = paste(str_to_title(as.character(input$data))),
          fontfamily.labels = "Open Sans",
          fontfamily.title = "Open Sans ExtraBold"
        )
      } else if (input$viz_type == "Tree Map" & input$data == "All") {
        all_data() %>%
          mutate(label = paste0(percent, "% (", n, ")\n", answer)) %>%
          treemap::treemap(
            index = c("question", "label"),
            vSize = "n",
            palette = c(col()(12)[5:12]),
            title = "All Data",
            fontfamily.labels = "Open Sans",
            fontfamily.title = "Open Sans ExtraBold",
            fontsize.labels = 7.5,
            align.labels = list(c("center", "center"), c("right", "bottom")),
            bg.labels = "transparent"
          )
      } else if (input$viz_type == "Waffle Chart" & input$data != "All") {
        library(plyr)
          g2 <- if (input$data == "How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?") {
            mydata2() %>%
              dplyr::mutate(`get(input$data)` = factor(`get(input$data)`, levels = c(10,9,8,7,6,5,4,3,2,1,0))) %>%
              dplyr::arrange(desc(`get(input$data)`)) %>%
              ggplot2::ggplot(aes(values = n, fill = `get(input$data)`)) +
                waffle::geom_waffle(aes(color = `get(input$data)`), 
                                    n_rows = 10, flip = TRUE, make_proportional = T, size = 0.5, radius = unit(4, "pt")) +
                scale_fill_manual(values = c(rev(col()(10)))) + # custom colors
                scale_color_manual(values = c("black", "black", "white", "white", "white", "white", "white", "white",
                                              "white", "white", "white", "white", "white", "white"), guide = F) +
                labs(
                  fill = "Type", x = NULL, y = NULL,
                  title = paste(str_to_title(as.character(input$data))),
                  subtitle = paste0("Average NPS from ", input$date[1], " to ", input$date[2], ": ", sum(mydata2() %>% 
                                     filter(`get(input$data)` == 10 | `get(input$data)` == 9) %>%
                                     select(percent)) - sum(mydata2() %>%
                                                              filter(`get(input$data)` == 0:6) %>%
                                                              select(percent)))
                ) +
                coord_equal() +
                theme_void() +
                theme(
                  legend.title = element_blank(),
                  plot.title = element_text(hjust = 0.5, family = "Open Sans", face = "bold"),
                  plot.subtitle = element_text(hjust = 0.5, family = "Open Sans")
                )
          } else {
            mydata2() %>%
              dplyr::mutate(`get(input$data)` = factor(`get(input$data)`, levels = c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree"))) %>%
              dplyr::arrange(desc(`get(input$data)`)) %>%
              ggplot2::ggplot(aes(values = n, fill = `get(input$data)`)) +
                waffle::geom_waffle(aes(color = `get(input$data)`), 
                                    n_rows = 10, flip = TRUE, make_proportional = T, size = 1, radius = unit(4, "pt")) +
                scale_fill_manual(values = c(rev(col()(5)))) + # custom colors
                scale_color_manual(values = c("black", "black", "white", "white", "white"), guide = F) +
                labs(
                  fill = "Type", x = NULL, y = NULL,
                  title = paste(str_to_title(as.character(input$data))),
                  subtitle = paste0(sum(mydata2() %>% 
                                          dplyr::filter(`get(input$data)` == "Agree" | `get(input$data)` == "Strongly agree") %>%
                                          select(percent)) ,"% of people rated agree or strongly agree")
                ) +
                coord_equal() +
                theme_void() +
                theme(
                  legend.title = element_blank(),
                  plot.title = element_text(hjust = 0.5, family = "Open Sans", face = "bold"),
                  text = element_text(family = "Open Sans"),
                  plot.subtitle = element_text(hjust = 0.5, family = "Open Sans")
                )
          }
          detach("package:plyr", unload = TRUE)
          cowplot::ggdraw(g2)
      } else if (input$viz_type == "Waffle Chart" & input$data == "All") {
        library(plyr)
        g2 <- all_data() %>%
          dplyr::mutate(answer = factor(answer, levels = c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree"))) %>%
          dplyr::arrange(desc(answer)) %>%
          ggplot2::ggplot(aes(values = n, fill = answer)) +
            waffle::geom_waffle(aes(color = answer),
                                n_rows = 10, flip = TRUE, make_proportional = T, size = 1, radius = unit(4, "pt")) +
            facet_wrap( ~ question, nrow = 2) +
            scale_fill_manual(values = c(rev(col()(5)))) + # custom colors
            scale_color_manual(values = c("black", "black", "white", "white", "white"), guide = F) +
            labs(
              fill = "Type", x = NULL, y = NULL
            ) +
            coord_equal() +
            theme_void() +
            theme(
              legend.title = element_blank(),
              strip.text = element_text(face = "bold", family = "Open Sans", size = 7)
            )
        detach("package:plyr", unload = TRUE)
        cowplot::ggdraw(g2)
      }
    },
    height = 450,
    width = "auto",
    res = 80
  )
  
  # A slice sampled table, needs to add more features
  output$gt_text <- render_gt(
    text_viz_data() %>%
      slice_sample(n = 10) %>%
      dplyr::arrange(desc(as.numeric(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`))) %>%
      relocate(c("Professional Training Session"), .before = Date) %>%
      gt() %>%
      tab_header(
        title = md("&#128202; **Selected Reviews** &#128202;"),
        subtitle = glue::glue("Sorted best-worst and filtering for {str_to_lower(input$text_filters)} observations between
                              {input$date[1]} and {input$date[2]}")
      ) %>%
      cols_label(
        `How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?` = md("**Likeliness to recommend**"),
        `Why did you choose this rating?` = md("**Why did you choose this rating?**"),
        `What could have improved your experience?` = md("**What could have improved your experience?**"),
        `Professional Training Session` = md("**Professional training session**"),
        Date = md("**Date**"),
        # `Name Of Your Facilitator` = md("**Facilitator**"),
        `How, if in any way, this course helped you prepare for school opening after COVID-19?` = md("**How, if in any way, this course helped you prepare for school opening after COVID-19?**"),
        `Overall, what went well in this professional learning?` = md("**Overall, what went well in this professional learning?**"),
        `Which activities best supported your learning?` = md("**Which activities best supported your learning?**")
      ) %>%
      data_color(columns = vars(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`),
                 colors = scales::col_numeric(
                   palette = col()(10),
                   domain = NULL)
      ) %>%
      cols_align(
        align = "center",
        columns = vars(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`)
      ) %>%
      cols_width(
        vars(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`) ~ px(110),
        # vars(`Likert Plot Scale`) ~ px(400),
        everything() ~ px(225)
      ),
    align = "center"
  )
  
  # Right most table for time series
  output$tableData <- DT::renderDT({
    # Table prettyifying, cell borders that are striped and highlight on hover
    conditional_dt <- if (input$data == "How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?"){
      page_length <- 11 # Adjust for other table
      conditional_dt <- mydata() %>%
        mutate(`Date for the session` = myd(paste0(`Date for the session`, ", 01"))) %>%
        arrange(`Date for the session`) %>%
        mutate(`Date for the session` = format(`Date for the session`, format = "%B, %Y")) %>%
        DT::datatable(
          colnames = c("Date for the Session", "NPS"),
          extensions = "Buttons", # Add buttons
          options = list(
            pageLength = page_length,
            dom = "Bfrtip", # Buttons source
            buttons = c("copy", "csv", "excel", "pdf", "print", "pageLength"),
            lengthMenu = list(c(10, 25, 100, -1), c('10', '25', '100','All')),
            initComplete = DT::JS(
              "function(settings, json) {",
              "$('body').css({'font-family': 'Open Sans'});
                                     $(this.api().table().header()).css({'background-color': '#346475', 'color': '#fff'});",
              "}"
            )
          ), # Buttons spec
          rownames = F, # No rownames
          class = "cell-border stripe hover"
        )
      conditional_dt %>%
        formatStyle("Percent",
                    background = styleColorBar(c(0, mydata()$Percent), rev(col()(2)[2])),
                    backgroundSize = "95% 50%",
                    backgroundRepeat = "no-repeat",
                    backgroundPosition = "right"
      ) # Add percent bars
    } else if (input$data == "All") {
      page_length <- 10
      conditional_dt <- all_data_ts() %>%
        mutate(`Date for the session` = myd(paste0(`Date for the session`, ", 01"))) %>%
        arrange(desc(`Date for the session`)) %>%
        # mutate(`Date for the session` = format(`Date for the session`, format = "%B, %Y")) %>%
        ungroup() %>%
        select(-Rating) %>%
        DT::datatable(
          colnames = c("Date for the Session", "Question", "Percent Strongly Agree/Agree"),
          extensions = "Buttons", # Add buttons
          options = list(
            pageLength = page_length,
            dom = "Bfrtip", # Buttons source
            buttons = c("copy", "csv", "excel", "pdf", "print", "pageLength"),
            lengthMenu = list(c(10, 25, 100, -1), c('10', '25', '100','All')),
            initComplete = DT::JS(
              "function(settings, json) {",
              "$('body').css({'font-family': 'Open Sans'});
                                     $(this.api().table().header()).css({'background-color': '#346475', 'color': '#fff'});",
              "}"
            )
          ), # Buttons spec
          rownames = F, # No rownames
          class = "cell-border stripe hover"
        )
      conditional_dt %>%
        formatString("Percent", suffix = "%") %>%
        formatStyle("Percent",
                    background = styleColorBar(c(0, all_data_ts()$Percent), rev(col()(2)[2])),
                    backgroundSize = "95% 50%",
                    backgroundRepeat = "no-repeat",
                    backgroundPosition = "right"
        ) %>%
        formatDate(columns = 1,
                   method = "toLocaleDateString",
                   params = list(
                     'en-US',
                     list(year = 'numeric', month = 'long')
                   ))
    } else {
      page_length <- 5 # Adjust for other table
      conditional_dt <- mydata() %>%
        mutate(`Date for the session` = myd(paste0(`Date for the session`, ", 01"))) %>%
        arrange(`Date for the session`) %>%
        mutate(`Date for the session` = format(`Date for the session`, format = "%B, %Y")) %>%
        select(-Rating) %>%
        DT::datatable(
          colnames = c("Date for the Session", "Percent Strongly Agree/Agree"),
          extensions = "Buttons", # Add buttons
          options = list(
            pageLength = page_length,
            dom = "Bfrtip", # Buttons source
            buttons = c("copy", "csv", "excel", "pdf", "print", "pageLength"),
            lengthMenu = list(c(10, 25, 100, -1), c('10', '25', '100','All')),
            initComplete = DT::JS(
              "function(settings, json) {",
              "$('body').css({'font-family': 'Open Sans'});
                                     $(this.api().table().header()).css({'background-color': '#346475', 'color': '#fff'});",
              "}"
            )
          ), # Buttons spec
          rownames = F, # No rownames
          class = "cell-border stripe hover"
        )
      conditional_dt %>%
        formatString("Percent", suffix = "%") %>%
        formatStyle("Percent",
                    background = styleColorBar(c(0, mydata()$Percent), rev(col()(2)[2])),
                    backgroundSize = "95% 50%",
                    backgroundRepeat = "no-repeat",
                    backgroundPosition = "right"
        )
    }
  })

  # Left most table for all time
  output$tableData2 <- DT::renderDT({
    if (input$data != "All") {
    sketch <- htmltools::withTags(table(
      class = "display", style = "bootstrap",
      tableHeader(c("Likert Rating", "Percent", "Number")),
      tableFooter(colnames(mydata2()))
    ))
    page_length <- 11
    mydata2() %>%
      DT::datatable(
      colnames = c(paste(str_to_title(input$data)), "Percent", "Number"), # Column names
      container = sketch,
      extensions = "Buttons", # Add buttons
      options = list(
        pageLength = page_length,
        order = list(list(2, "desc")), # Sort by second column which is number
        dom = "Bfrtip", # Buttons source
        buttons = c("copy", "csv", "excel", "pdf", "print", "pageLength"),
        lengthMenu = list(c(10, 25, 100, -1), c('10', '25', '100','All')),
        initComplete = DT::JS(
          "function(settings, json) {",
          "$('body').css({'font-family': 'Open Sans'});
                                     $(this.api().table().header()).css({'background-color': '#346475', 'color': '#fff'});",
          "}"
        ),
        footerCallback = DT::JS(
          "function( tfoot, data, start, end, display ) {",
          "var api = this.api(), data;",
          "api.columns().eq(0).each( function(index) {",
          "var col = api.column(index);",
          "if(index == 0) return $(api.column(index).footer()).html('Total')",
          "var data = col.data();",
          "total = data.reduce( function(a, b) { return a + b }, 0 );",
          "$( api.column(2).footer() ).html(total.toFixed(0));",
          "$( api.column(1).footer() ).html('100%')",
          "})",
          "}"
        )
      ), # Buttons spec
      rownames = F, # No rownames
      class = "cell-border stripe hover"
    ) %>% # Table prettyifying, cell borders that are striped and highlight on hover
      formatString("percent", suffix = "%") %>%
      formatStyle("n",
        background = styleColorBar(c(0, mydata2()$n), rev(col()(2)[2])),
        backgroundSize = "95% 50%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "right"
      ) # Add percent bars
    } else {
      page_length <- 10
      all_data() %>%
        dplyr::mutate(answer = factor(answer, levels = rev(c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree")))) %>%
        arrange(question, desc(n)) %>%
        DT::datatable(
        colnames = c("Likert Rating", "Question", "Percent", "Number"), # Column names
        extensions = c("Buttons", "RowGroup"), # Add buttons
        options = list(
          pageLength = page_length,
          rowGroup = list(dataSrc = c(1)),
          # order = list(list(3, "desc")), # Sort by second column which is number
          dom = "Bfrtip", # Buttons source
          buttons = c("copy", "csv", "excel", "pdf", "print", "pageLength"),
          lengthMenu = list(c(10, 25, 100, -1), c('10', '25', '100','All')),
          initComplete = DT::JS(
            "function(settings, json) {",
            "$('body').css({'font-family': 'Open Sans'});
                                   $(this.api().table().header()).css({'background-color': '#346475', 'color': '#fff'});",
            "}"
          )
        ), # Buttons spec
        rownames = F, # No rownames
        class = "cell-border stripe hover",
        selection = "none"
      ) %>% # Table prettyifying, cell borders that are striped and highlight on hover
      formatString("percent", suffix = "%") %>%
      formatStyle("n",
                  background = styleColorBar(c(0, all_data()$n), rev(col()(2)[2])),
                  backgroundSize = "95% 50%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "right"
      )
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
































