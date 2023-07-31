#### Knowledge Assessments 2022-2023 Dashboard ####
library(bslib)
library(dplyr)
library(echarts4r)
library(gridlayout)
library(ggplot2)
library(shiny)
# library(shinycssloaders)
library(showtext)
library(tlShiny)

### Shiny Port ###
options(shiny.port = 7325)

### Shiny css loaders ###
# options(spinner.color = "#04abeb")
# options(spinner.type = 7)

### Add Calibri Fonts
font_add(family = "Calibri", regular = "www/Calibri.ttf")
font_add(family = "Calibri Bold", regular = "www/Calibri Bold.ttf")
font_add(family = "Roboto", regular = "www/Roboto-Black.ttf")

### Knowledge Assessments Data ###
knowledge_assessments <- readr::read_rds("data/knowledge_assessments_22_23.rds")
knowledge_assessments_detailed <- readr::read_rds("data/knowledge_assessments_22_23_detailed.rds")

# App template from the shinyuieditor
ui <- grid_page(
  layout = c(
    "header header",
    "sidebar summaryPlot",
    "sidebar detailedPlot"
  ),
  row_sizes = c(
    "70px",
    "60vh",
    "70vh"
  ),
  col_sizes = c(
    "250px",
    "1fr"
  ),
  theme = bslib::bs_theme(
    bg = "white", fg = "black",
    # Controls the accent (e.g., hyperlink, button, etc) colors
    primary = "#04abeb", secondary = "#48DAC6",
    base_font = c("Calibri", "sans-serif"),
    # Can also add lower-level customization
    "input-border-color" = "black"
  ) |>
    bslib::bs_add_rules(sass::sass_file("custom.scss")),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    item_alignment = "top",
    title = h4("Filters"),
    item_gap = "12px",
    selectInput(
      inputId = "know_assess",
      label = h5("Select a Knowledge Assessment"),
      choices = c("All Knowledge Assessments", sort(unique(knowledge_assessments$know_assess)))
    ),
    uiOutput("sites_filter"),
    dateRangeInput("daterange3", h5("Select a date range:"),
      start = "2022-07-01",
      end = Sys.Date(),
      min = "2022-07-01",
      max = Sys.Date(),
      format = "mm/dd/yy",
      separator = " - "
    )
  ),
  grid_card_text(
    area = "header",
    content = "Knowledge Assessments Dashboard",
    alignment = "center",
    is_title = TRUE
  ),
  grid_card_plot(area = "summaryPlot"),
  grid_card_plot(area = "detailedPlot")
)

# Define server logic
server <- function(input, output) {
  ### Filters Section ###
  ### Filter down the knowledge assessments data ###
  knowledge_assessments_filtered <- reactive({
    filtered <- knowledge_assessments |>
      dplyr::filter(dplyr::between(date, input$daterange3[1], input$daterange3[2])) |>
      tlShiny::neg_cond_filter("All Sites", input$site, site) |>
      tlShiny::neg_cond_filter("All Knowledge Assessments", input$know_assess, know_assess)

    filtered
  })

  ### Filter down the detailed knowledge assessments data ###
  knowledge_assessments_detailed_filtered <- reactive({
    filtered <- knowledge_assessments_detailed |>
      dplyr::filter(between(date, input$daterange3[1], input$daterange3[2])) |>
      tlShiny::neg_cond_filter("All Sites", input$site, site) |>
      tlShiny::neg_cond_filter("All Knowledge Assessments", input$know_assess, know_assess)

    filtered
  })

  ### Filter down list of sites ###
  all_sites_filtered <- reactive({
    filtered <- knowledge_assessments |>
      tlShiny::neg_cond_filter("All Knowledge Assessments", input$know_assess, know_assess) |>
      dplyr::distinct(site) |>
      dplyr::pull() |>
      sort()

    filtered
  })
  
  ### Reactive height for bottom plot ###
  reactive_height <- reactive({
    
    if (input$know_assess == "All Knowledge Assessments") {
      "2500px"
    } else {
      "1000px"
    }
    
  })

  ########################

  ### Plots Section ###

  output$summaryPlot <- renderPlot({
    
    req(input$know_assess)
    tlShiny::know_assess_summary(
      data = knowledge_assessments_filtered(),
      know_assess = input$know_assess,
      summary_path = NULL
    )
    
  })
  
  output$detailedPlot <- renderPlot({
    
    req(input$know_assess)
    
    if (input$know_assess != "All Knowledge Assessments") {
      tlShiny::know_assess_summary_detailed(
        data = knowledge_assessments_detailed_filtered(),
        know_assess = input$know_assess
      )
    } else {
      
      ggplot() +
        geom_text(aes(x = 0, y = 0, label = "Please select a specific\nknowledge assessment to see details"),
                  family = "Calibri", fontface = "bold", size = 8) +
        theme_void()
      
    }
    
  })

  ########################

  ### Reactive Outputs Section ###

  output$sites_filter <- renderUI({
    div(
      id = "sites_filter",
      selectInput(
        inputId = "site",
        label = h5("Select a Site"),
        choices = c("All Sites", all_sites_filtered())
      )
    )
  })

  ########################
}

shinyApp(ui, server)
