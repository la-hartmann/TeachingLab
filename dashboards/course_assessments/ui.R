ui <- tagList(
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico")
  ),
  grid_page(
    shinyjs::useShinyjs(),
    layout = c(
      "header header",
      "sidebar summaryPlot"
    ),
    row_sizes = c(
      "70px",
      "1fr"
    ),
    col_sizes = c(
      "18%",
      "82%"
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
    ### App Title ###
    grid_card_text(
      area = "header",
      content = "Course Assessments Dashboard",
      alignment = "center",
      icon = "just_logo.png",
      is_title = TRUE
    ),
    grid_card(
      area = "sidebar",
      fill = TRUE,
      item_alignment = "top",
      style = "display:block; overflow:scroll;",
      card_body(
        min_height = "1000px",
        selectInput(
          inputId = "know_assess",
          label = h5("Select a Course Assessment"),
          choices = sort(unique(knowledge_assessments$know_assess))
        ), ### MAKE THIS RESET TO FIRST LEVEL WHEN CHANGED AT OTHER LEVELS
        uiOutput("sites_filter"),
        dateRangeInput("daterange3", h5("Select a date range:"),
          start = "2023-07-01",
          end = Sys.Date(),
          min = "2023-07-01",
          max = Sys.Date(),
          format = "mm/dd/yy",
          separator = " - "
        ),
        tags$br(),
        tags$div(
          id = "feedbackDiv", class = "extras",
          tags$h6(
            "Have feedback? Please take",
            tags$a("the linked survey!", href = "https://teachinglab.iad1.qualtrics.com/jfe/form/SV_0Bqu3SUziXrmvlA")
          )
        ),
        # tags$br(),
        tags$div(
          id = "tutorialDiv", class = "extras",
          tags$h6(
            "Have questions about dashboard use or course assessments scoring?",
            tags$a("Check out the video tutorial here!", href = "https://www.youtube.com/watch?v=o0CEkx0r5yY")
          )
        )
      )
    ),
    # grid_card(
    girafeOutput("summaryPlot", height = "900px", width = "1100px") |>
      shinycssloaders::withSpinner(color = "#04abeb", type = 7)
    # )
  )
)
