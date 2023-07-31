### Kimberly Robertson Dashboard ###
library(bslib)
library(dplyr)
library(ggplot2)
library(googledrive)
library(googlesheets4)
library(gt)
library(janitor)
library(shiny)
library(shinyWidgets)
library(showtext)
library(stringr)
library(tidyr)
library(tlShiny)

## Add Calibri Fonts ##
font_add(family = "Calibri", regular = "www/Calibri.ttf")
font_add(family = "Calibri Bold", regular = "www/Calibri Bold.ttf")
font_add(family = "Roboto", regular = "www/Roboto-Black.ttf")
###

# Authorize google drive
drive_auth(path = "data/thermal-cathode-310719-1445194b99c7.json")
gs4_auth(token = drive_token()) # REMEMBER YOU JUST CHANGED THIS

# Get data
facilitator_data <- read_sheet("https://docs.google.com/spreadsheets/d/1J2JoQ0RvKGPGfEDrwaiWQo3AlWCIyNrlnQjulY4kVx0/edit#gid=41065202")

# Define UI for application that draws a histogram
ui <- navbarPage(

  # Application title
  titlePanel("Welcome to your dashboard Kimberly Robertson"),
  theme = bslib::bs_theme(
    bg = "white", fg = "black",
    # Controls the accent (e.g., hyperlink, button, etc) colors
    primary = "#04abeb", secondary = "#48DAC6",
    base_font = c("Calibri", "sans-serif"),
    # Can also add lower-level customization
    "input-border-color" = "#EA80FC"
  ) |>
    bslib::bs_add_rules(sass::sass_file("custom.scss")),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      width = 3,
      style = "position:fixed;width:inherit;",
      shiny::selectizeInput(
        inputId = "course",
        label = h3("Select a course"),
        choices = facilitator_data$Course |>
          unique() |>
          as.character() |>
          sort() |>
          purrr::prepend("All Courses"),
        multiple = T,
        selected = "All Courses",
        options = list(plugins = list("remove_button"))
      ),
      shiny::selectizeInput(
        inputId = "site",
        label = h3("Select a site"),
        choices = facilitator_data$Site |>
          unique() |>
          as.character() |>
          sort() |>
          purrr::prepend("All Sites"),
        multiple = T,
        selected = "All Sites",
        options = list(plugins = list("remove_button"))
      ),
      shiny::selectizeInput(
        inputId = "content_area",
        label = h3("Select a content area"),
        choices = facilitator_data$`Content Area` |>
          unique() |>
          as.character() |>
          sort() |>
          purrr::prepend("All Content Areas"),
        multiple = T,
        selected = "All Content Areas",
        options = list(plugins = list("remove_button"))
      ),
      splitLayout(
        cell_widths = c("50%", "50%"),
        cell_args = "padding: 0px;",
        style = "background-color: transparent;",
        shinyWidgets::airDatepickerInput(
          inputId = "date_min",
          label = h4("Minimum date"),
          value = min(as.Date(facilitator_data$Date), na.rm = T),
          minDate = min(as.Date(facilitator_data$Date), na.rm = T),
          maxDate = max(as.Date(facilitator_data$Date), na.rm = T) + 1,
          dateFormat = "MM-dd-yyyy",
          width = "250px"
        ),
        shinyWidgets::airDatepickerInput(
          inputId = "date_max",
          label = h4("Maximum date"),
          value = Sys.Date() + 1,
          minDate = min(as.Date(facilitator_data$Date), na.rm = T),
          maxDate = Sys.Date() + 1,
          dateFormat = "MM-dd-yyyy",
          width = "250px"
        )
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("qualFeedback",
        height = "800px"
      ),
      fluidRow(
        column(width = 3, gt::gt_output(outputId = "qualitative_table_2")),
        column(width = 3, gt::gt_output(outputId = "qualitative_table_3")),
        column(width = 3, gt::gt_output(outputId = "qualitative_table_4")),
        column(width = 3, gt::gt_output(outputId = "qualitative_table_1"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  n_size_data <- reactive({
    validate(
      need(!is.null(input$site), "Please select at least one site"),
      need(!is.null(input$content_area), "Please select at least one content area"),
      need(!is.null(input$course), "Please select at least one course"),
      need(input$date_min <= input$date_max, "Please select a minimum date that is less than the maximum.")
    )

    n_size <- nrow(facilitator_data)

    return(n_size)
  })

  filtered_data <- reactive({
    validate(
      need(!is.null(input$site), "Please select at least one site"),
      need(!is.null(input$content_area), "Please select at least one content area"),
      need(!is.null(input$course), "Please select at least one course"),
      need(input$date_min <= input$date_max, "Please select a minimum date that is less than the maximum.")
    )

    filtered_fac_data <- facilitator_data |>
      dplyr::mutate(Date = as.Date(Date)) |>
      dplyr::filter(between(Date, input$date_min, input$date_max)) |>
      tlShiny::neg_cond_filter(
        if_not_this = "All Sites",
        filter_this = input$site,
        dat_filter = Site
      ) |>
      tlShiny::neg_cond_filter(
        if_not_this = "All Content Areas",
        filter_this = input$content_area,
        dat_filter = `Content Area`
      ) |>
      tlShiny::neg_cond_filter(
        if_not_this = "All Courses",
        filter_this = input$course,
        dat_filter = Course
      ) |>
      dplyr::select(
        `They demonstrated deep knowledge of the content they facilitated`,
        `They facilitated the content clearly`,
        `They effectively built a safe learning community`,
        `They were fully prepared for the session`,
        `They responded to the group’s needs`
      ) |>
      tidyr::pivot_longer(everything()) |>
      dplyr::group_by(name, value) |>
      dplyr::count() |>
      dplyr::ungroup() |>
      dplyr::group_by(name) |>
      dplyr::mutate(
        percent = 100 * n / sum(n),
        name = stringr::str_wrap(name, width = 20)
      )

    validate(
      need(nrow(filtered_fac_data) > 0, "There is no data available for these filters")
    )

    return(filtered_fac_data)
  })


  output$qualFeedback <- renderPlot({
    
    ggplot(filtered_data(), aes(x = name, y = percent, fill = value)) +
      geom_col(position = position_stack(vjust = 0.5, reverse = T)) +
      geom_text(aes(label = paste0(round(percent, 2), "%")),
        position = position_stack(vjust = 0.5, reverse = T),
        fontface = "bold"
      ) +
      scale_fill_manual(values = c(
        "(1) Strongly disagree" = "#040404",
        "(2) Disagree" = "#032E3F",
        "(3) Neither agree nor disagree" = "#02587A",
        "(4) Agree" = "#0182B4",
        "(5) Strongly agree" = "#00ACF0"
      )) +
      scale_color_manual(values = c(
        "(1) Strongly disagree" = "white",
        "(2) Disagree" = "white",
        "(3) Neither agree nor disagree" = "black",
        "(4) Agree" = "black",
        "(5) Strongly agree" = "black"
      )) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      coord_flip() +
      labs(
        x = "", y = "",
        title = paste0("Participant Perceptions of Sessions (n = ", n_size_data(), ")")
      ) +
      tlShiny::theme_tl(legend = T) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(face = "bold")
      )
  })

  qualitative_data_1 <- reactive({
    
    validate(
      need(!is.null(input$site), "Please select at least one site"),
      need(!is.null(input$content_area), "Please select at least one content area"),
      need(!is.null(input$course), "Please select at least one course"),
      need(input$date_min <= input$date_max, "Please select a minimum date that is less than the maximum.")
    )
    
    selected_feedback <- facilitator_data |>
      dplyr::mutate(Date = as.Date(Date)) |>
      dplyr::filter(between(Date, input$date_min, input$date_max)) |>
      tlShiny::neg_cond_filter(
        if_not_this = "All Sites",
        filter_this = input$site,
        dat_filter = Site
      ) |>
      tlShiny::neg_cond_filter(
        if_not_this = "All Content Areas",
        filter_this = input$content_area,
        dat_filter = `Content Area`
      ) |>
      tlShiny::neg_cond_filter(
        if_not_this = "All Courses",
        filter_this = input$course,
        dat_filter = Course
      ) |>
      dplyr::select(`Additional feedback`)
    
    validate(
      need(nrow(selected_feedback) > 0, "There is no feedback here.")
    )

    return(selected_feedback)
  })

  output$qualitative_table_1 <- gt::render_gt(
    tlShiny::quote_viz(
      data = qualitative_data_1(),
      viz_type = "gt",
      print = F,
      n = 5,
      width = 100
    )
  )
  
  qualitative_data_2 <- reactive({
    
    validate(
      need(!is.null(input$site), "Please select at least one site"),
      need(!is.null(input$content_area), "Please select at least one content area"),
      need(!is.null(input$course), "Please select at least one course"),
      need(input$date_min <= input$date_max, "Please select a minimum date that is less than the maximum.")
    )
    
    selected_feedback <- facilitator_data |>
      dplyr::mutate(Date = as.Date(Date)) |>
      dplyr::filter(between(Date, input$date_min, input$date_max)) |>
      tlShiny::neg_cond_filter(
        if_not_this = "All Sites",
        filter_this = input$site,
        dat_filter = Site
      ) |>
      tlShiny::neg_cond_filter(
        if_not_this = "All Content Areas",
        filter_this = input$content_area,
        dat_filter = `Content Area`
      ) |>
      tlShiny::neg_cond_filter(
        if_not_this = "All Courses",
        filter_this = input$course,
        dat_filter = Course
      ) |>
      dplyr::select(`What is one thing from today's learning that you plan to take back to your classroom?`)
    
    validate(
      need(nrow(selected_feedback) > 0, "There is no feedback here.")
    )
    
    return(selected_feedback)
  })
  
  output$qualitative_table_2 <- gt::render_gt(
    tlShiny::quote_viz(
      data = qualitative_data_2(),
      viz_type = "gt",
      print = F,
      n = 5,
      width = 100
    )
  )
  
  qualitative_data_3 <- reactive({
    
    validate(
      need(!is.null(input$site), "Please select at least one site"),
      need(!is.null(input$content_area), "Please select at least one content area"),
      need(!is.null(input$course), "Please select at least one course"),
      need(input$date_min <= input$date_max, "Please select a minimum date that is less than the maximum.")
    )
    
    selected_feedback <- facilitator_data |>
      dplyr::mutate(Date = as.Date(Date)) |>
      dplyr::filter(between(Date, input$date_min, input$date_max)) |>
      tlShiny::neg_cond_filter(
        if_not_this = "All Sites",
        filter_this = input$site,
        dat_filter = Site
      ) |>
      tlShiny::neg_cond_filter(
        if_not_this = "All Content Areas",
        filter_this = input$content_area,
        dat_filter = `Content Area`
      ) |>
      tlShiny::neg_cond_filter(
        if_not_this = "All Courses",
        filter_this = input$course,
        dat_filter = Course
      ) |>
      dplyr::select(`What went well in today’s session?`)
    
    validate(
      need(nrow(selected_feedback) > 0, "There is no feedback here.")
    )
    
    return(selected_feedback)
  })
  
  output$qualitative_table_3 <- gt::render_gt(
    tlShiny::quote_viz(
      data = qualitative_data_3(),
      viz_type = "gt",
      print = F,
      n = 5,
      width = 100
    )
  )
  
  qualitative_data_4 <- reactive({
    
    validate(
      need(!is.null(input$site), "Please select at least one site"),
      need(!is.null(input$content_area), "Please select at least one content area"),
      need(!is.null(input$course), "Please select at least one course"),
      need(input$date_min <= input$date_max, "Please select a minimum date that is less than the maximum.")
    )
    
    selected_feedback <- facilitator_data |>
      dplyr::mutate(Date = as.Date(Date)) |>
      dplyr::filter(between(Date, input$date_min, input$date_max)) |>
      tlShiny::neg_cond_filter(
        if_not_this = "All Sites",
        filter_this = input$site,
        dat_filter = Site
      ) |>
      tlShiny::neg_cond_filter(
        if_not_this = "All Content Areas",
        filter_this = input$content_area,
        dat_filter = `Content Area`
      ) |>
      tlShiny::neg_cond_filter(
        if_not_this = "All Courses",
        filter_this = input$course,
        dat_filter = Course
      ) |>
      dplyr::select(`What could have been better about today’s session?`)
    
    validate(
      need(nrow(selected_feedback) > 0, "There is no feedback here.")
    )
    
    return(selected_feedback)
  })
  
  output$qualitative_table_4 <- gt::render_gt(
    tlShiny::quote_viz(
      data = qualitative_data_4(),
      viz_type = "gt",
      print = F,
      n = 5,
      width = 100
    )
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
