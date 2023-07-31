#### Coaching Participant Feedback Dashboard ####
uiText <- function(id, label = "Counter") {
  ns <- NS(id)
  shiny::tagList(
    shiny.semantic::sidebar_layout(
      sidebar_panel = shiny.semantic::sidebar_panel(
        style = sidebar_style,
        shiny.semantic::menu_item(
          tabName = "site_menu",
          shiny::selectInput("site",
                             label = h5("Select a Site"),
                             choices = c(sort(unique(coaching_participant_feedback$Site)) |>
                                           prepend("All Sites"))
          ),
          icon = shiny.semantic::icon("clone")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "content_menu",
          uiOutput("site_ui"),
          icon = shiny.semantic::icon("edit")
        )
      ),
      main_panel = shiny.semantic::main_panel(
        width = 4,
        div(
          class = "ui two column stackable grid container",
          div(
            class = "sixteen wide column",
            gt_output("qualitative_feedback") |>
              withSpinner(type = 3, color.background = "white")
          )
        )
      )
    )
  )
}

textServer <- function(id) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    
    observe({
      print(sign_ins()$name)
    })
    
    #### Site Filter Conditional On Knowledge Assessments ####
    output$site_ui <- renderUI({
      # Wait for site selection
      req(!is.null(input$site))
      
      selectizeInput("coach",
                     label = "Select Coaches to Include",
                     choices = coaching_participant_feedback |>
                       neg_cond_filter("All Sites", input$site, Site) |>
                       pull(Coach) |>
                       unique() |>
                       sort() |>
                       prepend("All Coaches"),
                     multiple = T,
                     selected = "All Coaches",
                     options = list(plugins = list("remove_button"))
      )
    })
    
    output$qualitative_feedback <- render_gt({
      qualitative_data <- coaching_participant_feedback |>
        select(Additional_feedback, Gone_well, Could_be_better) |>
        drop_na() |>
        rename(`What additional feedback do you have about their coaching skills, if any?` =
                 `Additional_feedback`,
               `What has gone well in your coaching sessions?` = Gone_well,
               `What could be better about your coaching sessions?` = Could_be_better)
      
      
      qualitative_data |>
        tlShiny::quote_viz(text_col = c("What additional feedback do you have about their coaching skills, if any?",
                                        "What has gone well in your coaching sessions?",
                                        "What could be better about your coaching sessions?"),
                           print = F,
                           width = 100,
                           align = "left")
    },
    width = px(1000))
    
  })
}
