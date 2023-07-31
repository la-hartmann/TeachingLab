#### Coaching Participant Feedback Dashboard ####
uiAgree <- function(id, label = "Counter") {
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
            plotOutput(outputId = "agree_plot", height = "900px") |>
              withSpinner(type = 3, color.background = "white")
          )
        )
      )
    )
  )
}

agreeServer <- function(id) {
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
    
    agree_plot_data <- reactive({
      
      ### Require site and coach inputs ###  
      validate(
        need(!is.null(input$site), "Please select at least one site."),
        need(!is.null(input$coach), "Please select at least one coach.")
      )
      
      ### Make data by extracting percentages in strongly agree-strongly disagree ###
      agree_data <- coaching_participant_feedback |>
        neg_cond_filter("All Sites", input$site, Site) |>
        neg_cond_filter("All Coaches", input$coach, Coach) |>
        select(
          `They demonstrated deep knowledge of the content they coach.`,
          `Their coaching is clear.`,
          `They seem fully prepared for the coaching sessions.`,
          `They effectively build a safe learning environment.`,
          `They make necessary adjustments based on my needs.`
        ) |>
        pivot_longer(everything(), names_to = "question", values_to = "answer") |>
        drop_na(answer) |>
        group_by(question, answer) |>
        count(sort = T) |>
        ungroup() |>
        group_by(question) |>
        mutate(
          percent = round(100 * n / sum(n), 2),
          answer = factor(answer, levels = c(
            "(1) Strongly disagree",
            "(2) Disagree",
            "(3) Neither agree nor disagree",
            "(4) Agree",
            "(5) Strongly agree"
          )),
          question = tlShiny::html_wrap(question, n = 30)
        ) |>
        ungroup()
      
      ### Return the data ###
      agree_data
    })
    
    #### Agree Data Plot ####
    output$agree_plot <- renderPlot({
      ggplot(data = agree_plot_data(), aes(x = fct_reorder(question, percent, .desc = T), 
                                           y = percent, 
                                           color = answer,
                                           fill = answer)) +
        geom_col(color = NA) +
        geom_text(aes(label = ifelse(percent >= 5,
                                     paste0(round(percent), "%\n(n = ", n, ")"),
                                     "")), 
                  position = position_stack(vjust = 0.5),
                  family = "Calibri Bold",
                  size = 4.5) +
        labs(
          x = "", y = "",
          title = "% Answering Each Item in Coaching Participant Feedback",
          fill = ""
        ) +
        scale_fill_manual(values = c(
          "(1) Strongly disagree" = "#040404", 
          "(2) Disagree" = "#032E3F",
          "(3) Neither agree nor disagree" = "#02587A", 
          "(4) Agree" = "#0182B4", 
          "(5) Strongly agree" = "#00ACF0"
        )) +
        scale_color_manual(values = c("(1) Strongly disagree" = "white",
                                      "(2) Disagree" = "white",
                                      "(3) Neither agree nor disagree" = "black", 
                                      "(4) Agree" = "black", 
                                      "(5) Strongly agree" = "black")) +
        guides(fill = guide_legend(label.position = "bottom",
                                   reverse = T),
               color = "none") +
        scale_y_continuous(labels = scales::label_percent(scale = 1), 
                           expand = c(0.14, 0),
                           limits = c(0, 100.01),
                           breaks = scales::pretty_breaks(n = 5)) +
        scale_x_discrete(expand = c(-0.5, 0)) +
        coord_flip() +
        theme(
          strip.text.x = element_markdown(size = 20),
          axis.text.y = element_markdown(size = 14),
          axis.text.x = element_markdown(size = 14),
          strip.text = element_markdown(hjust = 0.5, family = "Calibri Bold"),
          plot.title = element_markdown(family = "Calibri Bold", size = 20),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.size = unit(1, "cm"),
          legend.text = element_text(size = 10, family = "Calibri"),
          legend.key.width = unit(2, "cm")
        )
    })
    
  })
}
