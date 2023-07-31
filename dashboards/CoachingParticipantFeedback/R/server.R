#### Server for Coaching Participant Feedback ####

shinyServer(function(input, output) {
  function(...) { }

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
