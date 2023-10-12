### THINGS THAT STILL NEED TO BE DONE:
### ANSWERS X AXIS CAN BE COLORED TO INDICATE WHICH ANSWER IS CORRECT, possibly do via metadata?, add a "correct" column that is mutated based on answer, metadata equality
### KNOWLEDGE ASSESSMENTS OPTIONS SHOULD DISAPPEAR AFTER CHOOSING ONE, AND YOU SHOULD ALLOW SETTING KNOWLEDGE ASSESSMENT FROM SIDEBAR
### MAKE TUTORIAL VIDEO AND QUALTRICS SURVEY, MAKE QUALTRICS SURVEY HAVE EMBEDDED DATA DEPENDING ON WHERE IT IS LINKED FROM

# Define server logic
server <- function(input, output) {
  ########################################################################################################################
  ### Applies filters, returns data in summary format, question format, or detailed question format, along with title ###
  knowledge_assessments_filtered <- reactive({
    filtered <- knowledge_assessments |>
      dplyr::filter(dplyr::between(date, input$daterange3[1], input$daterange3[2] + 1)) |>
      tlShiny::neg_cond_filter("All Sites", input$site, site) |>
      tlShiny::neg_cond_filter("All Knowledge Assessments", input$know_assess, know_assess)

    if (is.null(input$custom_bar_click$clicked_level)) {
      final_df <- filtered |>
        dplyr::filter(question1 == "Score") |>
        dplyr::mutate(percent = 100 * score / max_score) |>
        dplyr::group_by(prepost, know_assess) |>
        dplyr::summarise(percent = round(mean(percent, na.rm = T), 2),
                         n = dplyr::n()) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          prepost = ifelse(prepost == "pre",
            "Before",
            "After"
          ),
          prepost = factor(prepost, levels = c("Before", "After")),
          know_assess = paste0(know_assess, " % Correct")
        ) |>
        print()
    } else if (input$custom_bar_click$clicked_level == "level2") {
      
      final_df <- filtered |>
        dplyr::filter(question1 != "Score" & know_assess == input$custom_bar_click$drilled_place) |>
        dplyr::mutate(
          percent = 100 * score / max_score,
          question2 = ifelse(is.na(question2), question1, question2)
        ) |>
        dplyr::group_by(prepost, know_assess, question2) |>
        dplyr::summarise(percent = round(mean(percent, na.rm = T), 2),
                         n = length(unique(id))) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          prepost = ifelse(prepost == "pre",
            "Before",
            "After"
          ),
          prepost = factor(prepost, levels = c("Before", "After")),
          question2 = stringr::str_wrap(question2, width = 30)
          ) |>
          print()
    } else if (input$custom_bar_click$clicked_level == "level3") {
      
      final_df <- filtered |>
        dplyr::filter(question1 != "Score") |>
        dplyr::mutate(question2 = ifelse(is.na(question2), question1, question2)) |>
        dplyr::filter(question2 == input$custom_bar_click$drilled_place) |>
        dplyr::group_by(prepost, know_assess) |>
        dplyr::mutate(max_n = n()/length(unique(question1))) |> ## Calculates maximum possible number of answers to each question via count of rows over unique number of questions which is constant
        dplyr::ungroup() |>
        drop_na(answer) |> ### The NA's in answer are counts of those who did not select that, so it is unnecessary to count
        dplyr::group_by(know_assess, prepost, answer, question1) |>
        dplyr::reframe(percent = 100 * (n()/max_n), # this is percent selected
                       number_selected = n(),
                       question2 = first(question2)) |>
        distinct(answer, question1, question2, percent, number_selected, .keep_all = TRUE) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          prepost = ifelse(prepost == "pre",
                           "Before",
                           "After"
          ),
          prepost = factor(prepost, levels = c("Before", "After")),
          question2 = stringr::str_wrap(paste0(question2, ": % Selected per Answer"), width = 60),
          answer = tlShiny::html_wrap(answer, n = 23),
          answer = if_else(stringr::str_replace_all(answer, "<br>", " ") %in% knowledge_assessments_answers, paste0("<b style = 'color: #0c8a1f'>", answer, "</b>"), answer)
          ) |>
        print()
      
    }

    return(list(data = final_df, title = title))
  })

  ########################################################################################################################
  
  ### Resets the value of input$custom_bar_click$clicked_level to NULL so that when people go to another course assessment while clicked into 
  ### level 2 or 3 it resets to level 1 and does not error ###
  observeEvent(input$know_assess, {
    
    shinyjs::runjs('Shiny.setInputValue("custom_bar_click", {clicked_level: null})')
    
  }, ignoreInit = TRUE)

  ### Plots Section ###

  output$summaryPlot <- renderGirafe({
    req(input$know_assess)
    print(input$custom_bar_click)

    if (is.null(input$custom_bar_click$clicked_level)) {
      knowledge_assessments_filtered()$data |>
        plot_know_assess()
    } else if (input$custom_bar_click$clicked_level == "level2") {
      knowledge_assessments_filtered()$data |>
        plot_know_assess(title = stringr::str_wrap(paste0(input$custom_bar_click$drilled_place, " % Correct By Question"), width = 55), 
                         level = input$custom_bar_click$clicked_level)
    } else if (input$custom_bar_click$clicked_level == "level3") {
      knowledge_assessments_filtered()$data |>
        plot_know_assess(title = paste0("% Selected Answers"), 
                         level = input$custom_bar_click$clicked_level)
    }
  })

  ########################################################################################################################

  ### Reactive Outputs Section ###

  ### Filter down list of sites ###
  all_sites_filtered <- reactive({
    filtered <- knowledge_assessments |>
      tlShiny::neg_cond_filter("All Course Assessments", input$know_assess, know_assess) |>
      dplyr::distinct(site) |>
      dplyr::pull() |>
      sort()

    filtered
  })

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

  ########################################################################################################################
}
