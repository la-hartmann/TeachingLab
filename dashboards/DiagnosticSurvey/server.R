######## Define server logic ##########
shinyServer(function(input, output) {

    function(...) { }
    
    output$diagnostic_correct <- renderPlot({
        diagnostic %>%
            select(grades) %>%
            pivot_longer(everything()) %>%
            tidyr::drop_na(value) %>%
            group_by(name) %>%
            count(sort = T) %>%
            mutate(name = readr::parse_number(name)) %>%
            mutate(name = replace_na(name, "K")) %>%
            ggplot(aes(fct_reorder(name, n), n)) +
            geom_col(aes(fill = n)) +
            coord_flip() +
            labs(y = "Count", x = "Grade") +
            scale_fill_continuous() +
            theme(axis.text = element_markdown(size = 20),
                  axis.title = element_markdown(size = 24))
    })
    
    output$diagnostic_text <- render_gt({
        diagnostic %>%
            select(other_curricula) %>%
            pivot_longer(everything()) %>%
            drop_na(value) %>%
            arrange(value) %>%
            quote_viz(text_col = "value", title = "Other Curricula Used")
    })
    
    # Plot Titles, Plot Heights
    values <- reactiveValues(always_title = "How often do you use the following curricula for your instruction on average?",
                             sometimes_title = "Which of the curricula materials do you use the most?",
                             height = 1600,
                             always_height = 3,
                             sometimes_height = 1,
                             always_average = "blank_default")
    
    always_plot <- reactive({
        # Create correct factor levels depending on grouping
        if (input$grouping == "teacher_curricula_use") {
            
            values$always_average <- curricula_average
            values$always_title <- glue::glue("How often do you use the following curricula for your instruction on average?<br> Most common response: \"{values$always_average$value[1]}\": {round(values$always_average[3])}%")
            values$height <- 1600
            values$always_height <- 3
            values$sometimes_height <- 1
            
            # Select all but two
            plot_data <- diagnostic %>%
                select(teacher_curricula_use) %>%
                pivot_longer(everything()) %>%
                mutate(name = str_remove_all(name, c("how_often_do_you_use_the_following_curricula_for_your_instruction_on_average_|_2"))) %>%
                mutate(name = html_wrap(str_to_title(str_replace_all(name, "_", " ")), n = 55)) %>%
                tidyr::drop_na(value) %>%
                group_by(value, name) %>% # Group by both name and value
                summarise(n = n()) %>% # Get n of union of subgroup
                group_by(name) %>% # Group by just value
                mutate(percent = 100*(n/sum(n))) %>% # Get percent of value within name
            # String replacement so axis text doesn't get crowded
                mutate(value = str_replace_all(value, 
                                               "Three times a week", 
                                               "Three times<br>a week"),
                       value = str_replace_all(value, 
                                               "More than three times a week", 
                                               "More than<br>three times<br>a week"),
                       value = factor(value, levels = c("Never", 
                                                        "Once a week",
                                                        "Twice a week", 
                                                        "Three times<br>a week", 
                                                        "More than<br>three times<br>a week")))
        } else if (input$grouping == "teacher_mindsets") {
            
            values$always_average <- teacher_mindset_average
            values$always_title <- glue::glue("To what extent do you agree or disagree with the following statements<br> Average score: {values$always_average}/5")
            values$height <- 1600
            
            # Select all
            plot_data <- diagnostic %>%
                select(teacher_mindsets) %>%
                pivot_longer(everything()) %>%
                mutate(name = str_remove_all(name, c("to_what_extent_do_you_agree_or_disagree_with_the_following_statements_"))) %>%
                mutate(name = html_wrap(str_to_title(str_replace_all(name, "_", " ")), n = 55)) %>%
                tidyr::drop_na(value) %>%
                group_by(value, name) %>% # Group by both name and value
                summarise(n = n()) %>% # Get n of union of subgroup
                group_by(name) %>% # Group by just name
                mutate(percent = 100*(n/sum(n))) %>% # Get percent of name within value
                mutate(value = str_remove_all(value, "[:digit:]- "),
                       value = str_replace_all(value, "Neither agree nor disagree", "Neither agree<br>nor disagree"),
                       value = factor(value, levels = c("Strongly disagree",
                                                        "Disagree",
                                                        "Neither agree<br>nor disagree",
                                                        "Agree",
                                                        "Strongly agree")))
        } else if (input$grouping == "teacher_crse_practices") {
            sometimes_average <- teacher_crse_average
            
            values$sometimes_title <- glue::glue("Please rate your confidence on the following items<br>Average Score: {sometimes_average}/10")
            
            # Select confidence items
            plot_data <- diagnostic %>%
                select(teacher_crse_practices) %>%
                pivot_longer(everything()) %>%
                mutate(name = str_remove_all(name, c("please_rate_your_confidence_on_the_following_items_br_"))) %>%
                mutate(name = html_wrap(str_to_title(str_replace_all(name, "_", " ")), n = 55)) %>%
                tidyr::drop_na(value) %>%
                group_by(value, name) %>% # Group by both name and value
                summarise(n = n()) %>% # Get n of union of subgroup
                group_by(name) %>% # Group by just value
                mutate(percent = 100*(n/sum(n))) %>% # Get percent of value within name
                mutate(value = readr::parse_number(as.character(value)),
                       value = factor(value, levels = c(0:10)))
        } else if (input$grouping == "teacher_school_environment") {
            
            values$always_average <- teacher_school_environment_average
            
            values$always_title <- glue::glue("To what extent do you agree or disagree with the following statements<br> Average score: {values$always_average}/5")
            values$height <- 1600
            
            # Select all
            plot_data <- diagnostic %>%
                select(teacher_school_environment) %>%
                pivot_longer(everything()) %>%
                mutate(name = str_remove_all(name, c("to_what_extent_do_you_agree_or_disagree_with_the_following_statements_"))) %>%
                mutate(name = html_wrap(str_to_title(str_replace_all(name, "_", " ")), n = 55)) %>%
                tidyr::drop_na(value) %>%
                group_by(value, name) %>% # Group by both name and value
                summarise(n = n()) %>% # Get n of union of subgroup
                group_by(name) %>% # Group by just value
                mutate(percent = 100*(n/sum(n))) %>% # Get percent of value within name
                mutate(value = str_remove_all(value, "[:digit:]- "),
                       value = str_replace_all(value, "Neither agree nor disagree", "Neither agree<br>nor disagree"),
                       value = factor(value, levels = c("Strongly disagree",
                                                        "Disagree",
                                                        "Neither agree<br>nor disagree",
                                                        "Agree",
                                                        "Strongly agree")))
        } else if (input$grouping == "administrator_mindsets") {
            
            values$always_average <- administrator_mindsets_average
            
            values$always_title <- glue::glue("To what extent do you agree or disagree with the following statements<br> Average score: {values$always_average}/5")
            values$height <- 1600
            
            # Select all
            plot_data <- diagnostic %>%
                select(administrator_mindsets) %>%
                pivot_longer(everything()) %>%
                mutate(name = str_remove_all(name, c("to_what_extent_do_you_agree_or_disagree_with_the_following_statements_|_2"))) %>%
                mutate(name = html_wrap(str_to_title(str_replace_all(name, "_", " ")), n = 55)) %>%
                tidyr::drop_na(value) %>%
                group_by(value, name) %>% # Group by both name and value
                summarise(n = n()) %>% # Get n of union of subgroup
                group_by(name) %>% # Group by just value
                mutate(percent = 100*(n/sum(n))) %>% # Get percent of value within name
                mutate(value = str_remove_all(value, "[:digit:]- "),
                       value = str_replace_all(value, "Neither agree nor disagree", "Neither agree<br>nor disagree"),
                       value = factor(value, levels = c("Strongly disagree",
                                                        "Disagree",
                                                        "Neither agree<br>nor disagree",
                                                        "Agree",
                                                        "Strongly agree")))
        } else if (input$grouping == "administrator_support_crt") {
            
            values$always_average <- administrator_support_crt_average
            
            values$always_title <- glue::glue("To what extent do you agree or disagree with the following statements<br> Average score: {values$always_average}/10")
            values$height <- 1600
            
            # Select all
            plot_data <- diagnostic %>%
                select(administrator_support_crt) %>%
                pivot_longer(everything()) %>%
                mutate(name = str_remove_all(name, c("please_rate_the_extent_to_which_you_believe_you_can_support_teachers_in_the_following_areas_br_"))) %>%
                mutate(name = html_wrap(str_to_title(str_replace_all(name, "_", " ")), n = 55)) %>%
                tidyr::drop_na(value) %>%
                group_by(value, name) %>% # Group by both name and value
                summarise(n = n()) %>% # Get n of union of subgroup
                group_by(name) %>% # Group by just value
                mutate(percent = 100*(n/sum(n))) # Get percent of value within name
            
            plot_data$value <- readr::parse_number(as.character(plot_data$value))
            plot_data$value <- factor(plot_data$value, levels = c(0:10))
        } else if (input$grouping == "administrator_observational_practices") {
            
            values$always_average <- administrator_observational_average
            
            values$always_title <- glue::glue("To what extent do you agree or disagree with the following statements<br> Average score: {values$always_average}/5")
            values$height <- 1600
            
            # Select all
            plot_data <- diagnostic %>%
                select(administrator_observational_practices) %>%
                pivot_longer(everything()) %>%
                mutate(name = str_remove_all(name, c("how_often_does_your_observation_of_teacher_practice_focus_on_the_following_"))) %>%
                mutate(name = html_wrap(str_to_title(str_replace_all(name, "_", " ")), n = 55)) %>%
                tidyr::drop_na(value) %>%
                group_by(value, name) %>% # Group by both name and value
                summarise(n = n()) %>% # Get n of union of subgroup
                group_by(name) %>% # Group by just value
                mutate(percent = 100*(n/sum(n))) %>% # Get percent of value within name
                mutate(value = str_remove_all(value, "[:digit:]- "),
                       value = factor(value, levels = c("Almost never",
                                                        "Rarely",
                                                        "Sometimes",
                                                        "Very often",
                                                        "Almost always")))
        }
        ####### End of Conditionals #########
        return(plot_data)
    })
    
    sometimes_plot <- reactive({
        if (input$grouping == "teacher_curricula_use") {
            
            values$sometimes_title <- "Of the curriculum materials you indicated using regularly please indicate which was most used"
            
            # Select remaining two
            plot_data <- diagnostic %>%
                select("of_the_curriculum_materials_you_indicated_using_regularly_once_a_week_or_more_please_indicate_the_one_you_use_the_most",
                       "of_the_curriculum_materials_you_indicated_using_regularly_once_a_week_or_more_please_indicate_the_one_you_use_the_most_2") %>%
                pivot_longer(everything()) %>%
                mutate(name = html_wrap(str_to_title(str_replace_all(name, "_", " ")), n = 55)) %>%
                tidyr::drop_na(value) %>%
                filter(value != "<NA>") %>%
                group_by(value, name) %>% # Group by both name and value
                summarise(n = n()) %>% # Get n of union of subgroup
                group_by(name) %>% # Group by just value
                mutate(percent = 100*(n/sum(n))) %>% # Get percent of value within name
                mutate(value = str_replace_all(value, c("Curricula I create myself" = "Curricula I<br>create<br>myself",
                                                        "Another published ELA curriculum \\(specified in the previous question\\)" = "Another<br>ELA<br>Curriculum",
                                                        "Curricula my school or district has created" = "Curricula my<br>school or<br>district has<br>created",
                                                        "Guidebooks/ LearnZillion Guidebooks" = "Guidebooks/<br>LearnZillion<br>Guidebooks",
                                                        "Illustrative Mathematics" = "Illustrative<br>Mathematics",
                                                        "EngageNY/Eureka" = "EngageNY/<br>Eureka")),
                       value = factor(value, levels = c("Curricula I<br>create<br>myself",
                                                        "Another<br>ELA<br>Curriculum",
                                                        "Curricula my<br>school or<br>district has<br>created",
                                                        "Guidebooks/<br>LearnZillion<br>Guidebooks",
                                                        "Illustrative<br>Mathematics",
                                                        "EngageNY/<br>Eureka",
                                                        "Zearn")))
        }
        
        ####### End of Conditionals #########
        return(plot_data)
    })
    
    output$main_plot <- renderPlot({
        
        p1 <- always_plot() %>%
            ggplot(aes(x = value, y = percent)) +
            geom_col(aes(fill = percent)) +
            geom_richtext(aes(label = paste0("**", round(percent), "%<br>n = ", n, "**")), vjust = -0.1,
                      size = 8, lineheight = 0.5, fill = NA, label.color = NA, color = "black") +
            facet_wrap( ~ name, ncol = 3, scales = "free_x") +
            labs(y = "Count", x = "Response",
                 title = values$always_title) +
            scale_x_discrete() +
            scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
            theme(axis.text.x = element_markdown(family = "Calibri", size = 20, lineheight = 0.7),
                  axis.text.y = element_markdown(size = 20, family = "Calibri"),
                  strip.text = element_markdown(family = "Calibri Bold", size = 22, 
                                                hjust = 0.5,
                                                margin = margin(5, -20, 5, -20)),
                  axis.title = element_markdown(family = "Calibri", size = 22),
                  plot.title = element_markdown(family = "Calibri Bold", size = 32))
        if (input$grouping %in% c("teacher_curricula_use")) {
            p2 <- sometimes_plot() %>%
                ggplot(aes(x = value, y = percent)) +
                geom_col(aes(fill = percent)) +
                geom_richtext(aes(label = paste0("**", round(percent), "%<br>n = ", n, "**")), vjust = -0.1,
                              size = 8, lineheight = 0.5, fill = NA, label.color = NA, color = "black") +
                facet_wrap( ~ name, ncol = 3) +
                labs(y = "Count", x = "Response",
                     title = values$sometimes_title) +
                scale_x_discrete() +
                scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
                theme(axis.text.x = element_markdown(family = "Calibri", size = 20, lineheight = 0.7),
                      axis.text.y = element_markdown(size = 20, family = "Calibri"),
                      strip.text = element_markdown(family = "Calibri Bold", size = 22, 
                                                    hjust = 0.5,
                                                    margin = margin(5, -20, 5, -20)),
                      axis.title = element_markdown(family = "Calibri", size = 22),
                      plot.title = element_markdown(family = "Calibri Bold", size = 32))
            
            p1 / p2 +
                plot_layout(heights = c(values$always_height, values$sometimes_height))
        } else {
            p1
        }
        
        
    }, height = function() {values$height})

})
