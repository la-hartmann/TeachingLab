#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    function(...) { }
    
    output$diagnostic_correct <- renderPlot({
        diagnostic %>%
            select(c("what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_k", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_1", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_2", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_3", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_4", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_5", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_6", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_7", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_8", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_9", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_10", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_11", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_12"
            )) %>%
            pivot_longer(everything()) %>%
            drop_na(value) %>%
            group_by(name) %>%
            count(sort = T) %>%
            mutate(name = readr::parse_number(name)) %>%
            mutate(name = replace_na(name, "K")) %>%
            ggplot(aes(name, n)) +
            geom_col() +
            coord_flip() +
            TeachingLab::theme_tl()
    })
    
    
    
    #### Site Filter Conditional On Knowledge Assessments ####
    output$site_ui <- renderUI({
        # Wait for knowledge assessment selection
        req(!is.null(input$know_assessment))
        # Store which dataframe is selected
        know_site <- input$know_assessment
        
        selectizeInput("site", 
                       label = "Select Sites to Include",
                       choices = readr::read_rds(paste0("data/processed/", input$know_assessment)) %>% 
                           pull(site) %>% 
                           unique() %>% 
                           sort(),
                       multiple = T,
                       options = list(plugins= list('remove_button')))
    })
    
    #### Data Reactives ####
    know_data <- reactive({
        # Wait for knowledge assessment selection
        req(input$know_assessment, input$site)
        # Filter data for site
        df <- readr::read_rds(paste0("data/processed/", input$know_assessment)) %>%
            dplyr::filter(site %in% input$site) %>%
            dplyr::group_by(answer, prepost) %>%
            dplyr::mutate(percent = mean(percent, na.rm = T))
        print(df)
    })
    
    #### Matched Data Plot ####
    output$plot_matched <- renderPlot({
        know_data() %>%
            ggplot(aes(fct_reorder(answer, percent), percent, fill = prepost, group = prepost)) +
            geom_col(position = position_dodge2(preserve = "single", reverse = T)) +
            geom_text(aes(label = paste0(round(percent), "%")), hjust = -0.2, position = position_dodge2(width = 0.9, reverse = T)) +
            facet_wrap( ~ question, scales = "free", ncol = 2) +
            coord_flip() +
            scale_fill_manual(values = c("pre" = "#040404", "post" = "#00ACF0"), labels = c("Pre", "Post")) +
            labs(x = "", y = "",
                 title = "% Answering Each Item in ELA Bootcamp General",
                 fill = "") +
            guides(fill = guide_legend(label.position = "left")) +
            scale_y_continuous(labels = scales::label_percent(scale = 1), expand = c(0.14, 0)) +
            scale_x_discrete(expand = c(-0.5, 0)) +
            theme(
                axis.text.y = element_markdown(size = 14),
                strip.text = element_markdown(hjust = 0.5, family = "Calibri Bold"),
                plot.title = element_markdown(family = "Calibri Bold"),
                legend.position = c(0.5, 1.1),
                legend.direction = "horizontal",
                legend.key.size = unit(2, "cm"),
                legend.text = element_text(size = 14, family = "Calibri"),
                legend.key.width = unit(4, "cm")
            )
        
    })
    
    #### Unmatched Data Plot ####
    output$plot_unmatched <- renderPlot({
        know_data() %>%
            ggplot(aes(fct_reorder(answer, percent), percent, fill = prepost, group = prepost)) +
            geom_col(position = position_dodge2(preserve = "single", reverse = T)) +
            geom_text(aes(label = paste0(round(percent), "%")), hjust = -0.2, position = position_dodge2(width = 0.9, reverse = T)) +
            facet_wrap( ~ question, scales = "free", ncol = 2) +
            coord_flip() +
            scale_fill_manual(values = c("pre" = "#040404", "post" = "#00ACF0"), labels = c("Pre", "Post")) +
            labs(x = "", y = "", caption = "*Correct answers are colored blue and in bold",
                 title = "% Answering Each Item in ELA Bootcamp General",
                 fill = "") +
            guides(fill = guide_legend(label.position = "left")) +
            scale_y_continuous(labels = scales::label_percent(scale = 1), expand = c(0.14, 0)) +
            scale_x_discrete(expand = c(-0.5, 0)) +
            theme(
                axis.text.y = element_markdown(family = "Calibri", size = 14),
                strip.text = element_markdown(hjust = 0.5, family = "Calibri Bold"),
                plot.title = element_markdown(family = "Calibri Bold"),
                legend.position = c(0.5, 1.1),
                legend.direction = "horizontal",
                legend.key.size = unit(2, "cm"),
                legend.text = element_text(size = 14),
                legend.key.width = unit(4, "cm")
            )
    })

})
