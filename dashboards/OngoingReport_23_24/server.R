# Define server logic
server <- function(input, output) {
  
  # Series of reactives to filter the data by site and subsite #
  
  educator_survey_filtered <- reactive({
    
    df <- educator_survey |>
      tlShiny::neg_cond_filter(if_not_this = "All Sites", filter_this = input$siteSelect, dat_filter = site) |>
      tlShiny::neg_cond_filter(if_not_this = "All Races", filter_this = input$raceSelect, dat_filter = race)
    
    df
    
  })
  
  followup_educator_survey_filtered <- reactive({
    
    df <- followup_educator |>
      tlShiny::neg_cond_filter(if_not_this = "All Sites", filter_this = input$siteSelect, dat_filter = site) |>
      tlShiny::neg_cond_filter(if_not_this = "All Races", filter_this = input$raceSelect, dat_filter = race)
    
    df
    
  })
  
  session_survey_filtered <- reactive({
    
    df <- session_survey |>
      tlShiny::neg_cond_filter(if_not_this = "All Sites", filter_this = input$siteSelect, dat_filter = site) |>
      tlShiny::neg_cond_filter(if_not_this = "All Races", filter_this = input$raceSelect, dat_filter = race)
    
    df
    
  })
  
  course_survey_filtered <- reactive({
    
    df <- course_survey |>
      tlShiny::neg_cond_filter(if_not_this = "All Sites", filter_this = input$siteSelect, dat_filter = site) |>
      tlShiny::neg_cond_filter(if_not_this = "All Races", filter_this = input$raceSelect, dat_filter = race)
    
    df
    
  })
  
  ongoing_coach_survey_filtered <- reactive({
    
    df <- ongoing_coaching |>
      tlShiny::neg_cond_filter(if_not_this = "All Sites", filter_this = input$siteSelect, dat_filter = site) |>
      tlShiny::neg_cond_filter(if_not_this = "All Races", filter_this = input$raceSelect, dat_filter = race)
    
    df
    
  })
  
  end_coach_survey_filtered <- reactive({
    
    df <- end_coaching |>
      tlShiny::neg_cond_filter(if_not_this = "All Sites", filter_this = input$siteSelect, dat_filter = site) |>
      tlShiny::neg_cond_filter(if_not_this = "All Races", filter_this = input$raceSelect, dat_filter = race)
    
    df
    
  })
  
  knowledge_assessments_filtered <- reactive({
    tibble::tibble() # For now
  })
  
  ipg_forms_filtered <- reactive({
    
    df <- ipg_forms |>
      tlShiny::neg_cond_filter(if_not_this = "All Sites", filter_this = input$siteSelect, dat_filter = site)
    
    df
    
  })
  
  student_work_filtered <- reactive({
    tibble::tibble() # For now
  })
  
  contact_lead_filtered <- reactive({
    tibble::tibble() # For now
  })
  
  final_sec_intro <- reactive({
    counts <- list()
    
    counts$intro <- paste0("Teaching Lab collects a variety of data to understand the impact of its PL for educators and students. Please see this <a href = 'https://docs.google.com/document/d/19yDyFCx1KeSPHlD9SLvLfG76FRGdfChL1hnCo_dV1zQ/edit'>resource</a> for a visual overview of our evaluation framework.<br>Additionally, the <a href = 'https://docs.google.com/document/d/19yDyFCx1KeSPHlD9SLvLfG76FRGdfChL1hnCo_dV1zQ/edit'>narrative</a> of our evaluation plan provides more details.<br><br>To date (", Sys.Date() |> format("%b %d, %Y"), "), Teaching Lab has collected the following data during SY22-23:")
    
    if (nrow(session_survey_filtered()) >= 1) {
      sec2_intro <- "Participant Perceptions"
      counts$p1 <- paste0("• ", format(nrow(session_survey_filtered()), big.mark = ","), " complete responses to the End of Session Survey section of the survey that gathers participant feedback on facilitation from session to session;")
    }
    if (nrow(course_survey_filtered()) >= 1) {
      sec2_intro <- "Participant Perceptions"
      counts$p2 <- paste0("• ", format(nrow(course_survey_filtered()), big.mark = ","), " complete responses to the End of Course Survey section of the survey that gathers participant feedback on each PL course;")
    }
    if (nrow(ongoing_coach_survey_filtered()) >= 1) {
      sec2_intro <- "Participant Perceptions"
      counts$p3 <- paste0("• ", format(nrow(ongoing_coach_survey_filtered()), big.mark = ","), " complete responses to the Ongoing Coaching section of the survey that gathers participant feedback on each Coaching session;\n")
    }
    if (nrow(end_coach_survey_filtered()) >= 1) {
      sec2_intro <- "Participant Perceptions"
      counts$p4 <- paste0("• ", format(nrow(end_coach_survey_filtered()), big.mark = ","), " complete responses to the End of Coaching section of the survey that gathers participant feedback on each Coaching performance;")
    }
    if (sum(nrow(session_survey_filtered()), nrow(course_survey_filtered()), 
            nrow(ongoing_coach_survey_filtered()), nrow(end_coach_survey_filtered())) == 0) {
      sec2_intro <- NA
    }
    if (nrow(knowledge_assessments_filtered()) >= 1) {
      sec3_intro <- "Participant Knowledge"
      counts$p5 <- paste0("• ", nrow(knowledge_assessments_filtered() |> dplyr::filter(pre == "pre") |> dplyr::distinct(id)), " complete responses to the first Knowledge or Self-Reported Practices Assessments and ", post_know() |> dplyr::filter(pre == "post") |> dplyr::distinct(id), " to the second Knowledge or Self-Reported Practices Assessments;")
    } else {
      sec3_intro <- NA
    }
    if (nrow(educator_survey_filtered()) >= 1) {
      sec1_intro <- "Participant Background and Demographics Summary"
      sec4_intro <- "Teachers' Mindsets & School Environment"
      sec5_intro <- "School Leaders' Mindsets & Observational Practices"
      counts$p6 <- paste0("• ", format(nrow(educator_survey_filtered()), big.mark = ","), " complete responses to the Baseline Diagnostic Educator Survey that collects information on teachers' use of curricula, mindsets, self-reported practices, and school environment & ", format(nrow(followup_educator_survey_filtered()), big.mark = ","), " responses to the Follow-up Educator Survey.")
    } else {
      sec1_intro <- NA
      sec4_intro <- NA
      sec5_intro <- NA
    }
    if (nrow(ipg_forms_filtered()) >= 1) {
      sec6_intro <- "Instructional Practice"
      counts$p7 <- paste0("• ", format(nrow(ipg_forms_filtered()), big.mark = ","), " observations of classroom instruction.")
    } else {
      sec6_intro <- NA
    }
    if (nrow(student_work_filtered()) >= 1) {
      sec7_intro <- "Student Outcomes"
      counts$p8 <- paste0("• ", format(nrow(student_work_count()), big.mark = ","), " high quality samples of student work.")
    } else {
      sec7_intro <- NA
    }
    if (nrow(contact_lead_filtered())) {
      sec8_intro <- "Partnership Opportunities Moving Forward"
    } else {
      sec8_intro <- NA
    }
    
    inline_sections <- na.omit(c(
      sec1_intro,
      sec2_intro,
      sec3_intro,
      sec4_intro,
      sec5_intro,
      sec6_intro,
      sec7_intro,
      sec8_intro
    ))
    
    final_sec_intro <- glue::glue_collapse(inline_sections, ", ", last = " and ")
    final_sec_intro <- paste0("In this dashboard we have compiled the results in various sections:<br>",
                              final_sec_intro, ".")
    
    list(counts = counts, final_sec_intro = final_sec_intro)
  })
  
  ########################################################### Intro #########################################################
  
  output$counts <- renderUI({
    # counts <- final_sec_intro()[grep('p', names(final_sec_intro()))]
    print(final_sec_intro()$counts)
    final_counts <- final_sec_intro()$counts
    test <- purrr::map_chr(1:length(final_counts), ~ final_counts[[.x]])
    HTML(paste(test, collapse = "<br></br>"))
  })
  
  output$final_sec_intro <- renderText({
    final_sec_intro()$final_sec_intro
  })
  
  
  ##################################################### End of Intro #########################################################
  
  ########################################## Section 1 #################################################################
  
  output$gender_plot <- renderPlot({
    
    gender_plot <- TeachingLab::gt_percent_n(
      df = educator_survey_filtered(),
      column = "gender",
      custom_column_name = "Gender Identity", 
      viz_type = sample(c("treemap"), size = 1)
    )
    gender_plot
  })
  
  output$race_plot <- renderPlot({
    
    if (nrow(educator_survey_filtered()) >= 1) {
    # List of columns to select for race #
    race_cols <- c("race_1", "race_2", "race_3", "race_4", "race_5", "race_6", "race_7")
    
    race_plot <- educator_survey_filtered() |>
      dplyr::select(
        dplyr::all_of(
          race_cols
        )
      ) |>
      dplyr::transmute(race = paste(race_1, race_2, race_3, race_4, race_5, race_6, race_7, sep = ", "),
                       race = stringr::str_remove_all(race, "NA, |, NA")) |>
      dplyr::mutate(race = forcats::fct_lump(factor(race), 3)) |>
      dplyr::rename(`Race Identity` = race) |>
      TeachingLab::gt_percent_n(
        column = "Race Identity",
        custom_column_name = "Racial Identity",
        viz_type = sample(c("pie", "waffle", "treemap"), size = 1)
      )
    
    race_plot
    } else {
      TeachingLab:::no_data_plot
    }
  })
  
  output$ethnicity_plot <- renderPlot({
    
    ethnicity_gt <- TeachingLab::gt_percent_n(
      educator_survey_filtered(),
      column = "ethnicity",
      custom_column_name = "Ethnicity", viz_type = sample(c("pie", "waffle", "treemap"),
                                                          size = 1
      )
    )
    
    ethnicity_gt
    
  })
  
  output$teacher_experience_plot <- renderPlot({
    
    if (nrow(educator_survey_filtered()) >= 1) {
      n <- sum(!is.na(educator_survey_filtered()$teaching_experience))
      
      teaching_experience_plot <- educator_survey_filtered() |>
        drop_na(teaching_experience) |>
        dplyr::filter(!str_detect(teaching_experience, "NA")) |>
        dplyr::mutate(teaching_experience = na.omit(teaching_experience)) |>
        group_by(teaching_experience) |>
        count(sort = T) |>
        ungroup() |>
        mutate(
          percent = round(100 * n / sum(n), 1),
          color = if_else(percent < 10, "black", "white"),
          hjust = if_else(color == "white", 1.5, -0.5)
        ) |>
        ggplot(aes(teaching_experience, percent)) +
        geom_col(aes(fill = percent)) +
        geom_text(
          aes(
            label = paste0(percent, "%"),
            color = color, hjust = hjust
          ),
          fontface = "bold"
        ) +
        coord_flip() +
        labs(title = glue::glue("Years of Teaching Experience (n = {format(n, big.mark = ',')})"), x = "", y = "") +
        scale_color_manual(values = c("black", "white")) +
        scale_fill_continuous() +
        scale_y_continuous(
          labels = scales::percent_format(scale = 1),
          breaks = scales::pretty_breaks(n = 6)
        ) +
        scale_x_discrete(labels = c("Less than 1", "0-9", "10-19", "20-29", "30-39", "40-49", "50+")) +
        ggplot2::theme(
          axis.text = ggtext::element_markdown(size = 20),
          axis.title = ggtext::element_markdown(size = 24)
        ) +
        TeachingLab::theme_tl()
      
      teaching_experience_plot
    } else {
      TeachingLab:::no_data_plot
    }
    
  })
  
  output$content_area_plot <- renderPlot({
    
    educator_survey_filtered() |>
      select(content_area) |>
      drop_na() |>
      gt_percent_n(
        column = "content_area",
        custom_column_name = "Content Area", viz_type = sample(c("waffle", "pie", "treemap"),
                                                               size = 1
        )
      )
    
  })
  
  output$grades_taught_plot <- renderPlot({
    
    if (nrow(educator_survey_filtered()) >= 1) {
      grades <- c(map_chr(1:14, ~ paste0("grade_level_", .x)))
      
      n <- educator_survey_filtered() |>
        select(all_of(grades)) |>
        janitor::remove_empty("rows") |>
        nrow()
      
      grades_plot <- educator_survey_filtered() |>
        select(all_of(grades)) |>
        mutate(across(everything(), ~ as.character(.x))) |>
        pivot_longer(everything()) |>
        tidyr::drop_na(value) |>
        group_by(value) |>
        count(sort = T) |>
        ungroup() |>
        mutate(
          percent = round(100 * n / sum(n), 1),
          value = factor(value, levels = c(
            "K", "1", "2", "3", "4", "5",
            "6", "7", "8", "9", "10", "11",
            "12", "Other"
          ))
        ) |>
        ggplot(aes(value, percent)) +
        geom_col(aes(fill = percent)) +
        geom_text(
          aes(
            label = paste0(percent, "%"),
            vjust = -0.5
          ),
          fontface = "bold"
        ) +
        labs(
          y = "", x = "Grade",
          title = glue::glue("Grades Taught (n = {format(n, big.mark = ',')})"),
          subtitle = "<i>More than one could be selected.</i>"
        ) +
        scale_color_manual(values = c("black", "white")) +
        scale_fill_continuous() +
        scale_y_continuous(
          labels = scales::percent_format(scale = 1),
          breaks = scales::pretty_breaks(n = 6)
        ) +
        theme_tl() +
        theme(
          axis.text = element_markdown(size = 20),
          axis.title = element_markdown(size = 24),
          plot.subtitle = element_markdown()
        )
      
      print(grades_plot)
    } else {
      TeachingLab:::no_data_plot
    }
    
  })
  
  output$tl_pl_plot <- renderPlot({
    
      tl_pl_gt <- gt_percent_n(
        df = educator_survey_filtered(),
        column = "tl_pl_participation",
        custom_column_name = "Previous Experience with Teaching Lab", viz_type = sample(c("waffle", "pie", "treemap"),
                                                                                        size = 1
        )
      )
      
      tl_pl_gt
    
    
  })
  
  output$roles_plot_1 <- renderPlot({
    
    roles_gt_1 <- gt_percent_n(
      df = educator_survey_filtered(),
      column = "role",
      custom_column_name = "Primary Role",
      viz_type = sample(c("waffle", "treemap"),
                        size = 1
      )
    )
    
    roles_gt_1
    
  })
  
  output$roles_plot_2 <- renderPlot({
    
    roles_gt_2 <- gt_percent_n(
      df = educator_survey_filtered(),
      column = "lab_leader",
      custom_column_name = "Primary Role",
      viz_type = sample(c("waffle", "treemap"),
                        size = 1
      )
    )
    
    roles_gt_2
    
  })
  
  ########################################## End Section 1 #################################################################
  
  ########################################## Section 2 #################################################################
  
  output$session_plot_1 <- renderPlot({
    
    TeachingLab::session_feedback_graph(data = session_survey_filtered(),
                                        race_filter = input$raceSelect, 
                                        content_area_filter = "All")
    
  })
  
  output$session_summary_sentences <- renderUI({
    
    session_summary_bullets(session_survey_filtered())
    
  })
  
  output$course_plot_1 <- renderPlot({
    
    TeachingLab::course_feedback_graph(data = course_survey_filtered(),
                                        race_filter = input$raceSelect, 
                                        content_area_filter = "All")
    
  })
  
  output$course_summary_sentences <- renderUI({
    
    course_summary_bullets(course_survey_filtered())
    
  })
  
  output$ongoing_coach_plot_1 <- renderPlot({
    
    TeachingLab::ongoing_coaching_feedback_graph(data = ongoing_coach_survey_filtered(),
                                        race_filter = input$raceSelect, 
                                        content_area_filter = "All")
    
  })
  
  output$ongoing_coach_summary_sentences <- renderUI({
    
    session_summary_bullets(ongoing_coach_survey_filtered())
    
  })
  
  output$end_coach_plot_1 <- renderPlot({
    
    TeachingLab::end_coaching_feedback_graph(data = end_coach_survey_filtered(),
                                        race_filter = input$raceSelect, 
                                        content_area_filter = "All")
    
  })
  
  output$end_coach_summary_sentences <- renderUI({
    
    course_summary_bullets(end_coach_survey_filtered())
    
  })
  
  
  ########################################## End Section 2 #################################################################
}
