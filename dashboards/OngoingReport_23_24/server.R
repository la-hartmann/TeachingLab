# Define server logic
server <- function(input, output) {
  # Series of reactives to filter the data by site and subsite #

  educator_survey_filtered <- reactive({
    
    df <- educator_survey |>
      tlShiny::neg_cond_filter(if_not_this = "All Sites", filter_this = input$siteSelect, dat_filter = site) |>
      tlShiny::neg_cond_filter(if_not_this = "All Races", filter_this = input$raceSelect, dat_filter = race) |>
      tlShiny::neg_cond_filter(if_not_this = "All Content Areas", filter_this = input$contentAreaSelect, dat_filter = content_area)

    df
  })

  followup_educator_survey_filtered <- reactive({
    df <- followup_educator |>
      tlShiny::neg_cond_filter(if_not_this = "All Sites", filter_this = input$siteSelect, dat_filter = site) |>
      tlShiny::neg_cond_filter(if_not_this = "All Races", filter_this = input$raceSelect, dat_filter = race) |>
      tlShiny::neg_cond_filter(if_not_this = "All Content Areas", filter_this = input$contentAreaSelect, dat_filter = content_area)

    df
  })

  session_survey_filtered <- reactive({
    df <- session_survey |>
      tlShiny::neg_cond_filter(if_not_this = "All Sites", filter_this = input$siteSelect, dat_filter = site) |>
      tlShiny::neg_cond_filter(if_not_this = "All Races", filter_this = input$raceSelect, dat_filter = race) |>
      tlShiny::neg_cond_filter(if_not_this = "All Content Areas", filter_this = input$contentAreaSelect, dat_filter = content_area)

    df
  })

  course_survey_filtered <- reactive({
    df <- course_survey |>
      tlShiny::neg_cond_filter(if_not_this = "All Sites", filter_this = input$siteSelect, dat_filter = site) |>
      tlShiny::neg_cond_filter(if_not_this = "All Races", filter_this = input$raceSelect, dat_filter = race) |>
      tlShiny::neg_cond_filter(if_not_this = "All Content Areas", filter_this = input$contentAreaSelect, dat_filter = content_area)

    df
  })

  ongoing_coach_survey_filtered <- reactive({
    df <- ongoing_coaching |>
      tlShiny::neg_cond_filter(if_not_this = "All Sites", filter_this = input$siteSelect, dat_filter = site) |>
      tlShiny::neg_cond_filter(if_not_this = "All Races", filter_this = input$raceSelect, dat_filter = race) |>
      tlShiny::neg_cond_filter(if_not_this = "All Content Areas", filter_this = input$contentAreaSelect, dat_filter = content_area)

    df
  })

  end_coach_survey_filtered <- reactive({
    df <- end_coaching |>
      tlShiny::neg_cond_filter(if_not_this = "All Sites", filter_this = input$siteSelect, dat_filter = site) |>
      tlShiny::neg_cond_filter(if_not_this = "All Races", filter_this = input$raceSelect, dat_filter = race) |>
      tlShiny::neg_cond_filter(if_not_this = "All Content Areas", filter_this = input$contentAreaSelect, dat_filter = content_area)

    df
  })

  knowledge_assessments_filtered <- reactive({
    df <- knowledge_assessments |>
      tlShiny::neg_cond_filter(if_not_this = "All Sites", filter_this = input$siteSelect, dat_filter = site)

    df
  })

  ipg_forms_filtered <- reactive({
    df <- ipg_forms |>
      tlShiny::neg_cond_filter(if_not_this = "All Sites", filter_this = input$siteSelect, dat_filter = site) |>
      tlShiny::neg_cond_filter(if_not_this = "All Content Areas", filter_this = input$contentAreaSelect, dat_filter = content_area) |>
      tlShiny::neg_cond_filter(if_not_this = "All Rubrics", filter_this = input$ipgFormsSelect, dat_filter = ipg_rubric)

    df
  })

  student_work_filtered <- reactive({
    df <- student_work #|>
    # tlShiny::neg_cond_filter(if_not_this = "All Sites", filter_this = input$siteSelect, dat_filter = site) |>
    # tlShiny::neg_cond_filter(if_not_this = "All Content Areas", filter_this = input$contentAreaSelect, dat_filter = content_area)
    # NOTE RACE FILTER HERE IS IMPOSSIBLE #

    df
  })

  student_survey_filtered <- reactive({
    df <- student_survey |>
    tlShiny::neg_cond_filter(if_not_this = "All Sites", filter_this = input$siteSelect, dat_filter = site) |>
    tlShiny::neg_cond_filter(if_not_this = "All Races", filter_this = input$raceSelect, dat_filter = race)

    df
  })

  contact_lead_filtered <- reactive({
    tibble::tibble() # For now
  })

  final_sec_intro <- reactive({
    counts <- list()

    counts$intro <- paste0("Teaching Lab collects a variety of data to understand the impact of its PL for educators and students. Please see this <a href = 'https://docs.google.com/document/d/19yDyFCx1KeSPHlD9SLvLfG76FRGdfChL1hnCo_dV1zQ/edit'>resource</a> for a visual overview of our evaluation framework.<br>Additionally, the <a href = 'https://docs.google.com/document/d/19yDyFCx1KeSPHlD9SLvLfG76FRGdfChL1hnCo_dV1zQ/edit'>narrative</a> of our evaluation plan provides more details.<br><br>To date (", Sys.Date() |> format("%b %d, %Y"), "), Teaching Lab has collected the following data during SY23-24:")

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
    if (sum(
      nrow(session_survey_filtered()), nrow(course_survey_filtered()),
      nrow(ongoing_coach_survey_filtered()), nrow(end_coach_survey_filtered())
    ) == 0) {
      sec2_intro <- NA
    }
    if (nrow(knowledge_assessments_filtered()) >= 1) {
      sec3_intro <- "Participant Knowledge"
      counts$p5 <- paste0("• ", nrow(knowledge_assessments_filtered() |> dplyr::filter(prepost == "pre") |> dplyr::distinct(id)), " complete responses to the first Knowledge or Self-Reported Practices Assessments and ", knowledge_assessments_filtered() |> dplyr::filter(prepost == "post") |> dplyr::distinct(id) |> length(), " to the second Knowledge or Self-Reported Practices Assessments;")
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
    final_sec_intro <- paste0(
      "In this dashboard we have compiled the results in various sections:<br>",
      final_sec_intro, "."
    )

    list(counts = counts, final_sec_intro = final_sec_intro)
  })

  ########################################################### Intro #########################################################

  output$counts <- renderUI({
    # counts <- final_sec_intro()[grep('p', names(final_sec_intro()))]
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
    gender_plot <- tlShiny::gt_percent_n(
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
        dplyr::transmute(
          race = paste(race_1, race_2, race_3, race_4, race_5, race_6, race_7, sep = ", "),
          race = stringr::str_remove_all(race, "NA, |, NA")
        ) |>
        dplyr::mutate(race = forcats::fct_lump(factor(race), 3)) |>
        dplyr::rename(`Race Identity` = race) |>
        tlShiny::gt_percent_n(
          column = "Race Identity",
          custom_column_name = "Racial Identity",
          viz_type = sample(c("pie", "waffle", "treemap"), size = 1)
        )

      race_plot
    } else {
      tlShiny:::no_data_plot_filters
    }
  })

  output$ethnicity_plot <- renderPlot({
    ethnicity_gt <- tlShiny::gt_percent_n(
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
        tlShiny::theme_tl()

      teaching_experience_plot
    } else {
      tlShiny:::no_data_plot_filters
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

      grades_plot
    } else {
      tlShiny:::no_data_plot_filters
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
    TeachingLab::session_feedback_graph(
      data = session_survey_filtered()
    )
  })

  output$session_summary_sentences <- renderUI({
    session_summary_bullets(session_survey_filtered())
  })

  output$course_plot_1 <- renderPlot({
    TeachingLab::course_feedback_graph(
      data = course_survey_filtered()
    )
  })

  output$course_summary_sentences <- renderUI({
    course_summary_bullets(course_survey_filtered())
  })

  output$ongoing_coach_plot_1 <- renderPlot({
    TeachingLab::ongoing_coaching_feedback_graph(
      data = ongoing_coach_survey_filtered()
    )
  })

  output$ongoing_coach_summary_sentences <- renderUI({
    session_summary_bullets(ongoing_coach_survey_filtered())
  })

  output$end_coach_plot_1 <- renderPlot({
    TeachingLab::end_coaching_feedback_graph(
      data = end_coach_survey_filtered()
    )
  })

  output$end_coach_summary_sentences <- renderUI({
    course_summary_bullets(end_coach_survey_filtered())
  })

  output$contact_lead_plot_1 <- renderPlot({
    tlShiny:::no_data_plot_filters
  })


  ########################################## End Section 2 #################################################################

  ########################################## Section 3 #################################################################

  output$knowledge_assess_summary <- renderPlot({
    if (nrow(knowledge_assessments_filtered()) >= 1) {
      knowledge_assessments_filtered() |>
        dplyr::filter(question1 == "Score") |>
        dplyr::mutate(percent = 100 * score / max_score) |>
        dplyr::group_by(prepost, know_assess) |>
        dplyr::summarise(
          percent = round(mean(percent, na.rm = T), 2),
          n = dplyr::n()
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          prepost = ifelse(prepost == "pre",
            "Before",
            "After"
          ),
          prepost = factor(prepost, levels = c("Before", "After")),
          know_assess = paste0(know_assess, " % Correct")
        ) |>
        ggplot(aes(x = prepost, y = percent, fill = prepost)) +
        geom_col() +
        geom_text(aes(label = paste0(percent, "% (n = ", n, ")"), color = prepost), vjust = -0.45, fontface = "bold", size = 11) +
        facet_wrap(~know_assess, labeller = label_wrap_gen(width = 50)) +
        labs(title = NULL) +
        scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
        scale_fill_manual(values = c("#040404", "#04abeb")) +
        scale_color_manual(values = c("#040404", "#04abeb")) +
        theme_tl() +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 20),
          strip.text = element_text(hjust = 0.5, face = "bold", size = 30)
        )
    } else {
      tlShiny:::no_data_plot_filters
    }
  })

  ########################################## End Section 3 #################################################################

  ########################################## Section 4 ####################################################################

  output$mindsets_table_1 <- render_gt({
    n_size_1 <- sum(!is.na(educator_survey_filtered()$`mindsets_ts_1_1`))
    n_size_2 <- sum(!is.na(followup_educator_survey_filtered()$`mindsets_ts_1_1`))

    if (n_size_1 >= 1 & n_size_2 >= 1) {
      educator_survey_filtered() |>
        dplyr::bind_rows(followup_educator_survey_filtered()) |>
        dplyr::select(
          prepost, mindsets_ts_1_1, mindsets_ts_1_4, mindsets_ts_1_5, mindsets_ts_1_6, mindsets_ts_1_7,
          mindsets_ts_1_11, mindsets_ts_1_13, mindsets_ts_1_14, mindsets_ts_1_15, mindsets_ts_1_16,
          mindsets_ts_1_17, mindsets_ts_1_18
        ) |>
        tidyr::pivot_longer(!prepost, names_to = "name", values_to = "value") |>
        tidyr::drop_na(value) |>
        dplyr::group_by(name, value, prepost) |>
        dplyr::count(sort = T) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          pos_neg = dplyr::case_when(
            name %in% c(
              "mindsets_ts_1_1", "mindsets_ts_1_17", "mindsets_ts_1_18", "mindsets_ts_1_4", "mindsets_ts_1_5", "mindsets_ts_1_6",
              "mindsets_ts_1_11", "mindsets_ts_1_13", "mindsets_ts_1_14", "mindsets_ts_1_15"
            ) ~ "negative",
            TRUE ~ "positive"
          ),
          ` ` = dplyr::case_when(
            name %in% c("mindsets_ts_1_1", "mindsets_ts_1_16", "mindsets_ts_1_17", "mindsets_ts_1_18") ~ "Recognition of Race & Culture",
            name %in% c("mindsets_ts_1_4", "mindsets_ts_1_5", "mindsets_ts_1_6", "mindsets_ts_1_7", "mindsets_ts_1_11") ~ "High expectations",
            name %in% c("mindsets_ts_1_15", "mindsets_ts_1_14", "mindsets_ts_1_13") ~ "Growth mindsets"
          ),
          score_multiplier = dplyr::case_when(
            value == "1 - Strongly disagree" & pos_neg == "negative" ~ 1,
            value == "1 - Strongly disagree" & pos_neg == "positive" ~ 0,
            value == "2 - Disagree" & pos_neg == "negative" ~ 0.75,
            value == "2 - Disagree" & pos_neg == "positive" ~ 0.25,
            value == "3 - Neither agree nor disagree" & pos_neg == "negative" ~ 0.5,
            value == "3 - Neither agree nor disagree" & pos_neg == "positive" ~ 0.5,
            value == "4 - Agree" & pos_neg == "negative" ~ 0.25,
            value == "4 - Agree" & pos_neg == "positive" ~ 0.75,
            value == "5 - Strongly agree" & pos_neg == "negative" ~ 0,
            value == "5 - Strongly agree" & pos_neg == "positive" ~ 1
          ),
          name = stringr::str_replace_all(name, c(
            "mindsets_ts_1_4" = "I try to keep in mind the limits of my students’ ability and give them assignments that I know they can do so that they do not become discouraged",
            "mindsets_ts_1_5" = "Before students are asked to engage in complex learning tasks, they need to have a solid grasp of basic skills",
            "mindsets_ts_1_6" = "It is not fair to ask students who are struggling with English to take on challenging academic assignments",
            "mindsets_ts_1_7" = "Teachers should provide all students the opportunity to work with grade-level texts and tasks",
            "mindsets_ts_1_11" = "Grouping students of different levels of achievement for instruction may benefit some students, but it can undermine the progress that could otherwise be made by higher achieving students",
            "mindsets_ts_1_13" = "Students have a certain amount of intelligence, and they can’t really do much to change it",
            "mindsets_ts_1_14" = "Intelligence is something about students that they can’t change very much",
            "mindsets_ts_1_15" = "Students can learn new things, but they can’t really change their basic intelligence",
            "mindsets_ts_1_16" = "Every student who works hard, no matter what race they are, has an equal chance to be successful academically",
            "mindsets_ts_1_17" = "Racism against students from racial and ethnic minority backgrounds may have been a problem in the past, but it is not a problem today",
            "mindsets_ts_1_18" = "Students from racial and ethnic minority backgrounds have the same opportunities as White students",
            "mindsets_ts_1_1" = "I am color blind when it comes to my teaching - I don’t think of my students in terms of their race or ethnicity"
          )),
          name = dplyr::if_else(pos_neg == "negative", paste0("<p style='color:red;'>", name, "</p>"), name)
        ) |>
        dplyr::group_by(name, prepost) |>
        dplyr::mutate(score = (n * score_multiplier) / sum(n)) |>
        dplyr::summarise(
          score = sum(score),
          ` ` = min(` `)
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(` `, prepost) |>
        dplyr::summarise(score = mean(score)) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = prepost, values_from = score) |>
        dplyr::relocate(Post, .after = Pre) |>
        (\(.) dplyr::add_row(., ` ` = "<b>Overall score</b>", !!!colMeans(.[2]), !!!colMeans(.[3]), .before = 1))() |>
        gt::gt() |>
        gt::fmt_percent(c(Pre, Post),
          scale_values = TRUE
        ) |>
        gt::fmt_markdown(` `) |>
        gt::data_color(
          columns = c(Pre, Post),
          fn = scales::col_bin(
            palette = c(
              tlShiny::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
            ),
            domain = c(0, 1),
            bins = c(0, 0.39, 0.79, 1)
          )
        ) |>
        tlShiny::gt_theme_tl(align = "left") |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_1), locations = cells_column_labels("Pre")) |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_2), locations = cells_column_labels("Post"))
    } else if (n_size_1 >= 1 & n_size_2 == 0) {
      educator_survey_filtered() |>
        dplyr::select(
          prepost, mindsets_ts_1_1, mindsets_ts_1_4, mindsets_ts_1_5, mindsets_ts_1_6, mindsets_ts_1_7,
          mindsets_ts_1_11, mindsets_ts_1_13, mindsets_ts_1_14, mindsets_ts_1_15, mindsets_ts_1_16,
          mindsets_ts_1_17, mindsets_ts_1_18
        ) |>
        tidyr::pivot_longer(!prepost, names_to = "name", values_to = "value") |>
        tidyr::drop_na(value) |>
        dplyr::group_by(name, value, prepost) |>
        dplyr::count(sort = T) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          pos_neg = dplyr::case_when(
            name %in% c(
              "mindsets_ts_1_1", "mindsets_ts_1_17", "mindsets_ts_1_18", "mindsets_ts_1_4", "mindsets_ts_1_5", "mindsets_ts_1_6",
              "mindsets_ts_1_11", "mindsets_ts_1_13", "mindsets_ts_1_14", "mindsets_ts_1_15"
            ) ~ "negative",
            TRUE ~ "positive"
          ),
          ` ` = dplyr::case_when(
            name %in% c("mindsets_ts_1_1", "mindsets_ts_1_16", "mindsets_ts_1_17", "mindsets_ts_1_18") ~ "Recognition of Race & Culture",
            name %in% c("mindsets_ts_1_4", "mindsets_ts_1_5", "mindsets_ts_1_6", "mindsets_ts_1_7", "mindsets_ts_1_11") ~ "High expectations",
            name %in% c("mindsets_ts_1_15", "mindsets_ts_1_14", "mindsets_ts_1_13") ~ "Growth mindsets"
          ),
          score_multiplier = dplyr::case_when(
            value == "1 - Strongly disagree" & pos_neg == "negative" ~ 1,
            value == "1 - Strongly disagree" & pos_neg == "positive" ~ 0,
            value == "2 - Disagree" & pos_neg == "negative" ~ 0.75,
            value == "2 - Disagree" & pos_neg == "positive" ~ 0.25,
            value == "3 - Neither agree nor disagree" & pos_neg == "negative" ~ 0.5,
            value == "3 - Neither agree nor disagree" & pos_neg == "positive" ~ 0.5,
            value == "4 - Agree" & pos_neg == "negative" ~ 0.25,
            value == "4 - Agree" & pos_neg == "positive" ~ 0.75,
            value == "5 - Strongly agree" & pos_neg == "negative" ~ 0,
            value == "5 - Strongly agree" & pos_neg == "positive" ~ 1
          ),
          name = stringr::str_replace_all(name, c(
            "mindsets_ts_1_4" = "I try to keep in mind the limits of my students’ ability and give them assignments that I know they can do so that they do not become discouraged",
            "mindsets_ts_1_5" = "Before students are asked to engage in complex learning tasks, they need to have a solid grasp of basic skills",
            "mindsets_ts_1_6" = "It is not fair to ask students who are struggling with English to take on challenging academic assignments",
            "mindsets_ts_1_7" = "Teachers should provide all students the opportunity to work with grade-level texts and tasks",
            "mindsets_ts_1_11" = "Grouping students of different levels of achievement for instruction may benefit some students, but it can undermine the progress that could otherwise be made by higher achieving students",
            "mindsets_ts_1_13" = "Students have a certain amount of intelligence, and they can’t really do much to change it",
            "mindsets_ts_1_14" = "Intelligence is something about students that they can’t change very much",
            "mindsets_ts_1_15" = "Students can learn new things, but they can’t really change their basic intelligence",
            "mindsets_ts_1_16" = "Every student who works hard, no matter what race they are, has an equal chance to be successful academically",
            "mindsets_ts_1_17" = "Racism against students from racial and ethnic minority backgrounds may have been a problem in the past, but it is not a problem today",
            "mindsets_ts_1_18" = "Students from racial and ethnic minority backgrounds have the same opportunities as White students",
            "mindsets_ts_1_1" = "I am color blind when it comes to my teaching - I don’t think of my students in terms of their race or ethnicity"
          )),
          name = dplyr::if_else(pos_neg == "negative", paste0("<p style='color:red;'>", name, "</p>"), name)
        ) |>
        dplyr::group_by(name, prepost) |>
        dplyr::mutate(score = (n * score_multiplier) / sum(n)) |>
        dplyr::summarise(
          score = sum(score),
          ` ` = min(` `)
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(` `, prepost) |>
        dplyr::summarise(score = mean(score)) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = prepost, values_from = score) |>
        dplyr::mutate(Post = NA) |>
        dplyr::relocate(Post, .after = Pre) |>
        (\(.) dplyr::add_row(., ` ` = "<b>Overall score</b>", !!!colMeans(.[2]), !!!colMeans(.[3]), .before = 1))() |>
        gt::gt() |>
        gt::fmt_percent(c(Pre, Post),
          scale_values = TRUE
        ) |>
        gt::fmt_markdown(` `) |>
        gt::data_color(
          columns = c(Pre, Post),
          fn = scales::col_bin(
            palette = c(
              tlShiny::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
            ),
            domain = c(0, 1),
            bins = c(0, 0.39, 0.79, 1)
          )
        ) |>
        tlShiny::gt_theme_tl(align = "left") |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_1), locations = cells_column_labels("Pre")) |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_2), locations = cells_column_labels("Post"))
    } else {
      tibble::tibble() |>
        gt::gt() |>
        tab_header(title = md("**There is no data for this set of filters yet.**"))
    }
  })

  output$mindsets_table_2 <- render_gt({
    n_size_1 <- sum(!is.na(educator_survey_filtered()$`mindsets_ts_1_1`))
    n_size_2 <- sum(!is.na(followup_educator_survey_filtered()$`mindsets_ts_1_1`))

    if (n_size_2 >= 1 & n_size_1 >= 1) {
      educator_survey_filtered() |>
        dplyr::bind_rows(followup_educator_survey_filtered()) |>
        dplyr::select(
          prepost, mindsets_ts_1_1, mindsets_ts_1_16,
          mindsets_ts_1_17, mindsets_ts_1_18
        ) |>
        tidyr::pivot_longer(!prepost, names_to = " ", values_to = "value") |>
        tidyr::drop_na(value) |>
        dplyr::group_by(` `, value, prepost) |>
        dplyr::count(sort = T) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          pos_neg = dplyr::case_when(
            ` ` %in% c(
              "mindsets_ts_1_1", "mindsets_ts_1_17", "mindsets_ts_1_18", "mindsets_ts_1_4", "mindsets_ts_1_5", "mindsets_ts_1_6",
              "mindsets_ts_1_11", "mindsets_ts_1_13", "mindsets_ts_1_14", "mindsets_ts_1_15"
            ) ~ "negative",
            TRUE ~ "positive"
          ),
          score_multiplier = dplyr::case_when(
            value == "1 - Strongly disagree" & pos_neg == "negative" ~ 1,
            value == "1 - Strongly disagree" & pos_neg == "positive" ~ 0,
            value == "2 - Disagree" & pos_neg == "negative" ~ 0.75,
            value == "2 - Disagree" & pos_neg == "positive" ~ 0.25,
            value == "3 - Neither agree nor disagree" & pos_neg == "negative" ~ 0.5,
            value == "3 - Neither agree nor disagree" & pos_neg == "positive" ~ 0.5,
            value == "4 - Agree" & pos_neg == "negative" ~ 0.25,
            value == "4 - Agree" & pos_neg == "positive" ~ 0.75,
            value == "5 - Strongly agree" & pos_neg == "negative" ~ 0,
            value == "5 - Strongly agree" & pos_neg == "positive" ~ 1
          ),
          ` ` = stringr::str_replace_all(` `, c(
            "mindsets_ts_1_16" = "Every student who works hard, no matter what race they are, has an equal chance to be successful academically",
            "mindsets_ts_1_17" = "Racism against students from racial and ethnic minority backgrounds may have been a problem in the past, but it is not a problem today",
            "mindsets_ts_1_18" = "Students from racial and ethnic minority backgrounds have the same opportunities as White students",
            "mindsets_ts_1_1" = "I am color blind when it comes to my teaching - I don’t think of my students in terms of their race or ethnicity"
          )),
          ` ` = dplyr::if_else(pos_neg == "negative", paste0("<p style='color:red;'>", ` `, "</p>"), ` `)
        ) |>
        dplyr::group_by(` `, prepost) |>
        dplyr::mutate(score = (n * score_multiplier) / sum(n)) |>
        dplyr::summarise(
          score = sum(score)
        ) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = prepost, values_from = score) |>
        dplyr::relocate(Post, .after = Pre) |>
        (\(.) dplyr::add_row(., ` ` = "<b>Overall score</b>", !!!colMeans(.[2]), !!!colMeans(.[3]), .before = 1))() |>
        gt::gt() |>
        gt::fmt_percent(c(Pre, Post),
          scale_values = TRUE
        ) |>
        gt::fmt_markdown(` `) |>
        gt::data_color(
          columns = c(Pre, Post),
          fn = scales::col_bin(
            palette = c(
              tlShiny::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
            ),
            domain = c(0, 1),
            bins = c(0, 0.39, 0.79, 1)
          )
        ) |>
        tlShiny::gt_theme_tl(align = "left") |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_1), locations = cells_column_labels("Pre")) |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_2), locations = cells_column_labels("Post"))
    } else if (n_size_1 >= 1 & n_size_2 == 0) {
      educator_survey_filtered() |>
        dplyr::select(
          prepost, mindsets_ts_1_1, mindsets_ts_1_16,
          mindsets_ts_1_17, mindsets_ts_1_18
        ) |>
        tidyr::pivot_longer(!prepost, names_to = " ", values_to = "value") |>
        tidyr::drop_na(value) |>
        dplyr::group_by(` `, value, prepost) |>
        dplyr::count(sort = T) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          pos_neg = dplyr::case_when(
            ` ` %in% c(
              "mindsets_ts_1_1", "mindsets_ts_1_17", "mindsets_ts_1_18", "mindsets_ts_1_4", "mindsets_ts_1_5", "mindsets_ts_1_6",
              "mindsets_ts_1_11", "mindsets_ts_1_13", "mindsets_ts_1_14", "mindsets_ts_1_15"
            ) ~ "negative",
            TRUE ~ "positive"
          ),
          score_multiplier = dplyr::case_when(
            value == "1 - Strongly disagree" & pos_neg == "negative" ~ 1,
            value == "1 - Strongly disagree" & pos_neg == "positive" ~ 0,
            value == "2 - Disagree" & pos_neg == "negative" ~ 0.75,
            value == "2 - Disagree" & pos_neg == "positive" ~ 0.25,
            value == "3 - Neither agree nor disagree" & pos_neg == "negative" ~ 0.5,
            value == "3 - Neither agree nor disagree" & pos_neg == "positive" ~ 0.5,
            value == "4 - Agree" & pos_neg == "negative" ~ 0.25,
            value == "4 - Agree" & pos_neg == "positive" ~ 0.75,
            value == "5 - Strongly agree" & pos_neg == "negative" ~ 0,
            value == "5 - Strongly agree" & pos_neg == "positive" ~ 1
          ),
          ` ` = stringr::str_replace_all(` `, c(
            "mindsets_ts_1_16" = "Every student who works hard, no matter what race they are, has an equal chance to be successful academically",
            "mindsets_ts_1_17" = "Racism against students from racial and ethnic minority backgrounds may have been a problem in the past, but it is not a problem today",
            "mindsets_ts_1_18" = "Students from racial and ethnic minority backgrounds have the same opportunities as White students",
            "mindsets_ts_1_1" = "I am color blind when it comes to my teaching - I don’t think of my students in terms of their race or ethnicity"
          )),
          ` ` = dplyr::if_else(pos_neg == "negative", paste0("<p style='color:red;'>", ` `, "</p>"), ` `)
        ) |>
        dplyr::group_by(` `, prepost) |>
        dplyr::mutate(score = (n * score_multiplier) / sum(n)) |>
        dplyr::summarise(
          score = sum(score)
        ) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = prepost, values_from = score) |>
        dplyr::mutate(Post = NA) |>
        dplyr::relocate(Post, .after = Pre) |>
        (\(.) dplyr::add_row(., ` ` = "<b>Overall score</b>", !!!colMeans(.[2]), !!!colMeans(.[3]), .before = 1))() |>
        gt::gt() |>
        gt::fmt_percent(c(Pre, Post),
          scale_values = TRUE
        ) |>
        gt::fmt_markdown(` `) |>
        gt::data_color(
          columns = c(Pre, Post),
          fn = scales::col_bin(
            palette = c(
              tlShiny::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
            ),
            domain = c(0, 1),
            bins = c(0, 0.39, 0.79, 1)
          )
        ) |>
        tlShiny::gt_theme_tl(align = "left") |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_1), locations = cells_column_labels("Pre")) |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_2), locations = cells_column_labels("Post"))
    } else {
      tlShiny:::no_data_plot_filters
    }
  })

  output$mindsets_table_3 <- render_gt({
    n_size_1 <- sum(!is.na(educator_survey_filtered()$`mindsets_ts_1_1`))
    n_size_2 <- sum(!is.na(followup_educator_survey_filtered()$`mindsets_ts_1_1`))

    if (n_size_2 >= 1 & n_size_1 >= 1) {
      educator_survey_filtered() |>
        dplyr::bind_rows(followup_educator_survey_filtered()) |>
        dplyr::select(
          prepost, mindsets_ts_1_4, mindsets_ts_1_5,
          mindsets_ts_1_6, mindsets_ts_1_7, mindsets_ts_1_11
        ) |>
        tidyr::pivot_longer(!prepost, names_to = " ", values_to = "value") |>
        tidyr::drop_na(value) |>
        dplyr::group_by(` `, value, prepost) |>
        dplyr::count(sort = T) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          pos_neg = dplyr::case_when(
            ` ` %in% c(
              "mindsets_ts_1_1", "mindsets_ts_1_17", "mindsets_ts_1_18", "mindsets_ts_1_4", "mindsets_ts_1_5", "mindsets_ts_1_6",
              "mindsets_ts_1_11", "mindsets_ts_1_13", "mindsets_ts_1_14", "mindsets_ts_1_15"
            ) ~ "negative",
            TRUE ~ "positive"
          ),
          score_multiplier = dplyr::case_when(
            value == "1 - Strongly disagree" & pos_neg == "negative" ~ 1,
            value == "1 - Strongly disagree" & pos_neg == "positive" ~ 0,
            value == "2 - Disagree" & pos_neg == "negative" ~ 0.75,
            value == "2 - Disagree" & pos_neg == "positive" ~ 0.25,
            value == "3 - Neither agree nor disagree" & pos_neg == "negative" ~ 0.5,
            value == "3 - Neither agree nor disagree" & pos_neg == "positive" ~ 0.5,
            value == "4 - Agree" & pos_neg == "negative" ~ 0.25,
            value == "4 - Agree" & pos_neg == "positive" ~ 0.75,
            value == "5 - Strongly agree" & pos_neg == "negative" ~ 0,
            value == "5 - Strongly agree" & pos_neg == "positive" ~ 1
          ),
          ` ` = stringr::str_replace_all(` `, c(
            "mindsets_ts_1_4" = "I try to keep in mind the limits of my students’ ability and give them assignments that I know they can do so that they do not become discouraged",
            "mindsets_ts_1_5" = "Before students are asked to engage in complex learning tasks, they need to have a solid grasp of basic skills",
            "mindsets_ts_1_6" = "It is not fair to ask students who are struggling with English to take on challenging academic assignments",
            "mindsets_ts_1_7" = "Teachers should provide all students the opportunity to work with grade-level texts and tasks",
            "mindsets_ts_1_11" = "Grouping students of different levels of achievement for instruction may benefit some students, but it can undermine the progress that could otherwise be made by higher achieving students"
          )),
          ` ` = dplyr::if_else(pos_neg == "negative", paste0("<p style='color:red;'>", ` `, "</p>"), ` `)
        ) |>
        dplyr::group_by(` `, prepost) |>
        dplyr::mutate(score = (n * score_multiplier) / sum(n)) |>
        dplyr::summarise(
          score = sum(score)
        ) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = prepost, values_from = score) |>
        dplyr::relocate(Post, .after = Pre) |>
        (\(.) dplyr::add_row(., ` ` = "<b>Overall score</b>", !!!colMeans(.[2]), !!!colMeans(.[3]), .before = 1))() |>
        gt::gt() |>
        gt::fmt_percent(c(Pre, Post),
          scale_values = TRUE
        ) |>
        gt::fmt_markdown(` `) |>
        gt::data_color(
          columns = c(Pre, Post),
          fn = scales::col_bin(
            palette = c(
              tlShiny::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
            ),
            domain = c(0, 1),
            bins = c(0, 0.39, 0.79, 1)
          )
        ) |>
        tlShiny::gt_theme_tl(align = "left") |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_1), locations = cells_column_labels("Pre")) |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_2), locations = cells_column_labels("Post"))
    } else if (n_size_1 >= 1 & n_size_2 == 0) {
      educator_survey_filtered() |>
        dplyr::select(
          prepost, mindsets_ts_1_4, mindsets_ts_1_5,
          mindsets_ts_1_6, mindsets_ts_1_7, mindsets_ts_1_11
        ) |>
        tidyr::pivot_longer(!prepost, names_to = " ", values_to = "value") |>
        tidyr::drop_na(value) |>
        dplyr::group_by(` `, value, prepost) |>
        dplyr::count(sort = T) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          pos_neg = dplyr::case_when(
            ` ` %in% c(
              "mindsets_ts_1_1", "mindsets_ts_1_17", "mindsets_ts_1_18", "mindsets_ts_1_4", "mindsets_ts_1_5", "mindsets_ts_1_6",
              "mindsets_ts_1_11", "mindsets_ts_1_13", "mindsets_ts_1_14", "mindsets_ts_1_15"
            ) ~ "negative",
            TRUE ~ "positive"
          ),
          score_multiplier = dplyr::case_when(
            value == "1 - Strongly disagree" & pos_neg == "negative" ~ 1,
            value == "1 - Strongly disagree" & pos_neg == "positive" ~ 0,
            value == "2 - Disagree" & pos_neg == "negative" ~ 0.75,
            value == "2 - Disagree" & pos_neg == "positive" ~ 0.25,
            value == "3 - Neither agree nor disagree" & pos_neg == "negative" ~ 0.5,
            value == "3 - Neither agree nor disagree" & pos_neg == "positive" ~ 0.5,
            value == "4 - Agree" & pos_neg == "negative" ~ 0.25,
            value == "4 - Agree" & pos_neg == "positive" ~ 0.75,
            value == "5 - Strongly agree" & pos_neg == "negative" ~ 0,
            value == "5 - Strongly agree" & pos_neg == "positive" ~ 1
          ),
          ` ` = stringr::str_replace_all(` `, c(
            "mindsets_ts_1_4" = "I try to keep in mind the limits of my students’ ability and give them assignments that I know they can do so that they do not become discouraged",
            "mindsets_ts_1_5" = "Before students are asked to engage in complex learning tasks, they need to have a solid grasp of basic skills",
            "mindsets_ts_1_6" = "It is not fair to ask students who are struggling with English to take on challenging academic assignments",
            "mindsets_ts_1_7" = "Teachers should provide all students the opportunity to work with grade-level texts and tasks",
            "mindsets_ts_1_11" = "Grouping students of different levels of achievement for instruction may benefit some students, but it can undermine the progress that could otherwise be made by higher achieving students"
          )),
          ` ` = dplyr::if_else(pos_neg == "negative", paste0("<p style='color:red;'>", ` `, "</p>"), ` `)
        ) |>
        dplyr::group_by(` `, prepost) |>
        dplyr::mutate(score = (n * score_multiplier) / sum(n)) |>
        dplyr::summarise(
          score = sum(score)
        ) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = prepost, values_from = score) |>
        dplyr::mutate(Post = NA) |>
        dplyr::relocate(Post, .after = Pre) |>
        (\(.) dplyr::add_row(., ` ` = "<b>Overall score</b>", !!!colMeans(.[2]), !!!colMeans(.[3]), .before = 1))() |>
        gt::gt() |>
        gt::fmt_percent(c(Pre, Post),
          scale_values = TRUE
        ) |>
        gt::fmt_markdown(` `) |>
        gt::data_color(
          columns = c(Pre, Post),
          fn = scales::col_bin(
            palette = c(
              tlShiny::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
            ),
            domain = c(0, 1),
            bins = c(0, 0.39, 0.79, 1)
          )
        ) |>
        tlShiny::gt_theme_tl(align = "left") |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_1), locations = cells_column_labels("Pre")) |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_2), locations = cells_column_labels("Post"))
    } else {
      tlShiny:::no_data_plot_filters
    }
  })

  output$mindsets_table_4 <- render_gt({
    n_size_1 <- sum(!is.na(educator_survey_filtered()$`mindsets_ts_1_1`))
    n_size_2 <- sum(!is.na(followup_educator_survey_filtered()$`mindsets_ts_1_1`))

    if (n_size_2 >= 1 & n_size_1 >= 1) {
      educator_survey_filtered() |>
        dplyr::bind_rows(followup_educator_survey_filtered()) |>
        dplyr::select(
          prepost, mindsets_ts_1_13, mindsets_ts_1_14, mindsets_ts_1_15
        ) |>
        tidyr::pivot_longer(!prepost, names_to = " ", values_to = "value") |>
        tidyr::drop_na(value) |>
        dplyr::group_by(` `, value, prepost) |>
        dplyr::count(sort = T) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          pos_neg = dplyr::case_when(
            ` ` %in% c(
              "mindsets_ts_1_1", "mindsets_ts_1_17", "mindsets_ts_1_18", "mindsets_ts_1_4", "mindsets_ts_1_5", "mindsets_ts_1_6",
              "mindsets_ts_1_11", "mindsets_ts_1_13", "mindsets_ts_1_14", "mindsets_ts_1_15"
            ) ~ "negative",
            TRUE ~ "positive"
          ),
          score_multiplier = dplyr::case_when(
            value == "1 - Strongly disagree" & pos_neg == "negative" ~ 1,
            value == "1 - Strongly disagree" & pos_neg == "positive" ~ 0,
            value == "2 - Disagree" & pos_neg == "negative" ~ 0.75,
            value == "2 - Disagree" & pos_neg == "positive" ~ 0.25,
            value == "3 - Neither agree nor disagree" & pos_neg == "negative" ~ 0.5,
            value == "3 - Neither agree nor disagree" & pos_neg == "positive" ~ 0.5,
            value == "4 - Agree" & pos_neg == "negative" ~ 0.25,
            value == "4 - Agree" & pos_neg == "positive" ~ 0.75,
            value == "5 - Strongly agree" & pos_neg == "negative" ~ 0,
            value == "5 - Strongly agree" & pos_neg == "positive" ~ 1
          ),
          ` ` = stringr::str_replace_all(` `, c(
            "mindsets_ts_1_13" = "Students have a certain amount of intelligence, and they can’t really do much to change it",
            "mindsets_ts_1_14" = "Intelligence is something about students that they can’t change very much",
            "mindsets_ts_1_15" = "Students can learn new things, but they can’t really change their basic intelligence"
          )),
          ` ` = dplyr::if_else(pos_neg == "negative", paste0("<p style='color:red;'>", ` `, "</p>"), ` `)
        ) |>
        dplyr::group_by(` `, prepost) |>
        dplyr::mutate(score = (n * score_multiplier) / sum(n)) |>
        dplyr::summarise(
          score = sum(score)
        ) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = prepost, values_from = score) |>
        dplyr::relocate(Post, .after = Pre) |>
        (\(.) dplyr::add_row(., ` ` = "<b>Overall score</b>", !!!colMeans(.[2]), !!!colMeans(.[3]), .before = 1))() |>
        gt::gt() |>
        gt::fmt_percent(c(Pre, Post),
          scale_values = TRUE
        ) |>
        gt::fmt_markdown(` `) |>
        gt::data_color(
          columns = c(Pre, Post),
          fn = scales::col_bin(
            palette = c(
              tlShiny::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
            ),
            domain = c(0, 1),
            bins = c(0, 0.39, 0.79, 1)
          )
        ) |>
        tlShiny::gt_theme_tl(align = "left") |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_1), locations = cells_column_labels("Pre")) |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_2), locations = cells_column_labels("Post"))
    } else if (n_size_1 >= 1 & n_size_2 == 0) {
      educator_survey_filtered() |>
        dplyr::select(
          prepost, mindsets_ts_1_13, mindsets_ts_1_14, mindsets_ts_1_15
        ) |>
        tidyr::pivot_longer(!prepost, names_to = " ", values_to = "value") |>
        tidyr::drop_na(value) |>
        dplyr::group_by(` `, value, prepost) |>
        dplyr::count(sort = T) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          pos_neg = dplyr::case_when(
            ` ` %in% c(
              "mindsets_ts_1_1", "mindsets_ts_1_17", "mindsets_ts_1_18", "mindsets_ts_1_4", "mindsets_ts_1_5", "mindsets_ts_1_6",
              "mindsets_ts_1_11", "mindsets_ts_1_13", "mindsets_ts_1_14", "mindsets_ts_1_15"
            ) ~ "negative",
            TRUE ~ "positive"
          ),
          score_multiplier = dplyr::case_when(
            value == "1 - Strongly disagree" & pos_neg == "negative" ~ 1,
            value == "1 - Strongly disagree" & pos_neg == "positive" ~ 0,
            value == "2 - Disagree" & pos_neg == "negative" ~ 0.75,
            value == "2 - Disagree" & pos_neg == "positive" ~ 0.25,
            value == "3 - Neither agree nor disagree" & pos_neg == "negative" ~ 0.5,
            value == "3 - Neither agree nor disagree" & pos_neg == "positive" ~ 0.5,
            value == "4 - Agree" & pos_neg == "negative" ~ 0.25,
            value == "4 - Agree" & pos_neg == "positive" ~ 0.75,
            value == "5 - Strongly agree" & pos_neg == "negative" ~ 0,
            value == "5 - Strongly agree" & pos_neg == "positive" ~ 1
          ),
          ` ` = stringr::str_replace_all(` `, c(
            "mindsets_ts_1_13" = "Students have a certain amount of intelligence, and they can’t really do much to change it",
            "mindsets_ts_1_14" = "Intelligence is something about students that they can’t change very much",
            "mindsets_ts_1_15" = "Students can learn new things, but they can’t really change their basic intelligence"
          )),
          ` ` = dplyr::if_else(pos_neg == "negative", paste0("<p style='color:red;'>", ` `, "</p>"), ` `)
        ) |>
        dplyr::group_by(` `, prepost) |>
        dplyr::mutate(score = (n * score_multiplier) / sum(n)) |>
        dplyr::summarise(
          score = sum(score)
        ) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = prepost, values_from = score) |>
        dplyr::mutate(Post = NA) |>
        dplyr::relocate(Post, .after = Pre) |>
        (\(.) dplyr::add_row(., ` ` = "<b>Overall score</b>", !!!colMeans(.[2]), !!!colMeans(.[3]), .before = 1))() |>
        gt::gt() |>
        gt::fmt_percent(c(Pre, Post),
          scale_values = TRUE
        ) |>
        gt::fmt_markdown(` `) |>
        gt::data_color(
          columns = c(Pre, Post),
          fn = scales::col_bin(
            palette = c(
              tlShiny::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
            ),
            domain = c(0, 1),
            bins = c(0, 0.39, 0.79, 1)
          )
        ) |>
        tlShiny::gt_theme_tl(align = "left") |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_1), locations = cells_column_labels("Pre")) |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_2), locations = cells_column_labels("Post"))
    } else {
      tlShiny:::no_data_plot_filters
    }
  })

  output$mindsets_plot_1 <- renderPlot({
    n_size_1 <- sum(!is.na(educator_survey_filtered()$materials_1))
    n_size_2 <- sum(!is.na(followup_educator_survey_filtered()$materials_1))

    if (n_size_1 >= 1) {
      if (n_size_2 >= 1) {
        data <- educator_survey_filtered() |>
          bind_rows(followup_educator_survey_filtered())
      } else {
        data <- educator_survey_filtered()
      }

      data |>
        select(prepost, contains("materials")) |>
        pivot_longer(!prepost, names_to = "name", values_to = "value") |>
        drop_na(value) |>
        mutate(name = str_replace_all(name, c(
          "materials_1" = "curriculum materials adopted by your district",
          "materials_2" = "materials developed by your school or district",
          "materials_3" = "materials you found on the internet",
          "materials_4" = "materials developed by yourself or with colleagues"
        ))) |>
        group_by(name, value, prepost) |>
        count(sort = T) |>
        ungroup() |>
        drop_na(value) |>
        mutate(
          name = html_wrap(name, 25),
          value = str_wrap(value, 20)
        ) |>
        group_by(name, prepost) |>
        mutate(
          Percent = round(100 * n / sum(n), 2),
          value = factor(value, levels = c(
            "Never use",
            "Sometimes (once a\nmonth)",
            "Use often (once or\ntwice weekly)",
            "Use everyday"
          ))
        ) |>
        filter(value %in% c("Use often (once or\ntwice weekly)", "Use everyday")) |>
        summarise(
          Percent = sum(Percent),
          n = sum(n)
        ) |>
        ggplot(aes(
          x = fct_reorder(name, Percent, .desc = T),
          y = Percent
        )) +
        geom_col(aes(fill = prepost),
          color = NA, position = position_dodge2(width = 1, reverse = TRUE)
        ) +
        geom_text(
          aes(
            color = prepost,
            label = if_else(Percent >= 10, paste0(round(Percent), "%"), "")
          ),
          position = position_dodge2(reverse = TRUE, width = 1),
          hjust = -0.25,
          fontface = "bold",
          size = 6
        ) +
        labs(
          x = "Please indicate the extent to which you use...", y = "",
          title = glue::glue("Teacher Curriculum Usage\n% selected use everyday or use often"),
          fill = ""
        ) +
        scale_fill_manual(values = c(
          "Pre" = "black",
          "Post" = "#00ACF0"
        ), labels = c(glue::glue("Pre (n = {n_size_1})"), glue::glue("Post (n = {n_size_2})"))) +
        scale_color_manual(values = c(
          "Pre" = "black",
          "Post" = "#00ACF0"
        )) +
        guides(
          fill = guide_legend()
        ) +
        scale_y_continuous(
          labels = scales::label_percent(scale = 1),
          expand = c(0.14, 0)
        ) +
        coord_flip() +
        guides(color = "none") +
        theme_tl(legend = F) +
        theme(
          axis.text.y = element_markdown(
            margin = margin(t = 0, l = 0, r = -40, b = 0),
            size = 16
          ),
          axis.text.x = element_blank(),
          plot.title = element_text(size = 25, face = "bold"),
          legend.position = "bottom",
          legend.key.height = unit(1.3, "cm"),
          legend.key.width = unit(1.3, "cm"),
          legend.key.size = unit(0.75, "cm"),
          legend.text = element_text(size = 13),
          legend.margin = margin(-25, 0, 0, -150)
        )
    } else {
      tlShiny:::no_data_plot_filters
    }
  })

  output$mindsets_plot_2 <- renderPlot({
    n_size_1 <- sum(!is.na(educator_survey_filtered()$lesson_modifications))
    n_size_2 <- sum(!is.na(followup_educator_survey_filtered()$lesson_modifications))

    if (n_size_1 >= 1) {
      if (n_size_2 >= 1) {
        data <- educator_survey_filtered() |>
          bind_rows(followup_educator_survey_filtered())
      } else {
        data <- educator_survey_filtered()
      }

      data |>
        select(prepost, lesson_modifications) |>
        mutate(lesson_modifications = as.character(lesson_modifications)) |>
        drop_na(lesson_modifications) |>
        group_by(lesson_modifications, prepost) |>
        count(sort = T) |>
        ungroup() |>
        group_by(prepost) |>
        mutate(
          lesson_modifications = str_wrap(lesson_modifications, 20),
          Percent = round(100 * n / sum(n), 2),
          lesson_modifications = factor(lesson_modifications, levels = c(
            "with no or few\nmodifications",
            "with modifications\nto less than half of\na lesson plan",
            "with modifications\nto more than half of\na lesson plan",
            "my main materials do\nnot include lesson\nplans or I typically\ncreate my own lesson\nplans"
          ))
        ) |>
        ggplot(aes(
          x = prepost,
          y = Percent,
          fill = lesson_modifications
        )) +
        geom_col(color = NA, position = position_stack(reverse = TRUE), width = 0.7) +
        geom_text(
          aes(
            color = lesson_modifications,
            label = if_else(Percent >= 10, paste0(round(Percent), "%"), "")
          ),
          position = position_stack(vjust = 0.5, reverse = TRUE),
          fontface = "bold",
          size = 6
        ) +
        labs(
          x = "I typically use lessons with...", y = "",
          title = glue::glue("Teachers' use of lessons from district or\nschool-adopted materials (pre n = {n_size_1}, post n = {n_size_2})"),
          fill = ""
        ) +
        scale_fill_manual(values = c(
          "with no or few\nmodifications" = "#032E3F",
          "with modifications\nto less than half of\na lesson plan" = "#02587A",
          "with modifications\nto more than half of\na lesson plan" = "#0182B4",
          "my main materials do\nnot include lesson\nplans or I typically\ncreate my own lesson\nplans" = "#00ACF0"
        )) +
        scale_color_manual(values = c(
          "with no or few\nmodifications" = "white",
          "with modifications\nto less than half of\na lesson plan" = "white",
          "with modifications\nto more than half of\na lesson plan" = "black",
          "my main materials do\nnot include lesson\nplans or I typically\ncreate my own lesson\nplans" = "black"
        )) +
        guides(
          fill = guide_legend(),
          color = "none"
        ) +
        scale_y_continuous(
          labels = scales::label_percent(scale = 1),
          expand = c(0.14, 0)
        ) +
        coord_flip() +
        theme_tl(legend = F) +
        theme(
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 13),
          axis.text.x = element_blank(),
          plot.title = element_text(size = 25, face = "bold"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        )
    } else {
      tlShiny:::no_data_plot_filters
    }
  })

  output$mindsets_plot_3 <- renderPlot({
    n_size_1 <- sum(!is.na(educator_survey_filtered()$curriculum_sch_dist_1))
    n_size_2 <- sum(!is.na(followup_educator_survey_filtered()$curriculum_sch_dist_1))

    if (n_size_1 >= 1) {
      if (n_size_2 >= 1) {
        data <- educator_survey_filtered() |>
          bind_rows(followup_educator_survey_filtered())
      } else {
        data <- educator_survey_filtered()
      }

      data |>
        select(prepost, contains("curriculum_sch_dist")) |>
        pivot_longer(!prepost, names_to = "name", values_to = "value") |>
        mutate(name = str_replace_all(name, c(
          "curriculum_sch_dist_1" = "The curriculum materials adopted by my school or district are well-suited to the needs of my students",
          "curriculum_sch_dist_2" = "The  curriculum materials adopted by my school or district offer students high-quality opportunities to learn.",
          "curriculum_sch_dist_3" = "The  curriculum materials adopted by my school or district are well-organized and easy to use.",
          "curriculum_sch_dist_4" = "I like the  curriculum materials adopted by my school or district.",
          "curriculum_sch_dist_5" = "The  curriculum materials adopted by my school or district will help my students learn.",
          "curriculum_sch_dist_6" = "The  curriculum materials adopted by my school are too scripted and don't provide me with enough autonomy."
        ))) |>
        group_by(name, value, prepost) |>
        count(sort = T) |>
        ungroup() |>
        drop_na(value) |>
        mutate(name = html_wrap(name, 25)) |>
        group_by(name, prepost) |>
        mutate(
          Percent = round(100 * n / sum(n), 2),
          value = factor(value, levels = c(
            "Strongly disagree",
            "Disagree",
            "Neither agree nor disagree",
            "Agree",
            "Strongly agree"
          )),
          prepost = factor(prepost, levels = c("Pre", "Post"))
        ) |>
        filter(value %in% c("Agree", "Strongly agree")) |>
        ungroup() |>
        group_by(name, prepost) |>
        summarise(
          Percent = sum(Percent),
          n = sum(n)
        ) |>
        ggplot(aes(
          x = fct_reorder(name, Percent, .desc = T),
          y = Percent
        )) +
        geom_col(aes(fill = prepost),
          color = NA, position = position_dodge2(width = 1, reverse = TRUE)
        ) +
        geom_text(
          aes(
            color = prepost,
            label = if_else(Percent >= 10, paste0(round(Percent), "%"), "")
          ),
          position = position_dodge2(reverse = TRUE, width = 1),
          hjust = -0.25,
          fontface = "bold",
          size = 8
        ) +
        labs(
          x = "", y = "",
          title = glue::glue("Teacher perceptions of curriculum % that agree\nor strongly agree"),
          fill = ""
        ) +
        scale_fill_manual(values = c(
          "Pre" = "black",
          "Post" = "#00ACF0"
        ), labels = c(glue::glue("Pre (n = {n_size_1})"), glue::glue("Post (n = {n_size_2})"))) +
        scale_color_manual(values = c(
          "Pre" = "black",
          "Post" = "#00ACF0"
        )) +
        guides(
          fill = guide_legend(),
          color = "none"
        ) +
        scale_y_continuous(
          labels = scales::label_percent(scale = 1),
          expand = c(0.14, 0)
        ) +
        coord_flip() +
        theme_tl(legend = T) +
        theme(
          axis.text.y = element_markdown(
            margin = margin(t = 0, l = 0, r = -40, b = 0),
            size = 15
          ),
          axis.text.x = element_blank(),
          plot.title = element_text(size = 25, face = "bold"),
          legend.position = "bottom",
          legend.key.height = unit(1.3, "cm"),
          legend.key.width = unit(1.3, "cm"),
          legend.key.size = unit(0.75, "cm"),
          legend.text = element_text(size = 13),
          legend.margin = margin(-25, 0, 0, -80)
        )
    } else {
      tlShiny:::no_data_plot_filters
    }
  })

  output$mindsets_plot_4 <- renderPlot({
    n_size_1 <- sum(!is.na(educator_survey_filtered()$`ts_perceptions_sl_1`))
    n_size_2 <- sum(!is.na(followup_educator_survey_filtered()$`ts_perceptions_sl_1`))


    if (n_size_1 >= 1) {
      if (n_size_2 >= 1) {
        data <- educator_survey_filtered() |>
          bind_rows(followup_educator_survey_filtered())
      } else {
        data <- educator_survey_filtered()
      }
      educator_survey_filtered() |>
        bind_rows(followup_educator_survey_filtered()) |>
        select(prepost, contains("ts_perceptions_sl")) |>
        pivot_longer(!prepost, names_to = "name", values_to = "value") |>
        mutate(name = str_replace_all(name, c(
          "ts_perceptions_sl_1" = "My school leaders attend the professional development related to curriculum materials with us",
          "ts_perceptions_sl_2" = "My school leaders have created a shared vision for instruction that my curriculum-related professional development experiences is helping our school to implement",
          "ts_perceptions_sl_3" = "My school leaders make sure I have access to all the materials and resources I need to implement our adopted curriculum",
          "ts_perceptions_sl_4" = "My school leaders press me to implement the ideas I learn in curriculum-related professional development",
          "ts_perceptions_sl_5" = "My school leaders make time for my curriculum-related professional development.",
          "ts_perceptions_sl_6" = "Instructional guidance in my school conflicts with the approach taken in curriculum-aligned professional development",
          "ts_perceptions_sl_7" = "I sometimes feel pressure to teach in ways not aligned with the approach to instruction taken in curriculum-aligned professional development",
          "ts_perceptions_sl_8" = "Sometimes I feel like my school puts up barriers to implementing the things I learn in curriculum-related PL",
          "ts_perceptions_sl_9" = "My school leader is knowledgeable about the curriculum that I have been asked to implement"
        ))) |>
        group_by(name, value, prepost) |>
        count(sort = T) |>
        ungroup() |>
        drop_na(value) |>
        mutate(name = html_wrap(name, 60)) |>
        group_by(name, prepost) |>
        mutate(
          Percent = round(100 * n / sum(n), 2),
          value = factor(value, levels = c(
            "Not at all",
            "Rarely",
            "Sometimes",
            "Often",
            "All the time"
          ))
        ) |>
        filter(value %in% c("Often", "All the time")) |>
        ungroup() |>
        group_by(name, prepost) |>
        summarise(
          Percent = sum(Percent),
          n = sum(n)
        ) |>
        ggplot(aes(
          x = fct_reorder(name, Percent, .desc = T),
          y = Percent
        )) +
        geom_col(aes(fill = prepost),
          color = NA, position = position_dodge2(width = 1, reverse = TRUE)
        ) +
        geom_text(
          aes(
            color = prepost,
            label = paste0(round(Percent), "%")
          ),
          position = position_dodge2(reverse = TRUE, width = 1),
          hjust = -0.05,
          fontface = "bold",
          size = 8
        ) +
        labs(
          x = "", y = "",
          title = "Educators’ Perceptions of School Leaders\n(% often or all the time)",
          fill = ""
        ) +
        scale_fill_manual(values = c(
          "Pre" = "black",
          "Post" = "#00ACF0"
        ), labels = c(glue::glue("Pre (n = {n_size_1})"), glue::glue("Post (n = {n_size_2})"))) +
        scale_color_manual(values = c(
          "Pre" = "black",
          "Post" = "#00ACF0"
        )) +
        guides(
          fill = guide_legend(),
          color = "none"
        ) +
        scale_y_continuous(
          labels = scales::label_percent(scale = 1),
          expand = c(0.14, 0)
        ) +
        coord_flip() +
        theme_tl(legend = T) +
        theme(
          axis.text.y = element_markdown(
            margin = margin(t = 0, l = 0, r = -35, b = 0),
            size = 20
          ),
          axis.text.x = element_blank(),
          plot.title = element_text(size = 25, face = "bold"),
          legend.position = "bottom",
          legend.key.height = unit(1.3, "cm"),
          legend.key.width = unit(1.3, "cm"),
          legend.key.size = unit(0.75, "cm"),
          legend.text = element_text(size = 13),
          legend.margin = margin(-25, 0, 0, -50)
        )
    } else {
      tlShiny:::no_data_plot_filters
    }
  })

  output$mindsets_plot_5 <- renderPlot({
    n_size_1 <- sum(!is.na(educator_survey_filtered()$`school_environment_1`))
    n_size_2 <- sum(!is.na(followup_educator_survey_filtered()$`school_environment_1`))


    if (n_size_1 >= 1) {
      if (n_size_2 >= 1) {
        data <- educator_survey_filtered() |>
          bind_rows(followup_educator_survey_filtered())
      } else {
        data <- educator_survey_filtered()
      }
      educator_survey_filtered() |>
        bind_rows(followup_educator_survey_filtered()) |>
        select(prepost, contains("school_environment")) |>
        pivot_longer(!prepost, names_to = "name", values_to = "value") |>
        mutate(name = str_replace_all(name, c(
          "school_environment_1" = "I trust my fellow teachers in the school",
          "school_environment_2" = "I feel connected to my fellow teachers in the school",
          # "school_environment_3" = "I have influence over the professional learning that I receive through my school or district",
          "school_environment_4" = "I collaborate with my fellow teachers regularly"
        ))) |>
        group_by(name, value, prepost) |>
        count(sort = T) |>
        ungroup() |>
        drop_na(value) |>
        mutate(name = html_wrap(name, 25)) |>
        group_by(name, prepost) |>
        mutate(Percent = round(100 * n / sum(n), 2)) |>
        filter(value %in% c("4 - Agree", "5 - Strongly agree")) |>
        summarise(
          Percent = sum(Percent),
          n = sum(n)
        ) |>
        ggplot(aes(
          x = fct_reorder(name, Percent, .desc = T),
          y = Percent
        )) +
        geom_col(aes(fill = prepost),
          color = NA, position = position_dodge2(width = 1, reverse = TRUE)
        ) +
        geom_text(
          aes(
            color = prepost,
            label = if_else(Percent >= 10, paste0(round(Percent), "%"), "")
          ),
          position = position_dodge2(reverse = TRUE, width = 1),
          hjust = -0.25,
          fontface = "bold",
          size = 6
        ) +
        labs(
          x = "", y = "",
          title = glue::glue("Educators’ Perceptions of School Culture and Climate\n(% that agree or strongly agree)"),
          fill = ""
        ) +
        scale_fill_manual(values = c(
          "Pre" = "black",
          "Post" = "#00ACF0"
        ), labels = c(glue::glue("Pre (n = {n_size_1})"), glue::glue("Post (n = {n_size_2})"))) +
        scale_color_manual(values = c(
          "Pre" = "black",
          "Post" = "#00ACF0"
        )) +
        guides(
          fill = guide_legend(),
          color = "none"
        ) +
        scale_y_continuous(
          labels = scales::label_percent(scale = 1),
          expand = c(0.14, 0)
        ) +
        coord_flip() +
        theme_tl(legend = T) +
        theme(
          axis.text.y = element_markdown(
            margin = margin(t = 0, l = 0, r = -55, b = 0),
            size = 16
          ),
          axis.text.x = element_blank(),
          plot.title = element_text(size = 25, face = "bold"),
          legend.position = "bottom",
          legend.key.height = unit(1.3, "cm"),
          legend.key.width = unit(1.3, "cm"),
          legend.key.size = unit(0.75, "cm"),
          legend.text = element_text(size = 13),
          legend.margin = margin(-25, 0, 0, -150)
        )
    } else {
      tlShiny:::no_data_plot_filters
    }
  })

  ######################################## End Section 4 ####################################################################

  ############################################ Section 5 ####################################################################

  output$leaders_mindsets_table_1 <- render_gt({
    n_size_1 <- sum(!is.na(educator_survey_filtered()$`non_ts_mindsets_1`))
    n_size_2 <- sum(!is.na(followup_educator_survey_filtered()$`non_ts_mindsets_1`))

    if (n_size_1 >= 1 & n_size_2 >= 1) {
      educator_survey_filtered() |>
        dplyr::bind_rows(followup_educator_survey_filtered()) |>
        dplyr::select(
          prepost, non_ts_mindsets_1, non_ts_mindsets_2, non_ts_mindsets_3, non_ts_mindsets_4,
          non_ts_mindsets_7, non_ts_mindsets_8, non_ts_mindsets_9, non_ts_mindsets_10, non_ts_mindsets_11,
          non_ts_mindsets_12, non_ts_mindsets_13, non_ts_mindsets_14
        ) |>
        tidyr::pivot_longer(!prepost, names_to = "name", values_to = "value") |>
        tidyr::drop_na(value) |>
        dplyr::group_by(name, value, prepost) |>
        dplyr::count(sort = T) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          pos_neg = dplyr::case_when(
            name %in% c(
              "non_ts_mindsets_1", "non_ts_mindsets_3", "non_ts_mindsets_4",
              "non_ts_mindsets_7", "non_ts_mindsets_8", "non_ts_mindsets_9", "non_ts_mindsets_11",
              "non_ts_mindsets_12", "non_ts_mindsets_13", "non_ts_mindsets_14"
            ) ~ "negative",
            TRUE ~ "positive"
          ),
          ` ` = dplyr::case_when(
            name %in% c("non_ts_mindsets_1", "non_ts_mindsets_2", "non_ts_mindsets_3", "non_ts_mindsets_4") ~ "Recognition of Race & Culture",
            name %in% c("non_ts_mindsets_7", "non_ts_mindsets_8", "non_ts_mindsets_9", "non_ts_mindsets_10", "non_ts_mindsets_11") ~ "High expectations",
            name %in% c("non_ts_mindsets_12", "non_ts_mindsets_13", "non_ts_mindsets_14") ~ "Growth mindsets"
          ),
          score_multiplier = dplyr::case_when(
            value == "1 - Strongly disagree" & pos_neg == "negative" ~ 1,
            value == "1 - Strongly disagree" & pos_neg == "positive" ~ 0,
            value == "2 - Disagree" & pos_neg == "negative" ~ 0.75,
            value == "2 - Disagree" & pos_neg == "positive" ~ 0.25,
            value == "3 - Neither agree nor disagree" & pos_neg == "negative" ~ 0.5,
            value == "3 - Neither agree nor disagree" & pos_neg == "positive" ~ 0.5,
            value == "4 - Agree" & pos_neg == "negative" ~ 0.25,
            value == "4 - Agree" & pos_neg == "positive" ~ 0.75,
            value == "5 - Strongly agree" & pos_neg == "negative" ~ 0,
            value == "5 - Strongly agree" & pos_neg == "positive" ~ 1
          ),
          name = stringr::str_replace_all(name, c(
            "non_ts_mindsets_7" = "Teachers should keep in mind the limits of their students’ ability and give them assignments that they can do so that they do not become discouraged",
            "non_ts_mindsets_8" = "Before students are asked to engage in complex learning tasks, they need to have a solid grasp of basic skills",
            "non_ts_mindsets_9" = "It is not fair to ask students who are struggling with English to take on challenging academic assignments",
            "non_ts_mindsets_10" = "Teachers should provide all students the opportunity to work with grade-level texts and tasks",
            "non_ts_mindsets_11" = "Grouping students of different levels of achievement for instruction may benefit some students, but it can undermine the progress that could otherwise be made by higher achieving students",
            "non_ts_mindsets_12" = "Students have a certain amount of intelligence, and they can’t really do much to change it",
            "non_ts_mindsets_13" = "Intelligence is something about students that they can’t change very much",
            "non_ts_mindsets_14" = "Students can learn new things, but they can’t really change their basic intelligence",
            "non_ts_mindsets_2" = "Every student who works hard, no matter what race they are, has an equal chance to be successful academically",
            "non_ts_mindsets_3" = "Racism against students from racial and ethnic minority backgrounds may have been a problem in the past, but it is not a problem today",
            "non_ts_mindsets_4" = "Students from racial and ethnic minority backgrounds have the same opportunities as White students",
            "non_ts_mindsets_1" = "Teachers should be color blind when it comes to their teaching - They shouldn’t think of their students in terms of their race or ethnicity"
          )),
          name = dplyr::if_else(pos_neg == "negative", paste0("<p style='color:red;'>", name, "</p>"), name)
        ) |>
        dplyr::group_by(name, prepost) |>
        dplyr::mutate(score = (n * score_multiplier) / sum(n)) |>
        dplyr::summarise(
          score = sum(score),
          ` ` = min(` `)
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(` `, prepost) |>
        dplyr::summarise(score = mean(score)) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = prepost, values_from = score) |>
        dplyr::relocate(Post, .after = Pre) |>
        (\(.) dplyr::add_row(., ` ` = "<b>Overall score</b>", !!!colMeans(.[2]), !!!colMeans(.[3]), .before = 1))() |>
        gt::gt() |>
        gt::fmt_percent(c(Pre, Post),
          scale_values = TRUE
        ) |>
        gt::fmt_markdown(` `) |>
        gt::data_color(
          columns = c(Pre, Post),
          fn = scales::col_bin(
            palette = c(
              tlShiny::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
            ),
            domain = c(0, 1),
            bins = c(0, 0.39, 0.79, 1)
          )
        ) |>
        tlShiny::gt_theme_tl(align = "left") |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_1), locations = cells_column_labels("Pre")) |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_2), locations = cells_column_labels("Post"))
    } else if (n_size_1 >= 1 & n_size_2 == 0) {
      educator_survey_filtered() |>
        dplyr::select(
          prepost, non_ts_mindsets_1, non_ts_mindsets_2, non_ts_mindsets_3, non_ts_mindsets_4,
          non_ts_mindsets_7, non_ts_mindsets_8, non_ts_mindsets_9, non_ts_mindsets_10, non_ts_mindsets_11,
          non_ts_mindsets_12, non_ts_mindsets_13, non_ts_mindsets_14
        ) |>
        tidyr::pivot_longer(!prepost, names_to = "name", values_to = "value") |>
        tidyr::drop_na(value) |>
        dplyr::group_by(name, value, prepost) |>
        dplyr::count(sort = T) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          pos_neg = dplyr::case_when(
            name %in% c(
              "non_ts_mindsets_1", "non_ts_mindsets_3", "non_ts_mindsets_4",
              "non_ts_mindsets_7", "non_ts_mindsets_8", "non_ts_mindsets_9", "non_ts_mindsets_11",
              "non_ts_mindsets_12", "non_ts_mindsets_13", "non_ts_mindsets_14"
            ) ~ "negative",
            TRUE ~ "positive"
          ),
          ` ` = dplyr::case_when(
            name %in% c("non_ts_mindsets_1", "non_ts_mindsets_2", "non_ts_mindsets_3", "non_ts_mindsets_4") ~ "Recognition of Race & Culture",
            name %in% c("non_ts_mindsets_7", "non_ts_mindsets_8", "non_ts_mindsets_9", "non_ts_mindsets_10", "non_ts_mindsets_11") ~ "High expectations",
            name %in% c("non_ts_mindsets_12", "non_ts_mindsets_13", "non_ts_mindsets_14") ~ "Growth mindsets"
          ),
          score_multiplier = dplyr::case_when(
            value == "1 - Strongly disagree" & pos_neg == "negative" ~ 1,
            value == "1 - Strongly disagree" & pos_neg == "positive" ~ 0,
            value == "2 - Disagree" & pos_neg == "negative" ~ 0.75,
            value == "2 - Disagree" & pos_neg == "positive" ~ 0.25,
            value == "3 - Neither agree nor disagree" & pos_neg == "negative" ~ 0.5,
            value == "3 - Neither agree nor disagree" & pos_neg == "positive" ~ 0.5,
            value == "4 - Agree" & pos_neg == "negative" ~ 0.25,
            value == "4 - Agree" & pos_neg == "positive" ~ 0.75,
            value == "5 - Strongly agree" & pos_neg == "negative" ~ 0,
            value == "5 - Strongly agree" & pos_neg == "positive" ~ 1
          ),
          name = stringr::str_replace_all(name, c(
            "non_ts_mindsets_7" = "Teachers should keep in mind the limits of their students’ ability and give them assignments that they can do so that they do not become discouraged",
            "non_ts_mindsets_8" = "Before students are asked to engage in complex learning tasks, they need to have a solid grasp of basic skills",
            "non_ts_mindsets_9" = "It is not fair to ask students who are struggling with English to take on challenging academic assignments",
            "non_ts_mindsets_10" = "Teachers should provide all students the opportunity to work with grade-level texts and tasks",
            "non_ts_mindsets_11" = "Grouping students of different levels of achievement for instruction may benefit some students, but it can undermine the progress that could otherwise be made by higher achieving students",
            "non_ts_mindsets_12" = "Students have a certain amount of intelligence, and they can’t really do much to change it",
            "non_ts_mindsets_13" = "Intelligence is something about students that they can’t change very much",
            "non_ts_mindsets_14" = "Students can learn new things, but they can’t really change their basic intelligence",
            "non_ts_mindsets_2" = "Every student who works hard, no matter what race they are, has an equal chance to be successful academically",
            "non_ts_mindsets_3" = "Racism against students from racial and ethnic minority backgrounds may have been a problem in the past, but it is not a problem today",
            "non_ts_mindsets_4" = "Students from racial and ethnic minority backgrounds have the same opportunities as White students",
            "non_ts_mindsets_1" = "Teachers should be color blind when it comes to their teaching - They shouldn’t think of their students in terms of their race or ethnicity"
          )),
          name = dplyr::if_else(pos_neg == "negative", paste0("<p style='color:red;'>", name, "</p>"), name)
        ) |>
        dplyr::group_by(name, prepost) |>
        dplyr::mutate(score = (n * score_multiplier) / sum(n)) |>
        dplyr::summarise(
          score = sum(score),
          ` ` = min(` `)
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(` `, prepost) |>
        dplyr::summarise(score = mean(score)) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = prepost, values_from = score) |>
        dplyr::mutate(Post = NA) |>
        dplyr::relocate(Post, .after = Pre) |>
        (\(.) dplyr::add_row(., ` ` = "<b>Overall score</b>", !!!colMeans(.[2]), !!!colMeans(.[3]), .before = 1))() |>
        gt::gt() |>
        gt::fmt_percent(c(Pre, Post),
          scale_values = TRUE
        ) |>
        gt::fmt_markdown(` `) |>
        gt::data_color(
          columns = c(Pre, Post),
          fn = scales::col_bin(
            palette = c(
              tlShiny::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
            ),
            domain = c(0, 1),
            bins = c(0, 0.39, 0.79, 1)
          )
        ) |>
        tlShiny::gt_theme_tl(align = "left") |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_1), locations = cells_column_labels("Pre")) |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_2), locations = cells_column_labels("Post"))
    } else {
      tibble::tibble() |>
        gt::gt() |>
        tab_header(title = md("**There is no data for this set of filters yet.**"))
    }
  })

  output$leaders_mindsets_table_2 <- render_gt({
    n_size_1 <- sum(!is.na(educator_survey_filtered()$`non_ts_mindsets_1`))
    n_size_2 <- sum(!is.na(followup_educator_survey_filtered()$`non_ts_mindsets_1`))

    if (n_size_1 >= 1 & n_size_2 >= 1) {
      educator_survey_filtered() |>
        dplyr::bind_rows(followup_educator_survey_filtered()) |>
        dplyr::select(
          prepost, non_ts_mindsets_1, non_ts_mindsets_2, non_ts_mindsets_3, non_ts_mindsets_4
        ) |>
        tidyr::pivot_longer(!prepost, names_to = " ", values_to = "value") |>
        tidyr::drop_na(value) |>
        dplyr::group_by(` `, value, prepost) |>
        dplyr::count(sort = T) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          pos_neg = dplyr::case_when(
            ` ` %in% c(
              "non_ts_mindsets_1", "non_ts_mindsets_3", "non_ts_mindsets_4",
              "non_ts_mindsets_7", "non_ts_mindsets_8", "non_ts_mindsets_9", "non_ts_mindsets_11",
              "non_ts_mindsets_12", "non_ts_mindsets_13", "non_ts_mindsets_14"
            ) ~ "negative",
            TRUE ~ "positive"
          ),
          ` ` = stringr::str_replace_all(` `, c(
            "non_ts_mindsets_2" = "Every student who works hard, no matter what race they are, has an equal chance to be successful academically",
            "non_ts_mindsets_3" = "Racism against students from racial and ethnic minority backgrounds may have been a problem in the past, but it is not a problem today",
            "non_ts_mindsets_4" = "Students from racial and ethnic minority backgrounds have the same opportunities as White students",
            "non_ts_mindsets_1" = "Teachers should be color blind when it comes to their teaching - They shouldn’t think of their students in terms of their race or ethnicity"
          )),
          score_multiplier = dplyr::case_when(
            value == "1 - Strongly disagree" & pos_neg == "negative" ~ 1,
            value == "1 - Strongly disagree" & pos_neg == "positive" ~ 0,
            value == "2 - Disagree" & pos_neg == "negative" ~ 0.75,
            value == "2 - Disagree" & pos_neg == "positive" ~ 0.25,
            value == "3 - Neither agree nor disagree" & pos_neg == "negative" ~ 0.5,
            value == "3 - Neither agree nor disagree" & pos_neg == "positive" ~ 0.5,
            value == "4 - Agree" & pos_neg == "negative" ~ 0.25,
            value == "4 - Agree" & pos_neg == "positive" ~ 0.75,
            value == "5 - Strongly agree" & pos_neg == "negative" ~ 0,
            value == "5 - Strongly agree" & pos_neg == "positive" ~ 1
          ),
          ` ` = dplyr::if_else(pos_neg == "negative", paste0("<p style='color:red;'>", ` `, "</p>"), ` `)
        ) |>
        dplyr::group_by(` `, prepost) |>
        dplyr::mutate(score = (n * score_multiplier) / sum(n)) |>
        dplyr::summarise(
          score = sum(score)
        ) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = prepost, values_from = score) |>
        dplyr::relocate(Post, .after = Pre) |>
        (\(.) dplyr::add_row(., ` ` = "<b>Overall score</b>", !!!colMeans(.[2]), !!!colMeans(.[3]), .before = 1))() |>
        gt::gt() |>
        gt::fmt_percent(c(Pre, Post),
          scale_values = TRUE
        ) |>
        gt::fmt_markdown(` `) |>
        gt::data_color(
          columns = c(Pre, Post),
          fn = scales::col_bin(
            palette = c(
              tlShiny::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
            ),
            domain = c(0, 1),
            bins = c(0, 0.39, 0.79, 1)
          )
        ) |>
        tlShiny::gt_theme_tl(align = "left") |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_1), locations = cells_column_labels("Pre")) |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_2), locations = cells_column_labels("Post"))
    } else if (n_size_1 >= 1 & n_size_2 == 0) {
      educator_survey_filtered() |>
        dplyr::select(
          prepost, non_ts_mindsets_1, non_ts_mindsets_2, non_ts_mindsets_3, non_ts_mindsets_4
        ) |>
        tidyr::pivot_longer(!prepost, names_to = " ", values_to = "value") |>
        tidyr::drop_na(value) |>
        dplyr::group_by(` `, value, prepost) |>
        dplyr::count(sort = T) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          pos_neg = dplyr::case_when(
            ` ` %in% c(
              "non_ts_mindsets_1", "non_ts_mindsets_3", "non_ts_mindsets_4",
              "non_ts_mindsets_7", "non_ts_mindsets_8", "non_ts_mindsets_9", "non_ts_mindsets_11",
              "non_ts_mindsets_12", "non_ts_mindsets_13", "non_ts_mindsets_14"
            ) ~ "negative",
            TRUE ~ "positive"
          ),
          ` ` = stringr::str_replace_all(` `, c(
            "non_ts_mindsets_2" = "Every student who works hard, no matter what race they are, has an equal chance to be successful academically",
            "non_ts_mindsets_3" = "Racism against students from racial and ethnic minority backgrounds may have been a problem in the past, but it is not a problem today",
            "non_ts_mindsets_4" = "Students from racial and ethnic minority backgrounds have the same opportunities as White students",
            "non_ts_mindsets_1" = "Teachers should be color blind when it comes to their teaching - They shouldn’t think of their students in terms of their race or ethnicity"
          )),
          score_multiplier = dplyr::case_when(
            value == "1 - Strongly disagree" & pos_neg == "negative" ~ 1,
            value == "1 - Strongly disagree" & pos_neg == "positive" ~ 0,
            value == "2 - Disagree" & pos_neg == "negative" ~ 0.75,
            value == "2 - Disagree" & pos_neg == "positive" ~ 0.25,
            value == "3 - Neither agree nor disagree" & pos_neg == "negative" ~ 0.5,
            value == "3 - Neither agree nor disagree" & pos_neg == "positive" ~ 0.5,
            value == "4 - Agree" & pos_neg == "negative" ~ 0.25,
            value == "4 - Agree" & pos_neg == "positive" ~ 0.75,
            value == "5 - Strongly agree" & pos_neg == "negative" ~ 0,
            value == "5 - Strongly agree" & pos_neg == "positive" ~ 1
          ),
          ` ` = dplyr::if_else(pos_neg == "negative", paste0("<p style='color:red;'>", ` `, "</p>"), ` `)
        ) |>
        dplyr::group_by(` `, prepost) |>
        dplyr::mutate(score = (n * score_multiplier) / sum(n)) |>
        dplyr::summarise(
          score = sum(score)
        ) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = prepost, values_from = score) |>
        dplyr::mutate(Post = NA) |>
        dplyr::relocate(Post, .after = Pre) |>
        (\(.) dplyr::add_row(., ` ` = "<b>Overall score</b>", !!!colMeans(.[2]), !!!colMeans(.[3]), .before = 1))() |>
        gt::gt() |>
        gt::fmt_percent(c(Pre, Post),
          scale_values = TRUE
        ) |>
        gt::fmt_markdown(` `) |>
        gt::data_color(
          columns = c(Pre, Post),
          fn = scales::col_bin(
            palette = c(
              tlShiny::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
            ),
            domain = c(0, 1),
            bins = c(0, 0.39, 0.79, 1)
          )
        ) |>
        tlShiny::gt_theme_tl(align = "left") |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_1), locations = cells_column_labels("Pre")) |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_2), locations = cells_column_labels("Post"))
    } else {
      tibble::tibble() |>
        gt::gt() |>
        tab_header(title = md("**There is no data for this set of filters yet.**"))
    }
  })

  output$leaders_mindsets_table_3 <- render_gt({
    n_size_1 <- sum(!is.na(educator_survey_filtered()$`non_ts_mindsets_1`))
    n_size_2 <- sum(!is.na(followup_educator_survey_filtered()$`non_ts_mindsets_1`))

    if (n_size_1 >= 1 & n_size_2 >= 1) {
      educator_survey_filtered() |>
        dplyr::bind_rows(followup_educator_survey_filtered()) |>
        dplyr::select(
          prepost, non_ts_mindsets_7, non_ts_mindsets_8, non_ts_mindsets_9, non_ts_mindsets_10, non_ts_mindsets_11
        ) |>
        tidyr::pivot_longer(!prepost, names_to = " ", values_to = "value") |>
        tidyr::drop_na(value) |>
        dplyr::group_by(` `, value, prepost) |>
        dplyr::count(sort = T) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          pos_neg = dplyr::case_when(
            ` ` %in% c(
              "non_ts_mindsets_1", "non_ts_mindsets_3", "non_ts_mindsets_4",
              "non_ts_mindsets_7", "non_ts_mindsets_8", "non_ts_mindsets_9", "non_ts_mindsets_11",
              "non_ts_mindsets_12", "non_ts_mindsets_13", "non_ts_mindsets_14"
            ) ~ "negative",
            TRUE ~ "positive"
          ),
          ` ` = stringr::str_replace_all(` `, c(
            "non_ts_mindsets_7" = "Teachers should keep in mind the limits of their students’ ability and give them assignments that they can do so that they do not become discouraged",
            "non_ts_mindsets_8" = "Before students are asked to engage in complex learning tasks, they need to have a solid grasp of basic skills",
            "non_ts_mindsets_9" = "It is not fair to ask students who are struggling with English to take on challenging academic assignments",
            "non_ts_mindsets_10" = "Teachers should provide all students the opportunity to work with grade-level texts and tasks",
            "non_ts_mindsets_11" = "Grouping students of different levels of achievement for instruction may benefit some students, but it can undermine the progress that could otherwise be made by higher achieving students"
          )),
          score_multiplier = dplyr::case_when(
            value == "1 - Strongly disagree" & pos_neg == "negative" ~ 1,
            value == "1 - Strongly disagree" & pos_neg == "positive" ~ 0,
            value == "2 - Disagree" & pos_neg == "negative" ~ 0.75,
            value == "2 - Disagree" & pos_neg == "positive" ~ 0.25,
            value == "3 - Neither agree nor disagree" & pos_neg == "negative" ~ 0.5,
            value == "3 - Neither agree nor disagree" & pos_neg == "positive" ~ 0.5,
            value == "4 - Agree" & pos_neg == "negative" ~ 0.25,
            value == "4 - Agree" & pos_neg == "positive" ~ 0.75,
            value == "5 - Strongly agree" & pos_neg == "negative" ~ 0,
            value == "5 - Strongly agree" & pos_neg == "positive" ~ 1
          ),
          ` ` = dplyr::if_else(pos_neg == "negative", paste0("<p style='color:red;'>", ` `, "</p>"), ` `)
        ) |>
        dplyr::group_by(` `, prepost) |>
        dplyr::mutate(score = (n * score_multiplier) / sum(n)) |>
        dplyr::summarise(
          score = sum(score)
        ) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = prepost, values_from = score) |>
        dplyr::relocate(Post, .after = Pre) |>
        (\(.) dplyr::add_row(., ` ` = "<b>Overall score</b>", !!!colMeans(.[2]), !!!colMeans(.[3]), .before = 1))() |>
        gt::gt() |>
        gt::fmt_percent(c(Pre, Post),
          scale_values = TRUE
        ) |>
        gt::fmt_markdown(` `) |>
        gt::data_color(
          columns = c(Pre, Post),
          fn = scales::col_bin(
            palette = c(
              tlShiny::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
            ),
            domain = c(0, 1),
            bins = c(0, 0.39, 0.79, 1)
          )
        ) |>
        tlShiny::gt_theme_tl(align = "left") |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_1), locations = cells_column_labels("Pre")) |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_2), locations = cells_column_labels("Post"))
    } else if (n_size_1 >= 1 & n_size_2 == 0) {
      educator_survey_filtered() |>
        dplyr::select(
          prepost, non_ts_mindsets_7, non_ts_mindsets_8, non_ts_mindsets_9, non_ts_mindsets_10, non_ts_mindsets_11
        ) |>
        tidyr::pivot_longer(!prepost, names_to = " ", values_to = "value") |>
        tidyr::drop_na(value) |>
        dplyr::group_by(` `, value, prepost) |>
        dplyr::count(sort = T) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          pos_neg = dplyr::case_when(
            ` ` %in% c(
              "non_ts_mindsets_1", "non_ts_mindsets_3", "non_ts_mindsets_4",
              "non_ts_mindsets_7", "non_ts_mindsets_8", "non_ts_mindsets_9", "non_ts_mindsets_11",
              "non_ts_mindsets_12", "non_ts_mindsets_13", "non_ts_mindsets_14"
            ) ~ "negative",
            TRUE ~ "positive"
          ),
          ` ` = stringr::str_replace_all(` `, c(
            "non_ts_mindsets_7" = "Teachers should keep in mind the limits of their students’ ability and give them assignments that they can do so that they do not become discouraged",
            "non_ts_mindsets_8" = "Before students are asked to engage in complex learning tasks, they need to have a solid grasp of basic skills",
            "non_ts_mindsets_9" = "It is not fair to ask students who are struggling with English to take on challenging academic assignments",
            "non_ts_mindsets_10" = "Teachers should provide all students the opportunity to work with grade-level texts and tasks",
            "non_ts_mindsets_11" = "Grouping students of different levels of achievement for instruction may benefit some students, but it can undermine the progress that could otherwise be made by higher achieving students"
          )),
          score_multiplier = dplyr::case_when(
            value == "1 - Strongly disagree" & pos_neg == "negative" ~ 1,
            value == "1 - Strongly disagree" & pos_neg == "positive" ~ 0,
            value == "2 - Disagree" & pos_neg == "negative" ~ 0.75,
            value == "2 - Disagree" & pos_neg == "positive" ~ 0.25,
            value == "3 - Neither agree nor disagree" & pos_neg == "negative" ~ 0.5,
            value == "3 - Neither agree nor disagree" & pos_neg == "positive" ~ 0.5,
            value == "4 - Agree" & pos_neg == "negative" ~ 0.25,
            value == "4 - Agree" & pos_neg == "positive" ~ 0.75,
            value == "5 - Strongly agree" & pos_neg == "negative" ~ 0,
            value == "5 - Strongly agree" & pos_neg == "positive" ~ 1
          ),
          ` ` = dplyr::if_else(pos_neg == "negative", paste0("<p style='color:red;'>", ` `, "</p>"), ` `)
        ) |>
        dplyr::group_by(` `, prepost) |>
        dplyr::mutate(score = (n * score_multiplier) / sum(n)) |>
        dplyr::summarise(
          score = sum(score)
        ) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = prepost, values_from = score) |>
        dplyr::mutate(Post = NA) |>
        dplyr::relocate(Post, .after = Pre) |>
        (\(.) dplyr::add_row(., ` ` = "<b>Overall score</b>", !!!colMeans(.[2]), !!!colMeans(.[3]), .before = 1))() |>
        gt::gt() |>
        gt::fmt_percent(c(Pre, Post),
          scale_values = TRUE
        ) |>
        gt::fmt_markdown(` `) |>
        gt::data_color(
          columns = c(Pre, Post),
          fn = scales::col_bin(
            palette = c(
              tlShiny::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
            ),
            domain = c(0, 1),
            bins = c(0, 0.39, 0.79, 1)
          )
        ) |>
        tlShiny::gt_theme_tl(align = "left") |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_1), locations = cells_column_labels("Pre")) |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_2), locations = cells_column_labels("Post"))
    } else {
      tibble::tibble() |>
        gt::gt() |>
        tab_header(title = md("**There is no data for this set of filters yet.**"))
    }
  })

  output$leaders_mindsets_table_4 <- render_gt({
    n_size_1 <- sum(!is.na(educator_survey_filtered()$`non_ts_mindsets_1`))
    n_size_2 <- sum(!is.na(followup_educator_survey_filtered()$`non_ts_mindsets_1`))

    if (n_size_1 >= 1 & n_size_2 >= 1) {
      educator_survey_filtered() |>
        dplyr::bind_rows(followup_educator_survey_filtered()) |>
        dplyr::select(
          prepost, non_ts_mindsets_12, non_ts_mindsets_13, non_ts_mindsets_14
        ) |>
        tidyr::pivot_longer(!prepost, names_to = " ", values_to = "value") |>
        tidyr::drop_na(value) |>
        dplyr::group_by(` `, value, prepost) |>
        dplyr::count(sort = T) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          pos_neg = dplyr::case_when(
            ` ` %in% c(
              "non_ts_mindsets_1", "non_ts_mindsets_3", "non_ts_mindsets_4",
              "non_ts_mindsets_7", "non_ts_mindsets_8", "non_ts_mindsets_9", "non_ts_mindsets_11",
              "non_ts_mindsets_12", "non_ts_mindsets_13", "non_ts_mindsets_14"
            ) ~ "negative",
            TRUE ~ "positive"
          ),
          ` ` = stringr::str_replace_all(` `, c(
            "non_ts_mindsets_12" = "Students have a certain amount of intelligence, and they can’t really do much to change it",
            "non_ts_mindsets_13" = "Intelligence is something about students that they can’t change very much",
            "non_ts_mindsets_14" = "Students can learn new things, but they can’t really change their basic intelligence"
          )),
          score_multiplier = dplyr::case_when(
            value == "1 - Strongly disagree" & pos_neg == "negative" ~ 1,
            value == "1 - Strongly disagree" & pos_neg == "positive" ~ 0,
            value == "2 - Disagree" & pos_neg == "negative" ~ 0.75,
            value == "2 - Disagree" & pos_neg == "positive" ~ 0.25,
            value == "3 - Neither agree nor disagree" & pos_neg == "negative" ~ 0.5,
            value == "3 - Neither agree nor disagree" & pos_neg == "positive" ~ 0.5,
            value == "4 - Agree" & pos_neg == "negative" ~ 0.25,
            value == "4 - Agree" & pos_neg == "positive" ~ 0.75,
            value == "5 - Strongly agree" & pos_neg == "negative" ~ 0,
            value == "5 - Strongly agree" & pos_neg == "positive" ~ 1
          ),
          ` ` = dplyr::if_else(pos_neg == "negative", paste0("<p style='color:red;'>", ` `, "</p>"), ` `)
        ) |>
        dplyr::group_by(` `, prepost) |>
        dplyr::mutate(score = (n * score_multiplier) / sum(n)) |>
        dplyr::summarise(
          score = sum(score)
        ) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = prepost, values_from = score) |>
        dplyr::relocate(Post, .after = Pre) |>
        (\(.) dplyr::add_row(., ` ` = "<b>Overall score</b>", !!!colMeans(.[2]), !!!colMeans(.[3]), .before = 1))() |>
        gt::gt() |>
        gt::fmt_percent(c(Pre, Post),
          scale_values = TRUE
        ) |>
        gt::fmt_markdown(` `) |>
        gt::data_color(
          columns = c(Pre, Post),
          fn = scales::col_bin(
            palette = c(
              tlShiny::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
            ),
            domain = c(0, 1),
            bins = c(0, 0.39, 0.79, 1)
          )
        ) |>
        tlShiny::gt_theme_tl(align = "left") |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_1), locations = cells_column_labels("Pre")) |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_2), locations = cells_column_labels("Post"))
    } else if (n_size_1 >= 1 & n_size_2 == 0) {
      educator_survey_filtered() |>
        dplyr::select(
          prepost, non_ts_mindsets_12, non_ts_mindsets_13, non_ts_mindsets_14
        ) |>
        tidyr::pivot_longer(!prepost, names_to = " ", values_to = "value") |>
        tidyr::drop_na(value) |>
        dplyr::group_by(` `, value, prepost) |>
        dplyr::count(sort = T) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          pos_neg = dplyr::case_when(
            ` ` %in% c(
              "non_ts_mindsets_1", "non_ts_mindsets_3", "non_ts_mindsets_4",
              "non_ts_mindsets_7", "non_ts_mindsets_8", "non_ts_mindsets_9", "non_ts_mindsets_11",
              "non_ts_mindsets_12", "non_ts_mindsets_13", "non_ts_mindsets_14"
            ) ~ "negative",
            TRUE ~ "positive"
          ),
          ` ` = stringr::str_replace_all(` `, c(
            "non_ts_mindsets_12" = "Students have a certain amount of intelligence, and they can’t really do much to change it",
            "non_ts_mindsets_13" = "Intelligence is something about students that they can’t change very much",
            "non_ts_mindsets_14" = "Students can learn new things, but they can’t really change their basic intelligence"
          )),
          score_multiplier = dplyr::case_when(
            value == "1 - Strongly disagree" & pos_neg == "negative" ~ 1,
            value == "1 - Strongly disagree" & pos_neg == "positive" ~ 0,
            value == "2 - Disagree" & pos_neg == "negative" ~ 0.75,
            value == "2 - Disagree" & pos_neg == "positive" ~ 0.25,
            value == "3 - Neither agree nor disagree" & pos_neg == "negative" ~ 0.5,
            value == "3 - Neither agree nor disagree" & pos_neg == "positive" ~ 0.5,
            value == "4 - Agree" & pos_neg == "negative" ~ 0.25,
            value == "4 - Agree" & pos_neg == "positive" ~ 0.75,
            value == "5 - Strongly agree" & pos_neg == "negative" ~ 0,
            value == "5 - Strongly agree" & pos_neg == "positive" ~ 1
          ),
          ` ` = dplyr::if_else(pos_neg == "negative", paste0("<p style='color:red;'>", ` `, "</p>"), ` `)
        ) |>
        dplyr::group_by(` `, prepost) |>
        dplyr::mutate(score = (n * score_multiplier) / sum(n)) |>
        dplyr::summarise(
          score = sum(score)
        ) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = prepost, values_from = score) |>
        dplyr::mutate(Post = NA) |>
        dplyr::relocate(Post, .after = Pre) |>
        (\(.) dplyr::add_row(., ` ` = "<b>Overall score</b>", !!!colMeans(.[2]), !!!colMeans(.[3]), .before = 1))() |>
        gt::gt() |>
        gt::fmt_percent(c(Pre, Post),
          scale_values = TRUE
        ) |>
        gt::fmt_markdown(` `) |>
        gt::data_color(
          columns = c(Pre, Post),
          fn = scales::col_bin(
            palette = c(
              tlShiny::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
            ),
            domain = c(0, 1),
            bins = c(0, 0.39, 0.79, 1)
          )
        ) |>
        tlShiny::gt_theme_tl(align = "left") |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_1), locations = cells_column_labels("Pre")) |>
        gt::tab_footnote(footnote = paste0("n = ", n_size_2), locations = cells_column_labels("Post"))
    } else {
      tibble::tibble() |>
        gt::gt() |>
        tab_header(title = md("**There is no data for this set of filters yet.**"))
    }
  })

  output$leaders_mindsets_plot_1 <- renderPlot({
    n_size_1 <- sum(!is.na(educator_survey_filtered()$`observation_practice_1`))
    n_size_2 <- sum(!is.na(followup_educator_survey_filtered()$`observation_practice_1`))


    if (n_size_1 >= 1) {
      if (n_size_2 >= 1) {
        data <- educator_survey_filtered() |>
          bind_rows(followup_educator_survey_filtered())
      } else {
        data <- educator_survey_filtered()
      }

      data |>
        select(prepost, contains("observation_practice")) |>
        pivot_longer(!prepost, names_to = "name", values_to = "value") |>
        mutate(name = str_replace_all(name, c(
          "observation_practice_1" = "Whether the lesson is focused on a high-quality text or task",
          "observation_practice_2" = "Whether the questions and tasks address the analytical thinking required by the grade-level standards",
          "observation_practice_3" = "Whether all students have opportunities to engage in the work of the lesson"
        ))) |>
        filter(value != "N/A - I do not observe teachers in my role.") |>
        group_by(name, value, prepost) |>
        count(sort = T) |>
        ungroup() |>
        drop_na(value) |>
        mutate(name = html_wrap(name, 25)) |>
        group_by(name, prepost) |>
        mutate(Percent = round(100 * n / sum(n), 2)) |>
        ungroup() |>
        filter(value %in% c("4 - Very often", "5 - Almost always")) |>
        group_by(name, prepost) |>
        summarise(
          Percent = sum(Percent),
          n = sum(n)
        ) |>
        ggplot(aes(
          x = fct_reorder(name, Percent, .desc = T),
          y = Percent
        )) +
        geom_col(aes(fill = prepost),
          color = NA, position = position_dodge2(width = 1, reverse = TRUE)
        ) +
        geom_text(
          aes(
            color = prepost,
            label = if_else(Percent >= 10, paste0(round(Percent), "%"), "")
          ),
          position = position_dodge2(reverse = TRUE, width = 1),
          hjust = -0.25,
          fontface = "bold",
          size = 8
        ) +
        labs(
          x = "", y = "",
          title = glue::glue('How often does your teacher practice observation focus on...\n% that selected "very often" or "almost always"'),
          fill = ""
        ) +
        scale_fill_manual(values = c(
          "Pre" = "black",
          "Post" = "#00ACF0"
        ), labels = c(glue::glue("Pre (n = {n_size_1})"), glue::glue("Post (n = {n_size_2})"))) +
        scale_color_manual(values = c(
          "Pre" = "black",
          "Post" = "#00ACF0"
        )) +
        guides(
          fill = guide_legend(),
          color = "none"
        ) +
        scale_y_continuous(
          labels = scales::label_percent(scale = 1),
          expand = c(0.14, 0)
        ) +
        coord_flip() +
        theme_tl() +
        theme(
          axis.text.y = element_markdown(
            margin = margin(t = 0, l = 0, r = -55, b = 0),
            size = 17
          ),
          axis.text.x = element_blank(),
          plot.title = element_text(size = 25, face = "bold"),
          legend.position = "bottom",
          legend.key.height = unit(1.3, "cm"),
          legend.key.width = unit(1.3, "cm"),
          legend.key.size = unit(0.75, "cm"),
          legend.text = element_text(size = 13),
          legend.margin = margin(-25, 0, 0, -60)
        )
    } else {
      tlShiny:::no_data_plot_filters
    }
  })

  ######################################## End Section 5 ####################################################################

  ############################################ Section 6 ####################################################################

  output$crse_plot_1 <- renderPlot({
    
    n_size_1 <- sum(!is.na(educator_survey_filtered()$`ts_crse_after_7.30_1`))
    n_size_2 <- sum(!is.na(followup_educator_survey_filtered()$`ts_crse_after_7.30_1`))
    
    if (n_size_1 >= 1) {
      if (n_size_2 >= 1) {
        data <- educator_survey_filtered() |>
          bind_rows(followup_educator_survey_filtered())
      } else {
        data <- educator_survey_filtered()
      }
      
      data |>
        select(prepost, contains("ts_crse_after")) |>
        pivot_longer(!prepost, names_to = "name", values_to = "value") |>
        mutate(name = str_replace_all(name, c(
          "ts_crse_after_7.30_1" = "I adapt instruction to meet the needs of my students",
          "ts_crse_after_7.30_2" = "I identify ways that the school culture (e.g., values, norms, and practices) is different from my students’ home culture",
          "ts_crse_after_7.30_3" = "I use my students’ prior knowledge to help them make sense of new information",
          "ts_crse_after_7.30_4" = "I revise instructional material to include a better representation of cultural groups",
          "ts_crse_after_7.30_5" = "Use my students’ cultural background to help make learning meaningful.",
          "ts_crse_after_7.30_6" = "Use examples that are familiar to students from diverse cultural backgrounds.",
          "ts_crse_after_7.30_7" = "Take time to learn about the cultures represented by students in my classroom.",
          "ts_crse_after_7.30_8" = "Teach the curriculum to students with unfinished learning",
          "ts_crse_after_7.30_9" = "Teach the curriculum to students who are from historically marginalized groups"
        ))) |>
        filter(value != "N/A - I do not observe teachers in my role.") |>
        group_by(name, value, prepost) |>
        count(sort = T) |>
        ungroup() |>
        drop_na(value) |>
        mutate(name = html_wrap(name, 25)) |>
        group_by(name, prepost) |>
        mutate(Percent = round(100 * n / sum(n), 2)) |>
        filter(value %in% c("5- Very often", "4- Often")) |>
        group_by(name, prepost) |>
        summarise(
          Percent = sum(Percent),
          n = sum(n)
        ) |>
        ggplot(aes(
          x = fct_reorder(name, Percent, .desc = T),
          y = Percent
        )) +
        geom_col(aes(fill = prepost),
                 color = NA, position = position_dodge2(width = 1, reverse = TRUE)
        ) +
        geom_text(
          aes(
            color = prepost,
            label = if_else(Percent >= 10, paste0(round(Percent), "%"), "")
          ),
          position = position_dodge2(reverse = TRUE, width = 1),
          hjust = -0.25,
          fontface = "bold",
          size = 8
        ) +
        labs(
          x = "", y = "",
          title = glue::glue("Teachers CRSE Self-reported Practices\n % that selected often or very often"),
          fill = ""
        ) +
        scale_fill_manual(values = c(
          "Pre" = "black",
          "Post" = "#00ACF0"
        ), labels = c(glue::glue("Pre (n = {n_size_1})"), glue::glue("Post (n = {n_size_2})"))) +
        scale_color_manual(values = c(
          "Pre" = "black",
          "Post" = "#00ACF0"
        )) +
        guides(
          fill = guide_legend(),
          color = "none"
        ) +
        scale_y_continuous(
          labels = scales::label_percent(scale = 1),
          expand = c(0.14, 0)
        ) +
        coord_flip() +
        theme_tl() +
        theme(
          axis.text.y = element_markdown(
            margin = margin(t = 0, l = 0, r = -55, b = 0),
            size = 17
          ),
          axis.text.x = element_blank(),
          plot.title = element_text(size = 25, face = "bold"),
          legend.position = "bottom",
          legend.key.height = unit(1.3, "cm"),
          legend.key.width = unit(1.3, "cm"),
          legend.key.size = unit(0.75, "cm"),
          legend.text = element_text(size = 13),
          legend.margin = margin(-25, 0, 0, -40)
        )
    } else {
      tlShiny:::no_data_plot_filters
    }
    
  })
  
  output$direct_to_ts_pos_plot <- renderPlot({
    
    data <- ipg_forms_filtered() |>
      dplyr::filter(service_tl_only == "Direct-to-Teacher Coaching" & direct_to_ts_obs != "Ongoing")
    
    
    n_size_1 <- sum(!is.na(data$k12_m_ca1a)) + sum(!is.na(data$k12_ela_ca1a)) + sum(!is.na(data$fsot_ac1))
    
    if (n_size_1 >= 1) {
      
      ipg_data_1 <- data |>
        dplyr::select(
          direct_to_ts_obs,
          fsot_ac1, fsot_ac2, # AC1/AC2, 1-4, Q69 is not 1-4
          fsot_td1, fsot_td2, fsot_td3, fsot_td4, # TD, 1-4
          fsot_sp1, fsot_sp2, fsot_sp3, fsot_sp4, # SP, 1-4
          fsot_ad1, fsot_ad2, # AD1/AD2, 1-3
          k12_ela_ca1a, k12_ela_ca1b, k12_ela_ca1c, # Core Action 1, Yes/No
          k12_ela_ca2a, k12_ela_ca2b, k12_ela_ca2c, k12_ela_ca2d, # Core Action 2, 1-4
          k12_ela_ca3a, k12_ela_ca3b, k12_ela_ca3c, k12_ela_ca3d, k12_ela_ca3e, k12_ela_ca3f, # Core Action 3, 1-4
          k12_m_ca1a, k12_m_ca1b, k12_m_ca1c, # Core Action 1 yes-no
          k12_m_ca2a, k12_m_ca2b, k12_m_ca2c, k12_m_ca2d, # Core Action 2 1-4
          k12_m_ca3a, k12_m_ca3b, k12_m_ca3c, k12_m_ca3d, k12_m_ca3e # Core Action 3 1-4
        ) |> 
        group_by(direct_to_ts_obs) |>
        summarise(
          across(c(k12_m_ca1a, k12_m_ca1b, k12_m_ca1c,
                   k12_ela_ca1a, k12_ela_ca1b, k12_ela_ca1c), ~ tlShiny::grade_ipg(.x, type = "character")),
          across(c(
            k12_m_ca2a, k12_m_ca2b, k12_m_ca2c, k12_m_ca2d,
            k12_m_ca3a, k12_m_ca3b, k12_m_ca3c, k12_m_ca3d, k12_m_ca3e,
            k12_ela_ca2a, k12_ela_ca2b, k12_ela_ca2c, k12_ela_ca2d,
            k12_ela_ca3a, k12_ela_ca3b, k12_ela_ca3c, k12_ela_ca3d, k12_ela_ca3e, k12_ela_ca3f,
            fsot_ac1, fsot_ac2,
            fsot_td1, fsot_td2, fsot_td3, fsot_td4,
            fsot_sp1, fsot_sp2, fsot_sp3, fsot_sp4
          ), ~ tlShiny::grade_ipg(.x, type = "numeric")),
          across(c(fsot_ad1, fsot_ad2), ~ tlShiny::grade_ipg(.x, type = "numeric_low")),
          n = n()
        ) |>
        pivot_longer(!c(direct_to_ts_obs, n), names_to = "Name", values_to = "value") |>
        drop_na(direct_to_ts_obs, value) |>
        dplyr::mutate(`Core Action` = case_when(
          str_detect(Name, "ca1") ~ "Core Action 1",
          str_detect(Name, "ca2") ~ "Core Action 2",
          str_detect(Name, "ca3") ~ "Core Action 3",
          str_detect(Name, "ac") ~ "Aligned\nContent",
          str_detect(Name, "ad") ~ "Assessment\n& Differentiation",
          str_detect(Name, "sp") ~ "Student\nPractice",
          str_detect(Name, "td") ~ "Teacher-Directed\nInstruction"
        )) |>
        group_by(direct_to_ts_obs, `Core Action`) |>
        reframe(value = weighted.mean(x = value, w = n, na.rm = T),
                  n = n) |>
        ungroup()
      
      
        ipg_data_2 <- ipg_data_1 |>
          bind_rows(ipg_data_1 |> group_by(direct_to_ts_obs) |> reframe(value = mean(value), `Core Action` = "Overall", n = first(n))) |>
          mutate(direct_to_ts_obs = factor(direct_to_ts_obs, levels = c(
          "Baseline (first observation of the year)",
          "Ongoing",
          "Mid-year (middle of service, if applicable)",
          "End of year (last observation of the year)"
          ))) |>
          arrange(direct_to_ts_obs)
        
        if (input$ipgFormsSelect == "All Rubrics") {
          ipg_data_2 |>
            dplyr::filter(`Core Action` == "Overall") |>
            ggplot(aes(x = direct_to_ts_obs, y = value)) +
            geom_col(fill = "#04abeb") +
            geom_text(aes(label = paste0(round(value), "%\n (n = ", n, ")")),
                      fontface = "bold",
                      vjust = -0.5,
                      size = 9
            ) +
            scale_y_continuous(
              labels = scales::percent_format(scale = 1),
              limits = c(0, 105)
            ) +
            labs(
              x = "", y = "",
              title = "% Positive Indicators Across\nAll Observation Rubrics",
              caption = '*Note that "Ongoing" observations are not included in this analysis'
            ) +
            theme_tl() +
            theme(axis.text.x = element_text(size = 20),
                  axis.text.y = element_text(size = 13),
                  plot.title = element_text(size = 24, face = "bold"),
                  plot.caption = element_text(size = 10))
        } else {
          ipg_data_2 |>
            group_by(`Core Action`, direct_to_ts_obs) |>
            reframe(value = weighted.mean(value, n), n = first(n)) |>
            ggplot(aes(x = `Core Action`, y = value)) +
            geom_col(fill = "#04abeb") +
            geom_text(aes(label = paste0(round(value), "%\n (n = ", n, ")")),
                      fontface = "bold",
                      vjust = -0.25,
                      size = 8
            ) +
            facet_wrap( ~ direct_to_ts_obs) +
            scale_y_continuous(
              labels = scales::percent_format(scale = 1),
              limits = c(0, 105)
            ) +
            labs(
              x = "", y = "",
              title = paste0("% Positive Indicators Across ", input$ipgFormsSelect),
              caption = '*Note that "Ongoing" observations are not included in this analysis'
            ) +
            theme_tl() +
            theme(axis.text.x = element_text(size = 20),
                  axis.text.y = element_text(size = 13),
                  plot.title = element_text(size = 24, face = "bold"),
                  strip.text = element_text(size = 21, face = "bold", hjust = 0.5),
                  plot.caption = element_text(size = 10))
        }
          
        
    } else {
      tlShiny:::no_data_plot_filters
    }
    
  })
  
  output$instructional_walkthroughs_plot_1 <- renderPlot({
    
    data <- ipg_forms_filtered() |>
      dplyr::filter(service_tl_only != "Direct-to-Teacher Coaching") |>
      dplyr::mutate(direct_to_ts_obs = "Baseline (first observation of the year)")
    
    n_size_1 <- sum(!is.na(data$k12_m_ca1a)) + sum(!is.na(data$k12_ela_ca1a)) + sum(!is.na(data$fsot_ac1))
    
    if (n_size_1 >= 1) {
      
      ipg_data_1 <- data |>
        dplyr::select(
          direct_to_ts_obs,
          fsot_ac1, fsot_ac2, # AC1/AC2, 1-4, Q69 is not 1-4
          fsot_td1, fsot_td2, fsot_td3, fsot_td4, # TD, 1-4
          fsot_sp1, fsot_sp2, fsot_sp3, fsot_sp4, # SP, 1-4
          fsot_ad1, fsot_ad2, # AD1/AD2, 1-3
          k12_ela_ca1a, k12_ela_ca1b, k12_ela_ca1c, # Core Action 1, Yes/No
          k12_ela_ca2a, k12_ela_ca2b, k12_ela_ca2c, k12_ela_ca2d, # Core Action 2, 1-4
          k12_ela_ca3a, k12_ela_ca3b, k12_ela_ca3c, k12_ela_ca3d, k12_ela_ca3e, k12_ela_ca3f, # Core Action 3, 1-4
          k12_m_ca1a, k12_m_ca1b, k12_m_ca1c, # Core Action 1 yes-no
          k12_m_ca2a, k12_m_ca2b, k12_m_ca2c, k12_m_ca2d, # Core Action 2 1-4
          k12_m_ca3a, k12_m_ca3b, k12_m_ca3c, k12_m_ca3d, k12_m_ca3e # Core Action 3 1-4
        ) |> 
        group_by(direct_to_ts_obs) |>
        summarise(
          across(c(k12_m_ca1a, k12_m_ca1b, k12_m_ca1c,
                   k12_ela_ca1a, k12_ela_ca1b, k12_ela_ca1c), ~ tlShiny::grade_ipg(.x, type = "character")),
          across(c(
            k12_m_ca2a, k12_m_ca2b, k12_m_ca2c, k12_m_ca2d,
            k12_m_ca3a, k12_m_ca3b, k12_m_ca3c, k12_m_ca3d, k12_m_ca3e,
            k12_ela_ca2a, k12_ela_ca2b, k12_ela_ca2c, k12_ela_ca2d,
            k12_ela_ca3a, k12_ela_ca3b, k12_ela_ca3c, k12_ela_ca3d, k12_ela_ca3e, k12_ela_ca3f,
            fsot_ac1, fsot_ac2,
            fsot_td1, fsot_td2, fsot_td3, fsot_td4,
            fsot_sp1, fsot_sp2, fsot_sp3, fsot_sp4
          ), ~ tlShiny::grade_ipg(.x, type = "numeric")),
          across(c(fsot_ad1, fsot_ad2), ~ tlShiny::grade_ipg(.x, type = "numeric_low")),
          n = n()
        ) |>
        pivot_longer(!c(direct_to_ts_obs, n), names_to = "Name", values_to = "value") |>
        drop_na(direct_to_ts_obs, value) |>
        dplyr::mutate(`Core Action` = case_when(
          str_detect(Name, "ca1") ~ "Core Action 1",
          str_detect(Name, "ca2") ~ "Core Action 2",
          str_detect(Name, "ca3") ~ "Core Action 3",
          str_detect(Name, "ac") ~ "Aligned\nContent",
          str_detect(Name, "ad") ~ "Assessment\n& Differentiation",
          str_detect(Name, "sp") ~ "Student\nPractice",
          str_detect(Name, "td") ~ "Teacher-Directed\nInstruction"
        )) |>
        group_by(direct_to_ts_obs, `Core Action`) |>
        reframe(value = weighted.mean(x = value, w = n, na.rm = T),
                n = n) |>
        ungroup()
      
      
      ipg_data_2 <- ipg_data_1 |>
        bind_rows(ipg_data_1 |> group_by(direct_to_ts_obs) |> reframe(value = mean(value), `Core Action` = "Overall", n = first(n))) |>
        mutate(direct_to_ts_obs = factor(direct_to_ts_obs, levels = c(
          "Baseline (first observation of the year)",
          "Ongoing",
          "Mid-year (middle of service, if applicable)",
          "End of year (last observation of the year)"
        ))) |>
        arrange(direct_to_ts_obs)
      
      if (input$ipgFormsSelect == "All Rubrics") {
        ipg_data_2 |>
          dplyr::filter(`Core Action` == "Overall") |>
          ggplot(aes(x = direct_to_ts_obs, y = value)) +
          geom_col(fill = "#04abeb") +
          geom_text(aes(label = paste0(round(value), "%\n (n = ", n, ")")),
                    fontface = "bold",
                    vjust = -0.5,
                    size = 9
          ) +
          scale_y_continuous(
            labels = scales::percent_format(scale = 1),
            limits = c(0, 105)
          ) +
          labs(
            x = "", y = "",
            title = "% Positive Indicators Across\nAll Observation Rubrics"
          ) +
          theme_tl() +
          theme(axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 13),
                plot.title = element_text(size = 24, face = "bold"))
      } else {
        ipg_data_2 |>
          group_by(`Core Action`, direct_to_ts_obs) |>
          reframe(value = weighted.mean(value, n), n = first(n)) |>
          ggplot(aes(x = `Core Action`, y = value)) +
          geom_col(fill = "#04abeb") +
          geom_text(aes(label = paste0(round(value), "%\n (n = ", n, ")")),
                    fontface = "bold",
                    vjust = -0.25,
                    size = 8
          ) +
          facet_wrap( ~ direct_to_ts_obs) +
          scale_y_continuous(
            labels = scales::percent_format(scale = 1),
            limits = c(0, 105)
          ) +
          labs(
            x = "", y = "",
            title = paste0("% Positive Indicators Across ", input$ipgFormsSelect)
          ) +
          theme_tl() +
          theme(axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 13),
                plot.title = element_text(size = 24, face = "bold"),
                strip.text = element_text(size = 21, face = "bold", hjust = 0.5))
      }
      
      
    } else {
      tlShiny:::no_data_plot_filters
    }
    
  })

  ######################################## End Section 6 ####################################################################


  ############################################ Section 7 ####################################################################

  output$student_crse_1 <- renderPlot({
    
    n_size_1 <- student_survey_filtered() |> nrow()
    
    if (n_size_1 >= 1) {
      tlShiny::student_bar_chart(
        data = student_survey,
        title = "Responses to please rate the following items",
        col_select = "crse",
        agree_select = c("4 - Often", "5 - Always"),
        string_remove = "Please rate the following items\\. - "
      )
    } else {
      tlShiny:::no_data_plot_filters
    }
    
  })
  
  output$student_crse_2 <- renderPlot({
    
    n_size_1 <- student_survey_filtered() |> nrow()
    
    if (n_size_1 >= 1) {
      tlShiny::student_bar_chart(
        data = student_survey,
        title = "Responses to please rate the following items",
        col_select = "teacher_student_rel",
        agree_select = c("4 - Somewhat agree", "5 - Agree"),
        string_remove = "Please rate the following items - "
      )
    } else {
      tlShiny:::no_data_plot_filters
    }
    
  })
  
  output$student_crse_3 <- renderPlot({
    
    n_size_1 <- student_survey_filtered() |> nrow()
    
    if (n_size_1 >= 1) {
      tlShiny::student_bar_chart(
        data = student_survey,
        title = "Responses to please rate the following items",
        col_select = "self_efficacy",
        agree_select = c("4 - Somewhat agree", "5 - Agree"),
        string_remove = "Please rate the following items - |How much do you agree or disagree with the statements below\\? - "
      )
    } else {
      tlShiny:::no_data_plot_filters
    }
    
  })
  
  output$student_crse_4 <- renderPlot({
    
    n_size_1 <- student_survey_filtered() |> nrow()
    
    if (n_size_1 >= 1) {
      tlShiny::student_bar_chart(
        data = student_survey,
        title = "Responses to please rate the following items",
        col_select = "happiness_belonging",
        agree_select = c("4 - Somewhat agree", "5 - Agree"),
        string_remove = "Please rate the following items - "
      )
    } else {
      tlShiny:::no_data_plot_filters
    }
    
  })
  
  output$student_crse_5 <- renderPlot({
    
    n_size_1 <- student_survey_filtered() |> nrow()
    
    if (n_size_1 >= 1) {
      tlShiny::student_bar_chart(
        data = student_survey,
        title = "Responses to please rate the following items",
        col_select = "being_challenged",
        agree_select = c("4 - Mostly true", "5 - Totally true"),
        string_remove = "Please rate the following items - "
      )
    } else {
      tlShiny:::no_data_plot_filters
    }
    
  })
  
  output$student_work_plot_1 <- renderPlot({
    
    tlShiny:::no_data_plot_currently
    
  })
  
  output$student_work_plot_2 <- renderPlot({
    
    tlShiny:::no_data_plot_currently
    
  })
  
  output$student_work_plot_3 <- renderPlot({
    
    tlShiny:::no_data_plot_currently
    
  })

  ######################################## End Section 7 #######################'#############################################


  ########################################## Section 8 ####################################################################

  output$summer_pl_plot <- renderPlot({
    tlShiny:::no_data_plot_currently
  })

  output$future_prof_learning_plot <- renderPlot({
    tlShiny:::no_data_plot_currently
  })

  output$future_prof_learn_loc_plot <- renderPlot({
    tlShiny:::no_data_plot_currently
  })

  ######################################## End Section 8 ####################################################################
}
