### Diagnostic Completion Dashboard ###
ui <- fluidPage(
  theme = my_theme,
  div(
    id = "page-top",
    shiny::radioButtons("current_theme", "App Theme:", c("Light" = "cerulean", "Dark" = "darkly"), inline = TRUE),
    div(class = "source_link", a(href = "https://teachinglab.github.io", "Return to Data Hub", icon("github"))),
  ),
  div(
    id = "app-title",
    titlePanel("Data Collection per Site 2021-2022"),
  ),
  div(
    id = "header",
    labeled_input(
      "prev_partner_btn", "Return to previous partner",
      actionButton("prev_partner", textOutput("prev_partner_label"), icon = icon("sync"))
    ),
    labeled_input(
      "partner_selector", "Search for a partner",
      selectizeInput("partner",
        label = NULL,
        choices = unique_partners %>%
          sort() %>%
          purrr::prepend("All Partners"),
        selected = "All Partners",
        multiple = F
      )
    ),
    labeled_input(
      "rand_partner_btn", "Try a random partner",
      actionButton("rnd_partner", icon("dice"))
    )
  ),
  gt_output("partner_gt")
)


server <- function(input, output, session) {

  # If the URL contains a partner on load, use that partner instead of the default of ann arbor
  bookmarked_partner <- "Nothing"
  current_partner <- reactiveVal(if (bookmarked_partner %in% unique_partners) bookmarked_partner else "All Partners")
  updateSelectizeInput(inputId = "partner", selected = isolate(current_partner()))

  # A book-keeping reactive so we can have a previous partner button
  previous_partner <- reactiveVal(NULL)

  observe({
    req(input$partner)
    # Set the previous partner to the non-updated current partner. If app is just
    # starting we want to populate the previous partner button with a random partner,
    # not the current partner
    selected_partner <- isolate(current_partner())
    just_starting <- selected_partner == input$partner
    previous_partner(if (just_starting) get_random_partner() else selected_partner)

    # Current partner now can be updated to the newly selected partner
    current_partner(input$partner)

    # Update the query string so the app will know what to do.
    # updateQueryString(make_url_hash(current_partner()), mode = "push")
  })

  observe({
    updateSelectizeInput(inputId = "partner", selected = isolate(previous_partner()))
  }) %>% bindEvent(input$prev_partner)

  observe({
    updateSelectizeInput(inputId = "partner", selected = get_random_partner())
  }) %>% bindEvent(input$rnd_partner)

  ### Diagnostic Data ###
  diagnostic_data_n <- reactive({
    diagnostic %>%
      group_by(
        your_site_district_parish_network_or_school_br_br,
        which_subject_area_do_you_teach_lead_if_you_teach_support_both_please_select_the_one_where_you_expect_to_work_with_teaching_lab_this_year_br_br
      ) %>%
      count(sort = T) %>%
      rename(`Content Area` = which_subject_area_do_you_teach_lead_if_you_teach_support_both_please_select_the_one_where_you_expect_to_work_with_teaching_lab_this_year_br_br) %>%
      mutate(`Content Area` = stringr::str_replace_all(`Content Area`, c(
        "ELA/Literacy" = "ELA",
        "Mathematics" = "Math"
      ))) %>%
      ungroup()
  })

  ### Knowledge Assessments Data ###
  knowledge_assessments_data_n <- reactive({
    knowledge_assessments %>%
      group_by(site, prepost, know_assess) %>%
      summarise(n = length(unique(id)), .groups = "drop") %>%
      pivot_wider(names_from = "prepost", values_from = "n") %>%
      mutate(`Content Area` = ifelse(str_detect(know_assess, "ela"),
        "ELA",
        "Math"
      ))
  })

  ### Course Survey Data ###
  course_survey_data_n <- reactive({
    course_survey %>%
      group_by(
        `Select your site (district, parish, network, or school).`,
        `Select the content area for today's professional learning session.`
      ) %>%
      count(sort = T) %>%
      rename(`Content Area` = `Select the content area for today's professional learning session.`) %>%
      ungroup()
  })

  data_final <- reactive({
    final_df <- knowledge_assessments_data_n() %>%
      rename(
        `Knowledge Assessment` = know_assess,
        `Pre responses` = pre,
        `Post responses` = post
      ) %>%
      left_join(course_survey_data_n(), by = c(
        "site" = "Select your site (district, parish, network, or school).",
        "Content Area"
      )) %>%
      rename(`End of Course responses` = n) %>%
      left_join(diagnostic_data_n(), by = c(
        "site" = "your_site_district_parish_network_or_school_br_br",
        "Content Area"
      )) %>%
      rename(`Diagnostic Educator Survey responses` = n) %>%
      relocate(`Post responses`, .after = `Pre responses`) %>%
      mutate(
        `Knowledge Assessment` = str_replace_all(
          str_to_title(str_replace_all(`Knowledge Assessment`, "_", " ")),
          c(
            "Ela" = "ELA",
            "El" = "EL"
          )
        ),
        site = replace_na(site, "Other"),
        `End of Course responses` = replace_na(`End of Course responses`, 0),
        `Diagnostic Educator Survey responses` = replace_na(`Diagnostic Educator Survey responses`, 0)
      ) %>%
      rename(Site = site) %>%
      group_by(Site) %>%
      mutate(across(c(
        `End of Course responses`,
        `Diagnostic Educator Survey responses`
      ), ~ ifelse(row_number() == 1,
        .x,
        NA
      ))) %>%
      arrange(desc(`End of Course responses`))

    if (input$partner != "All Partners") {
      final_df <- rbind(
        final_df %>% filter(Site == input$partner),
        final_df %>% filter(Site != input$partner)
      )
      print(final_df)
    }

    final_df
  })

  output$partner_gt <- render_gt(
    data_final() %>%
      ### Group by ELA/Math and Site ###
      gt::gt(groupname_col = c("Content Area", "Site")) %>%
      ### Reformat NAs as "None" ###
      gt::fmt_missing(
        columns = c(`Pre responses`, `Post responses`),
        missing_text = "None"
      ) %>%
      ### Reformat end of course and diagnostic to show only one instance per grouping ###
      gt::fmt_missing(
        columns = c(`End of Course responses`, `Diagnostic Educator Survey responses`),
        missing_text = ""
      ) %>%
      ### Add color to table for n sizes of end of course, diagnostic, knowledge pre/post ###
      gt::data_color(
        columns = c(`End of Course responses`, `Diagnostic Educator Survey responses`, `Pre responses`, `Post responses`),
        colors = scales::col_numeric(
          palette = paletteer::paletteer_d(
            palette = "ggsci::blue_material"
          ) %>% as.character(),
          domain = NULL
        )
      ) %>%
      ### Add tl theme ###
      tlShiny::gt_theme_tl() %>%
      ### Make column grouping a row ###
      gt::tab_options(
        row_group.as_column = FALSE,
        row_group.font.weight = "bold",
        row_group.font.size = px(17)
      ) %>%
      {
        if (input$current_theme == "darkly") tab_options(., table.background.color = "#303030") else .
      } %>%
      {
        if (input$current_theme == "darkly") tab_options(., column_labels.background.color = "#303030") else .
      },
    width = px(1080)
  )

  observe({
    # Make sure theme is kept current with desired
    session$setCurrentTheme(
      bs_theme_update(my_theme, bootswatch = input$current_theme)
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
