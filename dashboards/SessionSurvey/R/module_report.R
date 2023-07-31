#### Session Survey Dashboard ####
uiReport <- function(id, label = "Counter") {
  ns <- NS(id)
  shiny::tagList(
    shiny.semantic::sidebar_layout(
      sidebar_panel = shiny.semantic::sidebar_panel(
        style = sidebar_style,
        menu_item(
          tabName = "recent_menu",
          h5("Utilize the filters in the sidebar to download", style = "margin: calc(2rem - 3.1em) 0 1rem; margin-top:2px;"),
          h5("your custom report,", style = "margin: calc(2rem - 3.1em) 0 1rem"),
          h5("OR click one of the following recent", style = "margin: calc(2rem - 3.1em) 0 1rem"),
          h5("sessions found by a unique ", style = "margin: calc(2rem - 3.1em) 0 1rem"),
          h5("combination of site and facilitator,", style = "margin: calc(2rem - 3.1em) 0 1rem"),
          h5("and then just click download!", style = "margin: calc(2rem - 3.1em) 0 1rem"),
          shiny::selectizeInput(ns("recent_sessions"),
                                choices = recent_choices_final$choice,
                                label = NULL, 
                                selected = NULL, 
                                multiple = T,
                                options = list(plugins = list("remove_button"))
          ),
          icon = shiny.semantic::icon("free code camp")
        ),
        shiny.semantic::menu_item(
          tabName = "facilitator_menu",
          shiny::selectizeInput(
            inputId = ns("facilitator"),
            label = h3("Select a facilitator"),
            choices = session_survey$Facilitator |>
              unique() |>
              as.character() |>
              sort() |>
              purrr::prepend("All Facilitators"),
            multiple = T,
            selected = "All Facilitators",
            options = list(plugins = list("remove_button"))
          ),
          icon = shiny.semantic::icon("clone")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "content_menu",
          shiny::selectizeInput(
            inputId = ns("content"),
            label = h3("Select a content area"),
            choices = session_survey$`Select the content area for today’s professional learning session.` |>
              unique() |>
              as.character() |>
              sort() |>
              purrr::prepend("All Content Areas"),
            multiple = T,
            selected = "All Content Areas",
            options = list(plugins = list("remove_button"))
          ),
          icon = shiny.semantic::icon("edit")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "course_menu",
          shiny::selectizeInput(
            inputId = ns("course"),
            label = h3("Select a course"),
            choices = session_survey$`Select your course.` |>
              unique() |>
              as.character() |>
              sort() |>
              purrr::prepend("All Courses"),
            multiple = T,
            selected = "All Courses",
            options = list(plugins = list("remove_button"))
          ),
          icon = shiny.semantic::icon("clone")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "site_menu",
          shiny::selectizeInput(
            inputId = ns("site"),
            label = h3("Select a site"),
            choices = session_survey$`Select your site (district, parish, network, or school).` |>
              unique() |>
              as.character() |>
              sort() |>
              purrr::prepend("All Sites"),
            multiple = T,
            selected = "All Sites",
            options = list(plugins = list("remove_button"))
          ),
          icon = shiny.semantic::icon("location arrow")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "role_menu",
          shiny::selectizeInput(
            inputId = ns("role"),
            label = h3("Select a role"),
            choices = session_survey$`Select your role.` |>
              unique() |>
              as.character() |>
              sort() |>
              purrr::prepend("All Roles"),
            multiple = T,
            selected = "All Roles",
            options = list(plugins = list("remove_button"))
          ),
          icon = shiny.semantic::icon("user")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "date_slider_menu",
          split_layout(
            cell_widths = c("50%", "50%"),
            cell_args = "padding: 5px;",
            style = "background-color: transparent;",
            shinyWidgets::airDatepickerInput(
              inputId = ns("date_min"),
              label = h3("Select minimum date"),
              value = min(as.Date(session_survey$Date), na.rm = T),
              minDate = min(as.Date(session_survey$Date), na.rm = T),
              maxDate = max(as.Date(session_survey$Date), na.rm = T) + 1,
              dateFormat = "mm-dd-yyyy",
              width = "100px"
            ),
            shinyWidgets::airDatepickerInput(
              inputId = ns("date_max"),
              label = h3("Select maximum date"),
              value = Sys.Date() + 1,
              minDate = min(as.Date(session_survey$Date), na.rm = T),
              maxDate = Sys.Date() + 1,
              dateFormat = "mm-dd-yyyy",
              width = "100px"
            )
          ),
          icon = shiny.semantic::icon("calendar alternate")
        )
      ),
      main_panel = shiny.semantic::main_panel(
        width = 4,
        div(
          class = "ui three column stackable grid container",
          div(
            class = "six wide column",
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            downloadButton(ns("download_report1"),
                           label = tagList(
                             tags$span(tags$html("  Download Report PDF")),
                             tags$img(src = "report_example_pdf.png", 
                                      align = "center", 
                                      style = report_style)
                           ),
                           icon = shiny::icon("file-download")
            ) |>
              withSpinner(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br()
          ),
          div(
            class = "four wide column",
            align = "center",
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            downloadButton(ns("download_report2"),
                           label = tagList(
                             tags$span("Download Report DOCX"),
                             tags$img(src = "report_example_word.png", 
                                      align = "center", 
                                      style = report_style)
                           ),
                           icon = shiny::icon("file-download")
            ) |>
              withSpinner()
          ),
          br(),
          br(),
          br(),
          br(),
          br(),
          div(
            class = "four wide column",
            align = "center",
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            downloadButton(ns("download_csv"),
                           label = tagList(
                             tags$span("  Download a CSV of Raw Data"),
                             tags$img(src = "fake_raw.png", 
                                      align = "center", 
                                      style = report_style)
                           ),
                           icon = shiny::icon("save")
            ),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br()
          )
        )
      )
    )
  )
}

reportServer <- function(id) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {

    #### Applying Recent Groupings Logic ####

    session_survey_recent <- reactive({
      if (!is.null(input$recent_sessions)) {

        # Get previously created choices dataframe for filtering by id
        chosen <- recent_choices |>
          inner_join((recent_choices_final |>
            dplyr::filter(choice %in% input$recent_sessions)), by = "id")

        session_survey <- session_survey |>
          dplyr::filter(`Select your site (district, parish, network, or school).` %in% chosen$`Select your site (district, parish, network, or school).` &
            Facilitator %in% chosen$Facilitator)
      } else {
        session_survey
      }
    })

    #### TIME SERIES INPUTS ####
    data_plot_ts <- reactive({
      validate(
        need(!is.null(input$facilitator), "Please select at least one facilitator"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course"),
        need(input$date_min <= input$date_max, "Please select a minimum date that is less than the maximum.")
      )

      session_survey_recent() |>
        dplyr::filter(between(Date, input$date_min, input$date_max)) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Sites",
          filter_this = input$site,
          dat_filter = `Select your site (district, parish, network, or school).`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Roles",
          filter_this = input$role,
          dat_filter = `Select your role.`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Facilitators",
          filter_this = input$facilitator,
          dat_filter = Facilitator
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Content Areas",
          filter_this = input$content,
          dat_filter = `Select the content area for today’s professional learning session.`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Courses",
          filter_this = input$course,
          dat_filter = `Select your course.`
        ) |>
        select(
          Date,
            `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.`,
            `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.`,
            `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.`,
            `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.`,
            `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.`
        ) |>
        pivot_longer(!`Date`, names_to = "question", values_to = "answer") |>
        dplyr::mutate(question = str_remove_all(
          question,
          "How much do you agree with the following statements about this course\\? - "
        )) |>
        # Rename with line breaks every 27 characters
        mutate(question = gsub("(.{28,}?)\\s", "\\1\n", question)) |>
        drop_na(answer) |>
        dplyr::group_by(question, Date) |>
        # Group by input variable
        mutate(`Number Agree/Disagree` = n()) |>
        mutate(answer = str_remove_all(answer, "\\([:digit:]\\) ")) |>
        mutate(
          Rating = case_when(
            answer %in% c("Agree", "Strongly agree") ~ "Agree/Strongly Agree",
            answer %in% c("Neither agree nor disagree", "Disagree", "Strongly disagree") ~ "Neither/Disagree/Strongly Disagree"
          ),
          date_group = case_when(
            as.integer(diff(range(as.Date(input$date_max), as.Date(input$date_min)))) >= 30 ~ paste0(lubridate::month(Date, label = T, abbr = F), ", ", year(Date)),
            as.integer(diff(range(as.Date(input$date_max), as.Date(input$date_min)))) >= 14 & as.integer(diff(range(as.Date(input$date_max), as.Date(input$date_min)))) < 30 ~ paste0(year(Date), lubridate::week(Date)),
            as.integer(diff(range(as.Date(input$date_max), as.Date(input$date_min)))) < 14 ~ paste0(lubridate::day(Date))
          )
        ) |>
        ungroup() |>
        dplyr::mutate(question = str_remove_all(
          question,
          "How much do you agree with the\nfollowing statements about this\nfacilitator today\\? -"
        )) |>
        group_by(date_group, question) |>
        mutate(Percent = `Number Agree/Disagree` / sum(`Number Agree/Disagree`) * 100) |>
        filter(Rating == "Agree/Strongly Agree") |>
        group_by(date_group, Rating, question) |>
        summarise(
          Percent = round(sum(Percent), 2),
          Date = Date
        )
    })

    #### OVERALL AGREE PLOT ####

    data_plot_agree <- reactive({
      validate(
        need(!is.null(input$facilitator), "Please select at least one facilitator"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course"),
        need(input$date_min <= input$date_max, "Please select a minimum date that is less than the maximum.")
      )

      agree_plot <- session_survey_recent() |>
        dplyr::filter(between(Date, input$date_min, input$date_max)) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Sites",
          filter_this = input$site,
          dat_filter = `Select your site (district, parish, network, or school).`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Roles",
          filter_this = input$role,
          dat_filter = `Select your role.`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Facilitators",
          filter_this = input$facilitator,
          dat_filter = Facilitator
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Content Areas",
          filter_this = input$content,
          dat_filter = `Select the content area for today’s professional learning session.`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Courses",
          filter_this = input$course,
          dat_filter = `Select your course.`
        ) |>
        select(
          `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.`,
          `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.`,
          `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.`,
          `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.`,
          `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.`) |>
        pivot_longer(everything(), names_to = "Question", values_to = "Response") |>
        drop_na() |>
        dplyr::mutate(Question = str_remove_all(
          Question,
          "How much do you agree with the following statements about this facilitator today\\? - "
        )) |>
        group_by(Question, Response) |>
        count() |>
        ungroup() |>
        group_by(Question) |>
        mutate(Question = str_wrap(Question, width = 30)) |>
        summarise(
          n = n,
          Response = Response,
          Percent = n / sum(n) * 100
        )

      agree_plot
    })
    
    observe(print(data_plot_agree()))

    #### Agree plot n size ####
    agree_plot_n <- reactive({
      data_n <- session_survey_recent() |>
        dplyr::filter(between(Date, input$date_min, input$date_max)) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Sites",
          filter_this = input$site,
          dat_filter = `Select your site (district, parish, network, or school).`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Roles",
          filter_this = input$role,
          dat_filter = `Select your role.`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Facilitators",
          filter_this = input$facilitator,
          dat_filter = Facilitator
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Content Areas",
          filter_this = input$content,
          dat_filter = `Select the content area for today’s professional learning session.`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Courses",
          filter_this = input$course,
          dat_filter = `Select your course.`
        )
      
      nrow(data_n)
    })

    #### QUOTES 1 ####

    quote_viz_data1 <- reactive({
      validate(
        need(!is.null(input$facilitator), "Please select at least one facilitator"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course"),
        need(input$date_min <= input$date_max, "Please select a minimum date that is less than the maximum.")
      )

      quote_reactive1 <- session_survey_recent() |>
        dplyr::filter(between(Date, input$date_min, input$date_max)) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Sites",
          filter_this = input$site,
          dat_filter = `Select your site (district, parish, network, or school).`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Roles",
          filter_this = input$role,
          dat_filter = `Select your role.`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Facilitators",
          filter_this = input$facilitator,
          dat_filter = Facilitator
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Content Areas",
          filter_this = input$content,
          dat_filter = `Select the content area for today’s professional learning session.`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Courses",
          filter_this = input$course,
          dat_filter = `Select your course.`
        ) |>
        select(Facilitation_Feedback) |>
        pivot_longer(everything(), names_to = "Question", values_to = "Response") |>
        drop_na() |>
        filter(Response %!in% na_df) |>
        select(Response) %>%
        {
          if (nrow(.) > 0) slice_sample(., n = ifelse(nrow(.) > 10, 10, nrow(.))) else .
        }
    })

    #### QUOTES 2 ####

    quote_viz_data2 <- reactive({
      
      validate(
        need(!is.null(input$facilitator), "Please select at least one facilitator"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course"),
        need(input$date_min <= input$date_max, "Please select a minimum date that is less than the maximum.")
      )

      quote_reactive2 <- session_survey_recent() |>
        dplyr::filter(between(Date, input$date_min, input$date_max)) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Sites",
          filter_this = input$site,
          dat_filter = `Select your site (district, parish, network, or school).`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Roles",
          filter_this = input$role,
          dat_filter = `Select your role.`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Facilitators",
          filter_this = input$facilitator,
          dat_filter = Facilitator
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Content Areas",
          filter_this = input$content,
          dat_filter = `Select the content area for today’s professional learning session.`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Courses",
          filter_this = input$course,
          dat_filter = `Select your course.`
        ) |>
        select(`What went well in today’s session?`) |>
        pivot_longer(everything(), names_to = "Question", values_to = "Response") |>
        drop_na() |>
        filter(Response %!in% na_df) |>
        select(Response) %>%
        {
          if (nrow(.) > 0) slice_sample(., n = ifelse(nrow(.) > 10, 10, nrow(.))) else .
        }
    })

    #### QUOTES 3 ####

    quote_viz_data3 <- reactive({
      
      validate(
        need(!is.null(input$facilitator), "Please select at least one facilitator"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course"),
        need(input$date_min <= input$date_max, "Please select a minimum date that is less than the maximum.")
      )

      quote_reactive3 <- session_survey_recent() |>
        dplyr::filter(between(Date, input$date_min, input$date_max)) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Sites",
          filter_this = input$site,
          dat_filter = `Select your site (district, parish, network, or school).`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Roles",
          filter_this = input$role,
          dat_filter = `Select your role.`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Facilitators",
          filter_this = input$facilitator,
          dat_filter = Facilitator
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Content Areas",
          filter_this = input$content,
          dat_filter = `Select the content area for today’s professional learning session.`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Courses",
          filter_this = input$course,
          dat_filter = `Select your course.`
        ) |>
        select(`What could have been better about today’s session?`) |>
        pivot_longer(everything(), names_to = "Question", values_to = "Response") |>
        drop_na() |>
        filter(Response %!in% na_df) |>
        select(Response) %>%
        {
          if (nrow(.) > 0) slice_sample(., n = ifelse(nrow(.) > 10, 10, nrow(.))) else .
        }

      quote_reactive3
    })


    #### GENERATE THE PDF REPORT ####

    output$download_report1 <- downloadHandler(
      # For HTML output, change this to "report.html"
      filename = "report.pdf",
      content = function(file) {
        shiny.semantic::with_progress(
          message = "Downloading, please wait! This process generally takes 10-20 seconds.",
          {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            tempFont <- file.path(tempdir(), "calibri.otf")
            file.copy(
              from = "calibri.otf",
              to = tempFont,
              overwrite = TRUE
            )

            # Set up parameters to pass to Rmd document
            params <- list(
              plot_ts = data_plot_ts(),
              plot_agree = data_plot_agree(),
              agree_plot_n = agree_plot_n(),
              quote1 = quote_viz_data1(),
              quote2 = quote_viz_data2(),
              quote3 = quote_viz_data3(),
              subtitle = session_survey_recent() |>
                dplyr::filter(between(Date, input$date_min, input$date_max)) |>
                tlShiny::neg_cond_filter(
                  if_not_this = "All Sites",
                  filter_this = input$site,
                  dat_filter = `Select your site (district, parish, network, or school).`
                ) |>
                tlShiny::neg_cond_filter(
                  if_not_this = "All Roles",
                  filter_this = input$role,
                  dat_filter = `Select your role.`
                ) |>
                tlShiny::neg_cond_filter(
                  if_not_this = "All Facilitators",
                  filter_this = input$facilitator,
                  dat_filter = Facilitator
                ) |>
                tlShiny::neg_cond_filter(
                  if_not_this = "All Content Areas",
                  filter_this = input$content,
                  dat_filter = `Select the content area for today’s professional learning session.`
                ) |>
                tlShiny::neg_cond_filter(
                  if_not_this = "All Courses",
                  filter_this = input$course,
                  dat_filter = `Select your course.`
                ) |>
                select(Facilitator, `Select your site (district, parish, network, or school).`) |>
                transmute(course_fac = paste0(`Select your site (district, parish, network, or school).`, ", ", Facilitator)) |>
                distinct(course_fac) |>
                as_vector() |>
                paste(collapse = ", ")
            )

            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport,
              output_file = file,
              params = params,
              envir = new.env(parent = globalenv())
            )
          }
        )
      }
    )

    #### GENERATE THE DOCX REPORT ####

    output$download_report2 <- downloadHandler(
      # For HTML output, change this to "report.html"
      filename = "report.docx",
      content = function(file) {
        shiny.semantic::with_progress(
          message = "Downloading, please wait! This process generally takes 10-20 seconds.",
          {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "report_word.Rmd")
            tempFont <- file.path(tempdir(), "calibri.otf")
            file.copy("report_word.Rmd", tempReport, overwrite = TRUE)
            file.copy(
              from = "calibri.otf",
              to = tempFont,
              overwrite = TRUE
            )

            # Set up parameters to pass to Rmd document
            params <- list(
              plot_ts = data_plot_ts(),
              plot_agree = data_plot_agree(),
              agree_plot_n = agree_plot_n(),
              quote1 = quote_viz_data1(),
              quote2 = quote_viz_data2(),
              quote3 = quote_viz_data3(),
              subtitle = session_survey_recent() |>
                dplyr::filter(between(Date, input$date_min, input$date_max)) |>
                tlShiny::neg_cond_filter(
                  if_not_this = "All Sites",
                  filter_this = input$site,
                  dat_filter = `Select your site (district, parish, network, or school).`
                ) |>
                tlShiny::neg_cond_filter(
                  if_not_this = "All Roles",
                  filter_this = input$role,
                  dat_filter = `Select your role.`
                ) |>
                tlShiny::neg_cond_filter(
                  if_not_this = "All Facilitators",
                  filter_this = input$facilitator,
                  dat_filter = Facilitator
                ) |>
                tlShiny::neg_cond_filter(
                  if_not_this = "All Content Areas",
                  filter_this = input$content,
                  dat_filter = `Select the content area for today’s professional learning session.`
                ) |>
                tlShiny::neg_cond_filter(
                  if_not_this = "All Courses",
                  filter_this = input$course,
                  dat_filter = `Select your course.`
                ) |>
                select(Facilitator, `Select your site (district, parish, network, or school).`) |>
                transmute(course_fac = paste0(`Select your site (district, parish, network, or school).`, ", ", Facilitator)) |>
                distinct(course_fac) |>
                as_vector() |>
                paste(collapse = ", ")
            )

            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport,
              output_file = file,
              params = params,
              envir = new.env(parent = globalenv())
            )
          }
        )
      }
    )

    download_reactive <- reactive({
      df <- session_survey_recent() |>
        dplyr::filter(between(Date, input$date_min, input$date_max)) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Sites",
          filter_this = input$site,
          dat_filter = `Select your site (district, parish, network, or school).`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Roles",
          filter_this = input$role,
          dat_filter = `Select your role.`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Facilitators",
          filter_this = input$facilitator,
          dat_filter = Facilitator
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Content Areas",
          filter_this = input$content,
          dat_filter = `Select the content area for today’s professional learning session.`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Courses",
          filter_this = input$course,
          dat_filter = `Select your course.`
        )

      df
    })

    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("session_survey_data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        shiny.semantic::with_progress(
          message = "Downloading, please wait! This process generally takes 10-20 seconds.",
          {
            write.csv(download_reactive(), file)
          }
        )
      }
    )
  })
}
