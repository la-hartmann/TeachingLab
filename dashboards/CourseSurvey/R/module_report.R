#### Course Survey Dashboard ####
uiReport <- function(id, label = "Counter") {
  ns <- NS(id)
  shiny::tagList(
    # tags$head(tags$style(HTML("#sidebar_panel{min-width:320px;padding:5px;height:100vh;}"))),
    shiny.semantic::sidebar_layout(
      sidebar_panel = shiny.semantic::sidebar_panel(
        style = sidebar_style,
        menu_item(
          tabName = "recent_menu",
          h5("Utilize the filters in the sidebar to download", style = "margin: calc(2rem - 3.1em) 0 1rem; margin-top:2px;"),
          h5("your custom report,", style = "margin: calc(2rem - 3.1em) 0 1rem"),
          h5("OR click one of the following recent", style = "margin: calc(2rem - 3.1em) 0 1rem"),
          h5("sessions found by a unique ", style = "margin: calc(2rem - 3.1em) 0 1rem"),
          h5("combination of site and course,", style = "margin: calc(2rem - 3.1em) 0 1rem"),
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
        menu_item(
          tabName = "site_menu",
          shiny::selectizeInput(
            inputId = ns("site"),
            label = h3("Select a site", style = "font-weight: bold;"),
            choices = course_survey$`Select your site (district, parish, network, or school).` |>
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
        menu_item(
          tabName = "role_menu",
          shiny::selectizeInput(
            inputId = ns("role"),
            label = h3("Select a role", style = "font-weight: bold;"),
            choices = course_survey$`Select your role.` |>
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
        menu_item(
          tabName = "content_menu",
          shiny::selectizeInput(
            inputId = ns("content"),
            label = h3("Select a content area", style = "font-weight: bold;"),
            choices = course_survey$`Select the content area for today's professional learning session.` |>
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
        menu_item(
          tabName = "course_menu",
          shiny::selectizeInput(
            inputId = ns("course"),
            label = h3("Select a course", style = "font-weight: bold;"),
            choices = course_survey$`Select your course.` |>
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
        menu_item(
          tabName = "date_slider_menu",
          split_layout(
            cell_widths = c("50%", "50%"),
            cell_args = "padding: 5px;",
            style = "background-color: transparent;",
            shinyWidgets::airDatepickerInput(
              inputId = ns("date_min"),
              label = h3("Select minimum date"),
              value = as.Date("2021-06-30"),
              minDate = min(as.Date(course_survey$date_created), na.rm = T),
              maxDate = max(as.Date(course_survey$date_created), na.rm = T) + 1,
              dateFormat = "mm-dd-yyyy",
              width = "100px"
            ),
            shinyWidgets::airDatepickerInput(
              inputId = ns("date_max"),
              label = h3("Select maximum date"),
              value = Sys.Date(),
              minDate = min(as.Date(course_survey$date_created), na.rm = T),
              maxDate = Sys.Date(),
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
                tags$img(
                  src = "report_example_pdf.png",
                  align = "center",
                  style = report_style
                )
              ),
              icon = shiny::icon("file-download")
            ) |>
              withSpinner(type = 3, color.background = "white"),
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
                tags$img(
                  src = "report_example_word.png",
                  align = "center",
                  style = report_style
                )
              ),
              icon = shiny::icon("file-download")
            ) |>
              withSpinner(type = 3, color.background = "white")
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
                tags$img(
                  src = "fake_raw.png",
                  align = "center",
                  style = report_style
                )
              ),
              icon = shiny::icon("save")
            )
          )
        )
      )
    )
  )
}

reportServer <- function(id, in_site) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {

    #### Applying Recent Groupings Logic ####

    course_survey_recent <- reactive({
      if (!is.null(input$recent_sessions)) {

        # Get previously created choices dataframe for filtering by id
        chosen <- recent_choices |>
          inner_join((recent_choices_final |>
            dplyr::filter(choice %in% input$recent_sessions)), by = "id")

        course_survey <- course_survey |>
          dplyr::filter(`Select your site (district, parish, network, or school).` %in% chosen$`Select your site (district, parish, network, or school).` &
            `Select your course.` %in% chosen$`Select your course.`)
      } else {
        course_survey
      }
    })

    #### TIME SERIES INPUTS ####
    data_plot_ts <- reactive({
      validate(
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )

      data_plot <- course_survey_recent() |>
        dplyr::filter(between(date_created, input$date_min, input$date_max)) |>
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
          if_not_this = "All Content Areas",
          filter_this = input$content,
          dat_filter = `Select the content area for today's professional learning session.`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Courses",
          filter_this = input$course,
          dat_filter = `Select your course.`
        ) |>
        select(
          date_created,
          `How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`,
          `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`,
          `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course.`,
          `How much do you agree with the following statements about this course? - The strategies I’ve learned in this course will improve my instruction.`,
          `How much do you agree with the following statements about this course? - The strategies I’ve learned in this course will improve my coaching or supervision of teachers.`,
          `How much do you agree with the following statements about this course? - The strategies I’ve learned in the course are easy to implement.`,
          `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice.`,
          `How much do you agree with the following statements about this course? - This course has supported me in being responsive to students' backgrounds, cultures, and points of view.`
        ) |>
        pivot_longer(!`date_created`, names_to = "question", values_to = "answer") |>
        dplyr::mutate(question = str_remove_all(
          question,
          "How much do you agree with the following statements about this course\\? - "
        )) |>
        ### Rename with line breaks every 27 characters ###
        mutate(question = gsub("(.{28,}?)\\s", "\\1\n", question)) |>
        drop_na(answer) |>
        dplyr::group_by(question, date_created) |>
        ### Group by input variable ###
        mutate(`Number Agree/Disagree` = n()) |>
        mutate(answer = str_remove_all(answer, "\\([:digit:]\\) ")) |>
        mutate(
          Rating = case_when(
            answer %in% c("Agree", "Strongly agree") ~ "Agree/Strongly Agree",
            answer %in% c("Neither agree nor disagree", "Disagree", "Strongly disagree") ~ "Neither/Disagree/Strongly Disagree"
          ),
          date_group = case_when(
            as.integer(diff(range(as.Date(input$date_min), as.Date(input$date_max)))) >= 30 ~ paste0(lubridate::month(date_created, label = T, abbr = F), ", ", year(date_created)),
            as.integer(diff(range(as.Date(input$date_min), as.Date(input$date_max)))) >= 14 & as.integer(diff(range(as.Date(input$date_min), as.Date(input$date_max)))) < 30 ~ paste0(year(date_created), lubridate::week(date_created)),
            as.integer(diff(range(as.Date(input$date_min), as.Date(input$date_max)))) < 14 ~ paste0(lubridate::day(date_created))
          )
        ) |>
        ungroup() |>
        group_by(date_group, question) |>
        mutate(Percent = `Number Agree/Disagree` / sum(`Number Agree/Disagree`) * 100) |>
        filter(Rating == "Agree/Strongly Agree") |>
        group_by(date_group, Rating, question) |>
        summarise(
          Percent = round(sum(Percent), 2),
          date_created = date_created
        )

      data_plot
    })

    #### OVERALL AGREE PLOT ####

    data_plot_agree <- reactive({
      validate(
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )

      agree_plot <- course_survey_recent() |>
        dplyr::filter(between(date_created, input$date_min, input$date_max)) |>
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
          if_not_this = "All Content Areas",
          filter_this = input$content,
          dat_filter = `Select the content area for today's professional learning session.`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Courses",
          filter_this = input$course,
          dat_filter = `Select your course.`
        ) |>
        select(
          `How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`,
          `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`,
          `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course.`,
          `How much do you agree with the following statements about this course? - The strategies I’ve learned in this course will improve my instruction.`,
          `How much do you agree with the following statements about this course? - The strategies I’ve learned in this course will improve my coaching or supervision of teachers.`,
          `How much do you agree with the following statements about this course? - The strategies I’ve learned in the course are easy to implement.`,
          `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice.`,
          `How much do you agree with the following statements about this course? - This course has supported me in being responsive to students' backgrounds, cultures, and points of view.`
        ) |>
        pivot_longer(everything(), names_to = "Question", values_to = "Response") |>
        drop_na() |>
        dplyr::mutate(Question = str_remove_all(
          Question,
          "How much do you agree with the following statements about this course\\? - "
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

    #### Agree plot n size ####
    agree_plot_n <- reactive({
      data_n <- course_survey_recent() |>
        dplyr::filter(between(date_created, input$date_min, input$date_max)) |>
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
          if_not_this = "All Content Areas",
          filter_this = input$content,
          dat_filter = `Select the content area for today's professional learning session.`
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
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )

      quote_reactive1 <- course_survey_recent() |>
        dplyr::filter(between(date_created, input$date_min, input$date_max)) |>
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
          if_not_this = "All Content Areas",
          filter_this = input$content,
          dat_filter = `Select the content area for today's professional learning session.`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Courses",
          filter_this = input$course,
          dat_filter = `Select your course.`
        ) |>
        select(`Overall, what went well in this course?`) |>
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
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )

      quote_reactive2 <- course_survey_recent() |>
        dplyr::filter(between(date_created, input$date_min, input$date_max)) |>
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
          if_not_this = "All Content Areas",
          filter_this = input$content,
          dat_filter = `Select the content area for today's professional learning session.`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Courses",
          filter_this = input$course,
          dat_filter = `Select your course.`
        ) |>
        select(`Overall, what could have been better in this course?`) |>
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
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )

      quote_reactive3 <- course_survey_recent() |>
        dplyr::filter(between(date_created, input$date_min, input$date_max)) |>
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
          if_not_this = "All Content Areas",
          filter_this = input$content,
          dat_filter = `Select the content area for today's professional learning session.`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Courses",
          filter_this = input$course,
          dat_filter = `Select your course.`
        ) |>
        select(`What is the learning from this course that you are most excited about trying out?`) |>
        pivot_longer(everything(), names_to = "Question", values_to = "Response") |>
        drop_na() |>
        filter(Response %!in% na_df) |>
        select(Response) %>%
        {
          if (nrow(.) > 0) slice_sample(., n = ifelse(nrow(.) > 10, 10, nrow(.))) else .
        }

      quote_reactive3
    })

    #### QUOTES 4 ####

    quote_viz_data4 <- reactive({
      validate(
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )

      quote_reactive4 <- course_survey_recent() |>
        dplyr::filter(between(date_created, input$date_min, input$date_max)) |>
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
          if_not_this = "All Content Areas",
          filter_this = input$content,
          dat_filter = `Select the content area for today's professional learning session.`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Courses",
          filter_this = input$course,
          dat_filter = `Select your course.`
        ) |>
        select(`Which activities best supported your learning in this course?`) |>
        pivot_longer(everything(), names_to = "Question", values_to = "Response") |>
        drop_na() |>
        filter(Response %!in% na_df) |>
        select(Response) %>%
        {
          if (nrow(.) > 0) slice_sample(., n = ifelse(nrow(.) > 10, 10, nrow(.))) else .
        }

      quote_reactive4
    })

    #### QUOTES 5 ####

    quote_viz_data5 <- reactive({
      
      validate(
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )

      quote_reactive5 <- course_survey_recent() |>
        dplyr::filter(between(date_created, input$date_min, input$date_max)) |>
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
          if_not_this = "All Content Areas",
          filter_this = input$content,
          dat_filter = `Select the content area for today's professional learning session.`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Courses",
          filter_this = input$course,
          dat_filter = `Select your course.`
        ) |>
        select(`Feel free to leave us any additional comments, concerns, or questions.`) |>
        pivot_longer(everything(), names_to = "Question", values_to = "Response") |>
        drop_na() |>
        filter(Response %!in% na_df) |>
        select(Response) %>%
        {
          if (nrow(.) > 0) slice_sample(., n = ifelse(nrow(.) > 10, 10, nrow(.))) else .
        }

      quote_reactive5
    })
    
    data_plot_nps <- reactive({
      
      validate(
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )
      
      reactive_nps <- course_survey |>
        dplyr::filter(between(date_created, input$date_min, input$date_max)) |>
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
          if_not_this = "All Content Areas",
          filter_this = input$content,
          dat_filter = `Select the content area for today's professional learning session.`
        ) |>
        tlShiny::neg_cond_filter(
          if_not_this = "All Courses",
          filter_this = input$course,
          dat_filter = `Select your course.`
        ) |>
        mutate(`On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?` = readr::parse_number(as.character(`On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`))) |>
        mutate(`On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?` = na_if(`On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`, "No Response")) |>
        summarise(nps = calc_nps(suppressWarnings(as.numeric(`On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`)))) |>
        mutate(
          x = 0,
          y = nps
        )
      
      #### Return the data ####
      reactive_nps
      
    })

    #### Error handling for either of the reports, or the csv
    # observeEvent(c(input$site, input$role, input$content, input$course, input$date_created), {
    #   if (nrow(download_reactive()) == 0) {
    #     shinyjs::hide("download_report1")
    #     shinyjs::hide("download_report2")
    #     shinyjs::hide("download_csv")
    #   } else {
    #     shinyjs::show("download_report1")
    #     shinyjs::show("download_report2")
    #     shinyjs::show("download_csv")
    #   }
    # })
  
  

    ### Generate csv download data ###
    download_reactive <- reactive({
      df <- course_survey_recent() |>
        dplyr::filter(between(date_created, input$date_min, input$date_max)) |>
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
          if_not_this = "All Content Areas",
          filter_this = input$content,
          dat_filter = `Select the content area for today's professional learning session.`
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
        paste0("course_survey_data-", Sys.Date(), ".csv", sep = "")
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

    #### GENERATE THE PDF REPORT ####

    output$download_report1 <- downloadHandler(

      # For HTML output, change this to "report.html"
      filename = function() {
        if (!is.null(input$recent_sessions)) {
          download_reactive() |>
            select(`Select your course.`, `Select your site (district, parish, network, or school).`) |>
            transmute(course_site = paste0(`Select your course.`, " ", `Select your site (district, parish, network, or school).`)) |>
            distinct(course_site) |>
            as_vector() |>
            str_replace_all(c(
              " " = "_",
              "," = "_"
            )) |>
            paste0(".pdf", collapse = "_")
        } else {
          paste0(unique(input$site), "_", Sys.Date()) |>
            str_replace_all(c(
              " " = "_",
              "," = "_",
              "-" = "_"
            )) |>
            paste0(".pdf")
        }
      },
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
              plot_nps = data_plot_nps(),
              quote1 = quote_viz_data1(),
              quote2 = quote_viz_data2(),
              quote3 = quote_viz_data3(),
              quote4 = quote_viz_data4(),
              quote5 = quote_viz_data5(),
              subtitle = download_reactive() |>
                select(`Select your course.`, `Select your site (district, parish, network, or school).`) |>
                transmute(course_site = paste0(`Select your course.`, ", ", `Select your site (district, parish, network, or school).`)) |>
                distinct(course_site) |>
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
              plot_nps = data_plot_nps(),
              quote1 = quote_viz_data1(),
              quote2 = quote_viz_data2(),
              quote3 = quote_viz_data3(),
              quote4 = quote_viz_data4(),
              quote5 = quote_viz_data5(),
              subtitle = download_reactive() |>
                select(`Select your course.`, `Select your site (district, parish, network, or school).`) |>
                transmute(course_site = paste0(`Select your course.`, ", ", `Select your site (district, parish, network, or school).`)) |>
                distinct(course_site) |>
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
  })
}
