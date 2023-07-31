uiReport <- function(id, label = "Counter") {
  ns <- NS(id)
  shiny::tagList(
    # tags$head(tags$style(HTML("#sidebar_panel{min-width:320px;padding:5px;height:100vh;}"))),
    shiny.semantic::sidebar_layout(
      sidebar_panel = shiny.semantic::sidebar_panel(
        style = "position:fixed;overflow-x:auto;overflow-y:auto;width:inherit;max-width:330px;",
        menu_item(
          tabName = "site_menu",
          shiny::selectizeInput(
            inputId = ns("site"),
            label = h3("Select a site", style = "font-weight: bold;"),
            choices = course_survey$`Select your site (district, parish, network, or school).` %>%
              unique() %>%
              as.character() %>%
              sort() %>%
              purrr::prepend("All Sites"),
            multiple = T,
            selected = "All Sites",
            options = list(plugins= list('remove_button'))
          ),
          icon = shiny.semantic::icon("location arrow")
        ),
        br(),
        menu_item(
          tabName = "role_menu",
          shiny::selectizeInput(
            inputId = ns("role"),
            label = h3("Select a role", style = "font-weight: bold;"),
            choices = course_survey$`Select your role.` %>%
              unique() %>%
              as.character() %>%
              sort() %>%
              purrr::prepend("All Roles"),
            multiple = T,
            selected = "All Roles",
            options = list(plugins= list('remove_button'))
          ),
          icon = shiny.semantic::icon("user")
        ),
        br(),
        menu_item(
          tabName = "course_menu",
          shiny::selectizeInput(
            inputId = ns("course"),
            label = h3("Select a course", style = "font-weight: bold;"),
            choices = course_survey$`Select your course.` %>%
              unique() %>%
              as.character() %>%
              sort() %>%
              purrr::prepend("All Courses"),
            multiple = T,
            selected = "All Courses",
            options = list(plugins= list('remove_button'))
          ),
          icon = shiny.semantic::icon("clone")
        ),
        br(),
        menu_item(
          tabName = "grade_menu",
          shiny::selectizeInput(
            inputId = ns("grades"),
            label = h3("Select grades to include", style = "font-weight: bold;"),
            choices = 1:12 %>%
              as.character() %>%
              purrr::prepend(c("All Grades", "K")),
            multiple = T,
            selected = "All Grades",
            options = list(plugins = list('remove_button'))
          ),
          icon = shiny.semantic::icon("graduation cap")
        ),
        br(),
        menu_item(
          tabName = "date_slider_menu",
          shiny::sliderInput(
            inputId = ns("date_slider"),
            label = h3("Select a date range", style = "font-weight: bold;"),
            value = c(
              as.Date("2021-06-30"),
              max(as.Date(course_survey$date_created), na.rm = T)
            ),
            min = min(as.Date(course_survey$date_created), na.rm = T),
            max = max(as.Date(course_survey$date_created), na.rm = T),
            timeFormat = "%b %d, %Y"
          ),
          icon = shiny.semantic::icon("calendar alternate")
        )
      ),
      main_panel = shiny.semantic::main_panel(
        width = 4,
        div(
          class = "ui three column stackable grid container",
          div(
            class = "seven wide column",
            br(),
            br(),
            h3("Utilize the filters in the sidebar to download your custom report,"),
            h3(" OR click one of the following buttons to download one of the following sessions found by a unique combination of site and course, and then just click download!"),
            selectizeInput(ns("recent_sessions"), choices = recent_choices_final$choice, 
                           label = NULL, selected = NULL, multiple = T,
                           options = list(plugins= list('remove_button'))),
            br(),
            br(),
            br(),
            br(),
            br(),
            br()
          ),
          div(
            class = "five wide column",
            align = "center",
            br(),
            br(),
            br(),
            downloadButton(ns("download_report1"), 
                           label = tagList(tags$span("Download Report PDF"),
                                           tags$img(src = "report_example_pdf.png", align = "center")), 
                           icon = shiny::icon("file-download")) %>%
              withSpinner(),
            br(),
            br(),
            br(),
            br(),
            br(),
            downloadButton(ns("download_report2"), 
                           label = tagList(tags$span("Download Report DOCX"),
                                           tags$img(src = "report_example_word.png", align = "center")), 
                           icon = shiny::icon("file-download")) %>%
              withSpinner()
          ),
          div(
            class = "four wide column",
            align = "center",
            br(),
            br(),
            downloadButton(ns("download_csv"), 
                           label = tagList(tags$span("  Download a CSV of Raw Data"),
                                           tags$img(src = "fake_raw.png", align = "center")), 
                           icon = shiny::icon("save"))
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
        chosen <- recent_choices %>% 
          inner_join((recent_choices_final %>%
                      dplyr::filter(choice %in% input$recent_sessions)), by = "id")
      
        course_survey <- course_survey %>%
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
        need(!is.null(input$course), "Please select at least one course"),
        need(!is.null(input$grades), "Please select at least one grade")
      )
      
      data_plot <- course_survey_recent() %>%
        dplyr::filter(between(date_created, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (any(input$site != "All Sites")) dplyr::filter(., `Select your site (district, parish, network, or school).` %in% input$site) else .
        } %>%
        {
          if (any(input$role != "All Roles")) dplyr::filter(., `Select your role.` %in% input$role) else .
        } %>%
        {
          if (any(input$course != "All Courses")) dplyr::filter(., `Select your course.` %in% input$course) else .
        } %>%
        {
          if (any(input$grades != "All Grades")) dplyr::filter(., stringr::str_detect(grades_merge, grade_select())) else .
        } %>%
        select(
          date_created,
          c(
            "How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.",
            "How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.",
            "How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.",
            "How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course. even though we were meeting virtually.",
            "How much do you agree with the following statements about this course? - The strategies I’ve learned in this course will improve my instruction.",
            "How much do you agree with the following statements about this course? - The strategies I’ve learned in this course will improve my coaching or supervision of teachers.",
            "How much do you agree with the following statements about this course? - The strategies I’ve learned in the course are easy to implement.",
            "How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks.",
            "How much do you agree with the following statements about this course? - This course has supported me in being responsive to students' backgrounds, cultures, and points of view."
          )
        ) %>%
        pivot_longer(!`date_created`, names_to = "question", values_to = "answer") %>%
        dplyr::mutate(question = str_remove_all(
          question,
          "How much do you agree with the following statements about this course\\? - "
        )) %>%
        # Rename with line breaks every 27 characters
        mutate(question = gsub("(.{28,}?)\\s", "\\1\n", question)) %>%
        drop_na(answer) %>%
        dplyr::group_by(question, date_created) %>%
        # Group by input variable
        mutate(`Number Agree/Disagree` = n()) %>%
        mutate(answer = str_remove_all(answer, "\\([:digit:]\\) ")) %>%
        mutate(
          Rating = case_when(
            answer %in% c("Agree", "Strongly agree") ~ "Agree/Strongly Agree",
            answer %in% c("Neither agree nor disagree", "Disagree", "Strongly disagree") ~ "Neither/Disagree/Strongly Disagree"
          ),
          date_group = case_when(as.integer(diff(range(as.Date(input$date_slider)))) >= 30 ~ paste0(lubridate::month(date_created, label = T, abbr = F), ", ", year(date_created)),
                                 as.integer(diff(range(as.Date(input$date_slider)))) >= 14 & as.integer(diff(range(as.Date(input$date_slider)))) < 30 ~ paste0(year(date_created), lubridate::week(date_created)),
                                 as.integer(diff(range(as.Date(input$date_slider)))) < 14 ~ paste0(lubridate::day(date_created)))
        ) %>%
        ungroup() %>%
        group_by(date_group, question) %>%
        mutate(Percent = `Number Agree/Disagree` / sum(`Number Agree/Disagree`) * 100) %>%
        filter(Rating == "Agree/Strongly Agree") %>%
        group_by(date_group, Rating, question) %>%
        summarise(Percent = round(sum(Percent), 2),
                  date_created = date_created)

    })

    #### OVERALL AGREE PLOT ####

    data_plot_agree <- reactive({
      
      validate(
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$course), "Please select at least one course"),
        need(!is.null(input$grades), "Please select at least one grade")
      )
      
      agree_plot <- course_survey_recent() %>%
        dplyr::filter(between(date_created, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (any(input$site != "All Sites")) dplyr::filter(., `Select your site (district, parish, network, or school).` %in% input$site) else .
        } %>%
        {
          if (any(input$role != "All Roles")) dplyr::filter(., `Select your role.` %in% input$role) else .
        } %>%
        {
          if (any(input$course != "All Courses")) dplyr::filter(., `Select your course.` %in% input$course) else .
        } %>%
        {
          if (any(input$grades != "All Grades")) dplyr::filter(., stringr::str_detect(grades_merge, grade_select())) else .
        } %>%
        select(c(
          "How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.",
          "How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.",
          "How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.",
          "How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course. even though we were meeting virtually.",
          "How much do you agree with the following statements about this course? - The strategies I’ve learned in this course will improve my instruction.",
          "How much do you agree with the following statements about this course? - The strategies I’ve learned in this course will improve my coaching or supervision of teachers.",
          "How much do you agree with the following statements about this course? - The strategies I’ve learned in the course are easy to implement.",
          "How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks.",
          "How much do you agree with the following statements about this course? - This course has supported me in being responsive to students' backgrounds, cultures, and points of view."
        )) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        drop_na() %>%
        dplyr::mutate(Question = str_remove_all(
          Question,
          "How much do you agree with the following statements about this course\\? - "
        )) %>%
        group_by(Question, Response) %>%
        count() %>%
        ungroup() %>%
        group_by(Question) %>%
        mutate(Question = str_wrap(Question, width = 30)) %>%
        summarise(
          n = n,
          Response = Response,
          Percent = n / sum(n) * 100
        )
      
      agree_plot
    })
    
    #### Agree plot n size ####
    agree_plot_n <- reactive({
      data_n <- course_survey_recent() %>%
        dplyr::filter(between(date_created, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (any(input$site != "All Sites")) dplyr::filter(., `Select your site (district, parish, network, or school).` %in% input$site) else .
        } %>%
        {
          if (any(input$role != "All Roles")) dplyr::filter(., `Select your role.` %in% input$role) else .
        } %>%
        {
          if (any(input$course != "All Courses")) dplyr::filter(., `Select your course.` %in% input$course) else .
        } %>%
        {
          if (any(input$grades != "All Grades")) dplyr::filter(., stringr::str_detect(grades_merge, grade_select())) else .
        }
      nrow(data_n)
    })

    #### QUOTES 1 ####

    quote_viz_data1 <- reactive({

      validate(
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$course), "Please select at least one course"),
        need(!is.null(input$grades), "Please select at least one grade")
      )

      quote_reactive1 <- course_survey_recent() %>%
        dplyr::filter(between(date_created, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (any(input$site != "All Sites")) dplyr::filter(., `Select your site (district, parish, network, or school).` %in% input$site) else .
        } %>%
        {
          if (any(input$role != "All Roles")) dplyr::filter(., `Select your role.` %in% input$role) else .
        } %>%
        {
          if (any(input$course != "All Courses")) dplyr::filter(., `Select your course.` %in% input$course) else .
        } %>%
        {
          if (any(input$grades != "All Grades")) dplyr::filter(., stringr::str_detect(grades_merge, grade_select())) else .
        } %>%
        select(`Overall, what went well in this course?`) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        drop_na() %>%
        filter(Response %!in% na_df) %>%
        select(Response) %>%
        slice_sample(n = 10)
    })

    #### QUOTES 2 ####

    quote_viz_data2 <- reactive({

      validate(
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$course), "Please select at least one course"),
        need(!is.null(input$grades), "Please select at least one grade")
      )

      quote_reactive2 <- course_survey_recent() %>%
        dplyr::filter(between(date_created, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (any(input$site != "All Sites")) dplyr::filter(., `Select your site (district, parish, network, or school).` %in% input$site) else .
        } %>%
        {
          if (any(input$role != "All Roles")) dplyr::filter(., `Select your role.` %in% input$role) else .
        } %>%
        {
          if (any(input$course != "All Courses")) dplyr::filter(., `Select your course.` %in% input$course) else .
        } %>%
        {
          if (any(input$grades != "All Grades")) dplyr::filter(., stringr::str_detect(grades_merge, grade_select())) else .
        } %>%
        select(`Overall, what could have been better in this course?`) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        drop_na() %>%
        filter(Response %!in% na_df) %>%
        select(Response) %>%
        slice_sample(n = 10)
    })

    #### QUOTES 3 ####

    quote_viz_data3 <- reactive({

      validate(
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$course), "Please select at least one course"),
        need(!is.null(input$grades), "Please select at least one grade")
      )

      quote_reactive3 <- course_survey_recent() %>%
        dplyr::filter(between(date_created, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (any(input$site != "All Sites")) dplyr::filter(., `Select your site (district, parish, network, or school).` %in% input$site) else .
        } %>%
        {
          if (any(input$role != "All Roles")) dplyr::filter(., `Select your role.` %in% input$role) else .
        } %>%
        {
          if (any(input$course != "All Courses")) dplyr::filter(., `Select your course.` %in% input$course) else .
        } %>%
        {
          if (any(input$grades != "All Grades")) dplyr::filter(., stringr::str_detect(grades_merge, grade_select())) else .
        } %>%
        select(`What is the learning from this course that you are most excited about trying out?`) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        drop_na() %>%
        filter(Response %!in% na_df) %>%
        select(Response) %>%
        slice_sample(n = 10)
      
      quote_reactive3
    })
    
    #### QUOTES 4 ####
    
    quote_viz_data4 <- reactive({
      validate(
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$course), "Please select at least one course"),
        need(!is.null(input$grades), "Please select at least one grade")
      )
      quote_reactive4 <- course_survey_recent() %>%
        dplyr::filter(between(date_created, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (any(input$site != "All Sites")) dplyr::filter(., `Select your site (district, parish, network, or school).` %in% input$site) else .
        } %>%
        {
          if (any(input$role != "All Roles")) dplyr::filter(., `Select your role.` %in% input$role) else .
        } %>%
        {
          if (any(input$course != "All Courses")) dplyr::filter(., `Select your course.` %in% input$course) else .
        } %>%
        {
          if (any(input$grades != "All Grades")) dplyr::filter(., stringr::str_detect(grades_merge, grade_select())) else .
        } %>%
        select(`Which activities best supported your learning in this course?`) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        drop_na() %>%
        filter(Response %!in% na_df) %>%
        select(Response) %>%
        slice_sample(n = 10)
      
      quote_reactive4
    })
    
    #### QUOTES 5 ####
    
    quote_viz_data5 <- reactive({
      validate(
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$course), "Please select at least one course"),
        need(!is.null(input$grades), "Please select at least one grade")
      )
      quote_reactive5 <- course_survey_recent() %>%
        dplyr::filter(between(date_created, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (any(input$site != "All Sites")) dplyr::filter(., `Select your site (district, parish, network, or school).` %in% input$site) else .
        } %>%
        {
          if (any(input$role != "All Roles")) dplyr::filter(., `Select your role.` %in% input$role) else .
        } %>%
        {
          if (any(input$course != "All Courses")) dplyr::filter(., `Select your course.` %in% input$course) else .
        } %>%
        {
          if (any(input$grades != "All Grades")) dplyr::filter(., stringr::str_detect(grades_merge, grade_select())) else .
        } %>%
        select(`Feel free to leave us any additional comments, concerns, or questions.`) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        drop_na() %>%
        filter(Response %!in% na_df) %>%
        select(Response) %>%
        slice_sample(n = 10)
      
      quote_reactive5
    })


    #### GENERATE THE PDF REPORT ####

    output$download_report1 <- downloadHandler(
      # For HTML output, change this to "report.html"
      filename = "report.pdf",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        tempFont <- file.path(tempdir(), "calibri.otf")
        file.copy(from = "calibri.otf",
                  to = tempFont,
                  overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(plot_ts = data_plot_ts(),
                       plot_agree = data_plot_agree(),
                       agree_plot_n = agree_plot_n(),
                       quote1 = quote_viz_data1(),
                       quote2 = quote_viz_data2(),
                       quote3 = quote_viz_data3(),
                       quote4 = quote_viz_data4(),
                       quote5 = quote_viz_data5(),
                       subtitle = course_survey_recent() %>% 
                         {
                           if (any(input$site != "All Sites")) dplyr::filter(., `Select your site (district, parish, network, or school).` %in% input$site) else .
                         } %>%
                         select(`Select your course.`, `Select your site (district, parish, network, or school).`) %>% 
                         transmute(course_site = paste0(`Select your course.`, ", ", `Select your site (district, parish, network, or school).`)) %>%
                         distinct(course_site) %>%
                         as_vector() %>%
                         paste(collapse = ", "))

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
    
    # observe({
    #   course_survey_recent() %>% 
    #     select(`Select your course.`, `Select your site (district, parish, network, or school).`) %>% 
    #     transmute(course_site = paste0(`Select your course.`, ", ", `Select your site (district, parish, network, or school).`)) %>%
    #     distinct(course_site) %>%
    #     # {
    #     #   if (length(course_site) > 3) dplyr::slice(., 1:3) %>% bind_rows(tibble::tibble(course_site = "and more")) else .
    #     # } %>%
    #     as_vector() %>%
    #     paste(collapse = ", ") %>%
    #     print()
    # })
    
    #### GENERATE THE DOCX REPORT ####
    
    output$download_report2 <- downloadHandler(
      # For HTML output, change this to "report.html"
      filename = "report.docx",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report_word.Rmd")
        tempFont <- file.path(tempdir(), "calibri.otf")
        file.copy("report_word.Rmd", tempReport, overwrite = TRUE)
        file.copy(from = "calibri.otf",
                  to = tempFont,
                  overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(plot_ts = data_plot_ts(),
                       plot_agree = data_plot_agree(),
                       agree_plot_n = agree_plot_n(),
                       quote1 = quote_viz_data1(),
                       quote2 = quote_viz_data2(),
                       quote3 = quote_viz_data3(),
                       quote4 = quote_viz_data4(),
                       quote5 = quote_viz_data5(),
                       subtitle = course_survey_recent() %>% 
                         dplyr::filter(between(date_created, input$date_slider[1], input$date_slider[2])) %>%
                         {
                           if (any(input$site != "All Sites")) dplyr::filter(., `Select your site (district, parish, network, or school).` %in% input$site) else .
                         } %>%
                         {
                           if (any(input$role != "All Roles")) dplyr::filter(., `Select your role.` %in% input$role) else .
                         } %>%
                         {
                           if (any(input$course != "All Courses")) dplyr::filter(., `Select your course.` %in% input$course) else .
                         } %>%
                         {
                           if (any(input$grades != "All Grades")) dplyr::filter(., stringr::str_detect(grades_merge, grade_select())) else .
                         } %>%
                         select(`Select your course.`, `Select your site (district, parish, network, or school).`) %>% 
                         transmute(course_site = paste0(`Select your course.`, ", ", `Select your site (district, parish, network, or school).`)) %>%
                         distinct(course_site) %>%
                         as_vector() %>%
                         paste(collapse = ", "))
        
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
    
    download_reactive <- reactive({
      
      if (input$recent_sessions != "No Selection") {
        df <- course_survey_recent() %>%
          dplyr::filter(between(date_created, input$date_slider[1], input$date_slider[2])) %>%
          {
            if (any(input$site != "All Sites")) dplyr::filter(., `Select your site (district, parish, network, or school).` %in% input$site) else .
          } %>%
          {
            if (any(input$role != "All Roles")) dplyr::filter(., `Select your role.` %in% input$role) else .
          } %>%
          {
            if (any(input$course != "All Courses")) dplyr::filter(., `Select your course.` %in% input$course) else .
          } %>%
          {
            if (any(input$grades != "All Grades")) dplyr::filter(., stringr::str_detect(grades_merge, grade_select())) else .
          }
      } else {
        df <- course_survey_recent()
      }
      
      df
    })
    
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("course_survey_data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(download_reactive(), file)
      }
    )
    
    # observeEvent(
    #   session$userData$settings$in_site(), 
    #   {
    #     if (!is.na(session$userData$settings$in_site())) 
    #       updateSelectizeInput(
    #         session, 
    #         "site", 
    #         selected=session$userData$settings$in_site()
    #       )
    #   }
    # )
    
    # return(
    #   list(
    #     in_site <- reactive({input$site}),
    #     in_course <- reactive({input$course}),
    #     in_role <- reactive({input$role}),
    #     in_date_slider <- reactive({input$date_slider})
    #   )
    # )
    

  })
}
