uiReport <- function(id, label = "Counter") {
  ns <- NS(id)
  shiny::tagList(
    shiny.semantic::sidebar_layout(
      sidebar_panel = shiny.semantic::sidebar_panel(
        style = "position:fixed;overflow-x:hidden;overflow-y:scroll;width:inherit;",
        shiny.semantic::menu_item(
          tabName = "facilitator_menu",
          shiny::selectizeInput(
            inputId = ns("facilitator"),
            label = h3("Select a facilitator"),
            choices = session_survey$Facilitator %>%
              unique() %>%
              as.character() %>%
              sort() %>%
              purrr::prepend("All Facilitators"),
            multiple = T,
            selected = "All Facilitators",
            options = list(plugins= list('remove_button'))
          ),
          icon = shiny.semantic::icon("clone")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "content_menu",
          shiny::selectizeInput(
            inputId = ns("content"),
            label = h3("Select a content area"),
            choices = session_survey$`Select the content area for today’s professional learning session.` %>%
              unique() %>%
              as.character() %>%
              sort() %>%
              purrr::prepend("All Content Areas"),
            multiple = T,
            selected = "All Content Areas",
            options = list(plugins= list('remove_button'))
          ),
          icon = shiny.semantic::icon("edit")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "course_menu",
          shiny::selectizeInput(
            inputId = ns("course"),
            label = h3("Select a course"),
            choices = session_survey$`Select your course.` %>%
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
        shiny.semantic::menu_item(
          tabName = "site_menu",
          shiny::selectizeInput(
            inputId = ns("site"),
            label = h3("Select a site"),
            choices = session_survey$`Select your site (district, parish, network, or school).` %>%
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
        shiny.semantic::menu_item(
          tabName = "role_menu",
          shiny::selectizeInput(
            inputId = ns("role"),
            label = h3("Select a role"),
            choices = session_survey$`Select your role.` %>%
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
        shiny.semantic::menu_item(
          tabName = "date_slider_menu",
          shiny::sliderInput(
            inputId = ns("date_slider"),
            label = h3("Select a date range"),
            value = c(
              min(as.Date(session_survey$Date), na.rm = T),
              max(as.Date(session_survey$Date), na.rm = T)
            ),
            min = min(as.Date(session_survey$Date), na.rm = T),
            max = max(as.Date(session_survey$Date), na.rm = T),
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
            h3(" OR click one of the following buttons to download one of the following most recent sessions found by a unique combination of site and facilitator, and then just click download!"),
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

reportServer <- function(id) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    
    #### Applying Recent Groupings Logic ####
    
    session_survey_recent <- reactive({
      
      if (!is.null(input$recent_sessions)) {
        
        # Get previously created choices dataframe for filtering by id
        chosen <- recent_choices %>% 
          inner_join((recent_choices_final %>%
                      dplyr::filter(choice %in% input$recent_sessions)), by = "id")
      
        session_survey <- session_survey %>%
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
        need(!is.null(input$course), "Please select at least one course")
      )

      session_survey_recent() %>%
        dplyr::filter(between(Date, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (input$site != "All Sites") dplyr::filter(., `Select your site (district, parish, network, or school).` %in% input$site) else .
        } %>%
        {
          if (input$role != "All Roles") dplyr::filter(., `Select your role.` %in% input$role) else .
        } %>%
        {
          if (input$facilitator != "All Facilitators") dplyr::filter(., Facilitator %in% input$facilitator) else .
        } %>%
        {
          if (input$content != "All Content Areas") dplyr::filter(., `Select the content area for today’s professional learning session.` %in% input$content) else .
        } %>%
        {
          if (input$course != "All Courses") dplyr::filter(., `Select your course.` %in% input$course) else .
        } %>%
        select(
          Date,
          c(
            "How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.",
            "How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.",
            "How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.",
            "How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.",
            "How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs."
          )
        ) %>%
        pivot_longer(!`Date`, names_to = "question", values_to = "answer") %>%
        dplyr::mutate(question = str_remove_all(
          question,
          "How much do you agree with the following statements about this course\\? - "
        )) %>%
        # Rename with line breaks every 27 characters
        mutate(question = gsub("(.{28,}?)\\s", "\\1\n", question)) %>%
        drop_na(answer) %>%
        dplyr::group_by(question, Date) %>%
        # Group by input variable
        mutate(`Number Agree/Disagree` = n()) %>%
        mutate(answer = str_remove_all(answer, "\\([:digit:]\\) ")) %>%
        mutate(
          Rating = case_when(
            answer %in% c("Agree", "Strongly agree") ~ "Agree/Strongly Agree",
            answer %in% c("Neither agree nor disagree", "Disagree", "Strongly disagree") ~ "Neither/Disagree/Strongly Disagree"
          ),
          date_group = case_when(as.integer(diff(range(as.Date(input$date_slider)))) >= 30 ~ paste0(lubridate::month(Date, label = T, abbr = F), ", ", year(Date)),
                                 as.integer(diff(range(as.Date(input$date_slider)))) >= 14 & as.integer(diff(range(as.Date(input$date_slider)))) < 30 ~ paste0(year(Date), lubridate::week(Date)),
                                 as.integer(diff(range(as.Date(input$date_slider)))) < 14 ~ paste0(lubridate::day(Date)))
        ) %>%
        ungroup() %>%
        dplyr::mutate(question = str_remove_all(
          question,
          "How much do you agree with the\nfollowing statements about this\nfacilitator today\\? -"
        )) %>%
        group_by(date_group, question) %>%
        mutate(Percent = `Number Agree/Disagree` / sum(`Number Agree/Disagree`) * 100) %>%
        filter(Rating == "Agree/Strongly Agree") %>%
        group_by(date_group, Rating, question) %>%
        summarise(Percent = round(sum(Percent), 2),
                  Date = Date)
    })

    #### OVERALL AGREE PLOT ####

    data_plot_agree <- reactive({

      validate(
        need(!is.null(input$facilitator), "Please select at least one facilitator"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )

      agree_plot <- session_survey_recent() %>%
        dplyr::filter(between(Date, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (input$site != "All Sites") dplyr::filter(., `Select your site (district, parish, network, or school).` %in% input$site) else .
        } %>%
        {
          if (input$role != "All Roles") dplyr::filter(., `Select your role.` %in% input$role) else .
        } %>%
        {
          if (input$facilitator != "All Facilitators") dplyr::filter(., Facilitator %in% input$facilitator) else .
        } %>%
        {
          if (input$content != "All Content Areas") dplyr::filter(., `Select the content area for today’s professional learning session.` %in% input$content) else .
        } %>%
        {
          if (input$course != "All Courses") dplyr::filter(., `Select your course.` %in% input$course) else .
        } %>%
        select(c(
          "How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.",
          "How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.",
          "How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.",
          "How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.",
          "How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs."
        )) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        drop_na() %>%
        dplyr::mutate(Question = str_remove_all(
          Question,
          "How much do you agree with the following statements about this facilitator today\\? - "
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
      data_n <- session_survey_recent() %>%
        dplyr::filter(between(Date, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (input$site != "All Sites") dplyr::filter(., `Select your site (district, parish, network, or school).` %in% input$site) else .
        } %>%
        {
          if (input$role != "All Roles") dplyr::filter(., `Select your role.` %in% input$role) else .
        } %>%
        {
          if (input$facilitator != "All Facilitators") dplyr::filter(., Facilitator %in% input$facilitator) else .
        } %>%
        {
          if (input$content != "All Content Areas") dplyr::filter(., `Select the content area for today’s professional learning session.` %in% input$content) else .
        } %>%
        {
          if (input$course != "All Courses") dplyr::filter(., `Select your course.` %in% input$course) else .
        }
      nrow(data_n)
    })

    #### QUOTES 1 ####

    quote_viz_data1 <- reactive({

      validate(
        need(!is.null(input$facilitator), "Please select at least one facilitator"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )

      quote_reactive1 <- session_survey_recent() %>%
        dplyr::filter(between(Date, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (input$site != "All Sites") dplyr::filter(., `Select your site (district, parish, network, or school).` %in% input$site) else .
        } %>%
        {
          if (input$role != "All Roles") dplyr::filter(., `Select your role.` %in% input$role) else .
        } %>%
        {
          if (input$facilitator != "All Facilitators") dplyr::filter(., Facilitator %in% input$facilitator) else .
        } %>%
        {
          if (input$content != "All Content Areas") dplyr::filter(., `Select the content area for today’s professional learning session.` %in% input$content) else .
        } %>%
        {
          if (input$course != "All Courses") dplyr::filter(., `Select your course.` %in% input$course) else .
        } %>%
        select(Facilitation_Feedback) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        drop_na() %>%
        filter(Response %!in% na_df) %>%
        select(Response) %>%
        slice_sample(n = 10)
    })

    #### QUOTES 2 ####

    quote_viz_data2 <- reactive({

      validate(
        need(!is.null(input$facilitator), "Please select at least one facilitator"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )

      quote_reactive2 <- session_survey_recent() %>%
        dplyr::filter(between(Date, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (input$site != "All Sites") dplyr::filter(., `Select your site (district, parish, network, or school).` %in% input$site) else .
        } %>%
        {
          if (input$role != "All Roles") dplyr::filter(., `Select your role.` %in% input$role) else .
        } %>%
        {
          if (input$facilitator != "All Facilitators") dplyr::filter(., Facilitator %in% input$facilitator) else .
        } %>%
        {
          if (input$content != "All Content Areas") dplyr::filter(., `Select the content area for today’s professional learning session.` %in% input$content) else .
        } %>%
        {
          if (input$course != "All Courses") dplyr::filter(., `Select your course.` %in% input$course) else .
        } %>%
        select(`What went well in today’s session?`) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        drop_na() %>%
        filter(Response %!in% na_df) %>%
        select(Response) %>%
        slice_sample(n = 10)
    })

    #### QUOTES 3 ####

    quote_viz_data3 <- reactive({

      validate(
        need(!is.null(input$facilitator), "Please select at least one facilitator"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )

      quote_reactive3 <- session_survey_recent() %>%
        dplyr::filter(between(Date, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (input$site != "All Sites") dplyr::filter(., `Select your site (district, parish, network, or school).` %in% input$site) else .
        } %>%
        {
          if (input$role != "All Roles") dplyr::filter(., `Select your role.` %in% input$role) else .
        } %>%
        {
          if (input$facilitator != "All Facilitators") dplyr::filter(., Facilitator %in% input$facilitator) else .
        } %>%
        {
          if (input$content != "All Content Areas") dplyr::filter(., `Select the content area for today’s professional learning session.` %in% input$content) else .
        } %>%
        {
          if (input$course != "All Courses") dplyr::filter(., `Select your course.` %in% input$course) else .
        } %>%
        select(`What could have been better about today’s session?`) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        drop_na() %>%
        filter(Response %!in% na_df) %>%
        select(Response) %>%
        slice_sample(n = 10)
      
      quote_reactive3
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
                       subtitle = session_survey_recent() %>% 
                         dplyr::filter(between(Date, input$date_slider[1], input$date_slider[2])) %>%
                         {
                           if (input$site != "All Sites") dplyr::filter(., `Select your site (district, parish, network, or school).` %in% input$site) else .
                         } %>%
                         {
                           if (input$role != "All Roles") dplyr::filter(., `Select your role.` %in% input$role) else .
                         } %>%
                         {
                           if (input$facilitator != "All Facilitators") dplyr::filter(., Facilitator %in% input$facilitator) else .
                         } %>%
                         {
                           if (input$content != "All Content Areas") dplyr::filter(., `Select the content area for today’s professional learning session.` %in% input$content) else .
                         } %>%
                         {
                           if (input$course != "All Courses") dplyr::filter(., `Select your course.` %in% input$course) else .
                         } %>%
                         select(Facilitator, `Select your site (district, parish, network, or school).`) %>% 
                         transmute(course_fac = paste0(`Select your site (district, parish, network, or school).`, ", ", Facilitator)) %>%
                         distinct(course_fac) %>%
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
                       subtitle = session_survey_recent() %>% 
                         dplyr::filter(between(Date, input$date_slider[1], input$date_slider[2])) %>%
                         {
                           if (input$site != "All Sites") dplyr::filter(., `Select your site (district, parish, network, or school).` %in% input$site) else .
                         } %>%
                         {
                           if (input$role != "All Roles") dplyr::filter(., `Select your role.` %in% input$role) else .
                         } %>%
                         {
                           if (input$facilitator != "All Facilitators") dplyr::filter(., Facilitator %in% input$facilitator) else .
                         } %>%
                         {
                           if (input$content != "All Content Areas") dplyr::filter(., `Select the content area for today’s professional learning session.` %in% input$content) else .
                         } %>%
                         {
                           if (input$course != "All Courses") dplyr::filter(., `Select your course.` %in% input$course) else .
                         } %>%
                         select(Facilitator, `Select your site (district, parish, network, or school).`) %>% 
                         transmute(course_fac = paste0(`Select your site (district, parish, network, or school).`, ", ", Facilitator)) %>%
                         distinct(course_fac) %>%
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
      df <- session_survey_recent() %>%
        dplyr::filter(between(date_created, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (input$site != "All Sites") dplyr::filter(., `Select your site (district, parish, network, or school).` %in% input$site) else .
        } %>%
        {
          if (input$role != "All Roles") dplyr::filter(., `Select your role.` %in% input$role) else .
        } %>%
        {
          if (input$facilitator != "All Facilitators") dplyr::filter(., Facilitator %in% input$facilitator) else .
        } %>%
        {
          if (input$content != "All Content Areas") dplyr::filter(., `Select the content area for today's professional learning session.` %in% input$content) else .
        } %>%
        {
          if (input$course != "All Courses") dplyr::filter(., `Select your course.` %in% input$course) else .
        }
      
      df
    })
    
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("session_survey_data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(download_reactive(), file)
      }
    )
    

  })
}
