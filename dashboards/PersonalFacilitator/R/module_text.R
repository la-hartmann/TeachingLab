#### Personal Facilitator Dashboard ####
textGridTemplate <- grid_template(
  default = list(
    areas = rbind(
      c("btn1", "btn2", "btn3"),
      c("gt1", "gt2", "gt3")
    ),
    cols_width = c("412px"),
    rows_height = c("50px", "auto")
  ),
  mobile = list(
    areas = rbind(
      "btn1",
      "gt1",
      "btn2",
      "gt2",
      "btn3",
      "gt3"
    ),
    rows_height = c("1fr"),
    cols_width = c("100%")
  )
)


uiText <- function(id, label = "Counter") {
  ns <- NS(id)
  shiny::tagList(
    shiny.semantic::sidebar_layout(
      sidebar_panel = shiny.semantic::sidebar_panel(
        style = sidebar_style,
        shiny.semantic::menu_item(
          tabName = "content_menu",
          shiny.semantic::selectInput(
            inputId = ns("content"),
            label = h3("Select a content area"),
            choices = session_survey$`Select the content area for today’s professional learning session.` %>%
              unique() %>%
              as.character() %>%
              sort() %>%
              purrr::prepend("All Content Areas")
          ),
          icon = shiny.semantic::icon("edit")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "course_menu",
          shiny.semantic::selectInput(
            inputId = ns("course"),
            label = h3("Select a course"),
            choices = session_survey$`Select your course.` %>%
              unique() %>%
              as.character() %>%
              sort() %>%
              purrr::prepend("All Courses")
          ),
          icon = shiny.semantic::icon("clone")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "site_menu",
          shiny.semantic::selectInput(
            inputId = ns("site"),
            label = h3("Select a site"),
            choices = session_survey$`Select your site (district, parish, network, or school).` %>%
              unique() %>%
              as.character() %>%
              sort() %>%
              purrr::prepend("All Partners")
          ),
          icon = shiny.semantic::icon("location arrow")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "role_menu",
          shiny.semantic::selectInput(
            inputId = ns("role"),
            label = h3("Select a role"),
            choices = session_survey$`Select your role.` %>%
              unique() %>%
              as.character() %>%
              sort() %>%
              purrr::prepend("All Roles")
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
                maxDate = max(as.Date(session_survey$Date), na.rm = T),
                dateFormat = "mm-dd-yyyy",
                width = "100px"
              ),
              shinyWidgets::airDatepickerInput(
                inputId = ns("date_max"),
                label = h3("Select maximum date"),
                value = max(as.Date(session_survey$Date), na.rm = T),
                minDate = min(as.Date(session_survey$Date), na.rm = T),
                maxDate = max(as.Date(session_survey$Date), na.rm = T),
                dateFormat = "mm-dd-yyyy",
                width = "100px"
              )
            ),
          icon = shiny.semantic::icon("calendar alternate")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "text_length_menu",
          shiny.semantic::numeric_input(
            input_id = ns("quote_length"),
            label = h3("Select a text length to filter for"),
            value = 30,
            min = 0,
            max = 100,
            step = 5
          ),
          icon = shiny.semantic::icon("text width")
        )
      ),
      main_panel = main_panel(
        width = 4,
        shiny.semantic::grid(
        textGridTemplate,
        area_styles = list(
          btn1 = "margin:auto",
          btn2 = "margin:auto",
          btn3 = "margin:auto"
        ),
        btn1 = uiOutput(ns("btn1")),
        btn2 = uiOutput(ns("btn2")),
        btn3 = uiOutput(ns("btn3")),
        gt1 = gt::gt_output(ns("quote_gt1")) %>%
          withSpinner(type = 3, color.background = "white"),
        gt2 = gt::gt_output(ns("quote_gt2")) %>%
          withSpinner(type = 3, color.background = "white"),
        gt3 = gt::gt_output(ns("quote_gt3")) %>%
          withSpinner(type = 3, color.background = "white")
      )
    )
    )
  )
}

textServer <- function(id, result_auth, in_content, in_course, in_site, in_role, in_date_slider) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    
    reactive_1_count <- reactive({
      
      validate(
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course"),
        need(input$date_min <= input$date_max, "Please select a minimum date that is less than the maximum.")
      )
      
      count <- session_survey %>%
        dplyr::filter(Facilitator == result_auth) %>%
        dplyr::filter(between(Date, input$date_min, input$date_max)) %>%
        {
          if (input$site != "All Sites") dplyr::filter(., `Select your site (district, parish, network, or school).` %in% input$site) else .
        } %>%
        {
          if (input$role != "All Roles") dplyr::filter(., `Select your role.` %in% input$role) else .
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
        filter(str_length(Response) > input$quote_length)
      
      nrow(count)
    })
    
    reactive_2_count <- reactive({
      
      validate(
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course"),
        need(input$date_min <= input$date_max, "Please select a minimum date that is less than the maximum.")
      )
      
      count <- session_survey %>%
        dplyr::filter(Facilitator == result_auth) %>%
        dplyr::filter(between(Date, input$date_min, input$date_max)) %>%
        {
          if (input$site != "All Sites") dplyr::filter(., `Select your site (district, parish, network, or school).` %in% input$site) else .
        } %>%
        {
          if (input$role != "All Roles") dplyr::filter(., `Select your role.` %in% input$role) else .
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
        filter(str_length(Response) > input$quote_length)
      nrow(count)
    })
    
    reactive_3_count <- reactive({
      
      validate(
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course"),
        need(input$date_min <= input$date_max, "Please select a minimum date that is less than the maximum.")
      )
      
      count <- session_survey %>%
        dplyr::filter(Facilitator == result_auth) %>%
        dplyr::filter(between(Date, input$date_min, input$date_max)) %>%
        {
          if (input$site != "All Sites") dplyr::filter(., `Select your site (district, parish, network, or school).` %in% input$site) else .
        } %>%
        {
          if (input$role != "All Roles") dplyr::filter(., `Select your role.` %in% input$role) else .
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
        filter(str_length(Response) > input$quote_length)
      nrow(count)
    })
    
    
    output$btn1 <- renderUI({ 
      shinyWidgets::actionBttn(
        inputId = ns("refresh1"),
        label = glue::glue("Refresh from\n{reactive_1_count()} responses"),
        style = "unite",
        color = "primary"
      )
    })
    output$btn2 <- renderUI({ 
      shinyWidgets::actionBttn(
        inputId = ns("refresh2"),
        label = glue::glue("Refresh from {reactive_2_count()} responses"),
        style = "unite",
        color = "primary"
      )
    })
    output$btn3 <- renderUI({ 
      shinyWidgets::actionBttn(
        inputId = ns("refresh3"),
        label = glue::glue("Refresh from {reactive_3_count()} responses"),
        style = "unite",
        color = "primary"
      )
    })
    
    # Getting quotes for table
    quote_viz_data1 <- reactive({
      
      validate(
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course"),
        need(input$date_min <= input$date_max, "Please select a minimum date that is less than the maximum.")
      )
      
      quote_reactive1 <- session_survey %>%
        dplyr::filter(Facilitator == result_auth) %>%
        dplyr::filter(between(Date, input$date_min, input$date_max)) %>%
        {
          if (input$site != "All Partners") dplyr::filter(., `Select your site (district, parish, network, or school).` == input$site) else .
        } %>%
        {
          if (input$role != "All Roles") dplyr::filter(., `Select your role.` == input$role) else .
        } %>%
        {
          if (input$content != "All Content Areas") dplyr::filter(., `Select the content area for today’s professional learning session.` == input$content) else .
        } %>%
        {
          if (input$course != "All Courses") dplyr::filter(., `Select your course.` == input$course) else .
        } %>%
        select(Facilitation_Feedback) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        drop_na() %>%
        filter(Response %!in% na_df) %>%
        select(Response) %>%
        filter(str_length(Response) > input$quote_length)
    })
    
    quote1 <- reactiveValues(table1 = NULL)
    
    observeEvent(c(input$refresh1, input$site, input$role, input$content, input$course, input$quote_length, input$date_min, input$date_max), {
      quote1$table1 <- quote_viz_data1() %>%
        {
          if (nrow(.) > 0) slice_sample(., n = ifelse(nrow(.) > 10, 10, nrow(.))) else .
        }
    })
    
    output$quote_gt1 <- gt::render_gt(
      
      quote_viz(
        data = quote1$table1,
        viz_type = "gt",
        print = F,
        width = 100,
        title = "Facilitator Feedback"
      ) %>%
        suppressWarnings() %>%
        suppressMessages()
      
    )

    quote_viz_data2 <- reactive({
      
      validate(
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course"),
        need(input$date_min <= input$date_max, "Please select a minimum date that is less than the maximum.")
      )
      
      quote_reactive2 <- session_survey %>%
        dplyr::filter(Facilitator == result_auth) %>%
        dplyr::filter(between(Date, input$date_min, input$date_max)) %>%
        {
          if (input$site != "All Partners") dplyr::filter(., `Select your site (district, parish, network, or school).` == input$site) else .
        } %>%
        {
          if (input$role != "All Roles") dplyr::filter(., `Select your role.` == input$role) else .
        } %>%
        {
          if (input$content != "All Content Areas") dplyr::filter(., `Select the content area for today’s professional learning session.` == input$content) else .
        } %>%
        {
          if (input$course != "All Courses") dplyr::filter(., `Select your course.` == input$course) else .
        } %>%
        select(`What went well in today’s session?`) %>%
        mutate(`What went well in today’s session?` = "Duncan was there.") %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        drop_na() %>%
        filter(Response %!in% na_df) %>%
        select(Response) %>%
        filter(str_length(Response) > input$quote_length)
    })
    
    quote2 <- reactiveValues(table2 = NULL)
    
    observeEvent(c(input$refresh2, input$site, input$role, input$content, input$course, input$quote_length, input$date_slider), {
      quote2$table2 <- quote_viz_data2() %>%
        {
          if (nrow(.) > 0) slice_sample(., n = ifelse(nrow(.) > 10, 10, nrow(.))) else .
        }
    })
    
    output$quote_gt2 <- gt::render_gt(
      
      quote_viz(
        data = quote2$table2,
        viz_type = "gt",
        print = F,
        width = 100,
        title = "What went well in today’s session?"
      ) %>%
        suppressWarnings() %>%
        suppressMessages()
      
    )

    quote_viz_data3 <- reactive({
      
      print(result_auth)
      
      validate(
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course"),
        need(input$date_min <= input$date_max, "Please select a minimum date that is less than the maximum.")
      )
      
      quote_reactive3 <- session_survey %>%
        dplyr::filter(Facilitator == result_auth) %>%
        dplyr::filter(between(Date, input$date_min, input$date_max)) %>%
        {
          if (input$site != "All Partners") dplyr::filter(., `Select your site (district, parish, network, or school).` == input$site) else .
        } %>%
        {
          if (input$role != "All Roles") dplyr::filter(., `Select your role.` == input$role) else .
        } %>%
        {
          if (input$content != "All Content Areas") dplyr::filter(., `Select the content area for today’s professional learning session.` == input$content) else .
        } %>%
        {
          if (input$course != "All Courses") dplyr::filter(., `Select your course.` == input$course) else .
        } %>%
        select(`What could have been better about today’s session?`) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        drop_na() %>%
        filter(Response %!in% na_df) %>%
        select(Response) %>%
        filter(str_length(Response) > input$quote_length)
    })
    
    quote3 <- reactiveValues(table3 = NULL)
    
    observeEvent(c(input$refresh3, input$site, input$role, input$content, input$course, input$quote_length, input$date_slider), {
      quote3$table3 <- quote_viz_data3() %>%
        {
          if (nrow(.) > 0) slice_sample(., n = ifelse(nrow(.) > 10, 10, nrow(.))) else .
        }
    })

    output$quote_gt3 <- gt::render_gt(
      
      quote_viz(
        data = quote3$table3,
        viz_type = "gt",
        print = F,
        width = 100,
        title = "What could have been better about today’s session?"
      ) %>%
        suppressWarnings() %>%
        suppressMessages()
      
    )

  })
}
