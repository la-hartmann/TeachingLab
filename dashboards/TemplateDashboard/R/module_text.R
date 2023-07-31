
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
        style = "position:fixed;overflow-x:hidden;overflow-y:auto;width:inherit;",
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
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "text_length_menu",
          shiny.semantic::numeric_input(
            input_id = ns("quote_length"),
            label = HTML("<span style = 'font-weight: 700;font-size:1.28571429rem;margin-bottom:0;margin-top:0;line-height:1.28571429em;padding:0;margin:calc(2rem - .1428571428571429em) 0 1rem;'>Select a text length to filter<br>for a minimum character length</span> <br>"),
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

textServer <- function(id) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    
    reactive_1_count <- reactive({
      
      validate(
        need(!is.null(input$facilitator), "Please select at least one facilitator"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )
      
      count <- session_survey %>%
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
        filter(str_length(Response) > input$quote_length)
      nrow(count)
    })
    
    reactive_2_count <- reactive({
      
      validate(
        need(!is.null(input$facilitator), "Please select at least one facilitator"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )
      
      count <- session_survey %>%
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
        filter(str_length(Response) > input$quote_length)
      nrow(count)
    })
    
    reactive_3_count <- reactive({
      
      validate(
        need(!is.null(input$facilitator), "Please select at least one facilitator"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )
      
      count <- session_survey %>%
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
        need(!is.null(input$facilitator), "Please select at least one facilitator"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )
      
      quote_reactive1 <- session_survey %>%
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
        filter(str_length(Response) > input$quote_length)
    })
    
    quote1 <- reactiveValues(table1 = NULL)
    
    observeEvent(c(input$refresh1, input$site, input$role, input$content, input$course, input$quote_length, input$date_slider, input$facilitator), {
      quote1$table1 <- quote_viz_data1() %>%
        slice_sample(n = 10)
    })
    
    output$quote_gt1 <- gt::render_gt(
      quote_viz(
        data = quote1$table1, text_col = "Response", viz_type = "gt",
        title = "What additional feedback do you have about their facilitation skills?",
        width = 60
      ) %>%
        suppressWarnings() %>%
        suppressMessages()
    )

    quote_viz_data2 <- reactive({
      
      validate(
        need(!is.null(input$facilitator), "Please select at least one facilitator"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )
      
      quote_reactive2 <- session_survey %>%
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
        filter(str_length(Response) > input$quote_length)
    })
    
    quote2 <- reactiveValues(table2 = NULL)
    
    observeEvent(c(input$refresh2, input$site, input$role, input$content, input$course, input$quote_length, input$date_slider, input$facilitator), {
      quote2$table2 <- quote_viz_data2() %>%
        slice_sample(n = 10)
    })
    
    output$quote_gt2 <- gt::render_gt(
      quote_viz(
        data = quote2$table2, text_col = "Response", viz_type = "gt",
        title = "What went well in today’s session?",
        width = 60
      ) %>%
        suppressWarnings() %>%
        suppressMessages()
    )

    quote_viz_data3 <- reactive({
      
      validate(
        need(!is.null(input$facilitator), "Please select at least one facilitator"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )
      
      quote_reactive3 <- session_survey %>%
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
        filter(str_length(Response) > input$quote_length)
    })
    
    quote3 <- reactiveValues(table3 = NULL)
    
    observeEvent(c(input$refresh3, input$site, input$role, input$content, input$course, input$quote_length, input$date_slider, input$facilitator), {
      quote3$table3 <- quote_viz_data3() %>%
        slice_sample(n = 10)
    })

    output$quote_gt3 <- gt::render_gt(
      quote_viz(
        data = quote3$table3, text_col = "Response", viz_type = "gt",
        title = "What could have been better about today’s session?",
        width = 60
      ) %>%
        suppressWarnings() %>%
        suppressMessages()
    )
    
    #### Filters reactive to other module inputs ####
    # facilitator_input <- callModule(agreeServer, "facilitator")
    # session$userData$settings <- reactiveValues(chosen_facilitator = NA)
    # observeEvent(facilitator_input(), {
    #   session$userData$settings$chosen_facilitator <- facilitator_input()
    # })
    

  })
}
