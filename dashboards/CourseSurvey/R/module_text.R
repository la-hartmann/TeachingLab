### Course Survey Dashboard ###
textGridTemplate <- shiny.semantic::grid_template(
  default = list(
    areas = rbind(
      c("btn1", "btn2", "btn3", "btn4", "btn5"),
      c("gt1", "gt2", "gt3", "gt4", "gt5")
    ),
    cols_width = c("250px"),
    rows_height = c("50px", "auto")
  ),
  mobile = list(
    areas = rbind(
      "btn1",
      "gt1",
      "btn2",
      "gt2",
      "btn3",
      "gt3",
      "btn4",
      "gt4",
      "btn5",
      "gt5"
    ),
    rows_height = c("1fr"),
    cols_width = c("100%")
  )
)


uiText <- function(id, label = "Counter") {
  ns <- NS(id)
  shiny::tagList(
    sidebar = shiny.semantic::sidebar_layout(
      shiny.semantic::sidebar_panel(
        style = sidebar_style,
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
          tabName = "content_menu",
          shiny::selectizeInput(
            inputId = ns("content"),
            label = h3("Select a content area", style = "font-weight: bold;"),
            choices = course_survey$`Select the content area for today's professional learning session.` %>%
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
          tabName = "date_slider_menu",
          split_layout(
            cell_widths = c("50%", "50%"),
            cell_args = "padding: 5px;",
            style = "background-color: transparent;",
            shinyWidgets::airDatepickerInput(
              inputId = ns("date_min"),
              label = h3("Select minimum date"),
              as.Date("2021-07-30"),
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
              maxDate = Sys.Date() + 1,
              dateFormat = "mm-dd-yyyy",
              width = "100px"
            )
          ),
          icon = shiny.semantic::icon("calendar alternate")
        ),
        br(),
        menu_item(
          tabName = "text_length_menu",
          shiny.semantic::numeric_input(
            input_id = ns("quote_length"),
            label = h3("Select a text length to filter for", style = "font-weight: bold;"),
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
            btn3 = "margin:auto",
            btn4 = "margin:auto",
            btn5 = "margin:auto"
          ),
          btn1 = uiOutput(ns("btn1")),
          btn2 = uiOutput(ns("btn2")),
          btn3 = uiOutput(ns("btn3")),
          btn4 = uiOutput(ns("btn4")),
          btn5 = uiOutput(ns("btn5")),
          gt1 = gt::gt_output(ns("quote_gt1")) %>%
            withSpinner(type = 3, color.background = "white"),
          gt2 = gt::gt_output(ns("quote_gt2")) %>%
            withSpinner(type = 3, color.background = "white"),
          gt3 = gt::gt_output(ns("quote_gt3")) %>%
            withSpinner(type = 3, color.background = "white"),
          gt4 = gt::gt_output(ns("quote_gt4")) %>%
            withSpinner(type = 3, color.background = "white"),
          gt5 = gt::gt_output(ns("quote_gt5")) %>%
            withSpinner(type = 3, color.background = "white")
        )
      )
    )
  )
}

textServer <- function(id, in_site) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    
    reactive_1_count <- reactive({
      
      ### Validate role, site, content, and course selection ###
      validate(
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )
      
      ### Get count of observations ###
      count <- course_survey |>
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
        drop_na() |>
        filter(`Overall, what went well in this course?` %!in% na_df) |>
        filter(str_length(`Overall, what went well in this course?`) > input$quote_length)
      
      ### Get actual count and return ###
      nrow(count)
    })
    
    reactive_2_count <- reactive({
      
      validate(
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )
      
      count <- course_survey |>
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
        drop_na() |>
        filter(`Overall, what could have been better in this course?` %!in% na_df) |>
        filter(str_length(`Overall, what could have been better in this course?`) > input$quote_length)
      
      nrow(count)
      
    })
    
    reactive_3_count <- reactive({
      
      validate(
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )
      
      count <- course_survey |>
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
        drop_na() |>
        filter(`What is the learning from this course that you are most excited about trying out?` %!in% na_df) |>
        filter(str_length(`What is the learning from this course that you are most excited about trying out?`) > input$quote_length)
      
      nrow(count)
      
    })
    
    reactive_4_count <- reactive({
      
      validate(
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )
      
      count <- course_survey |>
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
        drop_na() |>
        filter(`Which activities best supported your learning in this course?` %!in% na_df) |>
        filter(str_length(`Which activities best supported your learning in this course?`) > input$quote_length)
      
      nrow(count)
    })
    
    
    reactive_5_count <- reactive({
      
      validate(
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )
      
      count <- course_survey |>
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
        drop_na() |>
        filter(`Feel free to leave us any additional comments, concerns, or questions.` %!in% na_df) |>
        filter(str_length(`Feel free to leave us any additional comments, concerns, or questions.`) > input$quote_length)
      
      nrow(count)
    })
    
    
    output$btn1 = renderUI({
      shinyWidgets::actionBttn(
      inputId = ns("refresh1"),
      label = glue::glue("Refresh from {reactive_1_count()} responses"),
      style = "unite",
      color = "primary"
      )})
    output$btn2 = renderUI({shinyWidgets::actionBttn(
      inputId = ns("refresh2"),
      label = glue::glue("Refresh from {reactive_2_count()} responses"),
      style = "unite",
      color = "primary"
    )})
    output$btn3 = renderUI({shinyWidgets::actionBttn(
      inputId = ns("refresh3"),
      label = glue::glue("Refresh from {reactive_3_count()} responses"),
      style = "unite",
      color = "primary"
    )})
    output$btn4 = renderUI({shinyWidgets::actionBttn(
      inputId = ns("refresh4"),
      label = glue::glue("Refresh from {reactive_4_count()} responses"),
      style = "unite",
      color = "primary"
    )})
    output$btn5 = renderUI({shinyWidgets::actionBttn(
      inputId = ns("refresh5"),
      label = glue::glue("Refresh from {reactive_5_count()} responses"),
      style = "unite",
      color = "primary"
    )})
    

    # Getting quotes for table
    quote_viz_data1 <- reactive({
      
      validate(
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )
      
      quote_reactive1 <- course_survey |>
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
        drop_na() |>
        filter(`Overall, what went well in this course?` %!in% na_df) |>
        filter(str_length(`Overall, what went well in this course?`) > input$quote_length)
      
      #### Validate that there is data #### 
      validate(
        need(nrow(quote_reactive1) > 0, "There are no observations for this set of filters")
      )
      
      #### Return the data ####
      quote_reactive1
    })

    quote_viz_data2 <- reactive({
      
      validate(
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )
      
      quote_reactive2 <- course_survey |>
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
        drop_na() |>
        filter(`Overall, what could have been better in this course?` %!in% na_df) |>
        filter(str_length(`Overall, what could have been better in this course?`) > input$quote_length)
      
      #### Validate that there is data #### 
      validate(
        need(nrow(quote_reactive2) > 0, "There are no observations for this set of filters")
      )
      
      #### Return the data ####
      quote_reactive2
    })

    quote_viz_data3 <- reactive({
      validate(
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )
      quote_reactive3 <- course_survey |>
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
        drop_na() |>
        filter(`What is the learning from this course that you are most excited about trying out?` %!in% na_df) |>
        filter(str_length(`What is the learning from this course that you are most excited about trying out?`) > input$quote_length)
      
      #### Validate that there is data #### 
      validate(
        need(nrow(quote_reactive3) > 0, "There are no observations for this set of filters")
      )
      
      #### Return the data ####
      quote_reactive3
    })

    quote_viz_data4 <- reactive({
      
      validate(
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )
      
      quote_reactive4 <- course_survey |>
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
        drop_na() |>
        filter(`Which activities best supported your learning in this course?` %!in% na_df) |>
        filter(str_length(`Which activities best supported your learning in this course?`) > input$quote_length)
      
      #### Validate that there is data #### 
      validate(
        need(nrow(quote_reactive4) > 0, "There are no observations for this set of filters")
      )
      
      #### Return the data ####
      quote_reactive4
    })

    quote_viz_data5 <- reactive({
      
      validate(
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )
      
      quote_reactive5 <- course_survey |>
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
        drop_na() |>
        filter(`Feel free to leave us any additional comments, concerns, or questions.` %!in% na_df) |>
        filter(str_length(`Feel free to leave us any additional comments, concerns, or questions.`) > input$quote_length)
      
      #### Validate that there is data #### 
      validate(
        need(nrow(quote_reactive5) > 0, "There are no observations for this set of filters")
      )
      
      #### Return the data ####
      quote_reactive5
    })
    
    quote1 <- reactiveValues(table1 = NULL)
    
    observeEvent(c(input$refresh1, input$site, input$role, input$content, input$course, input$quote_length, input$date_slider), {
      quote1$table1 <- quote_viz_data1() %>%
        {
          if (nrow(.) > 0) slice_sample(., n = ifelse(nrow(.) > 10, 10, nrow(.))) else .
        }
    })

    output$quote_gt1 <- gt::render_gt(
      quote_viz(
        data = quote1$table1,
        viz_type = "gt",
        width = 70,
        print = F,
        align = "center"
      ) |>
        suppressWarnings() |>
        suppressMessages()
    )
    
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
        width = 70,
        print = F
      ) |>
        suppressWarnings() |>
        suppressMessages()
    )
    
    quote3 <- reactiveValues(table4 = NULL)
    
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
        width = 70,
        print = F
      ) |>
        suppressWarnings() |>
        suppressMessages()
    )
    
    quote4 <- reactiveValues(table4 = NULL)
    
    observeEvent(c(input$refresh4, input$site, input$role, input$content, input$course, input$quote_length, input$date_slider), {
      quote4$table4 <- quote_viz_data4() %>%
        {
          if (nrow(.) > 0) slice_sample(., n = ifelse(nrow(.) > 10, 10, nrow(.))) else .
        }
    })

    output$quote_gt4 <- gt::render_gt(
      quote_viz(
        data = quote4$table4,
        viz_type = "gt",
        width = 70,
        print = F
      ) |>
        suppressWarnings() |>
        suppressMessages()
    )
    
    quote5 <- reactiveValues(table5 = NULL)
    
    observeEvent(c(input$refresh5, input$site, input$role, input$content, input$course, input$quote_length, input$date_slider), {
      quote5$table5 <- quote_viz_data5() %>%
        {
          if (nrow(.) > 0) slice_sample(., n = ifelse(nrow(.) > 10, 10, nrow(.))) else .
        }
    })

    output$quote_gt5 <- gt::render_gt(
      quote_viz(
        data = quote5$table5, 
        viz_type = "gt",
        width = 70,
        print = F
      ) |>
        suppressWarnings() |>
        suppressMessages()
    )
    
  })
}
