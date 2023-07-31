#### Session Survey Dashboard ####
uiAgree <- function(id, label = "Counter") {
  ns <- NS(id)
  shiny::tagList(
    shiny.semantic::sidebar_layout(
      sidebar_panel = shiny.semantic::sidebar_panel(
        width = 1.5,
        style = "position:fixed;overflow-x:hidden;overflow-y:auto;width:inherit;",
        shiny.semantic::menu_item(
          tabName = "question_menu",
          shiny::selectizeInput(
            inputId = ns("question"),
            label = h3("Select a Question"),
            choices = ipg_plot_select_names,
            multiple = F,
            options = list(plugins = list("remove_button"))
          ),
          icon = shiny.semantic::icon("question")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "site_menu",
          shiny::selectizeInput(
            inputId = ns("site"),
            label = h3("Select a site"),
            choices = ipg_forms$`Name of Site (Parish, District, Network)` %>%
              unique() %>%
              as.character() %>%
              sort() %>%
              purrr::prepend("All Sites"),
            multiple = T,
            selected = "All Sites",
            options = list(plugins = list("remove_button"))
          ),
          icon = shiny.semantic::icon("location arrow")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "grade_content_menu",
          shiny::selectizeInput(
            inputId = ns("grade_content"),
            label = h3("Select a Grade Level/Content Area"),
            choices = ipg_forms$`Grade Level / Content Area` %>%
              unique() %>%
              as.character() %>%
              sort() %>%
              purrr::prepend("All Grade Levels/Content Areas"),
            multiple = T,
            selected = "All Grade Levels/Content Areas",
            options = list(plugins = list("remove_button"))
          ),
          icon = shiny.semantic::icon("user")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "rubric_menu",
          shiny::selectizeInput(
            inputId = ns("ipg_rubric"),
            label = h3("Select an IPG Rubric"),
            choices = ipg_forms$`IPG Rubric` %>%
              unique() %>%
              as.character() %>%
              sort() %>%
              purrr::prepend("All IPG Rubrics"),
            multiple = T,
            selected = "All IPG Rubrics",
            options = list(plugins = list("remove_button"))
          ),
          icon = shiny.semantic::icon("edit")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "teacher_menu",
          shiny::selectizeInput(
            inputId = ns("teacher"),
            label = h3("Select a Teacher"),
            choices = ipg_forms$`Teacher name` %>%
              unique() %>%
              as.character() %>%
              sort() %>%
              purrr::prepend("All Teachers"),
            multiple = T,
            selected = "All Teachers",
            options = list(plugins = list("remove_button"))
          ),
          icon = shiny.semantic::icon("clone")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "date_slider_menu",
          shiny::sliderInput(
            inputId = ns("date_slider"),
            label = h3("Select a date range"),
            value = c(
              min(as.Date(ipg_forms$day), na.rm = T),
              max(as.Date(ipg_forms$day), na.rm = T)
            ),
            min = min(as.Date(ipg_forms$day), na.rm = T),
            max = max(as.Date(ipg_forms$day), na.rm = T),
            timeFormat = "%b %d, %Y"
          ),
          icon = shiny.semantic::icon("calendar alternate")
        )
      ),
      main_panel = shiny.semantic::main_panel(
        width = 4,
        div(
          class = "ui two column stackable grid container",
          div(
            class = "sixteen wide column",
            plotOutput(ns("percent_agree_plot"), height = "800px") %>%
              withSpinner(type = 3, color.background = "white")
          ),
          div(
            class = "sixteen wide column",
            plotOutput(ns("agree_plot_ts"), height = "800px") %>%
              withSpinner(type = 3, color.background = "white")
          )
        )
      )
    )
  )
}

agreeServer <- function(id) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {

    # Time Series Plot
    data_plot_ts <- reactive({
      validate(
        need(!is.null(input$question), "Please select at least one question"),
        need(!is.null(input$teacher), "Please select at least one teacher"),
        need(!is.null(input$ipg_rubric), "Please select at least one grade_content"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$grade_content), "Please select at least one content area")
      )

      agree_plot_ts <- ipg_forms %>%
        dplyr::filter(between(day, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (input$site != "All Sites") dplyr::filter(., `Name of Site (Parish, District, Network)` %in% input$site) else .
        } %>%
        {
          if (input$grade_content != "All Grade Levels/Content Areas") dplyr::filter(., `Grade Level / Content Area` %in% input$grade_content) else .
        } %>%
        {
          if (input$teacher != "All Teachers") dplyr::filter(., `Teacher name` %in% input$teacher) else .
        } %>%
        {
          if (input$ipg_rubric != "All IPG Rubrics") dplyr::filter(., `IPG Rubric` %in% input$ipg_rubric) else .
        } %>%
        pivot_longer(!c(1:11)) %>%
        drop_na(value) %>%
        group_by(name, value, `Timeline of Obs`) %>%
        summarise(n = n()) %>%
        ungroup(value) %>%
        dplyr::group_by(name, `Timeline of Obs`) %>%
        mutate(percent = round(100 * n / sum(n))) %>%
        filter(name == input$question)

      agree_plot_ts
    })
    
    observe({
      print(data_plot_ts())
    })

    # Ggplot for time series plot
    output$agree_plot_ts <- renderPlot({
      dashboard_ipg_plot_ts(
        data = data_plot_ts(),
        name = input$question
      )
    })

    # Agree Percent Plot
    data_plot_agree <- reactive({
      validate(
        need(!is.null(input$question), "Please select at least one question"),
        need(!is.null(input$teacher), "Please select at least one teacher"),
        need(!is.null(input$grade_content), "Please select at least one grade_content"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$grade_content), "Please select at least one content area")
      )

      agree_plot <- ipg_forms %>%
        dplyr::filter(between(day, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (input$site != "All Sites") dplyr::filter(., `Name of Site (Parish, District, Network)` %in% input$site) else .
        } %>%
        {
          if (input$grade_content != "All Grade Levels/Content Areas") dplyr::filter(., `Grade Level / Content Area` %in% input$grade_content) else .
        } %>%
        {
          if (input$teacher != "All Teachers") dplyr::filter(., `Teacher name` %in% input$teacher) else .
        } %>%
        {
          if (input$ipg_rubric != "All IPG Rubrics") dplyr::filter(., `IPG Rubric` %in% input$ipg_rubric) else .
        } %>%
        pivot_longer(!c(1:11)) %>%
        drop_na(value) %>%
        group_by(name, value) %>%
        summarise(n = n()) %>%
        ungroup(value) %>%
        mutate(percent = round(100 * n / sum(n))) %>%
        filter(name == input$question)

      agree_plot
    })


    # Ggplot for agree percent plot
    output$percent_agree_plot <- renderPlot({
      dashboard_ipg_plot(
        data = data_plot_agree(),
        name = input$question
      )
    })
  })
}
