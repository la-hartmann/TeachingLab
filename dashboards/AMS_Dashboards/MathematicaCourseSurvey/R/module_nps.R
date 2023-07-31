uiNPS <- function(id, label = "Counter") {
  ns <- NS(id)
  shiny::tagList(
    sidebar = shiny.semantic::sidebar_layout(
      shiny.semantic::sidebar_panel(
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
            options = list(plugins= list('remove_button'))
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
              min(as.Date(course_survey$date_created), na.rm = T),
              max(as.Date(course_survey$date_created), na.rm = T)
            ),
            min = min(as.Date(course_survey$date_created), na.rm = T),
            max = max(as.Date(course_survey$date_created), na.rm = T),
            timeFormat = "%b %d, %Y"
          ),
          icon = shiny.semantic::icon("calendar alternate")
        ),
        br(),
        menu_item(
          tabName = "scale_adjust_menu",
          shiny.semantic::selectInput(
            inputId = ns("scale_adjust"),
            label = h3("Select a date scale", style = "font-weight: bold;"),
            value = "Month",
            choices = c("Monthly", "Weekly", "Daily")
          ),
          icon = shiny.semantic::icon("scale")
        )
      ),
      main_panel = shiny.semantic::main_panel(
        width = 4,
        div(
          class = "ui two column stackable grid container",
          div(
            class = "sixteen wide column",
            plotOutput(ns("nps_plot"), height = "800px") %>%
              withSpinner(type = 3, color.background = "white")
          ),
          div(
            class = "sixteen wide column",
            h2("Overall NPS", style = "text-align:center;")
          ),
          div(
            class = "sixteen wide column",
            plotOutput(ns("nps_plot2"), height = "800px") %>%
              withSpinner(type = 3, color.background = "white")
          )
        )
      )
    )
  )
}

npsServer <- function(id, in_site) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {


    # NPS Plot Data
    data_plot_nps <- reactive({
      validate(
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$course), "Please select at least one course"),
        need(!is.null(input$grades), "Please select at least one grade")
      )
      reactive_nps <- course_survey %>%
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
        select(`On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`) %>%
        # mutate(`On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?` = readr::parse_number(as.character(`On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`))) %>%
        drop_na() %>%
        group_by(`On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`) %>%
        count(sort = T) %>%
        ungroup() %>%
        mutate(Percent = round(100 * n / sum(n)))
      
      #### Validate that there is data ####
      validate(
        need(nrow(reactive_nps) > 0, "There are no observations for this set of filters.")
      )
      
      #### Return the data ####
      reactive_nps
    })

    # Ggplot for nps plot
    output$nps_plot <- renderPlot({
      ggplot(data = data_plot_nps(), aes(
        x = `On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`,
        y = Percent,
        fill = factor(`On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`),
        color = factor(`On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`)
      )) +
        geom_text(aes(label = if_else(Percent != 4, paste0(Percent, "%"), "")), vjust = -0.5) +
        geom_col() +
        scale_fill_manual(values = c("0" = "#040404", "1" = "#040404", "2" = "#03161E", "3" = "#032938", "4" = "#023C52", "5" = "#024E6C", "6" = "#016187", 
                                     "7" = "#0174A1", "8" = "#0086BB", "9" = "#0099D5", "10" = "#00ACF0")) +
        scale_color_manual(values = c("0" = "#040404", "1" = "#040404", "2" = "#03161E", "3" = "#032938", "4" = "#023C52", "5" = "#024E6C", "6" = "#016187", 
                                     "7" = "#0174A1", "8" = "#0086BB", "9" = "#0099D5", "10" = "#00ACF0")) +
        labs(
          fill = "", title = "Likeliness to Recommend to a Friend or Colleague",
          x = "Rating", y = "Percent"
        ) +
        scale_x_continuous(limits = c(0, 10.5), breaks = c(0:10)) +
        scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, max(data_plot_nps()$Percent, na.rm = T) + 10)) +
        theme_tl(legend = F) +
        theme(
          axis.text.y = element_text(lineheight = 1.1, size = 16),
          axis.text.x = element_text(size = 16),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(lineheight = 1.1)
        )
    })

    # Get average NPS
    data_sum_nps <- reactive({
      validate(
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$course), "Please select at least one course"),
        need(!is.null(input$grades), "Please select at least one grade")
      )
      sum <- course_survey %>%
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
        mutate(nps = suppressWarnings(as.numeric(`On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`))) %>%
        drop_na(nps) %>%
        summarise(nine_ten = length(which(nps %in% c(9, 10))) / length(nps),
                  zero_six = length(which(nps %in% c(0:6))) / length(nps)) %>%
        # print() %>%
        mutate(nps = round(100*(nine_ten - zero_six))) #%>%
        # summarise(nps = calc_nps(nps)) %>%
        # mutate(
        #   x = 0,
        #   y = nps
        # )
      
      sum <- tibble(
        x = 0, 
        nps = sum$nps
      )
      
      #### Validate that there is data ####
      validate(
        need(nrow(sum) > 0, "There are no observations for this set of filters.")
      )
      #### Return the data ####
      sum
    })

    output$nps_plot2 <- renderPlot({
      ggplot(data = data_sum_nps()) +
        geom_text(aes(x = 0, y = 0.5, label = "fake text"), color = "transparent") +
        geom_text(aes(x = -0.5, y = 0.5, label = "fake text"), color = "transparent") +
        with_outer_glow(
          geom_text(aes(x = 0, y = 0, label = nps), size = 30),
          colour = "#04abeb",
          sigma = 5,
          expand = 5
        ) +
        geom_text(aes(x = 0, y = -0.5, label = "fake text"), color = "transparent") +
        geom_text(aes(x = 0.5, y = -0.5, label = "fake text"), color = "transparent") +
        with_outer_glow(
          geom_circle(aes(x0 = 0, y0 = 0, r = 1), size = 5),
          colour = "#04abeb",
          sigma = 5,
          expand = 5
        ) +
        coord_fixed() +
        theme_void()
    })
    
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
    #     in_date_slider <- reactive({input$date_slider}),
    #   )
    # )
    
  })
}
