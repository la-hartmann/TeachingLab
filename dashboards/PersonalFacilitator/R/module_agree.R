#### Personal Facilitator Dashboard ####
uiAgree <- function(id, label = "Counter") {
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
        menu_item(
          tabName = "scale_adjust_menu",
          shiny.semantic::selectInput(
            inputId = ns("scale_adjust"),
            label = h3("Select a date scale", style = "font-weight: bold;"),
            value = "Month",
            choices = c("Monthly" = "1 month", "Weekly" = "1 week", "Daily" = "1 day")
          ),
          icon = shiny.semantic::icon("scale")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "download_menu",
          div(
            style = "display:inline-block;",
            shiny.semantic::selectInput(
              inputId = ns("plot_choice"),
              label = "Download",
              choices = c("Percentage Plot", "Percentage Over Time Plot")
            )
          ),
          div(
            style = "display:inline-block;margin-left:5px;",
            shiny.semantic::selectInput(
              inputId = ns("download_option"),
              label = "as",
              choices = c("png", "pdf (coming soon)")
            )
          )
        ),
        shiny.semantic::menu_item(
          tabName = "download_params",
          div(
            style = "display:inline-block;",
            shiny.semantic::numericInput(
              inputId = ns("width"),
              label = "width",
              value = 12,
              step = 1,
              min = 2,
              max = 20
            )
          ),
          div(
            style = "display:inline-block;margin-left:5px;",
            shiny.semantic::numericInput(
              inputId = ns("height"),
              label = "height",
              value = 7,
              step = 1,
              min = 2,
              max = 20
            )
          ),
          div(
            style = "display:inline-block;margin-left:10px;",
            shiny::downloadButton(
              outputId = ns("down"),
              label = ""
            )
          )
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

agreeServer <- function(id, result_auth, in_content, in_course, in_site, in_role, in_date_slider) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {

    ## Time Series Plot ##
    data_plot_ts <- reactive({

      ## List of validators ##
      validate(
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course"),
        need(input$date_min <= input$date_max, "Please select a minimum date that is less than the maximum.")
      )

      ## Data filters then summarises ##
      agree_plot_ts <- session_survey %>%
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
          date_group = case_when(
            input$scale_adjust == "1 month" ~ paste0(lubridate::month(Date, label = T, abbr = F), ", ", year(Date)),
            input$scale_adjust == "1 week" ~ paste0(year(Date), lubridate::week(Date)),
            input$scale_adjust == "1 day" ~ paste0(lubridate::day(Date))
          )
        ) %>%
        ungroup() %>%
        dplyr::mutate(question = str_remove_all(
          question,
          "How much do you agree with the\nfollowing statements about this\nfacilitator today\\? - "
        )) %>%
        group_by(date_group, question) %>%
        mutate(Percent = `Number Agree/Disagree` / sum(`Number Agree/Disagree`) * 100) %>%
        filter(Rating == "Agree/Strongly Agree") %>%
        group_by(date_group, Rating, question) %>%
        summarise(
          Percent = round(sum(Percent), 2),
          Date = Date
        )

      agree_plot_ts
    })

    # Ggplot for time series plot
    output$agree_plot_ts <- renderPlot({
      data_plot_ts() %>%
        ggplot(aes(
          x = ymd(Date),
          y = Percent
        )) +
        geom_area(color = "gray50", aes(fill = Rating), alpha = 0.6, position = position_identity()) + # position_identity is absolutely necessary here, not sure why
        geom_ribbon(color = "transparent", aes(
          ymin = Percent, ymax = 100,
          fill = "Neither Agree nor Disagree/Disagree/Strongly Disagree"
        ), alpha = 0.85) +
        geom_line(size = 1.25, alpha = 0.9, aes(group = 1)) +
        geom_point(size = 1, alpha = 0.9) +
        facet_wrap(~ fct_reorder(question, .x = `Percent`, .fun = mean, .desc = T)) +
        coord_cartesian() +
        scale_x_date(
          date_breaks = input$scale_adjust,
          date_labels = if (input$scale_adjust == "1 month") {
            "%b, %Y"
          } else if (input$scale_adjust == "1 week") {
            "%W"
          } else if (input$scale_adjust == "1 day") {
            "%W"
          },
          limits = c(min(data_plot_ts()$Date), max(data_plot_ts()$Date)),
          expand = c(0, 0)
        ) +
        scale_y_continuous(
          breaks = pretty_breaks(n = 5), limits = c(0, 100),
          labels = scales::percent_format(scale = 1), expand = c(0, 0)
        ) +
        scale_fill_manual(values = c(rev(tl_palette(n = 2, color = "blue", theme = "dark")))) +
        labs(x = "Date", title = glue::glue("{if (input$scale_adjust == '1 month') {'Monthly'} else if (input$scale_adjust == '1 week') {'Weekly'} else if (input$scale_adjust == '1 day') {'Daily'}} Percentage that Agree/Strongly Agree")) +
        theme_bw() + # BW Panel panel elements
        theme(
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(family = "Calibri"),
          text = element_text(family = "Calibri", face = "bold"),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(size = 10),
          strip.background = element_rect(fill = "white"),
          strip.text = element_text(size = 13),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5, family = "Calibri"),
          axis.line = element_line(size = 1.5)
        )
    })
    
    ## Agree plot n size ##
    agree_plot_n <- reactive({
      
      data_n <- session_survey %>%
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
        }
      
      nrow(data_n)
    })

    # Agree Percent Plot
    data_plot_agree <- reactive({
      
      ## List of validators ##
      validate(
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course"),
        need(input$date_min <= input$date_max, "Please select a minimum date that is less than the maximum.")
      )
      
      ## Data filters, then summarises ##
      agree_plot <- session_survey %>%
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


    # Ggplot for agree percent plot
    output$percent_agree_plot <- renderPlot({
      ggplot(data = data_plot_agree(), aes(x = Question, y = Percent, fill = factor(Response))) +
        geom_col() +
        geom_text(aes(label = if_else(Percent >= 3, paste0(round(Percent), "%"), "")), position = position_stack(vjust = 0.5)) +
        scale_fill_manual(values = c(
          "(1) Strongly disagree" = "#040404", "(2) Disagree" = "#032E3F",
          "(3) Neither agree nor disagree" = "#02587A", "(4) Agree" = "#0182B4", "(5) Strongly agree" = "#00ACF0"
        )) +
        labs(
          fill = "", title = "Percent that Agree/Strongly Agree with\nEach of the Following Statements",
          x = "", y = ""
        ) +
        coord_flip() +
        guides(fill = guide_legend(reverse = T)) +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        theme_tl(legend = T) +
        theme(
          axis.text.x = element_blank(),
          axis.text.y = element_text(lineheight = 1.1, size = 14),
          legend.position = "bottom",
          plot.title = element_text(lineheight = 1.1, size = 20, face = "bold"),
          legend.key.size = unit(1.25, "cm"),
          legend.text = element_text(size = 9)
        )
    })

    downloadGraph <- reactive({
      download_graph <- ggplot(data = data_plot_agree(), aes(x = Question, y = Percent, fill = factor(Response))) +
        geom_col() +
        geom_text(aes(label = if_else(Percent >= 3, paste0(round(Percent), "%"), "")), position = position_stack(vjust = 0.5)) +
        scale_fill_manual(values = c(
          "(1) Strongly disagree" = "#040404", "(2) Disagree" = "#032E3F",
          "(3) Neither agree nor disagree" = "#02587A", "(4) Agree" = "#0182B4", "(5) Strongly agree" = "#00ACF0"
        )) +
        labs(
          fill = "", title = "Percent that Agree/Strongly Agree with\nEach of the Following Statements",
          x = "", y = ""
        ) +
        coord_flip() +
        guides(fill = guide_legend(reverse = T)) +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        theme_tl(legend = T) +
        theme(
          axis.text.x = element_blank(),
          axis.text.y = element_text(lineheight = 1.1, size = 14),
          legend.position = "bottom",
          plot.title = element_text(lineheight = 1.1, size = 20, face = "bold"),
          legend.key.size = unit(1.25, "cm"),
          legend.text = element_text(size = 9)
        )
    })

    output$down <- downloadHandler(
      filename = function() {
        paste0("agree_plot.", input$download_option)
      },
      content = function(file) {
        if (input$download_option != "pdf (coming soon)") {
          ggsave(file, downloadGraph(), device = input$download_option, height = input$height, width = input$width)
        } else {
          options(shiny.usecairo = T)
          cairo_pdf(filename = file, width = input$width, height = input$height, family = "Calibri")
          downloadGraph()
        }
      }
    )
  })
}
