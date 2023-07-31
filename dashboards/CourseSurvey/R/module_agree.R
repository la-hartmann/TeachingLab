#### Course Survey Dashboard ####
uiAgree <- function(id, label = "Counter") {
  ns <- NS(id)
  shiny::tagList(
    shiny.semantic::sidebar_layout(
      sidebar_panel = shiny.semantic::sidebar_panel(
        style = sidebar_style,
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
            plotOutput(ns("percent_agree_plot"), height = "800px") |>
              withSpinner(type = 3, color.background = "white"),
            verbatimTextOutput(ns("agree_strongly_agree_text")) |>
              withSpinner(type = 3, color.background = "white")
          ),
          div(
            class = "sixteen wide column",
            plotOutput(ns("agree_plot_ts"), height = "800px") |>
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
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )

      data_plot <- course_survey |>
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
        # Rename with line breaks every 27 characters
        mutate(question = gsub("(.{28,}?)\\s", "\\1\n", question)) |>
        drop_na(answer) |>
        dplyr::group_by(question, date_created) |>
        # Group by input variable
        mutate(`Number Agree/Disagree` = n()) |>
        mutate(answer = str_remove_all(answer, "\\([:digit:]\\) ")) |>
        mutate(
          Rating = case_when(
            answer %in% c("Agree", "Strongly agree") ~ "Agree/Strongly Agree",
            answer %in% c("Neither agree nor disagree", "Disagree", "Strongly disagree") ~ "Neither/Disagree/Strongly Disagree"
          ),
          date_group = case_when(
            input$scale_adjust == "1 month" ~ paste0(lubridate::month(date_created, label = T, abbr = F), ", ", year(date_created)),
            input$scale_adjust == "1 week" ~ paste0(year(date_created), lubridate::week(date_created)),
            input$scale_adjust == "1 day" ~ paste0(lubridate::day(date_created))
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

      #### Validate that there is data ####
      validate(
        need(nrow(data_plot) > 0, "There are no observations for this set of filters")
      )

      #### Return the data ####
      data_plot
    })

    # Ggplot for time series plot
    output$agree_plot_ts <- renderPlot({
      data_plot_ts() |>
        ggplot(aes(
          x = ymd(date_created),
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
          limits = c(min(data_plot_ts()$date_created), max(data_plot_ts()$date_created)),
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

    # Agree Percent Plot
    data_plot_agree <- reactive({
      
      validate(
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )

      agree_plot <- course_survey |>
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

      #### Validate that there is data ####
      validate(
        need(nrow(agree_plot) > 0, "There are no observations for this set of filters")
      )

      #### Return the data ####
      agree_plot
    })

    agree_plot_n <- reactive({
      data_n <- course_survey |>
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
    
    agree_text <- reactive({
      
      ## List of validators ##
      validate(
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$role), "Please select at least one role"),
        need(!is.null(input$content), "Please select at least one content area"),
        need(!is.null(input$course), "Please select at least one course")
      )
      
      plot_agree <- course_survey |>
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
      
      facilitated_agree <- plot_agree |>
        tlShiny::agree_strongly_agree(question = "I am satisfied with how the\ncourse was facilitated.")
      
      quality_agree <- plot_agree |>
        tlShiny::agree_strongly_agree(question = "I am satisfied with the\noverall quality of this\ncourse.")
      
      community_agree <- plot_agree |>
        tlShiny::agree_strongly_agree(question = "I felt a sense of community\nwith the other participants in\nthis course.")
      
      practice_agree <- plot_agree |>
        tlShiny::agree_strongly_agree(question = "I will apply what I have\nlearned in this course to my\npractice.")
      
      implement_agree <- plot_agree |>
        tlShiny::agree_strongly_agree(question = "The strategies I’ve learned\nin the course are easy to\nimplement.")
      
      improve_coaching_agree <- plot_agree |>
        tlShiny::agree_strongly_agree(question = "The strategies I’ve learned\nin this course will improve\nmy coaching or supervision of\nteachers.")
      
      improve_instruction_agree <- plot_agree |>
        tlShiny::agree_strongly_agree(question = "The strategies I’ve learned\nin this course will improve my\ninstruction.")
      
      supported_agree <- plot_agree |>
        tlShiny::agree_strongly_agree(question = "This course has supported\nme in being responsive\nto students' backgrounds,\ncultures, and points of view.")
      
      new_data <- c(paste0("• ", facilitated_agree, " agreed they were satisfied with how the course was facilitated.", "\n",
                           "• ", quality_agree, " agreed they were satisfied with the overall quality of this course.", "\n",
                           "• ", community_agree, " agreed they felt a sense of community with the other participants in this course. even though we were meeting virtually.", "\n",
                           "• ", practice_agree, " agreed they will apply what I have learned in this course to my practice in the next 4-6 weeks.", "\n",
                           "• ", implement_agree, " agreed that the strategies I’ve learned in the course are easy to implement.", "\n",
                           "• ", improve_coaching_agree, " agreed that the strategies I’ve learned in this course will improve my coaching or supervision of teachers.", "\n",
                           "• ", improve_instruction_agree, " agreed that the strategies I’ve learned in this course will improve my instruction.", "\n",
                           "• ", supported_agree, " agreed that the course has supported me in being responsive to students' backgrounds, cultures, and points of view.", "\n"))
      
      new_data
    })
    
    output$agree_strongly_agree_text <- renderText({
      agree_text()
    })


    # Ggplot for agree percent plot
    output$percent_agree_plot <- renderPlot({
      ggplot(data = data_plot_agree(), aes(x = fct_reorder(Question, Percent, .desc = T), 
                                           y = Percent, 
                                           fill = factor(Response))) +
        geom_col() +
        geom_text(aes(label = if_else(Percent >= 3, paste0(round(Percent), "%"), "")), 
                  position = position_stack(vjust = 0.5),
                  family = "Calibri Bold",
                  fontface = "bold") +
        scale_fill_manual(values = c(
          "(1) Strongly disagree" = "#040404", "(2) Disagree" = "#032E3F",
          "(3) Neither agree nor disagree" = "#02587A", "(4) Agree" = "#0182B4", "(5) Strongly agree" = "#00ACF0"
        )) +
        labs(
          fill = "", title = "Percent that Agree/Strongly Agree with\nEach of the Following Statements",
          x = "", y = "",
          subtitle = glue::glue("Given the filters applied there are {agree_plot_n()} responses")
        ) +
        coord_flip() +
        guides(fill = guide_legend(reverse = T)) +
        scale_y_continuous(labels = scales::percent_format(scale = 1),
                           expand = c(0, 100.01)) +
        theme_tl(legend = T) +
        theme(
          axis.text.y = element_text(lineheight = 1.1, size = 12),
          # axis.text.x = element_text(size = 11),
          axis.text.x = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(lineheight = 1.1),
          legend.key.size = unit(1.25, "cm")
        )
    })

    downloadGraph <- reactive({
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
          axis.text.y = element_text(lineheight = 1.1, size = 12),
          # axis.text.x = element_text(size = 11),
          axis.text.x = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(lineheight = 1.1),
          legend.key.size = unit(1.25, "cm")
        )
    })

    downloadGraph2 <- reactive({
      data_plot_ts() |>
        ggplot(aes(
          x = ymd(date_created),
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
          limits = c(min(data_plot_ts()$date_created), max(data_plot_ts()$date_created)),
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
