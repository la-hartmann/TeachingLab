ui <- dashboardPage(
  theme = "cosmo",
  suppress_bootstrap = F,
  # tags$head(tags$link(rel="shortcut icon", href="www/favicon.ico")),
  dashboardHeader(
    title = h2("Staff and Culture", style = "font-family:'Calibri'"),
    logo_path = "https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/86dbfc41-323a-4550-9eb6-3d9bb7358ad6/20160223+Teaching+Lab+Logo+Final_SM+HORIZONTAL+v3.png?format=1500w",
    logo_align = "center",
    titleWidth = "wide",
    color = "black",
    inverted = F
  ),
  dashboardSidebar(
    size = "wide",
    sidebarMenu(
      menuItem(
        tabName = "instruction_tab",
        icon = shiny::icon("align-left"),
        uiOutput("text_appear")
      ),
      menuItem(
        tabName = "question_type_tab", icon = shiny::icon("home"),
        selectInput("question_type",
          label = h3("Select a Culture Question to View"),
          choices = c(
            "General Working Conditions", "Management/Leadership", 
            "Employee Engagement", "Equity & Inclusion", 
            "Social Learning Groups", "Teaching Lab Values"
          )
        )
      ),
      menuItem(
        tabName = "question_tab", icon = shiny::icon("question"),
        uiOutput("question_select")
      ),
      tags$style(type='text/css', "label { line-height: 2!important;}"),
      menuItem(
        tabName = "manager_race_tab", icon = shiny::icon("user-friends"),
        shiny.semantic::multiple_radio(
          input_id = "manager_race",
          label = "Choose a Method of Comparison",
          choices = c(
            "Compare by Race",
            "Compare by Leadership"
          ),
          choices_value = c("How do you identify racially or ethnically?",
                            "Are you part of the Leadership Team at Teaching Lab?"),
          status = "primary"
        )
      )
    )
  ),
  dashboardBody(
    add_busy_spinner(spin = "fading-circle", color = "#04abeb"),
    h2("Overall % that Agree/Strongly agree with the Selected Question Group", style = "text-align:center;font-weight:bold;width:100%;"),
    plotOutput("overall_plot", height = "900px"),
    plotOutput("plot1", height = "900px")
    # uiOutput("ui_plot")
  )
)

server <- function(input, output) {

  # Render Question Type Sub-Item
  output$question_select <- renderUI({
    if (input$question_type == "General Working Conditions") {
      shiny::selectizeInput("question",
        label = h3("Add a Question to Highlight"),
        choices = unique(gwc_1$Question) %>% sort(),
        multiple = T,
        selected = NULL,
        options = list(plugins = list("remove_button"))
      )
    } else if (input$question_type == "Management/Leadership") {
      shiny::selectizeInput("question",
        label = h3("Select a Question to Highlight"),
        choices = unique(mngmnt$Question) %>% sort(),
        multiple = T,
        selected = NULL,
        options = list(plugins = list("remove_button"))
      )
    } else if (input$question_type == "Employee Engagement") {
      shiny::selectizeInput("question",
        label = h3("Select a Question to Highlight"),
        choices = unique(employee_engagement$Question) %>% sort(),
        multiple = T,
        selected = NULL,
        options = list(plugins = list("remove_button"))
      )
    } else if (input$question_type == "Equity & Inclusion") {
      shiny::selectizeInput("question",
        label = h3("Select a Question to Highlight"),
        choices = unique(equity_inclusion$Question) %>% sort(),
        multiple = T,
        selected = NULL,
        options = list(plugins = list("remove_button"))
      )
    } else if (input$question_type == "Social Learning Groups") {
      shiny::selectizeInput("question",
        label = h3("Select a Question to Highlight"),
        choices = unique(slgs$Question) %>% sort(),
        multiple = T,
        selected = NULL,
        options = list(plugins = list("remove_button"))
      )
    } else if (input$question_type == "Teaching Lab Values") {
      shiny::selectizeInput("question",
                            label = h3("Select a Question to Highlight"),
                            choices = unique(tl_values$Question) %>% sort(),
                            multiple = T,
                            selected = NULL,
                            options = list(plugins = list("remove_button"))
      )
    }
  })

  output$text_appear <- renderUI({
    h3("Begin by selecting a group to investigate, and then choose a question to highlight for comparison across surveys.")
  })
    
    plot_1_data <- reactive({
      data_1 <- if (input$question_type == "General Working Conditions") {
        data %>%
          select(
            gwc_group,
            input$manager_race,
            time
          ) %>%
          mutate(across(input$manager_race, ~ str_replace_all(.x, c("Yes" = "Manager",
                                                                    "No" = "Not a manager",
                                                                    "Other" = "Prefer not to answer")))) %>%
          pivot_longer(!c(input$manager_race, time), names_to = "Question", values_to = "Rating") %>%
          filter(Question != "Are you part of the Leadership Team at Teaching Lab?" &
                   Question != "How do you identify racially or ethnically?") %>%
          mutate(Question = case_when(Question %in% input$question ~ paste("<strong>", Question, "</strong>"),
                                      T ~ as.character(Question)),
                 Question = TeachingLab::html_wrap(Question, n = 25)) %>%
          drop_na(Rating) %>%
          group_by(get(input$manager_race), Question, Rating, time) %>%
          summarise(n = n()) %>%
          ungroup() %>%
          group_by(Question, `get(input$manager_race)`, Rating, time) %>%
          mutate(n_group = sum(n)) %>%
          ungroup(Rating) %>%
          mutate(Percent = round(100*(n_group/sum(n_group)))) %>%
          mutate(n = sum(n)) %>%
          filter(Rating %in% c("4", "5")) %>%
          summarise(Percent = sum(Percent),
                    n = n) %>%
          distinct(`get(input$manager_race)`, .keep_all = T)
      } else if (input$question_type == "Management/Leadership") {
        data %>%
          select(
            mngmnt_group,
            input$manager_race,
            time
          ) %>%
          mutate(across(input$manager_race, ~ str_replace_all(.x, c("Yes" = "Manager",
                                                                    "No" = "Not a manager",
                                                                    "Other" = "Prefer not to answer")))) %>%
          pivot_longer(!c(input$manager_race, time), names_to = "Question", values_to = "Rating") %>%
          filter(Question != "Are you part of the Leadership Team at Teaching Lab?" &
                   Question != "How do you identify racially or ethnically?") %>%
          mutate(Question = case_when(Question %in% input$question ~ paste("<strong>", Question, "</strong>"),
                                      T ~ as.character(Question)),
                 Question = TeachingLab::html_wrap(Question, n = 25)) %>%
          drop_na(Rating) %>%
          group_by(get(input$manager_race), Question, Rating, time) %>%
          summarise(n = n()) %>%
          ungroup() %>%
          group_by(Question, `get(input$manager_race)`, Rating, time) %>%
          mutate(n_group = sum(n)) %>%
          ungroup(Rating) %>%
          mutate(Percent = round(100*(n_group/sum(n_group)))) %>%
          mutate(n = sum(n)) %>%
          filter(Rating %in% c("4", "5")) %>%
          summarise(Percent = sum(Percent),
                    n = n) %>%
          distinct(`get(input$manager_race)`, .keep_all = T)
      } else if (input$question_type == "Employee Engagement") {
        data %>%
          select(
            employee_group,
            input$manager_race,
            time
          ) %>%
          mutate(across(input$manager_race, ~ str_replace_all(.x, c("Yes" = "Manager",
                                                                    "No" = "Not a manager",
                                                                    "Other" = "Prefer not to answer")))) %>%
          pivot_longer(!c(input$manager_race, time), names_to = "Question", values_to = "Rating") %>%
          filter(Question != "Are you part of the Leadership Team at Teaching Lab?" &
                   Question != "How do you identify racially or ethnically?") %>%
          mutate(Question = case_when(Question %in% input$question ~ paste("<strong>", Question, "</strong>"),
                                      T ~ as.character(Question)),
                 Question = TeachingLab::html_wrap(Question, n = 25)) %>%
          drop_na(Rating) %>%
          group_by(get(input$manager_race), Question, Rating, time) %>%
          summarise(n = n()) %>%
          ungroup() %>%
          group_by(Question, `get(input$manager_race)`, Rating, time) %>%
          mutate(n_group = sum(n)) %>%
          ungroup(Rating) %>%
          mutate(Percent = round(100*(n_group/sum(n_group)))) %>%
          mutate(n = sum(n)) %>%
          filter(Rating %in% c("4", "5")) %>%
          summarise(Percent = sum(Percent),
                    n = n) %>%
          distinct(`get(input$manager_race)`, .keep_all = T)
      } else if (input$question_type == "Equity & Inclusion") {
        data %>%
          select(
            equity_group,
            input$manager_race,
            time
          ) %>%
          mutate(across(input$manager_race, ~ str_replace_all(.x, c("Yes" = "Manager",
                                                                    "No" = "Not a manager",
                                                                    "Other" = "Prefer not to answer")))) %>%
          pivot_longer(!c(input$manager_race, time), names_to = "Question", values_to = "Rating") %>%
          filter(Question != "Are you part of the Leadership Team at Teaching Lab?" &
                   Question != "How do you identify racially or ethnically?") %>%
          mutate(Question = case_when(Question %in% input$question ~ paste("<strong>", Question, "</strong>"),
                                      T ~ as.character(Question)),
                 Question = TeachingLab::html_wrap(Question, n = 30)) %>%
          drop_na(Rating) %>%
          group_by(get(input$manager_race), Question, Rating, time) %>%
          summarise(n = n()) %>%
          ungroup() %>%
          group_by(Question, `get(input$manager_race)`, Rating, time) %>%
          mutate(n_group = sum(n)) %>%
          ungroup(Rating) %>%
          mutate(Percent = round(100*(n_group/sum(n_group)))) %>%
          mutate(n = sum(n)) %>%
          filter(Rating %in% c("4", "5")) %>%
          summarise(Percent = sum(Percent),
                    n = n) %>%
          distinct(`get(input$manager_race)`, .keep_all = T)
      } else if (input$question_type == "Social Learning Groups") {
        data %>%
          select(
            slgs_group,
            input$manager_race,
            time
          ) %>%
          filter(time %!in% c("March 2019", "July 2019", "February 2020")) %>%
          mutate(across(input$manager_race, ~ str_replace_all(.x, c("Yes" = "Manager",
                                                                    "No" = "Not a manager",
                                                                    "Other" = "Prefer not to answer")))) %>%
          pivot_longer(!c(input$manager_race, time), names_to = "Question", values_to = "Rating") %>%
          filter(Question != "Are you part of the Leadership Team at Teaching Lab?" &
                   Question != "How do you identify racially or ethnically?") %>%
          mutate(Question = case_when(Question %in% input$question ~ paste("<strong>", Question, "</strong>"),
                                      T ~ as.character(Question)),
                 Question = TeachingLab::html_wrap(Question, n = 25)) %>%
          drop_na(Rating) %>%
          group_by(get(input$manager_race), Question, Rating, time) %>%
          summarise(n = n()) %>%
          ungroup() %>%
          group_by(Question, `get(input$manager_race)`, Rating, time) %>%
          mutate(n_group = sum(n)) %>%
          ungroup(Rating) %>%
          mutate(Percent = round(100*(n_group/sum(n_group)))) %>%
          mutate(n = sum(n)) %>%
          filter(Rating %in% c("4", "5")) %>%
          summarise(Percent = sum(Percent),
                    n = n) %>%
          distinct(`get(input$manager_race)`, .keep_all = T)
      } else if (input$question_type == "Teaching Lab Values") {
        data %>%
          select(
            tl_values_group,
            input$manager_race,
            time
          ) %>%
          mutate(across(input$manager_race, ~ str_replace_all(.x, c("Yes" = "Manager",
                                                                    "No" = "Not a manager",
                                                                    "Other" = "Prefer not to answer")))) %>%
          pivot_longer(!c(input$manager_race, time), names_to = "Question", values_to = "Rating") %>%
          filter(Question != "Are you part of the Leadership Team at Teaching Lab?" &
                   Question != "How do you identify racially or ethnically?") %>%
          mutate(Question = case_when(Question %in% input$question ~ paste("<strong>", Question, "</strong>"),
                                      T ~ as.character(Question)),
                 Question = TeachingLab::html_wrap(Question, n = 25)) %>%
          drop_na(Rating) %>%
          group_by(get(input$manager_race), Question, Rating, time) %>%
          summarise(n = n()) %>%
          ungroup() %>%
          group_by(Question, `get(input$manager_race)`, Rating, time) %>%
          mutate(n_group = sum(n)) %>%
          ungroup(Rating) %>%
          mutate(Percent = round(100*(n_group/sum(n_group)))) %>%
          mutate(n = sum(n)) %>%
          filter(Rating %in% c("4", "5")) %>%
          summarise(Percent = sum(Percent),
                    n = n) %>%
          distinct(`get(input$manager_race)`, .keep_all = T)
      }
      
      totals <- data_1 %>%
        group_by(`Question`, time) %>%
        summarise(Percent = round(weighted.mean(Percent, n)),
                  n = round(sum(n))) %>%
        mutate(`get(input$manager_race)` = "Overall")
      
      data_1 %>%
        bind_rows(totals) #%>%
        # mutate(size = case_when(input$question == str_replace_all(Question, "<br>", " ") ~ 10,
        #                          input$question != str_replace_all(Question, "<br>", " ") ~ 5))
      
    })
    
    
    output$plot1 <- renderPlot({
      plot_1_data() %>%
        ggplot(aes(factor(Question), Percent, fill = relevel(factor(`get(input$manager_race)`), "Overall"), color = relevel(factor(`get(input$manager_race)`), "Overall"))) +
        geom_col(position = position_dodge2(reverse = T, width = 1)) +
        geom_text(aes(label = paste0(Percent, "% (n = ", n, ")"), 
                      hjust = if_else(Percent < 30, -0.2, 1)), 
                  position = position_dodge2(width = 1, reverse = T), color = "black") +
        facet_grid( ~ time, scales = "free", space = "free") +
        labs(x = "", y = "", color = "", fill = "", title = "% that Agree/Strongly Agree Over Time") +
        scale_fill_manual(values = tl_palette(n = length(unique(plot_1_data()$`get(input$manager_race)`)),
                                              color = "tl_colors",
                                              theme = "light")) +
        scale_color_manual(values = tl_palette(n = length(unique(plot_1_data()$`get(input$manager_race)`)),
                                              color = "tl_colors",
                                              theme = "light")) +
        scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 105), breaks = seq(0, 100, by = 20)) +
        coord_flip() +
        theme_tl() +
        guides(fill = guide_legend(label.position = "bottom", override.aes = list(size = 10)),
               color = "none") +
        theme(axis.text.x = element_markdown(lineheight = 1.1, size = 12),
              axis.text.y = element_markdown(size = 12, lineheight = 1.1),
              legend.position = "none",
              plot.title = element_text(size = 24, face = "bold"),
              # legend.key.size = unit(1.2, "cm"),
              legend.text = element_text(size = 10),
              legend.spacing.x = unit(1, "cm"),
              strip.text = element_text(hjust = 0.5, face = "bold", size = 17))
  })
    
    
    overall_plot_data <- reactive({
      data_1 <- if (input$question_type == "General Working Conditions") {
        data %>%
          select(
            gwc_group,
            input$manager_race
          ) %>%
          mutate(across(input$manager_race, ~ str_replace_all(.x, c("Yes" = "Manager",
                                                                    "No" = "Not a manager",
                                                                    "Other" = "Prefer not to answer")))) %>%
          pivot_longer(!c(input$manager_race), names_to = "Question", values_to = "Rating") %>%
          filter(Question != "Are you part of the Leadership Team at Teaching Lab?" &
                   Question != "How do you identify racially or ethnically?") %>%
          mutate(Question = case_when(Question %in% input$question ~ paste("<strong>", Question, "</strong>"),
                                      T ~ as.character(Question)),
                 Question = TeachingLab::html_wrap(Question, n = 25)) %>%
          drop_na(Rating) %>%
          group_by(get(input$manager_race), Question, Rating) %>%
          summarise(n = n()) %>%
          ungroup() %>%
          group_by(Question, `get(input$manager_race)`, Rating) %>%
          mutate(n_group = sum(n)) %>%
          ungroup(Rating) %>%
          mutate(Percent = round(100*(n_group/sum(n_group)))) %>%
          filter(Rating %in% c("4", "5")) %>%
          summarise(Percent = sum(Percent),
                    n = sum(n))
      } else if (input$question_type == "Management/Leadership") {
        data %>%
          select(
            mngmnt_group,
            input$manager_race
          ) %>%
          mutate(across(input$manager_race, ~ str_replace_all(.x, c("Yes" = "Manager",
                                                                    "No" = "Not a manager",
                                                                    "Other" = "Prefer not to answer")))) %>%
          pivot_longer(!c(input$manager_race), names_to = "Question", values_to = "Rating") %>%
          filter(Question != "Are you part of the Leadership Team at Teaching Lab?" &
                   Question != "How do you identify racially or ethnically?") %>%
          mutate(Question = case_when(Question %in% input$question ~ paste("<strong>", Question, "</strong>"),
                                      T ~ as.character(Question)),
                 Question = TeachingLab::html_wrap(Question, n = 25)) %>%
          drop_na(Rating) %>%
          group_by(get(input$manager_race), Question, Rating) %>%
          summarise(n = n()) %>%
          ungroup() %>%
          group_by(Question, `get(input$manager_race)`, Rating) %>%
          mutate(n_group = sum(n)) %>%
          ungroup(Rating) %>%
          mutate(Percent = round(100*(n_group/sum(n_group)))) %>%
          filter(Rating %in% c("4", "5")) %>%
          summarise(Percent = sum(Percent),
                    n = sum(n))
      } else if (input$question_type == "Employee Engagement") {
        data %>%
          select(
            employee_group,
            input$manager_race
          ) %>%
          mutate(across(input$manager_race, ~ str_replace_all(.x, c("Yes" = "Manager",
                                                                    "No" = "Not a manager",
                                                                    "Other" = "Prefer not to answer")))) %>%
          pivot_longer(!c(input$manager_race), names_to = "Question", values_to = "Rating") %>%
          filter(Question != "Are you part of the Leadership Team at Teaching Lab?" &
                   Question != "How do you identify racially or ethnically?") %>%
          mutate(Question = case_when(Question %in% input$question ~ paste("<strong>", Question, "</strong>"),
                                      T ~ as.character(Question)),
                 Question = TeachingLab::html_wrap(Question, n = 25)) %>%
          drop_na(Rating) %>%
          group_by(get(input$manager_race), Question, Rating) %>%
          summarise(n = n()) %>%
          ungroup() %>%
          group_by(Question, `get(input$manager_race)`, Rating) %>%
          mutate(n_group = sum(n)) %>%
          ungroup(Rating) %>%
          mutate(Percent = round(100*(n_group/sum(n_group)))) %>%
          filter(Rating %in% c("4", "5")) %>%
          summarise(Percent = sum(Percent),
                    n = sum(n))
      } else if (input$question_type == "Equity & Inclusion") {
        data %>%
          select(
            equity_group,
            input$manager_race
          ) %>%
          mutate(across(input$manager_race, ~ str_replace_all(.x, c("Yes" = "Manager",
                                                                    "No" = "Not a manager",
                                                                    "Other" = "Prefer not to answer")))) %>%
          pivot_longer(!c(input$manager_race), names_to = "Question", values_to = "Rating") %>%
          filter(Question != "Are you part of the Leadership Team at Teaching Lab?" &
                   Question != "How do you identify racially or ethnically?") %>%
          mutate(Question = case_when(Question %in% input$question ~ paste("<strong>", Question, "</strong>"),
                                      T ~ as.character(Question)),
                 Question = TeachingLab::html_wrap(Question, n = 30)) %>%
          drop_na(Rating) %>%
          group_by(get(input$manager_race), Question, Rating) %>%
          summarise(n = n()) %>%
          ungroup() %>%
          group_by(Question, `get(input$manager_race)`, Rating) %>%
          mutate(n_group = sum(n)) %>%
          ungroup(Rating) %>%
          mutate(Percent = round(100*(n_group/sum(n_group)))) %>%
          filter(Rating %in% c("4", "5")) %>%
          summarise(Percent = sum(Percent),
                    n = sum(n))
      } else if (input$question_type == "Social Learning Groups") {
        data %>%
          select(
            slgs_group,
            input$manager_race
          ) %>%
          mutate(across(input$manager_race, ~ str_replace_all(.x, c("Yes" = "Manager",
                                                                    "No" = "Not a manager",
                                                                    "Other" = "Prefer not to answer")))) %>%
          pivot_longer(!c(input$manager_race), names_to = "Question", values_to = "Rating") %>%
          filter(Question != "Are you part of the Leadership Team at Teaching Lab?" &
                   Question != "How do you identify racially or ethnically?") %>%
          mutate(Question = case_when(Question %in% input$question ~ paste("<strong>", Question, "</strong>"),
                                      T ~ as.character(Question)),
                 Question = TeachingLab::html_wrap(Question, n = 25)) %>%
          drop_na(Rating) %>%
          group_by(get(input$manager_race), Question, Rating) %>%
          summarise(n = n()) %>%
          ungroup() %>%
          group_by(Question, `get(input$manager_race)`, Rating) %>%
          mutate(n_group = sum(n)) %>%
          ungroup(Rating) %>%
          mutate(Percent = round(100*(n_group/sum(n_group)))) %>%
          filter(Rating %in% c("4", "5")) %>%
          summarise(Percent = sum(Percent),
                    n = sum(n))
      } else if (input$question_type == "Teaching Lab Values") {
        data %>%
          select(
            tl_values_group,
            input$manager_race
          ) %>%
          mutate(across(input$manager_race, ~ str_replace_all(.x, c("Yes" = "Manager",
                                                                    "No" = "Not a manager",
                                                                    "Other" = "Prefer not to answer")))) %>%
          pivot_longer(!c(input$manager_race), names_to = "Question", values_to = "Rating") %>%
          filter(Question != "Are you part of the Leadership Team at Teaching Lab?" &
                   Question != "How do you identify racially or ethnically?") %>%
          mutate(Question = case_when(Question %in% input$question ~ paste("<strong>", Question, "</strong>"),
                                      T ~ as.character(Question)),
                 Question = TeachingLab::html_wrap(Question, n = 25)) %>%
          drop_na(Rating) %>%
          group_by(get(input$manager_race), Question, Rating) %>%
          summarise(n = n()) %>%
          ungroup() %>%
          group_by(Question, `get(input$manager_race)`, Rating) %>%
          mutate(n_group = sum(n)) %>%
          ungroup(Rating) %>%
          mutate(Percent = round(100*(n_group/sum(n_group)))) %>%
          filter(Rating %in% c("4", "5")) %>%
          summarise(Percent = sum(Percent),
                    n = sum(n))
      }
      
      totals <- data_1 %>%
        group_by(`Question`) %>%
        summarise(Percent = round(weighted.mean(Percent, n)),
                  n = round(sum(n))) %>%
        mutate(`get(input$manager_race)` = "Overall")
      
      data_1 %>%
        bind_rows(totals)
      
    })
    
    
    
    output$overall_plot <- renderPlot({
      overall_plot_data() %>%
        ggplot(aes(factor(Question), Percent, fill = relevel(factor(`get(input$manager_race)`), "Overall"), color = relevel(factor(`get(input$manager_race)`), "Overall"))) +
        geom_col(position = position_dodge2(reverse = T)) +
        geom_text(aes(label = paste0(Percent, "% (n = ", n, ")"), 
                      hjust = if_else(Percent < 30, -0.2, 1)), 
                  position = position_dodge2(width = 1, reverse = T), color = "black") +
        labs(x = "", y = "", color = "", fill = "") +
        scale_fill_manual(values = tl_palette(n = length(unique(overall_plot_data()$`get(input$manager_race)`)),
                                              color = "tl_colors",
                                              theme = "light")) +
        scale_color_manual(values = tl_palette(n = length(unique(overall_plot_data()$`get(input$manager_race)`)),
                                               color = "tl_colors",
                                               theme = "light")) +
        scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 105), breaks = seq(0, 100, by = 20)) +
        coord_flip() +
        theme_tl() +
        guides(fill = guide_legend(label.position = "bottom", override.aes = list(size = 10)),
               color = "none") +
        theme(axis.text.x = element_markdown(lineheight = 1.1, size = 12),
              axis.text.y = element_markdown(size = 12, lineheight = 1.1),
              legend.position = "top",
              # legend.key.size = unit(1.2, "cm"),
              legend.text = element_text(size = 10),
              legend.spacing.x = unit(1, "cm"),
              strip.text = element_text(hjust = 0.5, face = "bold", size = 17))
    })
    

}

shinyApp(ui, server)
