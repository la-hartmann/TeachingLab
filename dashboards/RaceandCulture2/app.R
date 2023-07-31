ui <- dashboardPage(
  theme = "cosmo",
  suppress_bootstrap = F,
  # tags$head(tags$link(rel="shortcut icon", href="www/favicon.ico")),
  dashboardHeader(
    title = h2("Race and Culture", style = "font-family:'Calibri'"),
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
          label = h3("Select a Culture Question to Highlight"),
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
      menuItem(
        tabName = "manager_race_tab", icon = shiny::icon("user-friends"),
        shiny.semantic::multiple_radio(
          input_id = "manager_race",
          label = "Choose a Method of Comparison",
          choices = c(
            "Compare by Race" = "How do you identify racially or ethnically?",
            "Compare by Leadership" = "Are you part of the Leadership Team at Teaching Lab?"
          ),
          status = "primary"
        )
      )
    )
  ),
  dashboardBody(
    add_busy_spinner(spin = "fading-circle", color = "#04abeb"),
    h2("% that Agree/Strongly agree with the Selected Question Group", style = "text-align:center;font-weight:bold;width:100%;"),
    plotOutput("plot1", height = "1000px")
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
      if (input$question_type == "General Working Conditions") {
      data_1 <- data %>%
        select(
          gwc_group,
          input$manager_race,
          time
        ) %>%
        group_by(time, get(input$manager_race)) %>%
        summarise(across(everything(), ~ round(100 * (sum(.x %in% c("4", "5"), na.rm = T) / length(which(!is.na(.x))))))) %>%
        pivot_longer(!c(time, `get(input$manager_race)`), names_to = "Question", values_to = "Percent") %>%
        filter(Question != "Are you part of the Leadership Team at Teaching Lab?" & 
                 Question != "How do you identify racially or ethnically?") %>%
        mutate(Question = case_when(Question %in% input$question ~ paste("<strong>", Question, "</strong>"),
                                    T ~ as.character(Question)),
               Question = TeachingLab::html_wrap(Question, n = 25))
    } else if (input$question_type == "Management/Leadership") {
        data %>%
            select(
                mngmnt_group,
                input$manager_race,
                time
            ) %>%
        group_by(time, get(input$manager_race)) %>%
        summarise(across(everything(), ~ round(100 * (sum(.x %in% c("4", "5"), na.rm = T) / length(which(!is.na(.x))))))) %>%
        pivot_longer(!c(time, `get(input$manager_race)`), names_to = "Question", values_to = "Percent") %>%
        filter(Question != "Are you part of the Leadership Team at Teaching Lab?" & 
                 Question != "How do you identify racially or ethnically?") %>%
        mutate(Question = case_when(Question %in% input$question ~ paste("<strong>", Question, "</strong>"),
                                    T ~ as.character(Question)),
               Question = TeachingLab::html_wrap(Question, n = 25))
    } else if (input$question_type == "Employee Engagement") {
        data %>%
            select(
                employee_group,
                input$manager_race,
                time
            ) %>%
        group_by(time, get(input$manager_race)) %>%
        summarise(across(everything(), ~ round(100 * (sum(.x %in% c("4", "5"), na.rm = T) / length(which(!is.na(.x))))))) %>%
        pivot_longer(!c(time, `get(input$manager_race)`), names_to = "Question", values_to = "Percent") %>%
        filter(Question != "Are you part of the Leadership Team at Teaching Lab?" & 
                 Question != "How do you identify racially or ethnically?") %>%
        mutate(Question = case_when(Question %in% input$question ~ paste("<strong>", Question, "</strong>"),
                                    T ~ as.character(Question)),
               Question = TeachingLab::html_wrap(Question, n = 25))
    } else if (input$question_type == "Equity & Inclusion") {
        data %>%
            select(
                equity_group,
                input$manager_race,
                time
            ) %>%
        group_by(time, get(input$manager_race)) %>%
        summarise(across(everything(), ~ round(100 * (sum(.x %in% c("4", "5"), na.rm = T) / length(which(!is.na(.x))))))) %>%
        pivot_longer(!c(time, `get(input$manager_race)`), names_to = "Question", values_to = "Percent") %>%
        filter(Question != "Are you part of the Leadership Team at Teaching Lab?" & 
                 Question != "How do you identify racially or ethnically?") %>%
        mutate(Question = case_when(Question %in% input$question ~ paste("<strong>", Question, "</strong>"),
                                    T ~ as.character(Question)),
               Question = TeachingLab::html_wrap(Question, n = 50))
    } else if (input$question_type == "Social Learning Groups") {
        data %>%
            select(
                slgs_group,
                input$manager_race,
                time
            ) %>%
        group_by(time, get(input$manager_race)) %>%
        summarise(across(everything(), ~ round(100 * (sum(.x %in% c("4", "5"), na.rm = T) / length(which(!is.na(.x))))))) %>%
        pivot_longer(!c(time, `get(input$manager_race)`), names_to = "Question", values_to = "Percent") %>%
        filter(Question != "Are you part of the Leadership Team at Teaching Lab?" & 
                 Question != "How do you identify racially or ethnically?") %>%
        mutate(Question = case_when(Question %in% input$question ~ paste("<strong>", Question, "</strong>"),
                                    T ~ as.character(Question)),
               Question = TeachingLab::html_wrap(Question, n = 25))
    } else if (input$question_type == "Teaching Lab Values") {
        data_1 <- data %>%
            select(
                tl_values_group,
                input$manager_race,
                time
            ) %>%
          group_by(time, get(input$manager_race)) %>%
          summarise(across(everything(), ~ round(100 * (sum(.x %in% c("4", "5"), na.rm = T) / length(which(!is.na(.x))))))) %>%
          pivot_longer(!c(time, `get(input$manager_race)`), names_to = "Question", values_to = "Percent") %>%
          filter(Question != "Are you part of the Leadership Team at Teaching Lab?" & 
                   Question != "How do you identify racially or ethnically?") %>%
          mutate(Question = case_when(Question %in% input$question ~ paste("<strong>", Question, "</strong>"),
                                      T ~ as.character(Question)),
                 Question = TeachingLab::html_wrap(Question, n = 25))
    }
      })
    
    plot_height <- reactive({
      plot_height_var <- if (input$question_type == "General Working Conditions") {
        1000
      } else if (input$question_type == "Equity & Inclusion") {
        1800
      } else if (input$question_type == "Social Learning Groups") {
        800
      } else if (input$question_type == "Management/Leadership") {
        900
      } else if (input$question_type == "Employee Engagement") {
        1400
      } else if (input$question_type == "Teaching Lab Values") {
        900
      }
      plot_height_var <- paste0(plot_height_var, "px")
      print(plot_height_var)
    })
    
    output$plot1 <- renderPlot({
      plot_1_data() %>%
        mutate(`get(input$manager_race)` = str_replace_all(`get(input$manager_race)`, c("Yes" = "Manager",
                                                                                        "No" = "Not a manager"))) %>%
        ggplot(aes(factor(Question), Percent, fill = `get(input$manager_race)`, color = `get(input$manager_race)`)) +
        geom_col(position = position_dodge()) +
        geom_text(aes(label = paste0(Percent, "%")), position = position_dodge(width = 1), hjust = -0.3) +
        facet_wrap( ~ time) +
        labs(x = "", y = "", color = "", fill = "") +
        scale_fill_manual(values = tl_palette(n = length(unique(plot_1_data()$`get(input$manager_race)`)),
                                              color = "tl_colors",
                                              theme = "light")) +
        scale_color_manual(values = tl_palette(n = length(unique(plot_1_data()$`get(input$manager_race)`)),
                                              color = "tl_colors",
                                              theme = "light")) +
        scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 105), breaks = seq(0, 100, by = 10)) +
        coord_flip() +
        theme_tl() +
        guides(fill = guide_legend(label.position = "bottom", override.aes = list(size = 10))) +
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
