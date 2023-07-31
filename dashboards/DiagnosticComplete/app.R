### Diagnostic Completion Dashboard ###
ui <- fluidPage(
  theme = my_theme,
  div(
    id = "page-top",
    shiny::radioButtons("current_theme", "App Theme:", c("Light" = "cerulean", "Dark" = "darkly"), inline = TRUE),
    div(class = "source_link", a(href = "https://teachinglab.github.io", "Return to Data Hub", icon("github"))),
  ),
  div(
    id = "app-title",
    titlePanel("Survey Responses per Partner"),
  ),
  gt_output("partner_gt"),
  div(
    id = "header",
    labeled_input(
      "prev_partner_btn", "Return to previous partner",
      actionButton("prev_partner", textOutput("prev_partner_label"), icon = icon("refresh"))
    ),
    labeled_input(
      "partner_selector", "Search for a partner",
      selectizeInput("partner",
        label = NULL,
        choices = levels(partner_data$your_site_district_parish_network_or_school) %>%
          sort() %>%
          purrr::prepend("All Partners"),
        selected = "All Partners",
        multiple = F
      )
    ),
    labeled_input(
      "rand_partner_btn", "Try a random partner",
      actionButton("rnd_partner", icon("dice"))
    )
  ),
  girafeOutput("partner_time", height = "900px")
)


server <- function(input, output, session) {
    
    # If the URL contains a partner on load, use that partner instead of the default of ann arbor
    bookmarked_partner <- "Nothing"
    current_partner <- reactiveVal( if(bookmarked_partner %in% unique_partners) bookmarked_partner else "All Partners")
    updateSelectizeInput(inputId = "partner", selected = isolate(current_partner()))
    
    # A book-keeping reactive so we can have a previous partner button
    previous_partner <- reactiveVal(NULL)
    
    observe({
        req(input$partner)
        # Set the previous partner to the non-updated current partner. If app is just
        # starting we want to populate the previous partner button with a random partner,
        # not the current partner
        selected_partner <- isolate(current_partner())
        just_starting <- selected_partner == input$partner
        previous_partner(if(just_starting) get_random_partner() else selected_partner)
        
        # Current partner now can be updated to the newly selected partner
        current_partner(input$partner)
        
        # Update the query string so the app will know what to do.
        # updateQueryString(make_url_hash(current_partner()), mode = "push")
    })
    
    observe({
        updateSelectizeInput(inputId = "partner", selected = isolate(previous_partner()))
    }) %>% bindEvent(input$prev_partner)
    
    observe({
        updateSelectizeInput(inputId = "partner", selected = get_random_partner())
    }) %>% bindEvent(input$rnd_partner)
    
    
    
    
  partner_data_final <- reactive({
    partner_data2 <- partner_data %>%
      group_by(your_site_district_parish_network_or_school) %>%
      summarise(
        n = n(),
        `Most Recent Observation` = lubridate::date(max(date_created))
      ) %>%
      ungroup() %>%
      rename(Site = 1, Count = 2) %>%
      arrange(desc(Count))
  })

  output$partner_gt <- render_gt(
    partner_data_final() %>%
      gt::gt() %>%
      tab_header(title = html("<strong>📈  Counts of Responses to Diagnostic Survey by Site  📈")) %>%
      data_color(
        columns = c(Count),
        colors = scales::col_numeric(
          palette = TeachingLab::tl_palette(theme = "dark", n = 10, color = "blue")[c(3:10)],
          domain = NULL
        )
      ) %>%
      fmt_number(columns = c(Count),
                 decimals = 0) %>%
      grand_summary_rows(
        columns = c(Count),
        fns = list(
          Total = ~ sum(., na.rm = T)
        ),
        formatter = fmt_number,
        decimals = 0
      ) %>%
      TeachingLab::gt_theme_tl(),
    width = px(1200)
  )

  partner_time_df <- reactive({
    validate(
      need(input$partner %in% partner_data$your_site_district_parish_network_or_school | input$partner == "All Partners",
           "No Observations for this Site Yet")
    )
    partner_data %>% {
      if (input$partner != "All Partners") dplyr::filter(., your_site_district_parish_network_or_school == input$partner) else .
    } %>%
    group_by(lubridate::date(date_created), your_site_district_parish_network_or_school) %>%
      count(sort = T) %>%
      ungroup() %>%
      rename(Date = 1, Site = 2, `Response Count` = 3)
  })

  output$partner_time <- renderggiraph({
    if (input$partner == "All Partners") {
      plot_time <- partner_time_df() %>%
        ggplot() +
        geom_line_interactive(aes(x = Date, y = `Response Count`, color = Site, 
                                  tooltip = Site, data_id = Site),
                              size = 1.3, alpha = 0.8) +
        geom_point_interactive(aes(x = Date, y = `Response Count`, 
                                   tooltip = paste0("Date: ", format(Date, "%b %d, %Y"), "\n Responses: ", `Response Count`, "\n Site: ", Site), 
                                   data_id = `Response Count`), 
                               color = "black", alpha = 0.4) +
        scale_color_manual(values = rev(tl_palette(n = length(unique(partner_time_df()$Site)), color = "blue", theme = "dark"))) +
        scale_fill_tl(n = length(unique(partner_time_df()$Site))) +
        scale_y_continuous(labels = scales::comma) +
        scale_x_date(limits = c(min(partner_time_df()$Date), max(partner_time_df()$Date)+2)) +
        theme_tl()
      girafe(ggobj = plot_time,
             options = list(opts_selection(type = "single", only_shiny = FALSE)))
    } else {
      plot_time <- partner_time_df() %>%
        # mutate(tooltip = glue::glue("Partner: {Site}\nResponses: {`Response Count`}")) %>%
        ggplot() +
        geom_line_interactive(aes(x = Date, y = `Response Count`, color = Site, 
                                  tooltip = Site, data_id = Site),
                              size = 3, alpha = 0.8) +
        geom_point_interactive(aes(x = Date, y = `Response Count`, 
                                   tooltip = paste0("Date: ", format(Date, "%b %d, %Y"), "\n Responses: ", `Response Count`, "\n Site: ", Site), 
                                   data_id = `Response Count`), 
                               color = "#04abeb", alpha = 0.4, size = 5) +
        scale_color_manual(values = rev(tl_palette(n = length(unique(partner_time_df()$Site)), color = "blue", theme = "dark"))) +
        scale_fill_tl(n = length(unique(partner_time_df()$Site))) +
        scale_y_continuous(labels = scales::comma) +
        scale_x_date(limits = c(min(partner_time_df()$Date), max(partner_time_df()$Date)+2)) +
        theme_tl()
      girafe(ggobj = plot_time,
             options = list(opts_selection(type = "single", only_shiny = FALSE)))
    }
  })

  observe({
    # Make sure theme is kept current with desired
    session$setCurrentTheme(
      bs_theme_update(my_theme, bootswatch = input$current_theme)
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
