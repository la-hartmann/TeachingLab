source("scripts/source_code_for_shiny.R")

ui <- fluidPage(
    
    title = "A Visual Exploration of Teaching Lab Data",
    
    # meta tags
    meta() %>%
        meta_social(
            title = "A Visual Exploration of Teaching Lab Data",
            description = "And an exercise in Shiny Scrollytelling",
            image = "https://raw.githubusercontent.com/connorrothschild/shiny-scrollytell/master/images/thumbnail.png",
            image_alt = "A Visual Exploration of Teaching Lab Data",
            twitter_creator = "@CL_Rothschild",
            twitter_card_type = "summary_large_image",
            twitter_site = "@CL_Rothschild"
        ),
    
    # suppress warning messages while data is loading on-screen
    tags$style(
        type = "text/css",
        ".shiny-output-error { visibility: hidden; }",
        ".shiny-output-error:before { visibility: hidden; }"
    ),
    tags$head(includeCSS("www/style.css")),
    
    # article title & name
    fluidRow(
        HTML(
            "<center>
                <h1>A Visual Exploration of Teaching Lab Data</h1>
                </center>"
        )
    ),
    
    br(),
    
    fluidRow(column(1),
             
             column(
                 10,
                 # intro text
                 fluidRow(id = 'text',
                          column(1),
                          column(
                              10,
                              br(),
                              text0,
                              hr()#,
                              # h1(
                              #   class = "instructions",
                              #   "How to read this chart:",
                              #   br(),
                              #   br(),
                              #   "The size of each",
                              #   icon("circle"),
                              #   "corresponds to the number of workers in that job.",
                              #   br(),
                              #   "Hover over each",
                              #   icon("circle"),
                              #   "to see details on the occupation's income and probability of automation.",
                              #   br(),
                              #   "Double click on a",
                              #   icon("circle"),
                              #   "in the legend to focus on a specific level of education."
                              # )
                          ),
                          column(1)),
                 # plot object for intro
                 girafeOutput("introPlot", height = '400px')
             ),
             
             column(1)),
    
    # scrollytelling plot
    scrolly_container(
        "scr"
        ,
        scrolly_graph(
            br(),
            br(),
            textOutput("section"),
            br(),
            HTML('<center>'),
            girafeOutput("plot", height = "800px"),
            HTML('</center>')
            
        )
        ,
        scrolly_sections(
            HTML('<center>'),
            scrolly_section(id = 0, render_text(0)),
            scrolly_section(id = 1, render_text(1)),
            scrolly_section(id = 2, render_text(2)),
            scrolly_section(id = 3, render_text(3)),
            scrolly_section(id = 4, render_text(4)),
            scrolly_section(id = 5, render_text(5)),
            scrolly_section(id = 6, render_text(6)),
            scrolly_section(id = 7, render_text(7)),
            scrolly_section(id = 8, render_text(8)),
            scrolly_section(id = 9, render_text(9)),
            scrolly_section(id = 10, render_text(10)),
            # add a scrolly_section with nothing in it;
            # this buffer prevents the plot from disappearing while reading last section
            scrolly_section(id = "buffer", br()),
            HTML('</center>')
        )
        
    ),
    
    # concluding text
    div(fluidRow(
        id = 'text',
        column(2),
        column(8,
               concludingtext,
               br()),
        column(2)
    ), style = 'margin-top: -300px;'),
    
    br(),
    br(),
    br(),
    hr(),
    
    fluidRow(column(1),
             column(10,
                    technicalnotes),
             column(1)),
    br(),
    br(),
    column(1)
    
)

# server
server <- function(input, output, session) {
    
    output$plot <- renderggiraph({
        add <- as.numeric(input$scr)
        print(add)
        
        title <- data_time_ela %>%
            dplyr::filter(reveal == add) %>%
            pull(name) %>%
            first()
        # title <- "test title"
        
        plot1 <- data_time_ela %>%
            dplyr::filter(reveal == add) %>%
            ggplot(aes(x = `Timeline of Obs`, y = value, fill = `Timeline of Obs`, color = `Timeline of Obs`,
                       tooltip = paste0("Average: ", round(value, 2)), data_id = value)) +
            geom_col_interactive() +
            geom_text(aes(label = paste0("n = ", n)), position = position_dodge(), vjust = -1) +
            guides(fill = guide_legend(nrow = 2, byrow = T)) +
            labs(x = "Season", y = "Average positive indicator(s)", title = "") +
            scale_fill_manual(values = c("Summer 2019" = "#040404", "Fall 2019" = "#032533",
                                         "Winter 2020" = "#024762", "Spring 2020" = "#016891",
                                         "Winter 2021" = "#008AC0", "Spring 2021" = "#00ACF0")) +
            scale_color_manual(values = c("Summer 2019" = "#040404", "Fall 2019" = "#032533",
                                          "Winter 2020" = "#024762", "Spring 2020" = "#016891",
                                          "Winter 2021" = "#008AC0", "Spring 2021" = "#00ACF0")) +
            scale_y_continuous(limits = c(0, 4.25)) +
            theme_tl(legend = T) +
            theme(legend.position = "bottom",
                  legend.box = "horizontal",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  strip.text = element_markdown(lineheight = 1.1, hjust = 0.5),
                  plot.title = element_markdown(size = 24, face = "bold"))
        girafe(ggobj = plot1,
               options = list(
                   opts_sizing(rescale = T, width = 0.7)
               ))
    })
    
    output$introPlot <- renderggiraph({
        introPlot
    })
    output$scr <- renderScrollytell({
        scrollytell()
    })
    renderText(paste0("Section: ", input$scr))
    observe({
        cat("section:", input$scr, "\n")
    })
    
}
# Run the application
shinyApp(ui = ui, server = server)
