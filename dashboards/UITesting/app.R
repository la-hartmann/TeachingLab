library(shiny)

ui <- fluidPage(
    fluidRow(
        column(
            width = 2
            , radioButtons(
                inputId = 'plotcount'
                , label   = 'Plot Count'
                , choices = as.character(1:4)
            )
        ),
        column(
            width = 10
            , uiOutput("plot.ui")
        )
    )
)

server <- function(input, output) {
    
    plotCount <- reactive({
        req(input$plotcount)
        as.numeric(input$plotcount)
    })
    
    plotHeight <- reactive(350 * plotCount())      
    
    output$plots <- renderPlot({
        
        req(plotCount())
        
        if (plotCount() == 0){
            plot.new()
            return()
        }
        
        opar <- par(mfrow = c(plotCount(), 1L))
        
        for (i in 1:plotCount()) {
            plot(1:100, 1:100, pch = 19)
        }
        
        par(opar)
    })
    
    output$plot.ui <- renderUI({
        plotOutput("plots", height = plotHeight())
    })
    
}

shinyApp(ui = ui, server = server)