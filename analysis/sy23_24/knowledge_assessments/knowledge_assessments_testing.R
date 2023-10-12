knowledge_assessments <- readr::read_rds("dashboards/knowledge_assessments/data/knowledge_assessments.rds")

test_data <- knowledge_assessments |>
  dplyr::filter(question == "Score") |>
  dplyr::mutate(percent = 100 * score / max_score) |>
  dplyr::group_by(prepost) |>
  dplyr::summarise(percent = round(mean(percent, na.rm = T), 2)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    prepost = ifelse(prepost == "pre",
      "Before",
      "After"
    ),
    prepost = factor(prepost, levels = c("Before", "After"))
  ) |>
  dplyr::add_row(prepost = "After", percent = 60)

know_count <- knowledge_assessments |>
  summarise(n = length(unique(id)), .by = prepost)

test_data |>
  e_chart(x = prepost, reorder = FALSE) |>
  e_bar(percent, name = "Before", color = c("#040404", "#04abeb"), colorBy = "data") |>
  e_labels(formatter = htmlwidgets::JS("(params) => params.value[1] + '%';"), fontWeight = "bold", fontSize = 14) |>
  e_axis(axis = "y", min = 0, max = 100, formatter = "{value}%") |>
  e_legend(bottom = "5%", show = FALSE) |>
  e_title(
    text = paste0(
      "Knowledge Assessments % Correct Before (n =", know_count$n[know_count$prepost == "pre"], ") and ",
      "After (n =", know_count$n[know_count$prepost == "post"], ")"
    ),
    left = "center",
    top = 10
  )

mock_data <- tibble::tribble(
  ~level1, ~level2, ~level3, ~sales,
  "Asia", "China", "Shanghai", 32L,
  "Asia", "China", "Beijing", 86L,
  "Asia", "China", "Chongqing", 30L,
  "Asia", "India", "Mumbai", 92L,
  "Asia", "India", "Kolkata", 75L,
  "Asia", "India", "Chennai", 99L,
  "America", "USA", "New York", 6L,
  "America", "USA", "Chicago", 12L,
  "America", "Argentina", "Buenos Aires", 54L,
  "America", "Argentina", "Rosario", 36L,
  "America", "Brasil", "Sao Paulo", 2L,
  "America", "Brasil", "Rio de Janeiro", 64L,
  "Europe", "Spain", "Madrid", 54L,
  "Europe", "Spain", "Barcelona", 46L,
  "Europe", "Spain", "Sevilla", 67L,
  "Europe", "Italy", "Rome", 22L,
  "Europe", "France", "Paris", 42L,
  "Europe", "France", "Marseille", 91L
)

plot_sales_data <- function(chart_data, chart_title, chart_color, chart_drill_to) {
  sales_chart <- chart_data |>
    e_chart(x = level) |>
    e_bar(total, name = chart_title, color = chart_color)
  # Adding the click observer only when drill_to is passed
  if (!is.null(chart_drill_to)) {
    print(chart_drill_to)
    sales_chart <- sales_chart |>
      e_on(
        query = "series.bar",
        # Set input values
        handler = glue::glue(
          "function(params){
             Shiny.setInputValue(
              'custom_bar_click',
              {clicked_level: 'level2', drilled_place: params.name}, {priority: 'event'}
             );
           }",
          .open = "<<", .close = ">>"
        ),
        event = "click"
      )
  }
  return(sales_chart)
}
ui <- fluidPage(
  h1("Drill Down in Shiny"),
  echarts4rOutput("chart")
)
# Define server
server <- function(input, output) {
  output$chart <- renderEcharts4r({
    # Our custom input value that we send from the bar click
    # print(input$custom_bar_click)
    print(input$custom_bar_click$clicked_level)
    if (is.null(input$custom_bar_click)) {
      # Prepare data for chart
      chart_data <- mock_data |>
        group_by(level = level1) |>
        summarise(total = sum(sales))
      
      # Create chart
      plot_sales_data(
        chart_data = chart_data,
        chart_title = "Sales by Continent",
        chart_color = "#5470C6",
        chart_drill_to = "level2"
      )
    } else if (input$custom_bar_click$clicked_level == "level2") {
      # Prepare data for chart
      chart_data <- mock_data |>
        filter(level1 == input$custom_bar_click$drilled_place) |>
        group_by(level = level2) |>
        summarise(total = sum(sales))
      
      # Create chart
      plot_sales_data(
        chart_data = chart_data,
        chart_title = glue::glue(
          "Sales by Country (Filtered for {input$custom_bar_click$drilled_place})"
        ),
        chart_color = "#91CC75",
        chart_drill_to = "level3"
      )
    } else if (input$custom_bar_click$clicked_level == "level3") {
      # Prepare data for chart
      chart_data <- mock_data |>
        filter(level2 == input$custom_bar_click$drilled_place) |>
        group_by(level = level3) |>
        summarise(total = sum(sales))
      
      # Create chart
      plot_sales_data(
        chart_data = chart_data,
        chart_title = glue::glue(
          "Sales by City (Filtered for {input$custom_bar_click$drilled_place})"
        ),
        chart_color = "#FAC858",
        chart_drill_to = NULL
      )
    }
  })
}
shinyApp(ui, server)

