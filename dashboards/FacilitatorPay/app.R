# library(shinyjs)
library(shiny)
library(bs4Dash)
library(tidyverse)
library(gt)
library(ggforce)
library(ggfx)
library(shinycssloaders)

####################################################################################
# Values for the dashboard
# Remove 1000 for REAL VALUES
new_fac_pay_hourly <- 150 #* 1000
return_fac_pay_hourly <- 165 #* 1000
new_tech_hourly <- 50 #* 1000
returning_tech_hourly <- 50 #* 1000
lead_constant <- 100 #* 1000
tech_constant <- 75 #* 1000
content_train <- 100 #* 1000
####################################################################################
ui <- bs4DashPage(
  title = "Teaching Lab Payment Calculator",
  navbar = bs4DashNavbar(
    skin = "dark",
    status = "gray-light"
  ),
  sidebar = bs4DashSidebar(
    disable = T
  ),
  controlbar = bs4DashControlbar(
    disable = T
  ),
  footer = bs4DashFooter(
    a(
      href = "https://twitter.com/teachinglabHQ",
      target = "_blank", "@teachinglabHQ"
    ),
    right_text = "© Teaching Lab, 2021"
  ),
  body = bs4DashBody(
    shinyjs::useShinyjs(),
    bs4TabItems(
      bs4TabItem(
        tabName = "item1",
        fluidRow(
          column(
            12,
            bs4Card(
              title = h4("Complete the information below to generate calculations of your per course (top right) and/or per session (bottom right) pay:",
                style = "font-weight:bold;",
                align = "center"
              ),
              footer = HTML("<em>Note: This calculator applies to facilitating our core professional learning content- not for observations, office hours, coaching etc.</em>"),
              closable = TRUE,
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              collapsible = TRUE,
              fluidRow(
                column(
                  6,
                  div(
                    style = "white-space: nowrap;",
                    div(style = "display: inline-block; width: 100%;", radioButtons("facilitator_type", "I am a",
                      selected = character(0),
                      choiceNames = list(
                        tags$span(style = "font-weight: normal;", "New Facilitator (Began SY21)"),
                        tags$span(style = "font-weight: normal;", "Returning Facilitator (Began before SY21)")
                      ),
                      choiceValues = list("New Facilitator (Began SY21)", "Returning Facilitator (Began before SY21)")
                    )),
                    # HTML elements that appear conditionally
                    p(id = "element", "* $165", style = "display:inline-block; margin-left: -575px; color: green;"),
                    p(id = "element2", "* $150", style = "display:inline-block; margin-left: -660px; margin-top: 23px; vertical-align: top; color: green;")
                  ),
                  div(
                    style = "white-space: nowrap;",
                    div(style = "display: inline-block; width: 100%;", radioButtons("facilitator_type2", "I will be the",
                    selected = character(0),
                    choiceNames = list(
                      tags$span(style = "font-weight: normal;", "Lead Facilitator"),
                      tags$span(style = "font-weight: normal;", "Tech/Support Facilitator")
                    ),
                    choiceValues = list("Lead Facilitator", "Tech/Support Facilitator")
                  )),
                  # HTML elements that appear conditionally
                  p(id = "element3", "+ $100", style = "display:inline-block; margin-left: -685px; color: green;"),
                  p(id = "element4", "+ $75", style = "display:inline-block; margin-left: -745px; margin-top: 23px; vertical-align: top; color: green;")
                  ),
                  uiOutput("content_training"),
                  shiny::selectInput("session_count", "How many sessions are in the course?",
                    choices = c(1:10),
                    selected = 1
                  )
                ),
                column(
                  6,
                  withSpinner(
                    plotOutput("new_plot", width = "100%")
                  )
                )
              )
            )
          ),
          column(
            6,
            bs4Card(
              title = "Session Info",
              closable = TRUE,
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              collapsible = TRUE,
              tabPanel(
                tabName = "",
                uiOutput("numeric_inputs")
              )
            )
          ),
          column(
            6,
            bs4Card(
              title = "Itemized Total Course Pay",
              closable = TRUE,
              width = 12,
              # height = 414,
              solidHeader = TRUE,
              status = "primary",
              collapsible = TRUE,
              withSpinner(
                gt_output("itemized_payment_new")
              )
            )
          )
        )
      )
    )
  )
)
server <- function(input, output, session) {
  
  # Enable further buttons for each press
  observe({
    if (is.null(input$facilitator_type)) {
      shinyjs::disable("facilitator_type2")
    } else {
      shinyjs::enable("facilitator_type2")
    }
    if (is.null(input$facilitator_type2)) {
      shinyjs::disable("session_count")
    } else if (input$facilitator_type2 == "Tech/Support Facilitator") {
      shinyjs::enable("session_count")
    } else if (input$facilitator_type2 == "Lead Facilitator" & is.null(input$first_time)) {
      shinyjs::disable("session_count")
    } else if (input$facilitator_type2 == "Lead Facilitator" & !is.null(input$first_time)) {
      shinyjs::enable("session_count")
    }
  })
  
  # Enable HTML encoded multipliers
  observe({
    # First Question
    if (is.null(input$facilitator_type)) {
      shinyjs::hide("element")
      shinyjs::hide("element2")
    } else if (input$facilitator_type == "Returning Facilitator (Began before SY21)") {
      shinyjs::show("element", anim = T, animType = "fade")
      shinyjs::hide("element2")
    } else if (input$facilitator_type == "New Facilitator (Began SY21)") {
      shinyjs::hide("element")
      shinyjs::show("element2", anim = T, animType = "fade")
    }
    # Second Question
    if (is.null(input$facilitator_type2)) {
      shinyjs::hide("element3")
      shinyjs::hide("element4")
    } else if (input$facilitator_type2 == "Tech/Support Facilitator") {
      shinyjs::show("element3", anim = T, animType = "fade")
      shinyjs::hide("element4")
    } else if (input$facilitator_type2 == "Lead Facilitator") {
      shinyjs::hide("element3")
      shinyjs::show("element4", anim = T, animType = "fade")
    }
    # Third Question
    req(input$facilitator_type2)
    if (is.null(input$first_time)) {
      shinyjs::hide("element5")
    } else if (input$first_time == "Yes") {
      shinyjs::show("element5", anim = T, animType = "fade")
    } else if (input$first_time == "No") {
      shinyjs::hide("element5")
    }
  })
  
  
# Make data based on user inputs, different multipliers and additives
  new_data <- reactive({
    returning_fac <- if (is.null(input$facilitator_type) | is.null(input$facilitator_type2)) {
      0
    } else if (input$facilitator_type == "New Facilitator (Began SY21)" & input$facilitator_type2 == "Lead Facilitator") {
      new_fac_pay_hourly
    } else if (input$facilitator_type == "Returning Facilitator (Began before SY21)" & input$facilitator_type2 == "Lead Facilitator") {
      return_fac_pay_hourly
    } else if (input$facilitator_type2 == "Tech/Support Facilitator" & input$facilitator_type == "New Facilitator (Began SY21)") {
      new_tech_hourly
    } else if (input$facilitator_type2 == "Tech/Support Facilitator" & input$facilitator_type == "Returning Facilitator (Began before SY21)") {
      returning_tech_hourly
    }
    learning_time <- if (is.null(input$first_time)) {
      0
    } else if (input$first_time == "No") {
      0
    } else if (input$first_time == "Yes" & input$facilitator_type2 == "Lead Facilitator") {
      content_train
    } else {
      0
    }
    flat_pay <- if (is.null(input$facilitator_type2)) {
      0
    } else if (input$facilitator_type2 == "Lead Facilitator") {
      lead_constant
    } else if (input$facilitator_type2 == "Tech/Support Facilitator") {
      tech_constant
    }
    hours <- if (input$session_count == 0) {
      0
    } else {
      sum(
        map(1:input$session_count, ~ as.numeric(eval(parse(text = paste0("input$session", .x, "_session_count"))))) %>%
          as_vector(),
        na.rm = T
      )
    }
    return_df <- tibble(
      hours,
      returning_fac,
      learning_time,
      pay = ((returning_fac * hours) + learning_time + flat_pay)
    )
  })

  ## NEW DATA PAY NEEDS TO CALCULATE BASED EXCLUSIVELY ON ITEMIZED PAYMENT ADDITIONS

  ## TECH/SUPPORT 50*NUMBER OF SESSION HOURS + 75
  ## LEAD DEPENDENT ON RETURNING OR NEW AND IS 165/150 + 100

  output$new_plot <- renderPlot({
    cat(new_data()$returning_fac, sep = "\n")
    cat(new_data()$hours, sep = "\n")
    ggplot(data = new_data()) +
      geom_text(aes(x = 0, y = 0.5, label = "fake text"), color = "transparent") +
      geom_text(aes(x = -0.5, y = 0.5, label = "fake text"), color = "transparent") +
      with_outer_glow(
        geom_text(aes(x = 0, y = 0, label = scales::dollar_format()(new_data()$pay)), size = 30),
        colour = "green",
        sigma = 5,
        expand = 5
      ) +
      geom_text(aes(x = 0, y = -0.5, label = "fake text"), color = "transparent") +
      geom_text(aes(x = 0.5, y = -0.5, label = "fake text"), color = "transparent") +
      with_outer_glow(
        geom_circle(aes(x0 = 0, y0 = 0, r = 1), size = 5),
        colour = "forestgreen",
        sigma = 5,
        expand = 5
      ) +
      coord_fixed() +
      theme_void()
  })

  output$content_training <- renderUI({
    req(input$facilitator_type2)
    if (input$facilitator_type2 == "Lead Facilitator") {
      div(
        style = "white-space: nowrap;",
        div(style = "display: inline-block; width: 100%;", radioButtons("first_time", "Is this your first time facilitating a course (i.e. will you need to attend content training)?",
        selected = character(0),
        choiceNames = list(
          tags$span(style = "font-weight: normal;", "Yes"),
          tags$span(style = "font-weight: normal;", "No")
        ),
        choiceValues = list("Yes", "No")
      )),
      p(id = "element5", "+ $100", style = "display:inline-block; margin-left: -830px; margin-top: 23px; vertical-align: top; color: green;")
      )
    }
  })

  # Figure out how to hide unless double condition not null and lead facilitator is true
  output$numeric_inputs <- renderUI({
    if (!is.null(input$facilitator_type) & !is.null(input$facilitator_type2)) {
      map(1:input$session_count, ~ shiny::numericInput(
        inputId = paste0("session", .x, "_session_count"),
        label = paste0("Session ", .x, " Hours:"), min = 0, max = 100, value = 0, step = 0.25
      ))
    } else {
      print(HTML("Please Enter <em><b>ALL</b></em> of The Above Information"))
    }
  })

  outputOptions(output, "numeric_inputs", suspendWhenHidden = FALSE)
  gt_data_new <- reactive({
    # cat(names(input), sep = ", ")
    req(input$session1_session_count)
    # req(input$facilitator_type)
    return_df <- tibble(
      group = c("Training", "Support", rep("Session", input$session_count)),
      session = c(
        "Content Training", "Site and Context Support",
        map(1:input$session_count, ~ paste0("Session ", .x))
      ),
      hours = c(
        if (is.null(input$first_time)) {
          0
        } else if (input$first_time == "No") {
          0
        } else {
          1
        },
        1,
        map(1:input$session_count, ~ eval(parse(text = (paste0("input$session", .x, "_session_count")))))
      ),
      pay = c(
        if (is.null(input$first_time)) {
          0
        } else if (input$first_time == "No") {
          0
        } else {
          content_train
        },
        if (is.null(input$facilitator_type2)) {
          0
        } else if (input$facilitator_type2 == "Lead Facilitator") {
          lead_constant
        } else if (input$facilitator_type2 == "Tech/Support Facilitator") {
          tech_constant
        },
        map(1:input$session_count, ~ eval(parse(text = paste0("input$session", .x, "_session_count"))) * (if (input$facilitator_type2 == "Lead Facilitator" & input$facilitator_type == "Returning Facilitator (Began before SY21)") {
          return_fac_pay_hourly
        } else if (input$facilitator_type2 == "Lead Facilitator" & input$facilitator_type == "New Facilitator (Began SY21)") {
          new_fac_pay_hourly
        } else if (input$facilitator_type2 == "Tech/Support Facilitator" & input$facilitator_type == "New Facilitator (Began SY21)") {
          new_tech_hourly
        } else if (input$facilitator_type2 == "Tech/Support Facilitator" & input$facilitator_type == "Returning Facilitator (Began before SY21)") {
          returning_tech_hourly
        }))
      )
    )
  })

  output$itemized_payment_new <- render_gt({
    gt_data_new() %>%
      rename(Source = session, Hours = hours, Pay = pay) %>%
      mutate(
        Pay = as.numeric(Pay),
        Hours = as.numeric(Hours)
      ) %>%
      gt(groupname_col = "group") %>%
      fmt_currency(
        columns = vars(Pay)
      ) %>%
      summary_rows(
        columns = vars(Pay),
        fns = list(
          Total = ~ sum(., na.rm = T)
        ),
        formatter = fmt_currency
      ) %>%
      summary_rows(
        columns = vars(Hours),
        fns = list(
          Total = ~ sum(., na.rm = T)
        )
      )
  })
}


shinyApp(ui = ui, server = server)
