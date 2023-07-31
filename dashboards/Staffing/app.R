##### Staffing Requests Page ######

bs4DashTheme <- create_theme(
  bs_vars_font(
    family_sans_serif = "Calibri",
    size_base = "20px"
  ),
  bs4dash_layout(
    main_bg = "white",
    sidebar_width = "0px"
  ), # main background
  bs4dash_color(
    gray_900 = "#00e9e5", 
    white = "#272c30",
    green = "#68AF8F",
    lime = "#93C6AF"
  )
)

ui <- dashboardPage(
  title = "Teaching Lab Staffing Site",
  header = dashboardHeader(
    title = h2("PM Staffing Requests Page",
      style = "font-weight:bold; color:#E6E9EB;margin-bottom: 1rem;margin-top: 1rem;text-align: center;margin-left: auto;",
      align = "center"
    ),
    skin = "light",
    status = "lightblue"
  ),
  sidebar = dashboardSidebar(
    disable = T
  ),
  footer = dashboardFooter(
    tags$head(tags$style(HTML("a {color: #04ABEB}"))),
    left = a(
      href = "https://www.surveymonkey.com/r/StaffingFeedback1",
      target = "_blank", "Feedback Survey"
    ),
    right = "© Teaching Lab, 2022"
  ),
  body = dashboardBody(
    HTML('<input type="text" id="client_time" name="client_time" style="display: none;"> '),
    HTML('<input type="text" id="client_time_zone_offset" name="client_time_zone_offset" style="display: none;"> '),
    tags$script('
    $(function() {
      var time_now = new Date()
      $("input#client_time").val(time_now.getTime())
      $("input#client_time_zone_offset").val(time_now.getTimezoneOffset())
    });
  '),
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    shinyjs::useShinyjs(),
    # fresh::use_theme(bs4DashTheme),
    # tabItems(
    # tabItem(
    # tabName = "item1",
    fluidRow(
      column(
        6,
        bs4Dash::box(
          title = h4("Course Information", style = "font-weight:bold; color:white;", align = "center"),
          closable = F,
          width = 12,
          solidHeader = TRUE,
          status = "lightblue",
          collapsible = F,
          # Make selection in line
          tags$head(
            tags$style(type = "text/css", ".inline label{ display: table-cell; text-align: left; vertical-align: middle; padding-right: 10px;}
                .inline .form-group { display: table-row;}")
          ),
          # Make selection drop down height, width, font size
          tags$head(tags$style(HTML(".selectize-input {height: 40px; width: 500px; font-size: 17px; margin-top: 20px;}"))),
          tags$div(
            class = "inline",
            shiny::selectInput("pm",
              label = h5(labelMandatory("PM"), style = "font-weight:bold;font-size: 20px;"),
              choices = purrr::prepend(PMs_Emails$PMs, "") %>% sort(),
              selected = "",
            ),
            shiny::selectInput("curriculum",
              label = h5(labelMandatory("Curriculum"), style = "font-weight:bold;font-size: 20px;"),
              choices = c("", "Adaptive", "EL", "Guidebooks", "Engage", "K-2", "CKLA", "IM", "Zearn") %>% sort(),
              selected = "",
            ),
            shiny::selectInput("site",
              label = h5(labelMandatory("Site "), style = "font-weight:bold;font-size: 20px;"),
              choices = purrr::prepend(Sites$Site, "") %>% sort(),
              selected = "",
            ),
            tags$head(tags$style(HTML("#content+ div>.selectize-input {height: 130px;}"))),
            shiny::selectizeInput("content",
              label = h5(labelMandatory("Content "), style = "font-weight:bold;font-size: 20px;"),
              choices = purrr::prepend(sort(Courses$Courses), ""),
              multiple = T, options = list(plugins= list('remove_button'))
            ),
            shiny::selectizeInput("calls_count",
              label = h5(labelMandatory("# of Calls "), style = "font-weight:bold;font-size: 20px;"),
              choices = c(0:10), selected = 0
            ),
            tags$head(tags$style(HTML(".sw-air-picker { max-width:300px;}"))),
            uiOutput("specific_yes")
          )
        )
      ),
      column(
        6,
        bs4Dash::box(
          title = h4("Call Times (All times are EST)", style = "font-weight:bold; color:white;", align = "center"),
          closable = F,
          width = 12,
          # height = 414,
          solidHeader = TRUE,
          status = "lightblue",
          collapsible = F,
          # Adjust height, width and font size of input text
          tags$head(tags$style(HTML(".input-group {height: 35px; width: 700px; font-size: 15px;}"))),
          # Adjust input of
          tags$head(tags$style(HTML(".airdatepicker--day-name {color: #04ABEB;}"))),
          uiOutput("call_times_gen")
        )
      ),
      column(
        12,
        bs4Dash::box(
          title = h4("Facilitator Information",
            style = "font-weight:bold; color:white;",
            align = "center"
          ),
          closable = F,
          width = 12,
          solidHeader = TRUE,
          status = "lightblue",
          collapsible = F,
          tags$div(
            class = "inline",
            shinyWidgets::numericInputIcon("lead_facilitators_needed", label = "# of Lead Facilitators Needed", value = 1, step = 1, min = 0), # Weirdly when this is run locally it steps by 2
            shinyWidgets::numericInputIcon("tech_facilitators_needed", label = "# of Tech/Support Facilitators Needed", value = 1, step = 1, min = 0),
            airDatepickerInput("response_needed",
              "What date do you need responses sent by?",
              dateFormat = "mm-dd-yyyy",
              value = Sys.Date() + 1,
              minDate = Sys.Date() + 1,
              multiple = F
            )
          ),
          br(),
          shiny::textAreaInput("additional_info",
            label = "What additional information would you like to provide?",
            width = "80%", height = "400px"
          ),
          shinyjs::useShinyjs(),
          tags$head(tags$script(src = "message-handler.js")),
          # Create submit effect
          tags$head(tags$style(HTML(".btn:focus {color: white !important;}"))),
          use_hover(),
          hover_action_button("submit", "Submit", shiny::icon("paper-plane"),
            style = "color: #fff; background-color: #04ABEB; border-color: #2e6da4",
            button_animation = "rotate",
            icon_animation = "spin"
          )
        )
      )
    )
  )
)
server <- function(input, output, session) {

  # Enable the Submit button when all mandatory fields are filled out
  observe({
    mandatoryFilled <-
      vapply(
        fieldsMandatory,
        function(x) {
          !is.null(input[[x]]) && input[[x]] != "" && input[[x]] != 0
          length(input[["specific_facilitator"]]) > 0
        },
        logical(1)
      )
    mandatoryFilled <- all(mandatoryFilled)

    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })

  # Create dropdown with facilitators who should be emailed
  output$specific_yes <- renderUI({
    req(input$curriculum)
    shinyWidgets::pickerInput("specific_facilitator",
      label = labelMandatory("Select the facilitators you would like to email:"),
      choices = Facilitators_Emails %>%
        pivot_longer(!c(1, 2), names_to = "Curriculum") %>%
        arrange(Facilitators) %>%
        filter(Curriculum == input$curriculum & value == 1) %>%
        select(Facilitators) %>%
        distinct() %>% 
        print(),
      multiple = T,
      width = "400px",
      options = list(
        `actions-box` = TRUE
      )
    )
  })

  # Generate call times selection
  output$call_times_gen <- renderUI({

    # Require all times
    req(input$pm)
    req(input$site)
    req(input$curriculum)
    req(input$pm)

    # local_zone <- Sites %>%
    #   filter(Site == input$site) %>%
    #   mutate(`Time Zone` = str_replace_all(`Time Zone`, c("CST" = "America/Chicago", "EST" = "America/New_York", "PST" = "America/Los_Angeles"))) %>%
    #   mutate(`Time Zone` = replace_na(`Time Zone`, "America/New_York")) %>%
    #   select(`Time Zone`) %>%
    #   as_vector()

    # Create a variable length input
    if (input$calls_count > 0) {
      map(1:input$calls_count, ~ list(
        div(style = "display:inline-block;width:300px;", airDatepickerInput(
          inputId = paste0("time1_", .x),
          label = h5(paste0("Select Date ", .x, " and Time ", .x, " (EST)"), style = "width: 400px;font-weight:bold;"),
          multiple = F,
          width = "300px",
          value = as_datetime(paste0(Sys.Date(), " 17:00:00"), tz = "UTC"),
          timepicker = T,
          separator = " to ",
          placeholder = "Pick to select date and time range",
          # minDate = Sys.Date() + 1,
          dateFormat = "mm-dd-yyyy",
          update_on = "change",
          addon = "left",
          # range = T,
          toggleSelected = F,
          timepickerOpts = timepickerOptions(
            dateTimeSeparator = " at ",
            # timeFormat = "hh:ii a",
            minutesStep = 5 # ,
            # hoursStep = 1
          )
        )),
        div(style = "display:inline-block;width:300px", airDatepickerInput(
          inputId = paste0("time2_", .x),
          label = h5(paste0("to ", stringi::stri_dup(intToUtf8(160), 6), "Date ", .x, " and Time ", .x, " (EST)"), style = "width: 400px;font-weight:bold;"),
          multiple = F,
          width = "300px",
          value = as_datetime(paste0(Sys.Date(), " 17:00:00"), tz = "UTC"),
          timepicker = T,
          separator = " to ",
          placeholder = "Pick to select date and time range",
          # minDate = Sys.Date() + 1,
          dateFormat = "mm-dd-yyyy",
          update_on = "change",
          addon = "left",
          # range = T,
          toggleSelected = F,
          timepickerOpts = timepickerOptions(
            dateTimeSeparator = " at ",
            # timeFormat = "hh:ii a",
            minutesStep = 5 # ,
            # hoursStep = 1
          )
        ))
      ))
    } else {
      print(HTML("Please Enter the # of Calls Desired"))
    }
  })

  # Restrict submit until there is at least one time entered
  # observeEvent(input$calls_count, {
  #   if (input$calls_count < 1) {
  #     shinyjs::disable("submit")
  #   } else {
  #     shinyjs::enable("submit")
  #   }
  # })

  client_time <- reactive(as.numeric(input$client_time) / 1000) # in s
  time_zone_offset <- reactive(as.numeric(input$client_time_zone_offset) / 60) # in h
  
  observe({print(time_zone_offset())})

  # Track new line of submitted data
  new_data <- reactive({
    # Create new data
    if (input$calls_count > 0) {
      new_data <- tibble(
        PMs = input$pm,
        Curriculum = input$curriculum,
        Site = input$site,
        Content = paste0(input$content, collapse = ", "),
        `Call Times` = paste(map(1:input$calls_count, ~ as.character(eval(parse(text = (paste0("input$time1_", .x)))) - hours(time_zone_offset()))) %>%
          map(., ~ as_datetime(.x, tz = "UTC")) %>%
          map(., ~ format(.x, "%m-%d-%Y %I:%M %P")), "to",
        map(1:input$calls_count, ~ as.character(eval(parse(text = (paste0("input$time2_", .x)))) - hours(time_zone_offset()))) %>%
          map(., ~ as_datetime(.x, tz = "UTC")) %>%
          map(., ~ format(.x, "%m-%d-%Y %I:%M %P")),
        collapse = ", "
        ),
        `Response Time` = as.character(input$response_needed),
        `Lead Facilitators` = as.character(input$lead_facilitators_needed),
        `Tech Facilitators` = as.character(input$tech_facilitators_needed),
        `Additional Comments` = input$additional_info,
        ID = ids::random_id(n = 1),
        Emails = Facilitators_Emails %>%
          filter(Facilitators %in% input$specific_facilitator) %>%
          pull(Emails) %>%
          paste(collapse = ", "),
        Names = Facilitators_Emails %>%
          filter(Facilitators %in% input$specific_facilitator) %>%
          pull(Facilitators) %>%
          paste(collapse = ", "),
        PM_emails = PMs_Emails %>%
          dplyr::filter(PMs == input$pm) %>%
          dplyr::pull(Email)
      ) # %>%
      # print(new_data)
      # print(input$calls_count)
      # print(new_data$`Call Times`)
    }
  })

  # Create action for submit button
  observeEvent(input$submit, {

    # Ensure no duplicate date/times based on length of selected number of calls and unique time/date sets
    times <- paste(map(1:input$calls_count, ~ as.character(eval(parse(text = (paste0("input$time1_", .x)))))) %>%
      map(., ~ as_datetime(.x, tz = "UTC")) %>%
      map(., ~ format(.x, "%m-%d-%Y %I:%M %P")), "to",
    map(1:input$calls_count, ~ as.character(eval(parse(text = (paste0("input$time2_", .x)))))) %>%
      map(., ~ as_datetime(.x, tz = "UTC")) %>%
      map(., ~ format(.x, "%m-%d-%Y %I:%M %P")),
    collapse = ", "
    ) %>%
      str_split(", ") %>%
      as_tibble(.name_repair = "unique") %>%
      distinct() %>%
      nrow()

    cat("Number of unique date/time sets: ", times)
    #
    # validate(
    #   need(crossing_number == input$calls_count, session$sendCustomMessage(type = 'testmessage',
    #                                                                message = 'Please enter non-duplicate times'))
    # )

    if (times != input$calls_count) {
      session$sendCustomMessage(
        type = "testmessage",
        message = "Invalid input: matching dates and times"
      )
    } # else if () { # AT SOME POINT TRY TO MAKE IT SO TIMES AND DATES HAVE TO BE VALID
    #   type = "testmessage",
    #   message = "Invalid input: check that dates are after the current date"
    # }

    # Require same number of requested calls as unique times & dates
    req(times == input$calls_count)

    # Write New RDS first
    write_rds(new_data(), "data/new_data.rds")

    # Write to sheet
    source("sheet_write.R")


    # Send pop-up message on success
    session$sendCustomMessage(
      type = "testmessage",
      message = "Thank you for your submission!"
    )
  })

  observe({
    # Validate by checking if data is a dataframe and then print the times
    req(is.data.frame(data))
    cat("The input times are ", data$`Call Times`[1], "\n")
    print("The input times are ", data$`Call Times`[1])
  })
}

shinyApp(ui = ui, server = server)
