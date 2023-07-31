#### Personal Facilitator Dashboard ####

router <- shiny.router::make_router(
  shiny.router::route("info", info_page),
  shiny.router::route("agree", uiAgree("p1")),
  shiny.router::route("text", uiText("p2"))
)



ui <- semanticPage(
  
  shinyjs::useShinyjs(),
  
  conditionalPanel(
    condition = "output.loginButton != 'YES'",
    fluidRow(
      column(12,
             align = "center", offset = 2,
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             h2("Please authenticate using your teachinglab.org gmail account."),
             googleSignInUI("loginButton")
      )
    )
  ),
  
  conditionalPanel(
    condition = "output.loginButton == 'YES'",
    title = "Personal Facilitator Dashboard",
    theme = "cosmo",
    # Define tabs
    shiny.semantic::horizontal_menu(
      list(
        list(name = "Info", link = route_link("index"), icon = "info"),
        list(name = "Strongly Agree/Agree", link = route_link("agree"), icon = "chart area"),
        list(name = "Survey Quotes", link = route_link("text"), icon = "align justify")
      ), logo = "imgs/teachinglab_logo.png"
    ),
    
    router$ui
  ),
  
  conditionalPanel(
    condition = "output.loginButton == 'NO'",
    div(style = "display:inline-block; left:39%; position:fixed;", helpText("Need to login with a valid email domain to see content"))
  ),
  conditionalPanel(
    condition = "output.loginButton == 'UNKNOWN'",
    helpText("Logged in successfully, but not with an authorised email. Please contact duncan.gates@teachinglab.org if you think you should have access to this content.")
  )
)

server <- function(input, output, session) {
  
  # Trick file date creation update
  onStop(function() {
    
    # File name
    p <- paste0(getwd(), "/app.R")
    
    # Update file 'date creation'
    Sys.setFileTime(p, lubridate::now())
    
  }) # onStop

  sign_ins <- shiny::callModule(googleSignIn, "loginButton")
  
  observeEvent(!is.null(sign_ins()), {
    if (is.null(sign_ins())) {
      result_auth$auth <- "Temp Facilitator"
    } else {
      result_auth$auth <- sign_ins()$name
    }
  })
  
  # observe(print(result_auth$auth))
  # observe(print(sign_ins()$name))
  
  # only display content to verified domain users
  output$loginButton <- renderText({
    if(!is.null(sign_ins())){
      if(check_email_domain(sign_ins()$email, "teachinglab.org")){
        return("YES")
      } else {
        print("unknown")
        return("UNKNOWN")
      }
    }
    print("no")
    "NO"
  })
  
  # need this so it works when conditionalPanel hides content
  outputOptions(output, "loginButton", suspendWhenHidden = F)

  router$server(input, output, session)
  agreeServer("p1", result_auth = result_auth$auth)
  textServer("p2", result_auth = result_auth$auth)
}

shinyApp(ui, server)
