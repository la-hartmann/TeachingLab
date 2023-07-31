#### Coaching Participant Feedback Dashboard ####

# Making the initial routes to follow
router <- shiny.router::make_router(
  shiny.router::route("info", info_page),
  shiny.router::route("agree", uiAgree("p1")),
  shiny.router::route("text", uiText("p2"))
)

ui <- semanticPage(
  
    tags$head(
      includeCSS("www/styles.css"),
      tags$link(rel = "shortcut icon", href = "favicon.ico")#,
      # tags$meta(name = "google-signin-scope", content = "profile email"),
      # tags$meta(name = "google-signin-client_id", content = "817160394165-i95di0hbu28taddurglfiuilji202jgo.apps.googleusercontent.com"),
      # HTML('<script src="https://apis.google.com/js/platform.js?onload=init"></script>'),
      # includeScript("signin.js"),
    ),
    
    shinyjs::useShinyjs(),
    
    conditionalPanel(
      condition = "output.loginButton != 'YES'",
      div(
        id = "main-ui",
        class = "middle-page",
        div(id = "prompt", h2("Please authenticate using your teachinglab.org gmail account.")),
        br(),
        div(
          googleSignInUI(id = "loginButton",
                       logout_name = "Log Out",
                       logout_class = "btn-info"),
          align = "center"
        )
      )
    ),
    conditionalPanel(
      condition = "output.loginButton == 'YES'",
      title = "Coaching Participant Feedback",
      theme = "united",
      shiny.semantic::horizontal_menu(
        list(
          list(name = "About the Coaching Participant Feedback Dashboard", link = route_link("info"), icon = "copy outline"),
          list(name = "Quantitative Responses", link = route_link("agree"), icon = "list alternate outline"),
          list(name = "Qualitative Answers", link = route_link("text"), icon = "envelope open outline")
        ),
        logo = "imgs/teachinglab_logo.png"
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
  # observe({
  #   print(isTruthy(input$g.name))
  # })

  # observe({
  #   if (isTruthy(input$g_id)) {
  #     hide("main-ui")
  #     show("second-ui")
  #   } else {
  #     show("main-ui")
  #     hide("second-ui")
  #   }
  # })
  
  observe({
    print(sign_ins()$name)
  })
  
  # Get googleSignIn module from googleAuthR
  sign_ins <- callModule(googleSignIn, "loginButton")
  
  # only display content to verified users in the list
  output$loginButton <- renderText({
    if(!is.null(sign_ins())){
      if(tlShiny::check_email_approved(sign_ins()$email, approved_emails_list)){
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
  agreeServer("p1")
  textServer("p2")
  
}

shinyApp(ui, server)
