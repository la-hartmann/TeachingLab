#### Mathematica Course Survey Dashboard ####

router <- shiny.router::make_router(
  shiny.router::route("index", info_page),
  shiny.router::route("agree", uiAgree("p1")),
  shiny.router::route("nps", uiNPS("p2")),
  shiny.router::route("text", uiText("p3")),
  shiny.router::route("report", uiReport("p4"))
)

ui <- secure_app(semanticPage(
  
  # tags$head(
  #   tags$link(rel="stylesheet", href="style.css", type="text/css")
  # ),
  
  shinyjs::useShinyjs(),
  
  # conditionalPanel(
  #   condition = "output.loginButton != 'YES'",
  #   fluidRow(
  #     column(12,
  #            align = "center", offset = 2,
  #            h2("Please authenticate using your teachinglab.org or mathematica-mpr.com account."),
  #            googleSignInUI("loginButton")
  #     )
  #   )
  # ),
  
  # conditionalPanel(
    # condition = "output.loginButton == 'YES'",
      title = "End of Course Dashboard",
      theme = "cosmo",
      # Define tabs
      shiny.semantic::horizontal_menu(
        list(
          list(name = "Info", link = route_link("index"), icon = "info"),
          list(name = "Quantitative", link = route_link("agree"), icon = "chart area"),
          list(name = "NPS", link = route_link("nps"), icon = "star"),
          list(name = "Qualitative", link = route_link("text"), icon = "align justify"),
          list(name = "Report/Download", link = route_link("report"), icon = "file")
        ), logo = "imgs/teachinglab_logo.png"
      ),
  
      router$ui
  # ),
  
  # conditionalPanel(
  #   condition = "output.loginButton == 'NO'",
  #   div(style = "display:inline-block; left:39%; position:fixed;", helpText("Need to login with a valid email domain to see content"))
  # ),
  # conditionalPanel(
  #   condition = "output.loginButton == 'UNKNOWN'",
  #   helpText("Logged in successfully, but not with an authorised email. Please contact duncan.gates@teachinglab.org if you think you should have access to this content.")
  # )
)
)


server <- function(input, output, session) {
  
  # sign_ins <- callModule(googleSignIn, "loginButton")
  # 
  # # only display content to verified domain users
  # output$loginButton <- renderText({
  #   if(!is.null(sign_ins())){
  #     if(check_email_domain(sign_ins()$email, "teachinglab.org") | check_email_domain(sign_ins()$email, "mathematica-mpr.com") | check_email_domain(sign_ins()$email, "gmail.com")){
  #       return("YES")
  #     } else {
  #       print("unknown")
  #       return("UNKNOWN")
  #     }
  #   }
  #   print("no")
  #   "NO"
  # })
  
  # need this so it works when conditionalPanel hides content
  # outputOptions(output, "loginButton", suspendWhenHidden = F)
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  router$server(input, output, session)
  agreeServer("p1", in_site)
  npsServer("p2", in_site)
  textServer("p3", in_site)
  reportServer("p4", in_site)
  
  
}

shinyApp(ui = ui, server = server)
