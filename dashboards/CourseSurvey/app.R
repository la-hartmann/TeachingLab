#### End of Course Survey Dashboard ####

# Making the initial routes to follow
router <- shiny.router::make_router(
  shiny.router::route("index", info_page),
  shiny.router::route("agree", uiAgree("p1")),
  shiny.router::route("nps", uiNPS("p2")),
  shiny.router::route("text", uiText("p3")),
  shiny.router::route("report", uiReport("p4"))
)

ui <- semanticPage(

  tags$head(
    includeCSS("www/styles.css")
  ),

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
        h2("Please authenticate using your @teachinglab.org gmail account."),
        googleSignInUI("loginButton")
      )
    )
  ),
  conditionalPanel(
    condition = "output.loginButton == 'YES'",
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
  
  ## Get googleSignIn module from googleAuthR ##
  sign_ins <- callModule(googleSignIn, "loginButton")

  ## Only display content to verified domain users, ##
  ## or special approvals added manually in R/data_load.R ##
  output$loginButton <- renderText({
    if (!is.null(sign_ins())) {
      if (tlShiny::check_email_domain(sign_ins()$email, "teachinglab.org") |
          tlShiny::check_email_approved(sign_ins()$email, special_approvals)) {
        return("YES")
      } else {
        print("unknown")
        return("UNKNOWN")
      }
    }
    print("no")
    "NO"
  })

  ## need this so it works when conditionalPanel hides content ##
  outputOptions(output, "loginButton", suspendWhenHidden = F)

  ## Server pages ##
  router$server(input, output, session)
  agreeServer("p1")
  npsServer("p2")
  textServer("p3")
  reportServer("p4")
}

shinyApp(ui = ui, server = server)

