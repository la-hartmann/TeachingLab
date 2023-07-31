#### IPG Dashboard ####

# Making the initial routes to follow
router <- shiny.router::make_router(
  shiny.router::route("info", info_page),
  shiny.router::route("agree", uiAgree("p1")),
  shiny.router::route("text", uiText("p2"))
)

ui <- semanticPage(
  
  shinyjs::useShinyjs(),

    title = "IPG Responses",
    theme = "spacelab",
    shiny.semantic::horizontal_menu(
      list(
        list(name = "About the IPG Dashboard", link = route_link("info"), icon = "copy outline"),
        list(name = "Quantitative Responses", link = route_link("agree"), icon = "list alternate outline"),
        list(name = "Qualitative Answers", link = route_link("text"), icon = "envelope open outline")
      ),
      logo = "imgs/teachinglab_logo.png"
    ),
    router$ui
)

server <- function(input, output, session) {

  router$server(input, output, session)
  agreeServer("p1")
  textServer("p2")
  
}

shinyApp(ui, server)
