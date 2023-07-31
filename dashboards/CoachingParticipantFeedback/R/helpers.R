#### Coaching Participant Feedback Dashboard ####

### Fonts add --------------------------------------------------------------------- ###
font_add(family = "Calibri", regular = "www/Calibri.ttf")
font_add(family = "Calibri Bold", regular = "www/Calibri Bold.ttf")
font_add(family = "Roboto", regular = "www/Roboto-Black.ttf")

### css --------------------------------------------------------------------- ###


sidebar_style <- "overflow-x:auto;width:inherit;max-width:400px;"

### options for app --------------------------------------------------- ###


options(googleAuthR.webapp.client_id = "817160394165-i95di0hbu28taddurglfiuilji202jgo.apps.googleusercontent.com")
options("googleAuthR.redirect" = "https://teachinglabhq.shinyapps.io/CoachingParticipantFeedback/")
options(shiny.port = 7325)
options(spinner.color = "#04ABEB")

### info page --------------------------------------------------- ###

info_page <- div(
  id = "check",
  fluidRow(
    column(12,
           align = "center",
           offset = 2,
           p("test")
           # conditionalPanel(
           #   condition = "input.g.id === null",
           #   
           # ),
           # conditionalPanel(
           #   condition = "input.g.id !== null",
           #   div(id = "main-ui")
           # )
    )
  )
)
