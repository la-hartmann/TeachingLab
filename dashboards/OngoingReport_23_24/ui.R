ui <- tagList(
  ### Import Google Sign In JavaScript Requirements ###
  ### Enable to add authentication ###
  tags$head(
    tags$head(
      tags$link(rel = "shortcut icon", href = "favicon.ico"),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ) # ,
    #   HTML('<script src ="https://accounts.google.com/gsi/client" async defer></script>'),
    #   tags$div(
    #     id = "g_id_onload",
    #     "data-client_id" = "714092058521-462cbo3ss655fuelgt11sjep083neemh.apps.googleusercontent.com",
    #     "data-callback" = "handleCredentialResponse"
    #   ),
    #   includeScript("signin.js"),
    #   useShinyjs()
  ),
  grid_page(
    ### Theme, layout ###
    theme = bs_theme(
      bg = "#FFFFFF", fg = "#46494f",
      primary = "#04abeb",
      secondary = "#FF374B",
      version = 5,
      row_sizes = 5,
      base_font = c("Calibri"),
      heading_font = c("Calibri")
    ),
    layout = c(
      "header header",
      "sidebar mainPage"
    ),
    ### Row and column sizing ###
    row_sizes = c(
      "70px",
      "1fr"
    ),
    col_sizes = c(
      "250px",
      "1fr"
    ),
    gap_size = "1rem",
    ### App Title ###
    grid_card_text(
      area = "header",
      content = "Teaching Lab Ongoing Report",
      alignment = "center",
      is_title = TRUE,
      icon = "just_logo.png"
    ),
    ### Sidebar Inputs ###
    grid_card(
      area = "sidebar",
      fill = TRUE,
      item_alignment = "top",
      style = "display:block; overflow:scroll;",
      ### Sign in div, customizable ###
      # tags$div(
      #   id = "signin",
      #   class = "g_id_signin",
      #   "data-type" = "standard",
      #   "data-shape" = "pill",
      #   "data-theme" = "filled_black",
      #   "data-width" = "225",
      #   "data-text" = "signin_with",
      #   "data-size" = "large",
      #   "data-logo_alignment" = "left"
      # ),
      ### At the moment there is no sign out button ###
      # shinyjs::hidden(actionButton("signout", "Sign Out", onclick = "signOut();", class = "btn-danger")),
      card_body(
        min_height = "1000px",
        selectInput(
          inputId = "siteSelect",
          label = "Please select a site",
          selected = "All Sites",
          choices = sites_list,
          width = "100%"
        ),
        selectInput(
          inputId = "raceSelect",
          label = "Please select a race",
          selected = "All Races",
          choices = c(
            "All Races",
            "Asian",
            "Black or African American",
            "I prefer to self describe",
            "Native American or Indian",
            "Prefer not to say",
            "White"
          )
        ),
        tags$div(
          id = "feedbackDiv", class = "extras",
          tags$h5(
            "Have feedback? Please take",
            tags$a("the linked survey!", href = "https://teachinglab.iad1.qualtrics.com/jfe/form/SV_0Bqu3SUziXrmvlA")
          ),
          tags$a(
            tags$img(src = "https://cdn.phenompeople.com/CareerConnectResources/QUALUS/social/qualtrics-share-image-1577795333059.jpg", style = "width:217px"),
            href = "https://teachinglab.iad1.qualtrics.com/jfe/form/SV_0Bqu3SUziXrmvlA"
          )
        ),
        tags$br(),
        tags$div(
          id = "tutorialDiv", class = "extras",
          tags$h5(
            "Have questions about dashboard use?",
            tags$a("Check out the video tutorial here!", href = "https://www.youtube.com/")
          ),
          tags$a(
            tags$img(src = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAJYAAACWCAMAAAAL34HQAAAAbFBMVEX/////AAD/VVX/8fH/y8v/w8P/5OT/3d3/vLz/r6//4OD/pqb/YmL/09P/xsb/wMD/m5v/Wlr/Jyf/a2v/e3v/lJT/6en/q6v/c3P/f3//kJD/9/f/i4v/hIT/SEj/EBD/QkL/Hx//Ozv/MDAywzyoAAACo0lEQVR4nO2a7W6zMAxGCYWEUgL0i35AaSn3f48vrfpuU7VpscERmp7zH3RaHDtOHAQAAAAAAAAAAAAAAAAAAPhKvah0aqyN8jyOi6JIkqQo4jiPImtMqquFvMKiMjaKD+XluG43V0Vic19np/MqGXT1lKrRIHPraC7fcu3D7WlVTeGUnifweWO8WbKZ3kqpPh5ntZOQenAaY1VKWSlVztJqCDCu1ULSSqmaqbWV1TryrFKRRfjJXbO0lrJWSiUsLeFvyE0S0lZK/SEtK69lGFriEc+L+UZe68zQEqvSn+wYWuL5Qakt3arO5LUyellctI6v1nz/lr5L1Xe3V4d1EHG1GFXROBbq8NHMrHhVvaMnLuvYdj21gvTE8rJ0Lcc3h6/Wz3BWbkTWcg2Y8KMjXRLbWk9aQXCmhhhdK2doBdRkkZO1Eo7WEGIhRYteq103EG9aQe36ex4svWkNlM6xfyBrrfhagXUNMXoL69pRf6c1LJi108P0jn8/Siuonf5t71oDp+7Xh+nb0/Faze9ae+9aTqWIrjUu5B3XIj22xiQIc3R8mJ4gxqTTzvFZj1m+jl2dWFrMUq3d8ugLeqnmbWyIW1T6xoazDSx7mpWX3WnSEaV8aFG/3xNG50NqyPSFIaWudC1S+3rgta8bevvqo9m/0Zv9ysfRCP3Wc6YHST6O3TjXKzM9pPRwpEvfBc72AHym1wUzvVyZq5Z4huAsxNlec0pfCvcpS0v6KzKuVp4IDxywp5NExzM4KV7eq+FbCdZrbmC9KKg9lhMdvZ1+Qwt8yIZVDN8x+13WTpLEutv6eKE3hz9SP0YW8+Wq2WVh39Fcrpt2kCkPcWSNh/HKjwHP6Dngmfwf8Mwja02qdcWdhAIAAAAAAAAAAAAAAAAA/ir/AMqULqUdxVe+AAAAAElFTkSuQmCC"),
            href = "https://www.youtube.com/",
            style = "width:217px"
          )
        ),
        tags$br(),
        tags$div(
          id = "dataHubDiv", class = "extras",
          tags$h5(
            "Other data needs?",
            tags$a("Head back to the data hub!", href = "https://teachinglab.github.io/")
          ),
          tags$a(
            tags$img(src = "data_hub.png", style = "width:217px"),
            href = "https://teachinglab.github.io/"
          )
        )
      )
    ),
    ### Main Page ###
    grid_card(
      area = "mainPage",
      tabsetPanel(
        id = "inTabset",
        type = "tabs",
        tabPanel(
          "Context",
          tags$br(),
          htmlOutput("counts"),
          tags$br(),
          htmlOutput("final_sec_intro")
        ),
        tabPanel(
          "Section 1: Participant Background and Demographics Summary",
          tags$h3("Gender and Racial Identity", style = "color:#FF7B43;font-weight:bold;"),
          tags$br(),
          splitLayout(plotOutput("gender_plot", width = "95%"),
            plotOutput("race_plot", width = "95%"),
            cellWidths = c("50%", "50%")
          ),
          tags$br(),
          plotOutput("ethnicity_plot", width = "95%"),
          tags$br(),
          tags$h3("Years of Teaching Experience", style = "color:#FF7B43;font-weight:bold;"),
          tags$br(),
          plotOutput("teacher_experience_plot"),
          tags$h3("Content Area", style = "color:#FF7B43;font-weight:bold;"),
          tags$br(),
          plotOutput("content_area_plot"),
          tags$h3("Grades", style = "color:#FF7B43;font-weight:bold;"),
          tags$br(),
          plotOutput("grades_taught_plot"),
          tags$h3("Prior Experience with Teaching Lab Professional Learning", style = "color:#FF7B43;font-weight:bold;"),
          tags$br(),
          plotOutput("tl_pl_plot"),
          tags$h3("Roles", style = "color:#FF7B43;font-weight:bold;"),
          tags$br(),
          splitLayout(plotOutput("roles_plot_1", width = "95%"), 
                      plotOutput("roles_plot_2", width = "95%"),
            cellWidths = c("50%", "50%")
          )
        ),
        tabPanel(
          "Section 2: Participant Perceptions",
          tags$h3("Section 2a: End of Session Summary", style = "color:#FF7B43;font-weight:bold;"),
          plotOutput("session_plot_1", height = "500px"),
          htmlOutput("session_summary_sentences"),
          tags$br(),
          plotOutput("course_plot_1", height = "700px"),
          htmlOutput("course_summary_sentences"),
          tags$br(),
          plotOutput("ongoing_coach_plot_1", height = "500px"),
          htmlOutput("ongoing_coach_summary_sentences"),
          tags$br(),
          plotOutput("end_coach_plot_1", height = "700px"),
          htmlOutput("end_coach_summary_sentences")
        ),
        tabPanel("Section 3: Participant Knowledge/Self-Reported Practices Assessments"),
        tabPanel("Section 4: Teachers’ Mindsets & School Environment"),
        tabPanel("Section 5: School Leaders’ Mindsets & Observational Practices Participants"),
        tabPanel("Section 6: Teachers’ Instructional Practices"),
        tabPanel("Section 7: Student Outcomes"),
        tabPanel("Section 8: Partnership Opportunities Moving Forward")
      )
    )
  )
)
