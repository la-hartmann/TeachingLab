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
        conditionalPanel(
          condition = "input.inTabset != 'Section 3: Participant Knowledge/Self-Reported Practices Assessments' && input.inTabset != 'Section 7: Student Outcomes' && input.inTabset != 'Section 8: Partnership Opportunities Moving Forward' && input.inTabset != 'Section 6: Teachers’ Instructional Practices'",
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
          )
        ),
        conditionalPanel(
          condition = "input.inTabset != 'Section 3: Participant Knowledge/Self-Reported Practices Assessments' && input.inTabset != 'Section 8: Partnership Opportunities Moving Forward'",
          selectInput(
            inputId = "contentAreaSelect",
            label = "Please select a content area",
            selected = "All Content Areas",
            choices = c(
              "All Content Areas",
              "Math",
              "ELA",
              "Other"
            )
          )
        ),
        conditionalPanel(
          condition = "input.inTabset == 'Section 6: Teachers’ Instructional Practices'",
          selectInput(
            inputId = "ipgFormsSelect",
            label = "Please select a rubric",
            selected = "All Rubrics",
            choices = c(
              "All Rubrics",
              "K-12: ELA/Literacy IPG" = "K-12: ELA/Literacy IPG (please use this tool for K-2 observations that are not focused on foundational skills)",
              "K-12: Mathematics IPG",
              "Foundational Skills Observational Tool - FSOT"
            )
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
          tags$h3("Section 2b: End of Course Summary", style = "color:#FF7B43;font-weight:bold;"),
          tags$br(),
          plotOutput("course_plot_1", height = "700px"),
          htmlOutput("course_summary_sentences"),
          tags$h3("Section 2c: Ongoing Coaching Feedback", style = "color:#FF7B43;font-weight:bold;"),
          tags$br(),
          plotOutput("ongoing_coach_plot_1", height = "500px"),
          htmlOutput("ongoing_coach_summary_sentences"),
          tags$h3("Section 2d: End of Coaching Feedback", style = "color:#FF7B43;font-weight:bold;"),
          tags$br(),
          plotOutput("end_coach_plot_1", height = "700px"),
          htmlOutput("end_coach_summary_sentences"),
          tags$h3("Section 2e: Contact Lead Feedback", style = "color:#FF7B43;font-weight:bold;"),
          tags$br(),
          plotOutput("contact_lead_plot_1", height = "700px"),
        ),
        tabPanel(
          "Section 3: Participant Knowledge/Self-Reported Practices Assessments",
          tags$h3("Results", style = "color:#FF7B43;font-weight:bold;"),
          tags$br(),
          plotOutput("knowledge_assess_summary", height = "1000px")
        ),
        tabPanel(
          "Section 4: Teachers’ Mindsets & School Environment",
          tags$h3("Section 4a: Teacher - Mindsets", style = "color:#FF7B43;font-weight:bold;"),
          tags$h4("Overall", style = "color:#D17DF7;font-weight:bold;"),
          gt::gt_output("mindsets_table_1"),
          tags$h4("Recognition of Race & Culture", style = "color:#D17DF7;font-weight:bold;"),
          gt::gt_output("mindsets_table_2"),
          tags$h4("High Expectations", style = "color:#D17DF7;font-weight:bold;"),
          gt::gt_output("mindsets_table_3"),
          tags$h4("Growth Mindsets", style = "color:#D17DF7;font-weight:bold;"),
          gt::gt_output("mindsets_table_4"),
          tags$h3("Section 4b: Teachers - Perceptions and Use of Curricula", style = "color:#FF7B43;font-weight:bold;"),
          plotOutput("mindsets_plot_1"),
          plotOutput("mindsets_plot_2"),
          plotOutput("mindsets_plot_3", height = "600px"),
          tags$h3("Section 4c: Educators’ Perceptions of School Culture and Climate", style = "color:#FF7B43;font-weight:bold;"),
          tags$h4("Teachers’ Perceptions of School Leaders and CBPL", style = "color:#D17DF7;font-weight:bold;"),
          plotOutput("mindsets_plot_4", height = "700px"),
          tags$h4("Teachers’ Perceptions of School Environment", style = "color:#D17DF7;font-weight:bold;"),
          plotOutput("mindsets_plot_5")
        ),
        tabPanel(
          "Section 5: School Leaders’ Mindsets & Observational Practices Participants",
          tags$h3("Section 5a: School Leaders - Mindsets", style = "color:#FF7B43;font-weight:bold;"),
          tags$h4("Overall", style = "color:#D17DF7;font-weight:bold;"),
          gt::gt_output("leaders_mindsets_table_1"),
          tags$h4("Recognition of Race & Culture", style = "color:#D17DF7;font-weight:bold;"),
          gt::gt_output("leaders_mindsets_table_2"),
          tags$h4("High Expectations", style = "color:#D17DF7;font-weight:bold;"),
          gt::gt_output("leaders_mindsets_table_3"),
          tags$h4("Growth Mindsets", style = "color:#D17DF7;font-weight:bold;"),
          gt::gt_output("leaders_mindsets_table_4"),
          tags$h3("Section 5b: School Leaders - Observational Practices", style = "color:#FF7B43;font-weight:bold;"),
          plotOutput("leaders_mindsets_plot_1", height = "500px")
        ),
        tabPanel(
          "Section 6: Teachers’ Instructional Practices",
          tags$h3("Section 6a: Teachers’ CRSE Self-Reported Practices Participants", style = "color:#FF7B43;font-weight:bold;"),
          plotOutput("crse_plot_1", height = "800px"),
          tags$h3("Section 6b: Direct-to-Teacher Coaching Observations", style = "color:#FF7B43;font-weight:bold;"),
          plotOutput("direct_to_ts_pos_plot", height = "800px"),
          tags$h3("Section 6c: Instructional Walkthroughs/ Observations and Debriefs", style = "color:#FF7B43;font-weight:bold;"),
          plotOutput("instructional_walkthroughs_plot_1")
        ),
        tabPanel(
          "Section 7: Student Outcomes",
          tags$h3("Section 7a: Student Learning Experiences - Teaching Lab Survey", style = "color:#FF7B43;font-weight:bold;"),
          tags$h4("CRSE", style = "color:#D17DF7;font-weight:bold;"),
          plotOutput("student_crse_1", height = "850px"),
          tags$h4("Teacher-Student Relationships", style = "color:#D17DF7;font-weight:bold;"),
          plotOutput("student_crse_2", height = "450px"),
          tags$h4("Self Efficacy", style = "color:#D17DF7;font-weight:bold;"),
          plotOutput("student_crse_3", height = "1000px"),
          tags$h4("Happiness and Belonging", style = "color:#D17DF7;font-weight:bold;"),
          plotOutput("student_crse_4", height = "800px"),
          tags$h4("Being Challenged", style = "color:#D17DF7;font-weight:bold;"),
          plotOutput("student_crse_5", height = "675px"),
          tags$h3("Section 7b: % On Grade Level", style = "color:#FF7B43;font-weight:bold;"),
          plotOutput("student_work_plot_1", height = "500px"),
          tags$h3("Section 7c: Student Performance on Grade-Level Tasks", style = "color:#FF7B43;font-weight:bold;"),
          plotOutput("student_work_plot_2", height = "550px"),
          plotOutput("student_work_plot_3", height = "500px")
        ),
        tabPanel(
          "Section 8: Partnership Opportunities Moving Forward",
          tags$h4("Summer PL", style = "color:#D17DF7;font-weight:bold;"),
          plotOutput("summer_pl_plot"),
          tags$h4("What activities would you want your teachers and educators to invest their time and effort in future professional learning experiences?", style = "color:#D17DF7;font-weight:bold;"),
          plotOutput("future_prof_learning_plot"),
          tags$h4("Would you prefer for future professional learning support to be virtual, in-person, or hybrid?", style = "color:#D17DF7;font-weight:bold;"),
          plotOutput("future_prof_learn_loc_plot")
        )
      )
    )
  )
)
