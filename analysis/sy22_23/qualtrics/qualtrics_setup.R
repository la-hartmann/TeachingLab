library(qualtRics)

## Secret: xtvFHOo3CAttQE53t1Cex9htXrK1pomJlljS0D345Hwi1SJR0QEeY3ApFAVOllNO
## Client: dc1460025ed4e498e28e5bf65a52e0ac
qualtrics_api_credentials(api_key = "r1vgrzHjb3AQrBQEKgLXd8khdF5R7FFjP5lp7bzT", 
                          base_url = "teachinglab.co1.qualtrics.com",
                          install = TRUE,
                          overwrite = TRUE)

surveys <- all_surveys()

session_survey <- fetch_survey(surveyID = "SV_djt8w6zgigaNq0C", 
                         verbose = TRUE)

### First facilitator feedback
first_fac_split <- session_survey |>
  group_split(Q7) |>
  set_names(session_survey$Q7 |> unique() |> sort())

#### THIS IS TEST DATA ####
adam_qual_1 <- first_fac_split$`Adam Smith` |>
  select(Q14, Q15, Q16)

first_fac_split$`Amanda Beale` |>
  select(Q9, Q14, Q15, Q16) |>
  set_names(c("What additional feedback do you have about their facilitation skills, if any?",
              "What is one thing from today's learning that you plan to take back to your classroom?",
              "What went well in today’s session?", 
              "What could have been better about today’s session?")) |>
  gt::gt() |>
  gt::tab_header(title = "Amanda Beale as a First Facilitator Feedback") |>
  gt::sub_missing(missing_text = "") |>
  TeachingLab::gt_theme_tl() |>
  gt::gtsave(here::here("Images/Qualtrics/amanda_1_qual_feedback.png"))

first_fac_split$`Patricia Thibodeaux` |>
  select(Q9, Q14, Q15, Q16) |>
  set_names(c("What additional feedback do you have about their facilitation skills, if any?",
              "What is one thing from today's learning that you plan to take back to your classroom?",
              "What went well in today’s session?", 
              "What could have been better about today’s session?")) |>
  gt::gt() |>
  gt::tab_header(title = "Patricia Thibodeaux as a First Facilitator Feedback") |>
  gt::sub_missing(missing_text = "") |>
  TeachingLab::gt_theme_tl() |>
  gt::gtsave(here::here("Images/Qualtrics/patricia_1_qual_feedback.png"))

### Second fac feedback ###
second_fac_split <- session_survey |>
  group_split(Q11) |>
  set_names(session_survey$Q11 |> unique() |> sort())

second_fac_split$`Amanda Beale` |>
  select(Q13, Q14, Q15, Q16) |>
  set_names(c("What additional feedback do you have about their facilitation skills, if any?",
              "What is one thing from today's learning that you plan to take back to your classroom?",
              "What went well in today’s session?", 
              "What could have been better about today’s session?")) |>
  gt::gt() |>
  gt::tab_header(title = "Amanda Beale as a Second Facilitator Feedback") |>
  gt::sub_missing(missing_text = "") |>
  TeachingLab::gt_theme_tl() |>
  gt::gtsave(here::here("Images/Qualtrics/amanda_2_qual_feedback.png"))

second_fac_split$`Patricia Thibodeaux` |>
  select(Q13, Q14, Q15, Q16) |>
  set_names(c("What additional feedback do you have about their facilitation skills, if any?",
              "What is one thing from today's learning that you plan to take back to your classroom?",
              "What went well in today’s session?", 
              "What could have been better about today’s session?")) |>
  gt::gt() |>
  gt::tab_header(title = "Patricia Thibodeaux as a Second Facilitator Feedback") |>
  gt::sub_missing(missing_text = "") |>
  TeachingLab::gt_theme_tl() |>
  gt::gtsave(here::here("Images/Qualtrics/patricia_2_qual_feedback.png"))

### QUALITATIVE COLUMNS
### Q9: Additional feedback about facilitator 1
### Q14: What is one thing from today's learning that you plan to take back to your classroom?
### Q15: What went well in today’s session?
### Q16: What could have been better about today’s session?
### Q10: Did you have second facilitator?
### Q13: Additional feedback for facilitator 2
### Facilitator Selection
### Facilitator 1: Q7
### Facilitator 2: Q11
