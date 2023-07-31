library(surveymonkey)
library(TeachingLab)
library(tidyverse)

options(sm_oauth_token = Sys.getenv("session_token"))
eic_student_survey <- surveymonkey::fetch_survey_obj(312658300) |>
  surveymonkey::parse_survey()

growth_mindsets <- c("How much do you agree or disagree with the statements below? - If you want to succeed in math, hard work alone just won’t cut it; you need to have a natural gift or talent",
                     "How much do you agree or disagree with the statements below? - You have a certain amount of intelligence, and you really can’t do much to change it",
                     "How much do you agree or disagree with the statements below? - When you have to try really hard in a subject in school, it means you can’t be good at that subject",
                     "How much do you agree or disagree with the statements below? - Being a “math person” or not is something that you really can’t change. Some people are good at math and other people aren’t   ",
                     "For you personally, how true or not true is the statement below? (Select only one) - My math teacher thinks failure helps us learn and grow.",
                     "How much do you agree or disagree with the statement below? (Select one only) - My math teacher believes that everybody in my class can be very good at math.",
                     "¿En qué medida estás de acuerdo o en desacuerdo con las siguientes declaraciones? - Si tú quieres tener éxito en matemáticas, el trabajo duro por sí solo no será suficiente; tú necesitas tener un don o talento natural",
                     "¿En qué medida estás de acuerdo o en desacuerdo con las siguientes declaraciones? - Tú tienes una cierta cantidad de inteligencia y tu realmente no puedes hacer mucho para cambiarla",
                     "¿En qué medida estás de acuerdo o en desacuerdo con las siguientes declaraciones? - Cuando tienes que esforzarte mucho en una materia en la escuela, significa que no puedes ser bueno en esa materia",
                     "¿En qué medida estás de acuerdo o en desacuerdo con las siguientes declaraciones? - Ser un \"persona matemática” o no es algo que realmente no puedes cambiar. Algunas personas son buenas en matemáticas y otras no",
                     "Para ti personalmente, ¿qué tan verdadera o no verdadera es la siguiente declaración? (Seleccione solo uno) - Mi maestro/a de matemáticas cree que el fracaso nos ayuda a aprender y a crecer.",
                     "¿En qué medida está de acuerdo o en desacuerdo con la siguiente declaración? (Seleccione solo uno) - Mi maestro/a de matemáticas cree que todos en mi clase pueden ser muy buenos en matemáticas.")

achievement_identity <- c("How much do you agree or disagree with the statements below? - I usually do well in math.",
                          "How much do you agree or disagree with the statements below? - I am good at working out difficult math problems.",
                          "¿En qué medida está de acuerdo o en desacuerdo con las siguientes declaraciones? - Generalmente me va bien en matemáticas",
                          "¿En qué medida está de acuerdo o en desacuerdo con las siguientes declaraciones? - Soy bueno/a resolviendo problemas difíciles de matemáticas")

self_efficacy <- c("How much do you agree or disagree with the statements below? - I believe that I can be successful in my math class.",
                   "How much do you agree or disagree with the statements below? - I am confident that I can understand the material in my math class.",
                   "How much do you agree or disagree with the statements below? - I know I can learn the materials in my math class.",
                   "¿En qué medida está de acuerdo o en desacuerdo con las siguientes declaraciones? - Creo que puedo tener éxito en mi clase de matemáticas.",
                   "¿En qué medida está de acuerdo o en desacuerdo con las siguientes declaraciones? - Estoy seguro/a de que puedo entender el material en la clase de matemáticas.",
                   "¿En qué medida está de acuerdo o en desacuerdo con las siguientes declaraciones? - Yo sé que puedo aprender los materiales en mi clase de matemáticas.")

math_enjoyment <- c("How much do you agree or disagree with the statements below? - I enjoy learning math.",
                    "How much do you agree or disagree with the statements below? - I learn many interesting things in math.",
                    "How much do you agree or disagree with the statements below? - I like to solve math problems.",
                    "How much do you agree or disagree with the statements below? - I like math.",
                    "¿En qué medida está de acuerdo o en desacuerdo con las siguientes declaraciones? - Yo disfruto aprendiendo matemáticas",
                    "¿En qué medida está de acuerdo o en desacuerdo con las siguientes declaraciones? - Yo aprendo muchas cosas interesantes en matemáticas",
                    "¿En qué medida está de acuerdo o en desacuerdo con las siguientes declaraciones? - A mí me gusta resolver problemas de matemáticas",
                    "¿En qué medida está de acuerdo o en desacuerdo con las siguientes declaraciones? - A mí me gustan las matemáticas")

meta_cognition <- c("When reading the following statements, think about your current math class and decide how well the statements describe you. - I don't think that hard when I am doing work for class.",
                    "When reading the following statements, think about your current math class and decide how well the statements describe you. - I go through the work for math class and make sure that it's right.",
                    "When reading the following statements, think about your current math class and decide how well the statements describe you. - When work is hard, I only study the easy parts.",
                    "When reading the following statements, think about your current math class and decide how well the statements describe you. - I think about different ways to solve a problem.",
                    "When reading the following statements, think about your current math class and decide how well the statements describe you. - I try to connect what I am learning to things I have learned before.",
                    "Al leer las siguientes declaraciones, piensa en tu actual clase de matemáticas y decide qué tan bien te describen estas declaraciones. - Yo no tengo que pensar tanto cuando estoy trabajando para la clase.",
                    "Al leer las siguientes declaraciones, piensa en tu actual clase de matemáticas y decide qué tan bien te describen estas declaraciones. - Yo reviso el trabajo para la clase de matemáticas y me aseguro de que esté bien",
                    "Al leer las siguientes declaraciones, piensa en tu actual clase de matemáticas y decide qué tan bien te describen estas declaraciones. - Cuando el trabajo es difícil, yo solo estudio las partes fáciles",
                    "Al leer las siguientes declaraciones, piensa en tu actual clase de matemáticas y decide qué tan bien te describen estas declaraciones. - Yo pienso en diferentes formas de resolver un problema.",
                    "Al leer las siguientes declaraciones, piensa en tu actual clase de matemáticas y decide qué tan bien te describen estas declaraciones. - Yo intento conectar lo que estoy aprendiendo con las cosas que he aprendido antes.")

value_importance_math <- c("When reading the following statements, think about your current math class and decide how well the statements describe you. - I want to understand what is learned in math.",
                           "When reading the following statements, think about your current math class and decide how well the statements describe you. - I talk about math outside of class.",
                           "When reading the following statements, think about your current math class and decide how well the statements describe you. - I do other things when I am supposed to be paying attention.",
                           "Al leer las siguientes declaraciones, piensa en tu actual clase de matemáticas y decide qué tan bien te describen estas declaraciones. - Yo quiero entender lo que se aprende en matemáticas",
                           "Al leer las siguientes declaraciones, piensa en tu actual clase de matemáticas y decide qué tan bien te describen estas declaraciones. - Yo hablo de matemáticas fuera de la clase",
                           "Al leer las siguientes declaraciones, piensa en tu actual clase de matemáticas y decide qué tan bien te describen estas declaraciones. - Yo hago otras cosas cuando se supone que debo estar prestando atención")

math_task_persist <- c("When reading the following statements, think about your current math class and decide how well the statements describe you. - I would rather be told the answer than have to do the work.",
                       "When reading the following statements, think about your current math class and decide how well the statements describe you. - I keep trying even if something is hard.",
                       "When reading the following statements, think about your current math class and decide how well the statements describe you. - I stay focused in math class.",
                       "Al leer las siguientes declaraciones, piensa en tu actual clase de matemáticas y decide qué tan bien te describen estas declaraciones. - Yo prefiero que me digan la respuesta a tener que hacer el trabajo",
                       "Al leer las siguientes declaraciones, piensa en tu actual clase de matemáticas y decide qué tan bien te describen estas declaraciones. - Yo sigo intentándolo incluso si algo es difícil",
                       "Al leer las siguientes declaraciones, piensa en tu actual clase de matemáticas y decide qué tan bien te describen estas declaraciones. - Yo me mantengo concentrado en la clase de matemáticas")

agency <- c("When reading the following statements, think about your current math class and decide how well the statements describe you. - I feel good when I am in math class.",
            "When reading the following statements, think about your current math class and decide how well the statements describe you. - I try to understand my mistakes when I get something wrong.",
            "Al leer las siguientes declaraciones, piensa en tu actual clase de matemáticas y decide qué tan bien te describen estas declaraciones. - Yo me siento bien cuando estoy en la clase de matemáticas",
            "Al leer las siguientes declaraciones, piensa en tu actual clase de matemáticas y decide qué tan bien te describen estas declaraciones. - Yo trato de entender mis errores cuando me equivoco en algo")

student_crse_practices <- c("How often does your math teacher do each of the following? - My math teacher uses examples of students’ different cultures/backgrounds/families in their lessons.",
                            "How often does your math teacher do each of the following? - My math teacher respects my culture and background.",
                            "¿Con qué frecuencia su profesor de matemáticas hace cada uno de los siguientes? - Mi maestro/a de matemáticas usa ejemplos relacionados con las diferentes culturas/orígenes/familias de los estudiantes durante sus lecciones",
                            "¿Con qué frecuencia su profesor de matemáticas hace cada uno de los siguientes? - Mi maestro/a de matemáticas respeta mi cultura y origen")

get_mean_student_eic <- function(data, answer, pos_neg = "positive") {
  ### Remove all NAs in the data ###
  data <- data[!is.na(data)]
  data <- data[!is.null(data)]
  
  if (pos_neg == "positive") {
    ### 1-6 scale replacement ###
    data <- stringr::str_replace_all(data, c("Strongly disagree" = "1",
                                             "Disagree" = "2",
                                             "Mostly disagree" = "3",
                                             "Mostly agree" = "4",
                                             "Agree" = "5",
                                             "Strongly agree" = "6"))
    ## Spanish ##
    data <- stringr::str_replace_all(data, c("Muy en desacuerdo" = "1",
                                             "En desacuerdo" = "2",
                                             "Mayormente en desacuerdo" = "3",
                                             "Mayormente de acuerdo" = "4",
                                             "De acuerdo" = "5",
                                             "Totalmente de acuerdo" = "6"))
    ### Truth 1-5 scale replacement ###
    data <- stringr::str_replace_all(data, c("Not at all true" = "1",
                                             "Slightly true" = "2",
                                             "Somewhat true" = "3",
                                             "Very true" = "4",
                                             "Extremely true" = "5"))
    
    ### Like me 1-5 scale replacement ###
    data <- stringr::str_replace_all(data, c("Not at all like me" = "1",
                                             "Not much like me" = "2",
                                             "Somewhat like me" = "3",
                                             "Mostly like me" = "4",
                                             "Very much like me" = "5"))
    
    ### Of the time 1-4 scale replacement ###
    data <- stringr::str_replace_all(data, c("Never" = "1",
                                             "Once in a while" = "2",
                                             "Some of the time" = "3",
                                             "Most or all of the time" = "4"))
    
  } else if (pos_neg == "special_replace") {
    ### Agree 1-4 scale replacement ###
    data <- stringr::str_replace_all(data, c("Disagree a lot" = "1",
                                             "Disagree a little" = "2",
                                             "Agree a little" = "3",
                                             "Agree a lot" = "4"))
  } else if (pos_neg == "negative") {
    ### 1-6 negative scale replacement ###
    data <- stringr::str_replace_all(data, c("Strongly disagree" = "6",
                                             "Disagree" = "5",
                                             "Mostly disagree" = "4",
                                             "Mostly agree" = "3",
                                             "Agree" = "2",
                                             "Strongly agree" = "1"))
    ## Spanish ##
    data <- stringr::str_replace_all(data, c("Muy en desacuerdo" = "6",
                                             "En desacuerdo" = "5",
                                             "Mayormente en desacuerdo" = "4",
                                             "Mayormente de acuerdo" = "3",
                                             "De acuerdo" = "2",
                                             "Totalmente de acuerdo" = "1"))
    
    ### Like me 1-5 negative scale replacement ###
    data <- stringr::str_replace_all(data, c("Not at all like me" = "5",
                                             "Not much like me" = "4",
                                             "Somewhat like me" = "3",
                                             "Mostly like me" = "2",
                                             "Very much like me" = "1"))
  }
  
  data <- as.numeric(data)
  
  ### Get average of data ###
  data_average <- mean(data, na.rm = T)
  
  ### Return
  data_average
}

eic_student_survey |>
  mutate(`What is the name of your district and school?` = TeachingLab::string_replace(`What is the name of your district and school?`,
                                                                                       "District 11",
                                                                                       "NYC District 11 - District-wide, NY"),
         `What is the name of your district and school?` = TeachingLab::string_replace(`What is the name of your district and school?`,
                                                                                       "Rochester",
                                                                                       "Rochester City School District"),
         prepost = ifelse(date_created >= as.Date("2022-05-01"), 
                          "Post", 
                          "Pre"),
         prepost = ifelse(`What is the name of your district and school?` == "NYC District 11 - District-wide, NY",
                          "Pre",
                          prepost),
         id = str_trim(tolower(coalesce(`Please ask your teacher for their teacher code.`,
                       `Por favor, pídele a tu profesor/a su código.`)), side = "both"),
         id = str_replace_all(id, "elho428", "elho428")) |>
  group_by(id, prepost) |>
  count(sort = T) |>
  pivot_wider(names_from = "prepost", values_from = "n") |>
  drop_na(Post, id) |>
  select(-`NA`) |>
  view()

eic_student_survey |>
  dplyr::mutate(`What is the name of your district and school?` = TeachingLab::string_replace(`What is the name of your district and school?`,
                                                                                              "District 11",
                                                                                              "NYC District 11 - District-wide, NY"),
                `What is the name of your district and school?` = TeachingLab::string_replace(`What is the name of your district and school?`,
                                                                                              "Rochester",
                                                                                              "Rochester City School District"),
                prepost = ifelse(date_created >= as.Date("2022-05-01"), 
                                 "Post", 
                                 "Pre"),
                prepost = ifelse(`What is the name of your district and school?` == "NYC District 11 - District-wide, NY",
                                 "Pre",
                                 prepost),
                id = str_trim(tolower(coalesce(`Please ask your teacher for their teacher code.`,
                                               `Por favor, pídele a tu profesor/a su código.`)), side = "both"),
                id = str_replace_all(id, "elho428", "elho428"),
                bipoc = ifelse(is.na(`What is your ethnicity? Select all that apply. - White`),
                               "BIPOC",
                               "White")) |>
  dplyr::filter(id %in% c("dam1010", "elh0428", "ajm1110", "amw0516")) |>
  tidyr::drop_na(`What is the name of your district and school?`) |>
  dplyr::group_by(`What is the name of your district and school?`, prepost, bipoc) |>
  ### Spanish columns are separated out here ###
  # dplyr::summarise(dplyr::across(c(66:69, 83, 88:89), ~ get_mean_student_eic(.x, pos_neg = "negative")),
  #                  dplyr::across(c(70:76, 81:82, 90:95), ~ get_mean_student_eic(.x)),
  #                  dplyr::across(c(77:80), ~ get_mean_student_eic(.x, pos_neg = "special_replace"))) |>
  dplyr::summarise(dplyr::across(c(22:25, 39, 44:45), ~ get_mean_student_eic(.x, pos_neg = "negative")),
                   dplyr::across(c(26:32, 37:38, 46:51), ~ get_mean_student_eic(.x)),
                   dplyr::across(c(33:36), ~ get_mean_student_eic(.x, pos_neg = "special_replace"))) |>
  dplyr::ungroup() |>
  tidyr::pivot_longer(!c(`What is the name of your district and school?`, prepost, bipoc)) |>
  dplyr::mutate(construct = dplyr::case_when(name %in% growth_mindsets ~ "Growth Mindsets",
                                             name %in% achievement_identity ~ "Achievement Identity",
                                             name %in% self_efficacy ~ "Self Efficacy",
                                             name %in% math_enjoyment ~ "Math Enjoyment",
                                             name %in% meta_cognition ~ "Metacognition",
                                             name %in% value_importance_math ~ "Value & Importance of Mathematics",
                                             name %in% math_task_persist ~ "Math Task Persistence",
                                             name %in% agency ~ "Agency",
                                             name %in% student_crse_practices ~ "Student Perception of Teachers’ CRSE Practices"),
                #### RESCALE ALL VALUES TO 5 POINT SCALE ###
                value = dplyr::case_when(name %in% c(growth_mindsets[c(1:4, 6)], 
                                                     achievement_identity,
                                                     self_efficacy) ~ value * (5/6),
                                         name %in% c(math_enjoyment,
                                                     student_crse_practices) ~ value * 1.25,
                                         T ~ value),
                name = stringr::str_remove_all(name, "How much do you agree or disagree with the statements below\\? - |When reading the following statements, think about your current math class and decide how well the statements describe you\\. - |For you personally, how true or not true is the statement below\\? \\(Select only one\\) - |How much do you agree or disagree with the statement below\\? \\(Select one only\\) - |How often does your math teacher do each of the following\\? - "),
                value = round(100* (value/5), 2),
                prepost = factor(prepost, levels = c("Pre", "Post"))) |>
  pivot_wider(names_from = "prepost", values_from = "value") |>
  dplyr::rename(District = `What is the name of your district and school?`) |>
  dplyr::relocate(construct, .before = 1) |>
  dplyr::arrange(construct) |>
  dplyr::group_by(construct, District, bipoc) |>
  gt::gt() |>
  gt::tab_header(title = gt::md("**EIC Student Survey Scores**"),
                 subtitle = gt::md("*Scores are averages of likert value responses on scales of 1-4, 1-5, and 1-6, and ultimately scaled so all responses are percentages of a 1-5 point range.*")) |>
  gt::cols_label(name = " ",
                 construct = "Construct") |>
  gt::sub_missing(missing_text = "No data") |>
  gt::data_color(
    columns = c(Pre, Post),
    colors = scales::col_numeric(
      palette = tl_palette(color = "blue", n = 6),
      domain = NULL
    )
  ) |>
  gt::fmt_percent(columns = c(Pre, Post),
                  scale_values = F,
                  decimals = 2) |>
  gt::summary_rows(groups = TRUE,
                   columns = c(Pre, Post),
                   fns = list(Average = ~ mean(., na.rm = TRUE)),
                   formatter = gt::fmt_percent,
                   decimals = 2,
                   scale_values = F) |>
  # gt::tab_style(
  #   style = gt::cell_fill(color = tl_palette(color = "blue", n = 6)[4]),
  #   locations = gt::cells_body(columns = "name",
  #                              rows = (construct == "Growth Mindsets"))
  # ) |>
  TeachingLab::gt_theme_tl(align = "center") |>
  gt::gtsave(here::here("images/html_tables/eic_student_survey_results.html"))

eic_student_survey |>
  dplyr::mutate(`What is the name of your district and school?` = TeachingLab::string_replace(`What is the name of your district and school?`,
                                                                                              "District 11",
                                                                                              "NYC District 11 - District-wide, NY"),
                `What is the name of your district and school?` = TeachingLab::string_replace(`What is the name of your district and school?`,
                                                                                              "Rochester",
                                                                                              "Rochester City School District"),
                prepost = ifelse(date_created >= as.Date("2022-05-01"), 
                                 "Post", 
                                 "Pre"),
                prepost = ifelse(`What is the name of your district and school?` == "NYC District 11 - District-wide, NY",
                                 "Pre",
                                 prepost),
                id = str_trim(tolower(coalesce(`Please ask your teacher for their teacher code.`,
                                               `Por favor, pídele a tu profesor/a su código.`)), side = "both"),
                id = str_replace_all(id, "elho428", "elho428"),
                `Are you Hispanic/Latino/a?` = str_replace_all(`Are you Hispanic/Latino/a?`, c("Yes" = "Hispanic",
                                                                                               "No" = "Not Hispanic"))) |>
  # dplyr::filter(id %in% c("dam1010", "elh0428", "ajm1110", "amw0516")) |>
  tidyr::drop_na(`What is the name of your district and school?`) |>
  dplyr::group_by(`What is the name of your district and school?`, prepost, `Are you Hispanic/Latino/a?`) |>
  ### Spanish columns are separated out here ###
  # dplyr::summarise(dplyr::across(c(66:69, 83, 88:89), ~ get_mean_student_eic(.x, pos_neg = "negative")),
  #                  dplyr::across(c(70:76, 81:82, 90:95), ~ get_mean_student_eic(.x)),
  #                  dplyr::across(c(77:80), ~ get_mean_student_eic(.x, pos_neg = "special_replace"))) |>
  dplyr::summarise(dplyr::across(c(22:25, 39, 44:45), ~ get_mean_student_eic(.x, pos_neg = "negative")),
                   dplyr::across(c(26:32, 37:38, 46:51), ~ get_mean_student_eic(.x)),
                   dplyr::across(c(33:36), ~ get_mean_student_eic(.x, pos_neg = "special_replace"))) |>
  dplyr::ungroup() |>
  tidyr::pivot_longer(!c(`What is the name of your district and school?`, prepost, `Are you Hispanic/Latino/a?`)) |>
  dplyr::mutate(construct = dplyr::case_when(name %in% growth_mindsets ~ "Growth Mindsets",
                                             name %in% achievement_identity ~ "Achievement Identity",
                                             name %in% self_efficacy ~ "Self Efficacy",
                                             name %in% math_enjoyment ~ "Math Enjoyment",
                                             name %in% meta_cognition ~ "Metacognition",
                                             name %in% value_importance_math ~ "Value & Importance of Mathematics",
                                             name %in% math_task_persist ~ "Math Task Persistence",
                                             name %in% agency ~ "Agency",
                                             name %in% student_crse_practices ~ "Student Perception of Teachers’ CRSE Practices"),
                #### RESCALE ALL VALUES TO 5 POINT SCALE ###
                value = dplyr::case_when(name %in% c(growth_mindsets[c(1:4, 6)], 
                                                     achievement_identity,
                                                     self_efficacy) ~ value * (5/6),
                                         name %in% c(math_enjoyment,
                                                     student_crse_practices) ~ value * 1.25,
                                         T ~ value),
                name = stringr::str_remove_all(name, "How much do you agree or disagree with the statements below\\? - |When reading the following statements, think about your current math class and decide how well the statements describe you\\. - |For you personally, how true or not true is the statement below\\? \\(Select only one\\) - |How much do you agree or disagree with the statement below\\? \\(Select one only\\) - |How often does your math teacher do each of the following\\? - "),
                value = round(100* (value/5), 2),
                prepost = factor(prepost, levels = c("Pre", "Post"))) |>
  pivot_wider(names_from = "prepost", values_from = "value") |>
  dplyr::rename(District = `What is the name of your district and school?`) |>
  dplyr::relocate(construct, .before = 1) |>
  dplyr::arrange(construct) |>
  dplyr::group_by(construct, District, `Are you Hispanic/Latino/a?`) |>
  gt::gt() |>
  gt::tab_header(title = gt::md("**EIC Student Survey Scores**"),
                 subtitle = gt::md("*Scores are averages of likert value responses on scales of 1-4, 1-5, and 1-6, and ultimately scaled so all responses are percentages of a 1-5 point range.*")) |>
  gt::cols_label(name = " ",
                 construct = "Construct") |>
  gt::sub_missing(missing_text = "No data") |>
  gt::data_color(
    columns = c(Pre, Post),
    colors = scales::col_numeric(
      palette = tl_palette(color = "blue", n = 6),
      domain = NULL
    )
  ) |>
  gt::fmt_percent(columns = c(Pre, Post),
                  scale_values = F,
                  decimals = 2) |>
  gt::summary_rows(groups = TRUE,
                   columns = c(Pre, Post),
                   fns = list(Average = ~ mean(., na.rm = TRUE)),
                   formatter = gt::fmt_percent,
                   decimals = 2,
                   scale_values = F) |>
  # gt::tab_style(
  #   style = gt::cell_fill(color = tl_palette(color = "blue", n = 6)[4]),
  #   locations = gt::cells_body(columns = "name",
  #                              rows = (construct == "Growth Mindsets"))
  # ) |>
  TeachingLab::gt_theme_tl(align = "center") |>
  gt::gtsave(here::here("images/html_tables/eic_student_survey_results_hispanic.html"))
  