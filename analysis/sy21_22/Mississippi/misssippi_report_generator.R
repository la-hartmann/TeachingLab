library(tidyverse)
library(rmarkdown)
library(googlesheets4)
library(fidelius)
googlesheets4::gs4_auth()

actual_teacher_names <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1RwTgvE_vp7dqxbRSAXXPiN_t3AVfVtN0fNoGDz49HlY/edit#gid=0",
  sheet = 1
) %>%
  pull(Name)

actual_teacher_names[1:86] %>%
  clipr::write_clip()

# replacement_vector <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1jJF42pxeZnMkzQ7GEH84mTSYoRfph7t7Akuu8JlNjUw/edit#gid=0",
#                                  sheet = 1) %>%
#   filter(!is.na(`New Name`)) 
# 
# paste0(replacement_vector$`Old Name`, " = ", replacement_vector$`New Name`) %>%
#   clipr::write_clip() %>%
#   datapasta::vector_paste_vertical()

# student_survey <- TeachingLab::get_student_survey(update = T)
# family_survey <- TeachingLab::get_family_survey(update = T)
# diagnostic <- TeachingLab::get_diagnostic_survey(update = T)

# students_teachers <- student_survey %>%
#   filter(str_detect(`What is the name of your school, district, or parish?`, ", MS")) %>%
#   select("teacher") %>%
#   pivot_longer(everything()) %>%
#   mutate(value = str_replace_all(value, replacement_vector)) %>%
#   pull(value) %>%
#   unique() %>%
#   purrr::keep(~ !is.na(.x))
# 
# family_teachers <- family_survey %>%
#   filter(str_detect(`What is the name of your child's school, district, or parish?`, ", MS")) %>%
#   select("teacher") %>%
#   pivot_longer(everything()) %>%
#   mutate(value = str_replace_all(value, replacement_vector)) %>%
#   pull(value) %>%
#   unique() %>%
#   purrr::keep(~ !is.na(.x))

# wrong_teachers_value <- as_tibble(c(append(students_teachers, family_teachers))) %>%
#   distinct(value) %>%
#   filter(value %!in% actual_teacher_names) %>%
#   arrange(value)

# range_write(ss = "https://docs.google.com/spreadsheets/d/1jJF42pxeZnMkzQ7GEH84mTSYoRfph7t7Akuu8JlNjUw/edit#gid=0",
#             sheet = 1,
#             data = wrong_teachers_value,
#             col_names = F,
#             reformat = F,
#             range = glue::glue("A2:A{nrow(wrong_teachers_value) + 1}"))

# min_string_dist <- function(lookup, data) {
#   # Make matrix
#   matrix <- stringdistmatrix(a = lookup, b = data, useNames = "strings", method = "cosine")
#   matrix <- matrix[order(matrix[, 1], decreasing = FALSE), ]
#   # list of minimun levenshteins
#   leven <- matrix
#   # return list of the row number of the minimum value
#   minlist <- dplyr::row_number(matrix)
#   # return list of matching values
#   matchwith <- lookup[minlist]
# 
#   # final answer
#   answer <- data.frame(data, matchwith, leven)
# 
#   rownames(answer) <- NULL
# 
#   final_answer <- answer %>%
#     filter(data != matchwith) %>%
#     arrange(leven) %>%
#     slice(1) %>%
#     pull(matchwith)
#   
#   if (length(final_answer) == 0) {
#     final_answer <- "No replacement"
#   }
#   
#   final_answer
#   
# }
# 
# min_string_dist(students_teachers[7], actual_teacher_names)

teacher <- list("Mitzi Mullins",
             "Nykol Robertson",
             "Chenata Ward",
             "Jasmine Carter",
             "Julie Richards",
             "Leah Huerkamp",
             "Alicia Smith",
             "Sharrel Johnson",
             "Letitia Coleman",
             "Orlando Thomas-Jones",
             "Cayla Freeman",
             "Kevin Haymore",
             "April Coleman",
             "Andrea Davis",
             "Brittany Hopson",
             "Shnika Jackson",
             "Chardricka Johnson",
             "Joshua Christmas",
             "Pearl Mabry",
             "Thomas Keys",
             "Jaleel King",
             "Suzanne Holder",
             "Felicia Thompson",
             "William Washington",
             "Samantha Myles",
             "Christopher McClinton",
             "Tamekia Williams",
             "Monica Anderson",
             "LaJuan McGill",
             "Clairice Magee",
             "Patrick Smith",
             "Nicholas Hoffman",
             "Kimberly Anderson",
             "Felicia Kersh",
             "Keyunna McNeil",
             "Shirley Hillaird-Epps",
             "Tracey Horton",
             "Quintavious Phillips",
             "Verkilra Donelson",
             "Katrina Wallace",
             "Doris Stocks",
             "Cristal Turner",
             "Lashondra McDonald",
             "Gidget Griffin-McMiller",
             "Roshanda Nash",
             "Suzzane Dean",
             "Theresa Savell",
             "Jamie Ducksworth",
             "Shawnnay Davenport",
             "Lexia Thomas",
             "Thalia Jones",
             "Jamisha Oliver",
             "Ashley Hite",
             "Carnice James",
             "Kelley King",
             "Heather Todd",
             "Rhonda Coleman",
             "Jame Turnage",
             "Senetria Dowsing",
             "Megan Tramill",
             "Keri Coker",
             "Tyrone Marshall",
             "Tammy Clark",
             "Brenda Washington",
             "Marilyn Hunter",
             "Rawanda Fisher",
             "Melandie Williams",
             "Talauntra Bullock",
             "Sherry Brown",
             "Keila Patrick",
             "Leslie Cowans",
             "Dinah Sullivan",
             "Danessa McCullin",
             "Veronica Elery-Smith",
             "Natasha Patten",
             "Eddie Smith",
             "Jacqueline Leach",
             "Rosalinda Norsworthy",
             "Desiree Wells",
             "Natasha Harris",
             "Vivekanand Yamagowni",
             "Samoria Conner",
             "Shannon McGhee",
             "Carrie Chadic",
             "Heather Kealhofer",
             "Pamela Huskey",
             "All")

passwords <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1eAUdWEzc0R7WzpztOvutRp2FYDtpks37LSFV8sq8ezg/edit#gid=739665772",
                                       sheet = "Mississippi Reports March") %>%
  pull(Key)

new_passwords <- rerun(.n = length(teacher) - length(passwords), TeachingLab::password_generator(length = 8)) %>%
  as.character()

passwords <- c(passwords[-1], passwords[1]) %>%
  setdiff(new_passwords)

walk(teacher, ~ rmarkdown::render(
  input = here::here("analysis/sy21_22/Mississippi/Mississippi_report.Rmd"),
  output_file = paste0("March_Report_", str_replace_all(tolower(.x), " ", "_")),
  output_dir = here::here("analysis/sy21_22/Mississippi/Reports"),
  params = list(teacher = .x)
))

files_gen <- list.files(here::here("analysis/sy21_22/Mississippi/Reports"),
  full.names = T
)

files_gen <- c(files_gen[-2], files_gen[2])

# file.copy(from = files_gen,
#           to = here::here("analysis/sy21_22/Mississippi/backup_reports"))

purrr::walk2(files_gen, passwords, ~ fidelius::charm(
  input = .x,
  password = .y,
  hint = "Check the google spreadsheet!",
  style = fidelius::stylize(font_family = "Calibri",
                            title_color = "#04abeb", btn_bg_color = "#04abeb", modal_title_color = "#04abeb",
                            btn_hover_color = "#43464d"),
  bundle = F
))

walk(files_gen[87], 
     ~ file.copy(from = .x, to = "~/Teaching Lab/Coding/teachinglab.github.io/mississippi_reports/",
                 overwrite = T))
