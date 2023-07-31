library(tidyverse)
library(googlesheets4)
library(googledrive)
library(clipr)
library(datapasta)

teachers_english <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Ow1MMspmxnpiIcmUH_NQWNZArBCtP-FQwq7gVG35Qcs/edit#gid=215742916", sheet = "Form Responses 1") %>%
  mutate(across(everything(), ~ as.character(.x)))
teachers_spanish <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1AOV1NKdBdw9dTXvKfLh_hU0CvscYB-0hKXMvWKaKjKc/edit#gid=1015214110", sheet = "Form Responses 1") %>%
  set_names(nm = colnames(teachers_english)) %>%
  mutate(across(everything(), ~ as.character(.x)))

teachers_full <- bind_rows(teachers_english, teachers_spanish) %>%
  drop_na(`Please type in your name using the format Last Name, First Name.`)

teacher_activity <- read_sheet("https://docs.google.com/spreadsheets/d/14QUw-OW6YzP785YavOy_c-l5rDUpOuLULgW1TQf41vs/edit#gid=0") %>%
  select(2:3) %>%
  mutate(Activity = unlist(Activity))

# Copy paste to sheets function
# specimen <- function(x)
#   deparse(x) %>%
#   str_c(collapse = '') %>%
#   str_replace_all('\\s+', ' ') %>%
#   str_replace_all('\\s*([^,\\()]+ =) (c\\()', '\n  \\1\n    \\2')  %>%
#   str_replace_all('(,) (class =)', '\\1\n  \\2') %>%
#   write_clip(allow_non_interactive = TRUE)
# This renders to clipboard
# vector_paste_vertical(unique(teachers_english$`Please select your school/department`))
school_list <- c(
  "Clovis High School",
  "Taylor Middle School",
  "School of Dreams Academy",
  "Curriculum and Instruction",
  "Amy Biehl Charter High School",
  "Raton Intermediate School",
  "Mesa Middle School",
  "Sandoval Academy of Bilingual Education",
  "Peñasco Middle and High School",
  "District Office/District Resource Teachers",
  "Robert F. Kennedy Charter School (Local Charter)",
  "La Academia Dolores Huerta",
  "Dora Elementary",
  "Forrester Elementary",
  "Wilferth Elementary"
)

# Replacement vector
school_replace_list <- c(
  "school1",
  "school2",
  "school3",
  "school4",
  "school5",
  "school6",
  "school7",
  "school8",
  "school9",
  "school10",
  "school11",
  "school12",
  "school13",
  "school14",
  "school15",
  "school16"
)

# dput(cat(paste0("\"", school_list, "\" = \"", school_replace_list, "\",")))

district_list <- c("Clovis Municipal Schools", 
                   "Albuquerque Public Schools", 
                   "School of Dreams Academy (State Charter)", 
                   "Albuquerque Public Schools", 
                   "Amy Biehl Charter High School (State Charter)", 
                   "Raton Public Schools", 
                   "Roswell Independent Schools District", 
                   "Sandoval Academy of Bilingual Education (State Charter)", "Peñasco Independent School District", 
                   "Albuquerque Public Schools", "Robert F. Kennedy Charter School (Local Charter)", 
                   "La Academia Dolores Huerta (State Charter)", "Dora Consolidated Schools", 
                   "Springer Municipal Schools", "Springer Municipal Schools")

# From vlookup in sheets
district_replace_list <- c(
  "Clovis High School" = "Clovis Municipal Schools", "Taylor Middle School" = "Albuquerque Public Schools", "School of Dreams Academy" = "School of Dreams Academy (State Charter)", "Curriculum and Instruction" = "Albuquerque Public Schools", "Amy Biehl Charter High School" = "Amy Biehl Charter High School (State Charter)", "Raton Intermediate School" = "Raton Public Schools", "Mesa Middle School" = "Roswell Independent Schools District", "Sandoval Academy of Bilingual Education" = "Sandoval Academy of Bilingual Education (State Charter)", "Peñasco Middle and High School" = "Peñasco Independent School District", "District Office/District Resource Teachers" = "Albuquerque Public Schools", "Robert F. Kennedy Charter School \\(Local Charter\\)" = "Robert F. Kennedy Charter School (Local Charter)", "La Academia Dolores Huerta" = "La Academia Dolores Huerta (State Charter)", "Dora Elementary" = "Dora Consolidated Schools", "Forrester Elementary" = "Springer Municipal Schools", "Wilferth Elementary" = "Springer Municipal Schools"
)

district_replace_list2 <- c(
  "district1",
  "district2",
  "district3",
  "district4",
  "district5",
  "district6",
  "district7",
  "district8",
  "district9",
  "district10",
  "district11",
  "district12",
  "district13",
  "district14",
  "district15"
)

# admin_vs_teacher <- c(1, 2)

no_attend <- c("Cardona, Victoria", "Rendon, Amy", "Loudermilk, Kathleen", "Sobol, Elana", "Ryan, Jeannie", "Thomson, John", "Nelson, Phihoang",
               "Kelley, Victoria", "Andrego, Miranda", "Browning, KatyMae", "Marquez, Tianna", "Ortega, Julianna", "Martinez, Michele", "Martinez, Amanda", "Morrison, Amy", "Lopez, Vanessa", "Kelly, John", "Williams, Mark", "Jones, Abby",
               "Mancha-Alvarez, Christina", "Todacheene, Lemmert", "Losey, Robin", "Dosumu, Charlotte", "King, Leah", "Zohnie, Perfeilia")

replacement_vector <- c(
  "Clovis High School" = "school1", 
  "Taylor Middle School" = "school2", 
  "School of Dreams Academy" = "school3", 
  "Curriculum and Instruction" = "school4", 
  "Amy Biehl Charter High School" = "school5", 
  "Raton Intermediate School" = "school6", 
  "Mesa Middle School" = "school7", 
  "Sandoval Academy of Bilingual Education" = "school8", 
  "Peñasco Middle and High School" = "school9", 
  "District Office/District Resource Teachers" = "school10", 
  "Robert F. Kennedy Charter School (Local Charter)" = "school11", 
  "La Academia Dolores Huerta" = "school12", 
  "Dora Elementary" = "school13", 
  "Forrester Elementary" = "school14", 
  "Wilferth Elementary" = "school15", 
  "Christine Duncan Heritage Academy" = "school17"
)

district_replace_list3 <- c("Clovis Municipal Schools" = "district1", 
                            "Albuquerque Public Schools" = "district2", 
                            "School of Dreams Academy \\(State Charter\\)" = "district3", 
                            "Amy Biehl Charter High School \\(State Charter\\)" = "district5", 
                            "Raton Public Schools" = "district6", 
                            "Roswell Independent Schools District" = "district7", 
                            "Sandoval Academy of Bilingual Education \\(State Charter\\)" = "district8", 
                            "Peñasco Independent School District" = "district9", 
                            "Robert F. Kennedy Charter School \\(Local Charter\\)" = "district11", 
                            "La Academia Dolores Huerta \\(State Charter\\)" = "district12", 
                            "Dora Consolidated Schools" = "district13", 
                            "Springer Municipal Schools" = "district14", 
                            "Christine Duncan Heritage Academy (Local Charter)" = "district15",
                            "Christine Duncan Heritage Academy" = "district15")

teachers_deidentified <- teachers_full %>%
  distinct(`Please type in your name using the format Last Name, First Name.`, .keep_all = T) %>%
  left_join(teacher_activity, by = c("Please type in your name using the format Last Name, First Name." = "Teacher")) %>%
  mutate(prepost = "pre",
         attendance = if_else(`Please type in your name using the format Last Name, First Name.` %in% no_attend, F, T),
         activity1 = case_when(Activity == "1" ~ T,
                               Activity == "1,2" ~ T,
                               Activity == "1, 2" ~ T,
                               Activity == "2" ~ F),
         activity2 = case_when(Activity == "2" ~ T,
                               Activity == "1,2" ~ T,
                               Activity == "1, 2" ~ T,
                               Activity == "1" ~ F)) %>%
  select(-Activity) %>%
  # Create district vector
  mutate(`District` = str_replace_all(`Please select your school/department`, district_replace_list),
         District = str_replace_all(District, district_replace_list3)) %>%
  # Change all school names
  mutate(`Please select your school/department` = str_replace_all(`Please select your school/department`, replacement_vector),
         `Please select your school/department` = str_replace_all(`Please select your school/department`, 
                                                                  "Robert F. Kennedy Charter School \\(Local Charter\\)", "school11")) %>%
  # Create id
  mutate(id = paste0("teacher", "_", row_number(), "_", `Please select your school/department`, "_", District, "_1")) %>%
  # Deselect identifiers
  # Run if you need teacher name identifier column
  # mutate(names = sub("(\\w+),\\s(\\w+)","\\2 \\1", `Please type in your name using the format Last Name, First Name.`),
  #        copy = paste0(names, "'s", " = ", id),
  #        copy2 = paste0(names, " = ", id)) %>%
  select(-c(1, 2)) %>%
  # Move id to front
  relocate(id, .before = 1)

# Cleanup
if (exists(ss)) {
  googledrive::drive_trash(ss)
}
# Spreadsheet creation
ss <- gs4_create("New Mexico Teacher Deidentified Data", sheets = "Deidentified Teachers")
# Write to sheet
teachers_deidentified %>%
  write_sheet(ss, sheet = "Deidentified Teachers")



##### SECONDARY BINDING DATA

# pre_data <- fake_data %>% select(3:6) %>% slice(-48, -c(87:90))
# 
# data2 <- read_csv("~/Downloads/FINAL New Mexico Teacher Deidentified Data - Deidentified Teachers.csv") %>%
#   bind_cols(pre_data)
# 
# write_csv(data2, here("Data/FINAL FINAL New Mexico Teacher Pre Deidentified Data - Deidentified Teachers.csv"))








