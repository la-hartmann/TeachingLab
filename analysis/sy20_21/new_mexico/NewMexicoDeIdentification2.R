library(tidyverse)
library(googlesheets4)
library(here)


# Remember to uncomment code with ids here first
# source(here("analysis/New Mexico/NewMexicoDeIdentification.R"))


teacher_activity <- read_sheet("https://docs.google.com/spreadsheets/d/14QUw-OW6YzP785YavOy_c-l5rDUpOuLULgW1TQf41vs/edit#gid=0") %>%
  select(2:3) %>%
  mutate(Activity = unlist(Activity))

# pre_ids <- teachers_deidentified %>% slice(-c(85:89)) %>% select(id)
# pre_names <- teachers_english %>% slice(-c(85:89)) %>% select(`Please type in your name using the format Last Name, First Name.`)
# 
# ids_names <- bind_cols(pre_ids, pre_names)

replace_vector <- c(
  "George Marquez" = "teacher_1_school17_district15_1", "Fernandez-Ana Garcia" = "teacher_2_school17_district15_1", 
  "Maria Baca" = "teacher_3_school17_district15_1", "Clara Sainvilmar" = "teacher_4_school8_district8_1", 
  "Sam Morerod" = "teacher_5_school3_district3_1", "Susan Smith" = "teacher_6_school14_district14_1", 
  "Dolores Lopez" = "teacher_7_school8_district8_1", "Kimberly Tafoya" = "teacher_8_school14_district14_1", 
  "Carlos Viera" = "teacher_9_school3_district3_1", "Julie Crum" = "teacher_10_school15_district14_1", 
  "Louisa Maestas" = "teacher_11_school15_district14_1", "Meredith Grant" = "teacher_12_school2_district2_1", 
  "Jacey Long" = "teacher_13_school6_district6_1", "Shana Burton" = "teacher_14_school14_district14_1", 
  "Amber Garcia" = "teacher_15_school15_district14_1", "Candice Putman" = "teacher_16_school14_district14_1", 
  "Militza Geisel" = "teacher_17_school8_district8_1", "Jay Brady" = "teacher_18_school1_district1_1", 
  "Aimee Feldman" = "teacher_19_school6_district6_1", "Jennifer Ryan" = "teacher_20_school1_district1_1", 
  "Lisa Lee" = "teacher_21_school1_district1_1", "Myra Skinner" = "teacher_22_school13_district13_1", 
  "Brian Tello" = "teacher_23_school2_district2_1", "Kassandra Buras" = "teacher_24_school13_district13_1", 
  "Emma Niiler" = "teacher_25_school2_district2_1", "Janella Hill" = "teacher_26_school1_district1_1", 
  "Kathy Hajner" = "teacher_27_school3_district3_1", "Virginia Gallegos" = "teacher_28_school12_district12_1", 
  "Cassie Hobbs" = "teacher_29_school7_district7_1", "Tamara Gaudet" = "teacher_30_school4_district2_1", 
  "Mia Trujillo" = "teacher_31_school6_district6_1", "Linda Sanchez" = "teacher_32_school11_district11_1", 
  "Alan French" = "teacher_33_school7_district7_1", "Click Mickey D." = "teacher_34_school7_district7_1", 
  "John Thomson" = "teacher_35_school5_district5_1", "Clara Ivonne" = "teacher_36_school8_district8_1", 
  "Mickey Click" = "teacher_37_school7_district7_1", "Mary Jane" = "teacher_38_school7_district7_1", 
  "Mary Jane" = "teacher_39_school7_district7_1", "Theresa Ambrogi" = "teacher_40_school4_district2_1", 
  "Karla Gade" = "teacher_41_school10_district2_1", "Brian Hobbs" = "teacher_42_school7_district7_1", 
  "Vanessa Gonzales" = "teacher_43_school6_district6_1", "Cathie Hephner" = "teacher_44_school6_district6_1", 
  "Stephanie Grande" = "teacher_45_school6_district6_1", "Brock Walton" = "teacher_46_school6_district6_1", 
  "Maggie Longwill" = "teacher_47_school6_district6_1", "Robby Armijo" = "teacher_48_school6_district6_1", 
  "Kristina Smith" = "teacher_49_school8_district8_1", "Krystle Winklepleck" = "teacher_50_school11_district11_1", 
  "Carolyn Aragon" = "teacher_51_school6_district6_1", "Diana Martinez" = "teacher_52_school6_district6_1", 
  "Joleene Starr" = "teacher_53_school6_district6_1", "Shelby Padilla" = "teacher_54_school6_district6_1", 
  "Patricia Resendiz" = "teacher_55_school3_district3_1", "Sue Holland" = "teacher_56_school6_district6_1", 
  "Phihoang Nelson" = "teacher_57_school5_district5_1", "Ronda Davis" = "teacher_58_school10_district2_1", 
  "Elana Sobol" = "teacher_59_school5_district5_1", "Julia Geffroy" = "teacher_60_school9_district9_1", 
  "Sydney Main" = "teacher_61_school6_district6_1", "Thomas Barksdale" = "teacher_62_school6_district6_1", 
  "Jamie Hephner" = "teacher_63_school6_district6_1", "Vanessa Horner" = "teacher_64_school6_district6_1", 
  "Ambrosita Sintas" = "teacher_65_school6_district6_1", "Stephanie Becker" = "teacher_66_school5_district5_1", 
  "Stefanie Ware" = "teacher_67_school6_district6_1", "Linda Ortiz" = "teacher_68_school6_district6_1", 
  "Lopez N. James" = "teacher_69_school2_district2_1", "Rosella Estorque" = "teacher_70_school7_district7_1", 
  "Wanda Henson" = "teacher_71_school6_district6_1", "Denise Taylor" = "teacher_72_school7_district7_1", 
  "Dana Vallejos" = "teacher_73_school2_district2_1", "Kathleen Loudermilk" = "teacher_74_school5_district5_1", 
  "Jeannie Ryan" = "teacher_75_school5_district5_1", "Jennifer Sears" = "teacher_76_school4_district2_1", 
  "Javier Viera" = "teacher_77_school3_district3_1", "Lynne McDonald" = "teacher_78_school1_district1_1", 
  "Ortiz-Nuria Mingorance" = "teacher_79_school17_district15_1", 
  "Meza-Julio Quezada" = "teacher_80_school17_district15_1", "Eva Ornelas" = "teacher_81_school17_district15_1", 
  "Toni Chavez" = "teacher_82_school17_district15_1", "Cynthia Toledo" = "teacher_83_school17_district15_1", 
  "Gina Gonzalez" = "teacher_84_school17_district15_1", "Melissa Maestas" = "teacher_85_school17_district15_1", 
  "Araceli Gutierrez" = "teacher_86_school17_district15_1", "Leos Rafael Gonzalez" = "teacher_87_school17_district15_1",
  "Alicia Garcia" = "teacher_88_school17_district15_1", "Ana Vazquez" = "teacher_89_school17_district15_1", 
  "Elena Zuniga" = "teacher_90_school17_district15_1", "Jessica Carrillo" = "teacher_91_school17_district15_1", 
  "Joel Sandoval" = "teacher_92_school17_district15_1", "Julio Quezada" = "teacher_93_school17_district15_1", 
  "Lynda Martinez" = "teacher_94_school17_district15_1", "Rafael Gonzalez" = "teacher_95_school17_district15_1", 
  "Sandoval Fatima" = "teacher_96_school17_district15_1", "Sandra Martinez" = "teacher_97_school17_district15_1", 
  "Sandra Orozco" = "teacher_98_school17_district15_1", "Tafoya-Kim Perez" = "teacher_99_school15_district14_1", 
  "Virginia Hernandez" = "teacher_100_school17_district15_1", "Robert Torrez" = "teacher_101_school3_district3_1"
)

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

# xnot_in_first_round <- c("Robert Torrez" = "teacher_124_school3_district3_1")

teacher_data <- read_sheet("https://docs.google.com/spreadsheets/d/1-P6ZazMnVnH2Cr8WsbcG4RUINI_1HRsDpN0Tiuc_w0I/edit?resourcekey#gid=1365509288") %>%
  drop_na(Timestamp) %>%
  distinct(`Please type in your name using the format Last Name, First Name.`, .keep_all = T)

teacher_data_spanish <- read_sheet("https://docs.google.com/spreadsheets/d/11AVhcrbdWucFWrKFlmUBJKJizuL-hxeQpVoukhnjEKk/edit#gid=1484968436")
# 
# teacher_data_final <- bind_rows(teacher_data, teacher_data_spanish) %>%
  # drop_na(Timestamp) %>%
  # distinct(`Please type in your name using the format Last Name, First Name.`, .keep_all = T)

teachers_deidentified_2 <- teacher_data %>%
  left_join(teacher_activity, by = c("Please type in your name using the format Last Name, First Name." = "Teacher")) %>%
  select(-1) %>%
  # Change format from last, first to first last
  mutate(`Please type in your name using the format Last Name, First Name.` = sub("(\\w+),\\s(\\w+)","\\2 \\1", `Please type in your name using the format Last Name, First Name.`)) %>%
  mutate(id = str_replace_all(`Please type in your name using the format Last Name, First Name.`, replace_vector)) %>%
  # mutate(id = str_replace_all(id, not_in_first_round)) %>%
  select(-1) %>%
  # Move id to front
  relocate(id, .before = 1) %>%
  mutate(prepost = as.character("post"),
         attendance = T,
         activity1 = if_else(Activity == 1, T, F),
         activity2 = if_else(Activity == 2, T, F)) %>%
  select(-Activity, -attendance) %>%
  mutate(school = str_replace_all(`Please select your school/department`, replacement_vector)) %>%
  select(-`Please select your school/department`)
  

# Cleanup
if (exists(ss)) {
  googledrive::drive_trash(ss)
}
# Spreadsheet creation
ss <- gs4_create("New Mexico Teacher Deidentified Post Data", sheets = "Deidentified Teachers")
# Write to sheet
teachers_deidentified_2 %>%
  write_sheet(ss, sheet = "Deidentified Teachers")


teachers_deidentified_3 <- teacher_data_spanish %>%
  left_join(teacher_activity, by = c("Escriba su nombre con el formato Apellido, Nombre." = "Teacher")) %>%
  select(-1) %>%
  # Change format from last, first to first last
  mutate(`Escriba su nombre con el formato Apellido, Nombre.` = sub("(\\w+),\\s(\\w+)","\\2 \\1", `Escriba su nombre con el formato Apellido, Nombre.`)) %>%
  mutate(id = str_replace_all(`Escriba su nombre con el formato Apellido, Nombre.`, replace_vector)) %>%
  # mutate(id = str_replace_all(id, not_in_first_round)) %>%
  select(-1) %>%
  # Move id to front
  relocate(id, .before = 1) %>%
  mutate(prepost = as.character("post"),
         attendance = T,
         activity1 = if_else(Activity == 1, T, F),
         activity2 = if_else(Activity == 2, T, F)) %>%
  select(-Activity, -attendance) %>%
  mutate(school = str_replace_all(`Seleccione su escuela / departamento`, replacement_vector)) %>%
  select(-`Seleccione su escuela / departamento`)


# Cleanup
if (exists(ss)) {
  googledrive::drive_trash(ss)
}
# Spreadsheet creation
ss <- gs4_create("New Mexico Teacher Deidentified Post Data Spanish", sheets = "Deidentified Teachers")
# Write to sheet
teachers_deidentified_3 %>%
  write_sheet(ss, sheet = "Deidentified Teachers")




