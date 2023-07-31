library(tidyverse)
library(googlesheets4)

sheet_identified <- read_sheet("https://docs.google.com/spreadsheets/d/1B_AIE4-WjNfY9ytZ9KdMozYhV44hVJCR2mzNAZ8YqMM/edit#gid=0")

# Teacher replacement vector from NewMexicoDeIdentification.R
replace_all <- c("Stephanie Gardner" = "admin_1_school2_district2_2",
                           "Nicaea Chavez" = "admin_2_school9_district9_2", 
                           "Annetta Hadley" = "admin3_school1_district1_2", 
                           "Linda Sanchez" = "admin4_school11_district11_2", 
                           "Shawn Morris" = "admin5_school11_district11_2", 
                           "Felicitas Adame-Reyes" = "admin6_school8_district8_2",
                           "Stephanie Becker" = "admin7_school5_district5_2", 
                           "Jamie Watson" = "admin8_school7_district7_2", 
                           "Christina Hidalgo" = "admin9_school14_district14_2",
                           "Sam Morerod" = "admin10_school3_district_district3_2",
                           "Kristie Medina" = "admin11_school6_district6_2",
                           "Jesus Moncada" = "admin12_school17_district17_2",
                           "Julie Crum" = "admin13_school14_district14_2",
                           "Jay Brady" = "admin14_school1_district1_2",
                           "George Marquez" = "teacher_1_school17_district15_1", "Fernandez-Ana Garcia" = "teacher_2_school17_district15_1", 
                           "Maria Baca" = "teacher_3_school17_district15_1", "Clara Sainvilmar" = "teacher_36_school8_district8_1", 
                           "Sam Morerod" = "teacher_5_school3_district3_1", "Susan Smith" = "teacher_6_school14_district14_1", 
                           "Dolores Lopez" = "teacher_7_school8_district8_1", "Kimberly Tafoya" = "teacher_8_school14_district14_1", 
                           "Carlos Viera" = "teacher_9_school3_district3_1", "Julie Crum" = "teacher_10_school15_district14_1", 
                           "Louisa Maestas" = "teacher_11_school15_district14_1", "Meredith Grant" = "teacher_12_school2_district2_1", 
                           "Jacey Long" = "teacher_13_school6_district6_1", "Shana Burton" = "teacher_14_school14_district14_1", 
                           "Amber Garcia" = "teacher_15_school15_district14_1", "Candice Putman" = "teacher_16_school14_district14_1", 
                           "Militza Geisel" = "teacher_17_school8_district8_1", "Jay Brady" = "teacher_18_school1_district1_1", 
                           "Aimee Feldman" = "teacher_19_school6_district6_1", "Jennifer Ryan" = "teacher_20_school1_district1_1", 
                           "Jennnifer Ryan" = "teacher_20_school1_district1_1",
                           "Lisa Lee" = "teacher_21_school1_district1_1", "Myra Skinner" = "teacher_22_school13_district13_1", 
                           "Brian Tello" = "teacher_23_school2_district2_1", "Kassandra Buras" = "teacher_24_school13_district13_1", 
                           "Emma Niiler" = "teacher_25_school2_district2_1", "Janella Hill" = "teacher_26_school1_district1_1", 
                           "Kathy Hajner" = "teacher_27_school3_district3_1", "Virginia Gallegos" = "teacher_28_school12_district12_1", 
                           "Cassie Hobbs" = "teacher_29_school7_district7_1", "Tamara Gaudet" = "teacher_30_school4_district2_1", 
                           "Mia Trujillo" = "teacher_31_school6_district6_1", "Linda Sanchez" = "teacher_32_school11_district11_1", 
                           "Alan French" = "teacher_33_school7_district7_1",
                           "John Thomson" = "teacher_35_school5_district5_1", "Clara Ivonne Sainvilmar" = "teacher_36_school8_district8_1",
                           "Clara Ivonne" = "teacher_36_school8_district8_1", 
                           "Mickey Click" = "teacher_37_school7_district7_1", "Mary Jane Bentacu" = "teacher_38_school7_district7_1",
                           "Mary Jane Click" = "teacher_38_school7_district7_1", 
                           "Mary Jane" = "teacher_38_school7_district7_1", 
                           "Theresa Ambrogi" = "teacher_40_school4_district2_1", 
                           "Karla Gade" = "teacher_41_school10_district2_1", "Brian Hobbs" = "teacher_42_school7_district7_1", 
                           "Vanessa Gonzales" = "teacher_43_school6_district6_1", "Cathie Hephner" = "teacher_44_school6_district6_1", 
                           "Stephanie Grande" = "teacher_45_school6_district6_1", "Brock Walton" = "teacher_46_school6_district6_1", 
                           "Maggie Longwill" = "teacher_47_school6_district6_1", "Robby Armijo" = "teacher_48_school6_district6_1", 
                           "Robby Armjio" = "teacher_48_school6_district6_1", 
                           "Kristina Smith" = "teacher_49_school8_district8_1", "Krystle Winklepleck" = "teacher_50_school11_district11_1", 
                           "Carolyn Aragon" = "teacher_51_school6_district6_1", "Diana Martinez" = "teacher_52_school6_district6_1", 
                           "Joleene Starr" = "teacher_53_school6_district6_1", "Shelby Padilla" = "teacher_54_school6_district6_1", 
                           "Patricia Resendiz" = "teacher_55_school3_district3_1", "Sue Holland" = "teacher_56_school6_district6_1", 
                           "Phihoang Nelson" = "teacher_57_school5_district5_1", "Ronda Davis" = "teacher_58_school10_district2_1", 
                           "Elana Sobol" = "teacher_59_school5_district5_1", "Julia Geffroy" = "teacher_60_school9_district9_1", 
                           "Sydney Main" = "teacher_61_school6_district6_1", "Thomas Barksdale" = "teacher_62_school6_district6_1", 
                           "Tommy Barksdale" = "teacher_62_school6_district6_1", 
                           "Jamie Hephner" = "teacher_63_school6_district6_1", "Vanessa Horner" = "teacher_64_school6_district6_1", 
                           "Ambrosita Sintas" = "teacher_65_school6_district6_1", "Stephanie Becker" = "teacher_66_school5_district5_1", 
                           "Stefanie Ware" = "teacher_67_school6_district6_1", "Linda Ortiz" = "teacher_68_school6_district6_1", 
                           "Lopez N. James" = "teacher_69_school2_district2_1", "Rosella Estorque" = "teacher_70_school7_district7_1", 
                           "Wanda Henson" = "teacher_71_school6_district6_1", "Denise Taylor" = "teacher_72_school7_district7_1", 
                           "Dana Vallejos" = "teacher_73_school2_district2_1", "Kathleen Loudermilk" = "teacher_74_school5_district5_1", 
                           "Jeannie Ryan" = "teacher_75_school5_district5_1", "Jennifer Sears" = "teacher_76_school4_district2_1", 
                           "Javier Viera" = "teacher_77_school3_district3_1", "Lynne McDonald" = "teacher_78_school1_district1_1", 
                           "Ortiz-Nuria Mingorance" = "teacher_79_school17_district15_1", "Nuria Ortiz-Mingorance" = "teacher_79_school17_district15_1",
                           "Meza-Julio Quezada" = "teacher_80_school17_district15_1", "Eva Ornelas" = "teacher_81_school17_district15_1", 
                           "Cynthia Toledo" = "teacher_83_school17_district15_1", 
                           "Gina Gonzalez" = "teacher_84_school17_district15_1", "Melissa Maestas" = "teacher_85_school17_district15_1", 
                           "Araceli Gutierrez" = "teacher_86_school17_district15_1", "Leos Rafael Gonzalez" = "teacher_87_school17_district15_1",
                           "Alicia Garcia" = "teacher_88_school17_district15_1", "Ana Vazquez" = "teacher_89_school17_district15_1", 
                           "Elena Zuniga" = "teacher_90_school17_district15_1", "Jessica Carrillo" = "teacher_91_school17_district15_1", 
                           "Joel Sandoval" = "teacher_92_school17_district15_1", "Julio Quezada" = "teacher_93_school17_district15_1", 
                           "Lynda Martinez" = "teacher_94_school17_district15_1", "Rafael Gonzalez" = "teacher_95_school17_district15_1", 
                           "Sandoval Fatima" = "teacher_96_school17_district15_1", "Sandra Martinez" = "teacher_97_school17_district15_1", 
                           "Sandra Orozco" = "teacher_98_school17_district15_1", "Tafoya-Kim Perez" = "teacher_99_school15_district14_1", 
                           "Kim Tafoya-Perez" = "teacher_99_school15_district14_1",
                           "Virginia Hernandez" = "teacher_100_school17_district15_1", "Robert Torrez" = "teacher_101_school3_district3_1",
                           "Victoria Cardona" = "teacher_102_school9_district9_1",
                           "Amy Rendon" = "teacher_103_school9_district9_1",
                           "Shea Jespersen" = "teacher_104_school15_district14_1",
                           "Judy Hogg" = "teacher_105_school15_district14_1",
                           "Christina Mancha-Alvarez" = "teacher_106_school11_district11_1",
                           "Victoria Kelley" = "teacher_107_school6_district6_1",
                           "Melissa Valenzuela" = "teacher_108_school3_district3_1",
                           "Victoria Swanson" = "teacher_109_school3_district3_1",
                           "Stacy Solorzano" = "teacher_110_school3_district3_1",
                           "Amanda Saiz" = "teacher_111_school3_district3_1",
                           "Jacob Omlor" = "teacher_112_school3_district3_1",
                           "Jennifer Nilvo" = "teacher_113_school3_district3_1",
                           "Mike Hinds" = "teacher_114_school3_district3_1",
                           "Joanna Hernandez" = "teacher_115_school3_district3_1",
                           "Lorena Herrera" = "teacher_116_school3_district3_1",
                           "Nicole Gardner" = "teacher_117_school3_district3_1",
                           "Dolores Gabaldon" = "teacher_118_school3_district3_1",
                           "Joann Fernandez" = "teacher_119_school3_district3_1",
                           "Lillian Cordova" = "teacher_120_school3_district3_1",
                           "Alma Castillo" = "teacher_121_school3_district3_1",
                           "Tammy Brandt" = "teacher_122_school3_district3_1",
                           "Robyn Albani" = "teacher_123_school3_district3_1",
                           "Brenda Alberts" = "teacher_124_school3_district3_1",
                           "Cristian Campo-Hernandez" = "teacher_125_school17_district17_1",
                           "Toni Chavez Gomez" = "teacher_82_school17_district15_1", 
                           "Ana Fernandez-Garcia" = "teacher_2_school17_district15_1",
                           "Rafael Leos-Gonzalez" = "teacher_127_school17_district17_1", 
                           "Gina Young" = "teacher_84_school17_district15_1",
                           "Gina Gonzalez Young" = "teacher_84_school17_district15_1",
                           "Brian Mahieu" = "teacher_128_school17_district17_1", 
                           "Julio Meza-Quezada" = "teacher_80_school17_district15_1",
                           "Anthony Romero" = "teacher_129_school16_district14_1", 
                           "Stephanie Del Angel" = "teacher_130_school7_district7_1",
                           "Toni Chavez" = "teacher_82_school17_district15_1"
)

school_replacement_vector <- c(
  "Clovis High School" = "school1", "Taylor Middle School" = "school2", "School of Dreams Academy" = "school3", "Curriculum and Instruction" = "school4", "Amy Biehl Charter High School" = "school5", "Raton Intermediate School" = "school6", "Mesa Middle School" = "school7", "Sandoval Academy of Bilingual Education" = "school8", "Peñasco Middle and High School" = "school9", "District Office/District Resource Teachers" = "school10", "Robert F. Kennedy Charter School" = "school11", "La Academia Dolores Huerta" = "school12", "Dora Elementary" = "school13", "Forrester Elementary" = "school14", "Wilferth Elementary" = "school15", "Springer Municipal Schools" = "school16", "Christine Duncan Heritage Academy" = "school17"
)

# Start at 85 because 82 teacher responses to pre-survey and 2 spanish responses

unidentified_sheet <- sheet_identified %>%
  mutate(`Anonymized ID 1` = str_replace_all(`Participant Name`, replace_all)) %>%
  mutate(`School ID` = str_replace_all(`School Name`, school_replacement_vector)) %>%
  mutate(`School ID` = str_remove_all(`School ID`, "\\(Local Charter\\)"))

no_replace <- unidentified_sheet %>% filter(!str_detect(`Anonymized ID 1`, "teacher|admin"))

unidentified_sheet %>% select(`Anonymized ID 1`) %>% range_write(ss = "https://docs.google.com/spreadsheets/d/1B_AIE4-WjNfY9ytZ9KdMozYhV44hVJCR2mzNAZ8YqMM/edit#gid=0",
                                                     range = "B2:B132", reformat = F, col_names = F, sheet = 1)

unidentified_sheet %>% select(`School ID`) %>% range_write(ss = "https://docs.google.com/spreadsheets/d/1B_AIE4-WjNfY9ytZ9KdMozYhV44hVJCR2mzNAZ8YqMM/edit#gid=0",
                                                                 range = "L2:L132", reformat = F, col_names = F, sheet = 1)

### CHANGE SHAWN MORRIS TO A DIFFERENT ID IN THE PRE-ADMIN SURVEY - teacher1_school1_district11_2


