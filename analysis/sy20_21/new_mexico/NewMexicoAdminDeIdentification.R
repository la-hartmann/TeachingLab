library(tidyverse)
library(googledrive)
library(googlesheets4)
# Read in google sheet
admin_data <- read_sheet("https://docs.google.com/spreadsheets/d/1V-tnR9CtI0E8WB8tZjNo1NL7CR8khFB2t6BjTGkGL-A/edit#gid=0", 
                         range = "Admin Responses!D1:GN25") %>%
  select(1:189)
# Admin replacement vector
colnames(admin_data) <- c("column_1", "column_2", "column_3", "column_4", "column_5", 
                          "column_6", "column_7", "column_8", "column_9", "column_10", 
                          "column_11", "column_12", "column_13", "column_14", "column_15", 
                          "column_16", "column_17", "column_18", "column_19", "column_20", 
                          "column_21", "column_22", "column_23", "column_24", "column_25", 
                          "column_26", "column_27", "column_28", "column_29", "column_30", 
                          "column_31", "column_32", "column_33", "column_34", "column_35", 
                          "column_36", "column_37", "column_38", "column_39", "column_40", 
                          "column_41", "column_42", "column_43", "column_44", "column_45", 
                          "column_46", "column_47", "column_48", "column_49", "column_50", 
                          "column_51", "column_52", "column_53", "column_54", "column_55", 
                          "column_56", "column_57", "column_58", "column_59", "column_60", 
                          "column_61", "column_62", "column_63", "column_64", "column_65", 
                          "column_66", "column_67", "column_68", "column_69", "column_70", 
                          "column_71", "column_72", "column_73", "column_74", "column_75", 
                          "column_76", "column_77", "column_78", "column_79", "column_80", 
                          "column_81", "column_82", "column_83", "column_84", "column_85", 
                          "column_86", "column_87", "column_88", "column_89", "column_90", 
                          "column_91", "column_92", "column_93", "column_94", "column_95", 
                          "column_96", "column_97", "column_98", "column_99", "column_100", 
                          "column_101", "column_102", "column_103", "column_104", "column_105", 
                          "column_106", "column_107", "column_108", "column_109", "column_110", 
                          "column_111", "column_112", "column_113", "column_114", "column_115", 
                          "column_116", "column_117", "column_118", "column_119", "column_120", 
                          "column_121", "column_122", "column_123", "column_124", "column_125", 
                          "column_126", "column_127", "column_128", "column_129", "column_130", 
                          "column_131", "column_132", "column_133", "column_134", "column_135", 
                          "column_136", "column_137", "column_138", "column_139", "column_140", 
                          "column_141", "column_142", "column_143", "column_144", "column_145", 
                          "column_146", "column_147", "column_148", "column_149", "column_150", 
                          "column_151", "column_152", "column_153", "column_154", "column_155", 
                          "column_156", "column_157", "column_158", "column_159", "column_160", 
                          "column_161", "column_162", "column_163", "column_164", "column_165", 
                          "column_166", "column_167", "column_168", "column_169", "column_170", 
                          "column_171", "column_172", "column_173", "column_174", "column_175", 
                          "column_176", "column_177", "column_178", "column_179", "column_180", 
                          "column_181", "column_182", "column_183", "column_184", "column_185", 
                          "column_186", "column_187", "column_188", "column_189")


school_replacement_vector <- c(
  "Clovis High School" = "school1", "Taylor Middle School" = "school2", "School of Dreams Academy" = "school3", "Curriculum and Instruction" = "school4", "Amy Biehl Charter High School" = "school5", "Raton Intermediate School" = "school6", "Mesa Middle School" = "school7", "Sandoval Academy of Bilingual Education" = "school8", "Peñasco Middle and High School" = "school9", "District Office/District Resource Teachers" = "school10", "Robert F. Kennedy Charter School" = "school11", "La Academia Dolores Huerta" = "school12", "Dora Elementary" = "school13", "Forrester Elementary" = "school14", "Wilferth Elementary" = "school15", "Springer Municipal Schools" = "school16", "Christine Duncan Heritage Academy" = "school17"
)

# Start at 85 because 82 teacher responses to pre-survey and 2 spanish responses
noresponse_replacement_vector <- c()

master_replace_vector <- c("Stephanie Gardner" = "admin_1_school2_district2_2",
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
                           "Gina Gonzalez Young" = "teacher_84_school17_district15_1",
                           "Brian Mahieu" = "teacher_128_school17_district17_1", 
                           "Julio Meza-Quezada" = "teacher_80_school17_district15_1",
                           "Anthony Romero" = "teacher_129_school16_district14_1", 
                           "Stephanie Del Angel" = "teacher_130_school7_district7_1",
                           "Toni Chavez" = "teacher_82_school17_district15_1"
)

# Replacing data
admin_deidentified_1 <- admin_data %>%
  select(-column_1) %>%
  mutate(`column_2` = str_replace_all(`column_2`, master_replace_vector),
         column_3 = str_replace_all(column_3, school_replacement_vector)) %>%
  mutate_all( ~ str_replace_all(., master_replace_vector)) #%>%
  # mutate_all( ~ str_replace(., " Bentacu's", "'s"))

# Writing to google sheets

if (exists(ss)) {
  googledrive::drive_trash(ss)
}
# Spreadsheet creation
ss <- gs4_create("Updated New Mexico Administrator Deidentified Data", sheets = "Deidentified Administrators/Teachers")
# Write to sheet
admin_deidentified_1 %>%
  write_sheet(ss, sheet = "Deidentified Administrators/Teachers")



