library(readxl)
library(tidyverse)

persuasive_writing_1 <- read_xlsx("data/sy23_24/Scored Pre-Test Persuasive Writing A Gr 7-10.xlsx")
persuasive_writing_2 <- read_xlsx("data/sy23_24/Scored Post-Test Persuasive Writing B Gr 7-10.xlsx")

### If there is no student id match then delete ###
# persuasive_writing_1$`Student ID`[which(persuasive_writing_1$`Student ID` |> duplicated())]
# persuasive_writing_1 |>
#   group_by(`Student ID`) |>
#   count(sort = T) |>
#   filter(n >= 2) |>
#   pull(`Student ID`) |>
#   clipr::write_clip()
# 
# persuasive_writing_2$`Student ID`[which(persuasive_writing_2$`Student ID` |> duplicated())]
# persuasive_writing_2 |>
#   group_by(`Student ID`) |>
#   count(sort = T) |>
#   filter(n >= 2) |>
#   pull(`Student ID`) |>
#   clipr::write_clip()

create_single_file <- function(file1, file2, output_file) {
  
  persuasive_writing_1 <- readxl::read_xlsx(paste0("data/sy23_24/", file1))
  persuasive_writing_2 <- readxl::read_xlsx(paste0("data/sy23_24/", file2))
  
  all_ids_list <- c(unique(persuasive_writing_1$`Student ID`), unique(persuasive_writing_2$`Student ID`))
  
  final_id_list <- unique(persuasive_writing_1$`Student ID`)[which(unique(persuasive_writing_1$`Student ID`) %in% unique(persuasive_writing_2$`Student ID`))]
  
  final_persuasive_writing_1 <- persuasive_writing_1 |>
    filter(`Student ID` %in% final_id_list)
  
  final_persuasive_writing_2 <- persuasive_writing_2 |>
    filter(`Student ID` %in% final_id_list) |>
    select(-c(1:4)) |>
    rename_with(~ paste0("Post ", .x),
                !matches(" ID|Grade"))
  
  final_persuasive_writing_1 |>
    left_join(final_persuasive_writing_2, by = c("Student ID"),
              multiple = "all") |>
    filter(!is.na(`Student ID`)) |>
    group_by(`Student ID`) |>
    mutate(n = n(),
           error_exists = !is.na(ERROR_MESSAGE)) |>
    arrange(desc(n), error_exists) |>
    select(-c(n, error_exists)) |>
    openxlsx::write.xlsx(file = paste0("data/sy23_24/", output_file))
  
}


create_single_file(file1 = "Scored Pre-Test Persuasive Writing A Gr 7-10.xlsx",
                   file2 = "Scored Post-Test Persuasive Writing B Gr 7-10.xlsx",
                   output_file = "Scored Pre-Post Persuasive Writing Gr 7-10.xlsx")


create_single_file(file1 = "Scored Pre-Test Persuasive Writing B Gr 7-10.xlsx",
                   file2 = "Scored Post-Test Persuasive Writing A Gr 7-10.xlsx",
                   output_file = "Pair 2 Combined Persuasive Writing Gr 7-10.xlsx")


create_single_file(file1 = "Scored Pre-Test Informational Writing A Gr 7-10.xlsx",
                   file2 = "Scored Post-Test Informational Writing B Gr 7-10.xlsx",
                   output_file = "Pair 3 Combined Informational Writing Gr 7-10.xlsx")


create_single_file(file1 = "Scored Pre-Test Informational Writing B Gr 7-10.xlsx",
                   file2 = "Scored Post-Test Informational Writing A Gr 7-10.xlsx",
                   output_file = "Pair 4 Combined Informational Writing Gr 7-10.xlsx")



