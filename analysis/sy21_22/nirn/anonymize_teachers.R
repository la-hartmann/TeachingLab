library(readxl)
library(tidyverse)

df <- readxl::read_xlsx("data/external/nirn/1FAIpQLSeTzsS8rQ9-Qc0rSVdvypbuYmQ20I3_W2BECe-mNgpETB9MOw_-MvJBSS3F0DN2dDUW14i_981897102_Final.TeacherMathClassStudentsData2020-21 (003).xlsx")

df_with_ids <- df %>% 
  dplyr::group_by(`Teacher Name`) %>%
  dplyr::mutate(id = as.character(TeachingLab::password_generator(length = 8))) %>%
  view()

df_with_ids %>% 
  dplyr::select(`ID #`, `Teacher Name`, Universal_ID, id) %>%
  dplyr::ungroup() %>%
  distinct(id, .keep_all = T) %>%
  readr::write_rds("data/external/nirn/nirn_ids.rds")

df_with_ids %>% 
  dplyr::select(`ID #`, `Teacher Name`, Universal_ID, id) %>%
  dplyr::ungroup() %>%
  distinct(id, .keep_all = T) %>%
  readr::write_csv("data/external/nirn/nirn_ids.csv")

deidentified_df <- df_with_ids %>%
  dplyr::ungroup() %>%
  dplyr::select(-Universal_ID, -`ID #`, -`Teacher Name`)

deidentified_df %>%
  readr::write_csv("data/external/nirn/nirn_deidentified.csv")
