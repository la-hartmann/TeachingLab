
# ipg_forms_test <- readr::read_rds(here::here("dashboards/IPGBoard/data/ipg_data.rds"))
# 
# sort(colnames(ipg_forms_test))[c(1:5, 8:33, 45:48, 53, 57:60, 67:68)] -> ipg_plot_select_names
# 
# ipg_plot_select_names %>%
#   readr::write_rds(here::here("dashboards/IPGBoard/data/ipg_plot_select_names.rds"))
# 
# 
# sort(colnames(ipg_forms_test))[c(6:7, 34:36, 54:55, 62)] -> ipg_text_select_names
# 
# ipg_text_select_names %>%
#   readr::write_rds(here::here("dashboards/IPGBoard/data/ipg_text_select_names.rds"))

rename_ipg <- function(x) {
  x %>%
    str_remove_all("....[:digit:][:digit:][:digit:]") %>%
    str_remove_all("....[:digit:][:digit:]") %>%
    str_remove_all("....[:digit:]") %>%
    str_replace_all(c("CA1\\.A" = "CA1a\\.",
                      "CA\\.2\\.A" = "CA2a\\.",
                      "CA\\.2\\.B" = "CA2b\\.",
                      "CA\\.2\\.C" = "CA2c\\.",
                      "CA\\.2\\.D" = "CA2d\\.",
                      "CA\\.2\\.E" = "CA2e\\.",
                      "CA\\.3\\.A" = "CA3a\\.",
                      "CA\\.3\\.B" = "CA3b\\.",
                      "CA\\.3\\.C" = "CA3c\\.",
                      "\\. \\." = "\\."
    ))
}

ipg_forms <- TeachingLab::get_ipg_forms() |>
  janitor::remove_empty("cols")

ipg_forms_clean <- ipg_forms |>
  pivot_longer(!1:11) |>
  dplyr::mutate(name = rename_ipg(name)) |>
  pivot_wider(!1:11) |>
  dplyr::mutate(`CA1a. The daily lesson accurately uses a sounds-first sequence to address grade-level standards as defined by high-quality instructional materials (HQIM) and is situated clearly within a systematic scope and sequence of foundational skill development within HQIM.` = dplyr::coalesce(`CA1a. The daily lesson accurately uses a sounds-first sequence to address grade-level standards as defined by high-quality instructional materials (HQIM) and is situated clearly within a systematic scope and sequence of foundational skill development within HQIM.`,
                                                                                                                                                                                                                                                                                                            `CA1a. The daily lesson accurately uses a sounds-first sequence to address grade-level standards as defined by HQIM and is situated clearly within a systematic scope and sequence of foundational skills development within HQIM.`),
                `CA2a. Questions and tasks address the text by attending to its particular qualitative features: its meaning/purpose and/or language, structure(s), or knowledge demands` = dplyr::coalesce(`CA2a. Questions and tasks address the text by attending to its particular qualitative features: its meaning/purpose and/or language, structure(s), or knowledge demands`,
                                                                                                                                                                                                            `CA2a.`),
                `CA2b. Questions and tasks require students to use evidence from the text to demonstrate understanding and to support their ideas about the text. These ideas are expressed through written and/or oral responses.` = dplyr::coalesce(`CA2b. Questions and tasks require students to use evidence from the text to demonstrate understanding and to support their ideas about the text. These ideas are expressed through written and/or oral responses.`,
                                                                                                                                                                                                                                                      `CA2b. Questions and tasks require students to use evidence from the text to demonstrate understanding and to support their ideas about the text`),
                `CA2e. The teacher executes a lesson that provides students with opportunities to connect foundational skills to making meaning from listening and/or reading and through speaking and/or writing.` = dplyr::coalesce(`CA2e. The teacher executes a lesson that provides students with opportunities to connect foundational skills to making meaning from listening and/or reading and through speaking and/or writing.`,
                                                                                                                                                                                                                                      `CA2e. The teacher executes a lesson that provides students with opportunities to connect foundational skills to making meaning from listening and/or reading and through speaking and/or writing`),
                `CA3d. The teacher creates the conditions for student conversations where students are encouraged to talk about each other’s thinking; Students talk and ask questions about each other’s thinking, in order to clarify or improve their own mathematical understanding` = dplyr::coalesce(`CA3d. The teacher creates the conditions for student conversations where students are encouraged to talk about each other’s thinking; Students talk and ask questions about each other’s thinking, in order to clarify or improve their own mathematical understanding`,
                                                                                                                                                                                                                                                                                                           `CA3d. The teacher creates the conditions for student conversations where students are encouraged to talk about each other’s thinking; Students talk and ask questions about each other’s thinking, in order to clarify or improve their understanding`)) |>
  dplyr::select(-c(`CA1a. The daily lesson accurately uses a sounds-first sequence to address grade-level standards as defined by HQIM and is situated clearly within a systematic scope and sequence of foundational skills development within HQIM.`,
                   `CA2a.`,
                   `CA2b. Questions and tasks require students to use evidence from the text to demonstrate understanding and to support their ideas about the text`,
                   `CA2e. The teacher executes a lesson that provides students with opportunities to connect foundational skills to making meaning from listening and/or reading and through speaking and/or writing`,
                   `CA3d. The teacher creates the conditions for student conversations where students are encouraged to talk about each other’s thinking; Students talk and ask questions about each other’s thinking, in order to clarify or improve their understanding`)) |>
  dplyr::mutate(`Timeline of Obs` = ifelse(is.na(`Timeline of Obs`),
                                           paste0(
                                             get_season(as.POSIXct(`Timestamp`)),
                                             " ",
                                             lubridate::year(as.POSIXct(`Timestamp`))
                                           ),
                                           `Timeline of Obs`),
                across(everything(), ~ str_replace_all(.x, "not observed", "Not observed")))

ipg_names <- colnames(ipg_forms)

ipg_names %>% 
  str_remove_all("....[:digit:][:digit:][:digit:]") %>%
  str_remove_all("....[:digit:][:digit:]") %>%
  str_remove_all("....[:digit:]") %>%
  str_replace_all(c("CA1\\.A" = "CA1a\\.",
                    "CA\\.2\\.A" = "CA2a\\.",
                    "CA\\.2\\.B" = "CA2b\\.",
                    "CA\\.2\\.C" = "CA2c\\.",
                    "CA\\.2\\.D" = "CA2d\\.",
                    "CA\\.2\\.E" = "CA2e\\.",
                    "CA\\.3\\.A" = "CA3a\\.",
                    "CA\\.3\\.B" = "CA3b\\.",
                    "CA\\.3\\.C" = "CA3c\\.",
                    "\\. \\." = "\\."
                    )) %>%
  as_tibble() %>%
  distinct(value) %>%
  arrange(value) %>%
  view()

ca1_questions <- ipg_names %>%
  keep( ~ str_detect(.x, "CA1")) %>%
  str_remove_all("....[:digit:][:digit:][:digit:]") %>%
  str_remove_all("....[:digit:][:digit:]") %>%
  str_remove_all("....[:digit:]") %>%
  unique()

ca2_questions <- ipg_names %>%
  keep( ~ str_detect(.x, "CA2")) %>%
  str_remove_all("....[:digit:][:digit:][:digit:]") %>%
  str_remove_all("....[:digit:][:digit:]") %>%
  str_remove_all("....[:digit:]") %>%
  unique()

ca3_questions <- ipg_names %>%
  keep( ~ str_detect(.x, "CA3")) %>%
  str_remove_all("....[:digit:][:digit:][:digit:]") %>%
  str_remove_all("....[:digit:][:digit:]") %>%
  str_remove_all("....[:digit:]") %>%
  unique()



