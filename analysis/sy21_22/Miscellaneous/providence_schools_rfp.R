# library(tidyverse)
# library(surveymonkey)
# library(TeachingLab)
# library(gt)
# 
# options(sm_oauth_token = "BjVjlV9MiVBgfe1XpS2xPS547c6gkAygKWAgm4Vv539-KbFct5lsqyVGRCZun0GDt21lnJrgn9hDvSjF.KybF58vc.P.jdeKJ8A2UEUHnE2.50e0lp.86EmQmy8-y9tm")
# session_survey <- surveymonkey::fetch_survey_obj(id = 308115193) %>%
#   surveymonkey::parse_survey() %>%
#   dplyr::mutate(date_created = lubridate::date(date_created)) %>%
#   dplyr::mutate(`Select your course.` = dplyr::coalesce(`Select your course.`, `Select your course._2`, `Select your course._3`,
#                                                         `Select your course._4`, `Select your course._5`, `Select your course._6`)) %>%
#   dplyr::mutate(Date = lubridate::mdy(`Select the date for this session. - \n    Date / Time\n`)) %>%
#   # Fix this cluttering of names the others result in a bunch of differernt formats
#   dplyr::mutate(dplyr::across(c("Select the name of your facilitator.", "Select the name of your facilitator. - Other (please specify)",
#                                 "Select the name of your facilitator._2", "Select the name of your facilitator. - Other (please specify)_2",
#                                 "Select the name of your facilitator._3", "Select the name of your facilitator. - Other (please specify)_3"), ~ na_if(.x, "Name"))) %>%
#   dplyr::mutate(Facilitator = dplyr::coalesce(`Select the name of your facilitator.`,
#                                               `Select the name of your facilitator._2`,
#                                               `Select the name of your facilitator._3`,
#                                               `Select the name of your facilitator. - Other (please specify)_3`,
#                                               `Select the name of your facilitator. - Other (please specify)_2`,
#                                               `Select the name of your facilitator. - Other (please specify)_3`),
#                 Facilitation_Feedback = dplyr::coalesce(`What additional feedback do you have about their facilitation skills?`,
#                                                         `What additional feedback do you have about their facilitation skills?_2`,
#                                                         `What additional feedback do you have about their facilitation skills?_3`)) %>%
#   dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.` =
#                   dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.`,
#                                   `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._2`,
#                                   `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._3`)) %>%
#   dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.` =
#                   dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.`,
#                                   `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._2`,
#                                   `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._3`)) %>%
#   dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.` =
#                   dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.`,
#                                   `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._2`,
#                                   `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._3`)) %>%
#   dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.` =
#                   dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.`,
#                                   `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._2`,
#                                   `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._3`)) %>%
#   dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.` =
#                   dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.`,
#                                   `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._2`,
#                                   `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._3`))
# 
# readr::write_rds(session_survey, "data-raw/session_surveymonkey.rds")
# readr::write_rds(session_survey, here::here("dashboards/SessionSurvey/Data/session_surveymonkey.rds"))
# 
# # Column names
# 
# column_names <- colnames(session_survey) %>%
#   as_tibble()
# 
# # Only 16 survey responses
# clev_brown_surveys <- session_survey %>%
#   dplyr::filter(`Select your site (district, parish, network, or school).` %in% c("Brownington Central School, VT",
#                                                                                   "Cleveland Metropolitan School District, OH")) #%>%
# # This yields no responses so not including
#   # dplyr::filter(`Select your course.` %in% c("EngageNY K-5: Cycle of Inquiry I: Eliciting Student Thinking", 
#   #                                            "Math Curriculum Flexible: Bootcamp", "IM K-5 Bootcamp"))
# 
# #### Graph #####
# 
# data_plot_agree <- clev_brown_surveys %>%
#   select(c(
#   "How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.",
#   "How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.",
#   "How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.",
#   "How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.",
#   "How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs."
# )) %>%
#   pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
#   drop_na() %>%
#   dplyr::mutate(Question = str_remove_all(
#     Question,
#     "How much do you agree with the following statements about this facilitator today\\? - "
#   )) %>%
#   group_by(Question, Response) %>%
#   count() %>%
#   ungroup() %>%
#   group_by(Question) %>%
#   mutate(Question = str_wrap(Question, width = 30)) %>%
#   summarise(
#     n = n,
#     Response = Response,
#     Percent = n / sum(n) * 100
#   )
# library(ggtext)
# ggplot(data = data_plot_agree, aes(x = Question, y = Percent, fill = factor(Response))) +
#   geom_col() +
#   geom_text(aes(label = if_else(Percent >= 3, paste0(round(Percent), "%"), "")), 
#             position = position_stack(vjust = 0.5),
#             size = 5) +
#   scale_fill_manual(values = c(
#     "(1) Strongly disagree" = "#040404", "(2) Disagree" = "#032E3F",
#     "(3) Neither agree nor disagree" = "#02587A", "(4) Agree" = "#0182B4", "(5) Strongly agree" = "#00ACF0"
#   )) +
#   labs(
#     fill = "", title = "Percent that Agree/Strongly Agree with Each of the Following Statements",
#     x = "", y = "",
#     subtitle = "<i>*Math responses only</i>"
#   ) +
#   coord_flip() +
#   guides(fill = guide_legend(reverse = T)) +
#   scale_y_continuous(labels = scales::percent_format(scale = 1)) +
#   theme_tl(legend = T) +
#   theme(
#     axis.text.x = element_blank(),
#     axis.text.y = element_text(lineheight = 1.1, size = 17),
#     legend.position = "bottom",
#     plot.title = element_text(lineheight = 1.1, size = 20, face = "bold"),
#     plot.subtitle = element_markdown(hjust = 0.5),
#     legend.key.size = unit(1.25, "cm"),
#     legend.text = element_text(size = 12)
#   )
# 
# ggsave(here::here("Images/2021-2022/providence_rfp/plot.png"), width = 12.5, height = 9)
# #### Table ####
# 
# just_text <- clev_brown_surveys %>% 
#   select(`Facilitation_Feedback`, `What went well in today’s session?`, `What could have been better about today’s session?`) %>%
#   pivot_longer(everything(), names_to = "Type", values_to = "Feedback") %>%
#   drop_na() %>%
#   dplyr::filter(Type != "What could have been better about today’s session?") %>%
#   slice(-c(1:5)) %>%
#   slice(-2) %>%
#   dplyr::filter(!str_detect(Feedback, "Mandi|unboxing"))
# 
# devtools::load_all()
# 
# # highlight = c("enjoyed", "module", "Modules")
# highlight = "fake"
# quote_viz(just_text, "Feedback", 
#           viz_type = "gt",
#           custom_highlight = T, 
#           title = "Feedback from Brownington Central School<br>and Cleveland Metropolitan") %>%
#   gt::tab_source_note(html("* <i>Math Responses Only</i>")) %>%
#   gt::gtsave(here::here("Images/2021-2022/providence_rfp/quotes.png"))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
