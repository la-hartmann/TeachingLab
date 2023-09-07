library(qualtRics)
library(tidyverse)

session_survey <- participant_feedback |>
  dplyr::filter(Finished == TRUE & last_session_or_not == "Yes - there will be more sessions for this PL course or coaching cycle.")

### Get second facilitator responses ###
second_fac <- session_survey |>
  select(
    `They demonstrated deep knowledge of the content they facilitated` = Q12_1,
    `They facilitated the content clearly` = Q12_2,
    `They effectively built a safe learning community` = Q12_3,
    `They were fully prepared for the session` = Q12_4,
    `They responded to the group’s needs` = Q12_5
  )
### Get first facilitator combined with second agree per question for end of session survey ###
plot_agree <- session_survey |>
  select(
    `They demonstrated deep knowledge of the content they facilitated` = Q8_1,
    `They facilitated the content clearly` = Q8_2,
    `They effectively built a safe learning community` = Q8_3,
    `They were fully prepared for the session` = Q8_4,
    `They responded to the group’s needs` = Q8_5
  ) |>
  bind_rows(second_fac) |>
  pivot_longer(everything(), names_to = "Question", values_to = "Response") |>
  drop_na() |>
  group_by(Question, Response) |>
  count() |>
  ungroup() |>
  group_by(Question) |>
  mutate(Question = str_wrap(Question, width = 25)) |>
  summarise(
    n = n,
    Response = Response,
    Percent = n / sum(n) * 100
  )

### Calculate n size ###
n_size_agree <- session_survey |>
  count(sort = T)

### Make ggplot of session survey agree percent ###
plot_agree |>
  group_by(Question, Response) |>
  summarise(Percent = weighted.mean(Percent, n)) |>
  mutate(Question = factor(Question)) |>
  ggplot(aes(x = Question, y = Percent, fill = factor(Response))) +
  geom_col(color = NA, width = 0.95, position = position_stack(reverse = TRUE)) +
  geom_text(aes(
    label = if_else(Percent >= 10, paste0(round(Percent), "%"), ""),
    color = Response
  ),
  size = 6.5,
  position = position_stack(vjust = 0.5, reverse = TRUE),
  fontface = "bold"
  ) +
  scale_fill_manual(values = c(
    "(1) Strongly disagree" = "#040404", "(2) Disagree" = "#032E3F",
    "(3) Neither agree nor disagree" = "#02587A", "(4) Agree" = "#0182B4", "(5) Strongly agree" = "#00ACF0"
  )) +
  scale_color_manual(values = c(
    "(1) Strongly disagree" = "white", "(2) Disagree" = "white",
    "(3) Neither agree nor disagree" = "black", "(4) Agree" = "black", "(5) Strongly agree" = "black"
  )) +
  labs(
    fill = "", title = glue::glue("Participant Perceptions of Course Facilitation 2022-2023 (n = {format(sum(n_size_agree$n), big.mark = ',')})"),
    x = "", y = ""
  ) +
  coord_flip() +
  guides(
    fill = guide_legend(),
    color = "none"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_tl(legend = TRUE) +
  theme(
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 15),
    plot.title = element_text(size = 30, face = "bold"),
    legend.text = element_text(size = 19),
    legend.position = "bottom"
  )

ggsave(here::here("images/sy22_23/ma_dese_rfr/facilitator_scores.png"),
       bg = "white",
       dpi = 500,
       units = "px",
       width = 1920*5,
       height = 1080*5)

course_survey <- participant_feedback |>
  dplyr::filter(Finished == TRUE & last_session_or_not == "No - this was the final session for this PL course (e.g., last day of Bootcamp, close of Inquiry cycle) or coaching.")

plot_agree <- course_survey |>
  select(
    `I looked forward to attending this PL` = course_feedback_1,
    `I was fully present/\"minds-on\" during these PL sessions` = course_feedback_2,
    `The activities were well-designed to help me meet the learning targets` = Q33_3,
    `I am satisfied with how the course was facilitated` = Q33_4,
    `This PL was a good use of my time` = Q33_5,
    `I talk to other teachers about the things I learned in this PL` = Q33_6,
    `I felt a sense of community with the other participants in this course` = Q33_7,
    `This course was relevant to my instructional practices` = Q33_8,
    `The strategies I’ve learned in this course will improve my instruction` = Q33_9,
    `The strategies I’ve learned in this course will improve my coaching or supervision of teachers` = course_feedback_10,
    `The strategies I’ve learned in the course are easy to implement` = course_feedback_11,
    `I have applied or will apply what I have learned in this course to my practice` = course_feedback_12,
    `This course has supported me in being responsive to students' backgrounds, cultures, and points of view` = course_feedback_13,
    `I am satisfied with the overall quality of this course` = course_feedback_14 # ,
    # `I think the professional learning will have a significant positive impact on my math instruction next year` = course_feedback_15
  ) |>
  pivot_longer(everything(), names_to = "Question", values_to = "Response") |>
  drop_na() |>
  group_by(Question, Response) |>
  count() |>
  ungroup() |>
  group_by(Question) |>
  mutate(Question = str_wrap(Question, width = 60)) |>
  summarise(
    n = n,
    Response = Response,
    Percent = n / sum(n) * 100
  )

n_size_agree <- course_survey |>
  count(sort = T)

plot_agree |>
  group_by(Question, Response) |>
  summarise(Percent = weighted.mean(Percent, n)) |>
  mutate(Question = factor(Question)) |>
  ggplot(aes(x = Question, y = Percent, fill = factor(Response))) +
  geom_col(color = NA, width = 0.95, position = position_stack(reverse = TRUE)) +
  geom_text(aes(
    label = if_else(Percent >= 19, paste0(round(Percent), "%"), ""),
    color = Response
  ),
  size = 6.5,
  position = position_stack(vjust = 0.5, reverse = TRUE),
  fontface = "bold"
  ) +
  scale_fill_manual(values = c(
    "(1) Strongly disagree" = "#040404", "(2) Disagree" = "#032E3F",
    "(3) Neither agree nor disagree" = "#02587A", "(4) Agree" = "#0182B4", "(5) Strongly agree" = "#00ACF0"
  )) +
  scale_color_manual(values = c(
    "(1) Strongly disagree" = "white", "(2) Disagree" = "white",
    "(3) Neither agree nor disagree" = "black", "(4) Agree" = "black", "(5) Strongly agree" = "black"
  )) +
  labs(
    fill = "", title = glue::glue("Participant Perceptions of Course 2022-2023 (n = {format(sum(n_size_agree$n), big.mark = ',')})"),
    x = "", y = ""
  ) +
  coord_flip() +
  guides(
    fill = guide_legend(),
    color = "none"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_tl(legend = TRUE) +
  theme(
    axis.text.y = element_text(size = 21, lineheight = 0.8),
    axis.text.x = element_text(size = 15),
    plot.title = element_text(size = 30, face = "bold"),
    legend.margin = margin(-10, 0, 0, -200),
    legend.text = element_text(size = 19),
    legend.position = "bottom"
  )

ggsave(here::here("images/sy22_23/ma_dese_rfr/course_scores.png"),
       bg = "white",
       dpi = 500,
       units = "px",
       width = 1920*5,
       height = 1080*5)

session_survey_21_22 <- readr::read_rds(here::here("data/sy21_22/session_survey_21_22data.rds"))
course_survey_21_22 <- readr::read_rds(here::here("data/sy21_22/course_survey_21_22.rds"))


colnames(session_survey_21_22)
colnames(course_survey_21_22)

plot_agree <- session_survey_21_22 |>
  select(
    `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.`,
    `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.`,
    `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.`,
    `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.`,
    `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.`
  ) |>
  rename_with( ~ stringr::str_remove_all(.x, "How much do you agree with the following statements about this facilitator today\\? - ")) |>
  pivot_longer(everything(), names_to = "Question", values_to = "Response") |>
  drop_na() |>
  group_by(Question, Response) |>
  count() |>
  ungroup() |>
  group_by(Question) |>
  mutate(Question = str_wrap(Question, width = 25)) |>
  summarise(
    n = n,
    Response = Response,
    Percent = n / sum(n) * 100
  )

### Calculate n size ###
n_size_agree <- session_survey_21_22 |>
  count(sort = T)

### Make ggplot of session survey agree percent ###
plot_agree |>
  group_by(Question, Response) |>
  summarise(Percent = weighted.mean(Percent, n)) |>
  mutate(Question = factor(Question)) |>
  ggplot(aes(x = Question, y = Percent, fill = factor(Response))) +
  geom_col(color = NA, width = 0.95, position = position_stack(reverse = TRUE)) +
  geom_text(aes(
    label = if_else(Percent >= 10, paste0(round(Percent), "%"), ""),
    color = Response
  ),
  size = 6.5,
  position = position_stack(vjust = 0.5, reverse = TRUE),
  fontface = "bold"
  ) +
  scale_fill_manual(values = c(
    "(1) Strongly disagree" = "#040404", "(2) Disagree" = "#032E3F",
    "(3) Neither agree nor disagree" = "#02587A", "(4) Agree" = "#0182B4", "(5) Strongly agree" = "#00ACF0"
  )) +
  scale_color_manual(values = c(
    "(1) Strongly disagree" = "white", "(2) Disagree" = "white",
    "(3) Neither agree nor disagree" = "black", "(4) Agree" = "black", "(5) Strongly agree" = "black"
  )) +
  labs(
    fill = "", title = glue::glue("Participant Perceptions of Course Facilitation 2021-2022 (n = {format(sum(n_size_agree$n), big.mark = ',')})"),
    x = "", y = ""
  ) +
  coord_flip() +
  guides(
    fill = guide_legend(),
    color = "none"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_tl(legend = TRUE) +
  theme(
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 15),
    plot.title = element_text(size = 30, face = "bold"),
    legend.text = element_text(size = 19),
    legend.position = "bottom"
  )

ggsave(here::here("images/sy22_23/ma_dese_rfr/facilitator_scores_21_22.png"),
       bg = "white",
       dpi = 500,
       units = "px",
       width = 1920*5,
       height = 1080*5)

plot_agree <- course_survey_21_22 |>
  select(
    `How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`,
    `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`,
    `How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.`,
    `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course.`,
    `How much do you agree with the following statements about this course? - The strategies I’ve learned in this course will improve my instruction.`,
    `How much do you agree with the following statements about this course? - The strategies I’ve learned in this course will improve my coaching or supervision of teachers.`,
    `How much do you agree with the following statements about this course? - The strategies I’ve learned in the course are easy to implement.`,
    `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice.`,
    `How much do you agree with the following statements about this course? - This course has supported me in being responsive to students' backgrounds, cultures, and points of view.`
  ) |>
  rename_with( ~ stringr::str_remove_all(.x, "How much do you agree with the following statements about this course\\? - ")) |>
  pivot_longer(everything(), names_to = "Question", values_to = "Response") |>
  drop_na() |>
  group_by(Question, Response) |>
  count() |>
  ungroup() |>
  group_by(Question) |>
  mutate(Question = str_wrap(Question, width = 60)) |>
  summarise(
    n = n,
    Response = Response,
    Percent = n / sum(n) * 100
  )

n_size_agree <- course_survey_21_22 |>
  count(sort = T)

plot_agree |>
  group_by(Question, Response) |>
  summarise(Percent = weighted.mean(Percent, n)) |>
  mutate(Question = factor(Question)) |>
  ggplot(aes(x = Question, y = Percent, fill = factor(Response))) +
  geom_col(color = NA, width = 0.95, position = position_stack(reverse = TRUE)) +
  geom_text(aes(
    label = if_else(Percent >= 19, paste0(round(Percent), "%"), ""),
    color = Response
  ),
  size = 6.5,
  position = position_stack(vjust = 0.5, reverse = TRUE),
  fontface = "bold"
  ) +
  scale_fill_manual(values = c(
    "(1) Strongly disagree" = "#040404", "(2) Disagree" = "#032E3F",
    "(3) Neither agree nor disagree" = "#02587A", "(4) Agree" = "#0182B4", "(5) Strongly agree" = "#00ACF0"
  )) +
  scale_color_manual(values = c(
    "(1) Strongly disagree" = "white", "(2) Disagree" = "white",
    "(3) Neither agree nor disagree" = "black", "(4) Agree" = "black", "(5) Strongly agree" = "black"
  )) +
  labs(
    fill = "", title = glue::glue("Participant Perceptions of Course 2021-2022 (n = {format(sum(n_size_agree$n), big.mark = ',')})"),
    x = "", y = ""
  ) +
  coord_flip() +
  guides(
    fill = guide_legend(),
    color = "none"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_tl(legend = TRUE) +
  theme(
    axis.text.y = element_text(size = 21, lineheight = 0.8),
    axis.text.x = element_text(size = 15),
    plot.title = element_text(size = 30, face = "bold"),
    legend.margin = margin(-10, 0, 0, -200),
    legend.text = element_text(size = 19),
    legend.position = "bottom"
  )

ggsave(here::here("images/sy22_23/ma_dese_rfr/course_scores_21_22.png"),
       bg = "white",
       dpi = 500,
       units = "px",
       width = 1920*5,
       height = 1080*5)
