library(googlesheets4)
library(tidyverse)
library(tidytext)
library(ggwordcloud)
library(cowplot)
library(ggtext)
library(gt)

quotes2 <- TeachingLab::get_ongoing_coaching(year = "22_23") |>
  mutate(month = lubridate::month(RecordedDate, label = TRUE, abbr = FALSE)) |>
  select(
    month, coach_add_feed_1, coach_add_feed_2, coach_gone_well_1,
    coach_gone_well_2, coach_went_well, coach_activities_sup, coach_learn_excited,
    coach_add_comment
  ) |>
  pivot_longer(!month, names_to = "Question", values_to = "Comment") |>
  drop_na(Comment)

affin <- quotes2 |>
  group_by(month, Question) |>
  tidytext::unnest_tokens(word, Comment, token = "words") |>
  anti_join(stop_words)

affin2 <- affin |>
  group_by(month) |>
  summarise(word = last(word)) |>
  bind_rows(affin) |>
  inner_join(get_sentiments("afinn"), by = "word") |>
  group_by(month, word) |>
  summarize(
    n = n(),
    contribution = sum(value),
    value = unique(value)
  ) |>
  group_by(month) |>
  mutate(contr_rel = value * n / sum(n)) |>
  arrange(month, -abs(contribution)) |>
  group_by(month) %>%
  mutate(contribution_love = if_else(str_detect(word, "helpful"), 0, contr_rel)) |>
  arrange(-contribution_love) |>
  mutate(rank = row_number()) |>
  mutate(line = if_else(word == "helped" | word == "helpful", rank, NA_integer_)) |>
  ungroup() |>
  mutate(
    month = factor(month, levels = c(
      "July", "August", "September",
      "October", "November", "December",
      "January", "February", "March",
      "April", "May", "June"
    )),
    word = str_to_upper(word),
    group = case_when(
      contribution_love == 0 ~ "helpful",
      contribution_love > 0 ~ "positive",
      contribution_love < 0 ~ "negative"
    ),
    course_lab = month
  )

affin2 %>%
  ggplot(aes(month, rank,
    label = word,
    color = group,
    size = abs(contr_rel)
  )) +
  ggtext::geom_richtext(
    family = "Calibri",
    fontface = "bold",
    fill = "grey7",
    label.color = NA,
    label.padding = unit(c(.1, .15, -.1, .05), "lines"),
    label.r = unit(0.05, "lines")
  ) +
  geom_line(
    data = affin2 |> filter(str_detect(word, "HELPED|HELPFUL")),
    aes(month, rank),
    group = 1,
    inherit.aes = F,
    color = "white"
  ) +
  scale_x_discrete(
    # expand = c(.04, .03),
    position = "top",
    # breaks = 1:10,
    # labels = affin2 |> distinct(month) |> pull(month) |> sort()
  ) +
  scale_y_continuous(expand = c(.01, .01)) +
  scale_color_manual(
    values = c("white", "#c3573f", "#04ABEB"),
    guide = "none"
  ) +
  scale_radius(
    range = c(3, 12),
    guide = F
  ) +
  labs(
    title = 'TEACHING LAB COACHING IS <b style="color:white;">HELPFUL</b>,  AN ORGANIZED WORDCLOUD',
    subtitle = '<span style="font-size:15pt;"><br><br>Based on a sentiment analysis of user reviews, each "word stripe" shows the words that contributed the most<br>each month, either in a <b style="color:#04abeb;">positive</b> or in a <b style="color:#c3573f;">negative</b> way. The size of each word indicates its contribution per month</span>',
    caption = "© Teaching Lab, 2023"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(color = "grey60"),
    plot.background = element_rect(fill = "grey7", color = "grey7"),
    plot.margin = margin(30, 80, 10, 50),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    axis.text.x.top = element_text(
      color = "grey55",
      size = 8.5,
      face = "bold",
      margin = margin(b = 9), lineheight = 1
    ),
    plot.title = element_markdown(
      size = 36, family = "Calibri",
      face = "bold", hjust = .5, margin = margin(25, 0, 15, 0),
      color = "grey75"
    ),
    plot.subtitle = element_markdown(
      lineheight = .9,
      margin = margin(b = 30),
      hjust = 0.5,
      size = 20,
      face = "bold",
      color = "grey55"
    ),
    plot.caption = element_text(size = 14, margin = margin(t = 20, b = 10), color = "grey55", hjust = 0.5, face = "bold")
  )


ggsave(here::here("images/sy23_24/coach_feedback_wordcloud.png"),
  width = 23, height = 14, bg = "black"
)


quotes3 <- TeachingLab::get_session_survey(year = "22_23") |>
  mutate(month = lubridate::month(RecordedDate, label = TRUE, abbr = FALSE)) |>
  select(
    month, fac_add1, fac_add_2, 
    went_well_today, take_back_class
  ) |>
  pivot_longer(!month, names_to = "Question", values_to = "Comment") |>
  drop_na(Comment)

affin <- quotes3 |>
  group_by(month, Question) |>
  tidytext::unnest_tokens(word, Comment, token = "words") |>
  anti_join(stop_words)

affin2 <- affin |>
  group_by(month) |>
  summarise(word = last(word)) |>
  bind_rows(affin) |>
  inner_join(get_sentiments("afinn"), by = "word") |>
  group_by(month, word) |>
  summarize(
    n = n(),
    contribution = sum(value),
    value = unique(value)
  ) |>
  group_by(month) |>
  mutate(contr_rel = value * n / sum(n)) |>
  arrange(month, -abs(contribution)) |>
  group_by(month) %>%
  mutate(contribution_love = if_else(str_detect(word, "love|loved"), 0, contr_rel)) |>
  arrange(-contribution_love) |>
  mutate(rank = row_number()) |>
  mutate(line = if_else(word == "love" | word == "loved", rank, NA_integer_)) |>
  ungroup() |>
  mutate(
    month = factor(month, levels = c(
      "July", "August", "September",
      "October", "November", "December",
      "January", "February", "March",
      "April", "May", "June"
    )),
    word = str_to_upper(word),
    group = case_when(
      contribution_love == 0 ~ "helpful",
      contribution_love > 0 ~ "positive",
      contribution_love < 0 ~ "negative"
    ),
    course_lab = month,
    rank = ifelse(month %in% c("December", "August", "April", "May", "June"), rank * 2, rank),
    rank = ifelse(month %in% c("July"), rank * 3, rank)
  )

affin2 |>
  ggplot(aes(month, rank,
             label = word,
             color = group,
             size = abs(contr_rel)
  )) +
  ggtext::geom_richtext(
    family = "Calibri",
    fontface = "bold",
    fill = "grey7",
    label.color = NA,
    label.padding = unit(c(.1, .15, -.1, .05), "lines"),
    label.r = unit(0.05, "lines")
  ) +
  geom_line(
    data = affin2 |> filter(str_detect(word, "LOVED")),
    aes(month, rank),
    group = 1,
    inherit.aes = F,
    color = "white"
  ) +
  scale_x_discrete(
    expand = c(.07, .03),
    position = "top",
    # breaks = 1:10,
    # labels = affin2 |> distinct(month) |> pull(month) |> sort()
  ) +
  scale_y_continuous(expand = c(.01, .01)) +
  scale_color_manual(
    values = c("white", "#c3573f", "#04ABEB"),
    guide = "none"
  ) +
  scale_radius(
    range = c(3, 12),
    guide = F
  ) +
  labs(
    title = 'TEACHING LAB FACILITATION IS <b style="color:white;">LOVED</b>,  AN ORGANIZED WORDCLOUD',
    subtitle = '<span style="font-size:15pt;"><br><br>Based on a sentiment analysis of user reviews, each "word stripe" shows the words that contributed the most<br>each month, either in a <b style="color:#04abeb;">positive</b> or in a <b style="color:#c3573f;">negative</b> way. The size of each word indicates its contribution per month</span>',
    caption = "© Teaching Lab, 2023"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(color = "grey60"),
    plot.background = element_rect(fill = "grey7", color = "grey7"),
    plot.margin = margin(30, 80, 10, 50),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    axis.text.x.top = element_text(
      color = "grey55",
      size = 8.5,
      face = "bold",
      margin = margin(b = 9), lineheight = 1
    ),
    plot.title = element_markdown(
      size = 36, family = "Calibri",
      face = "bold", hjust = .5, margin = margin(25, 0, 15, 0),
      color = "grey75"
    ),
    plot.subtitle = element_markdown(
      lineheight = .9,
      margin = margin(b = 30),
      hjust = 0.5,
      size = 20,
      face = "bold",
      color = "grey55"
    ),
    plot.caption = element_text(size = 14, margin = margin(t = 20, b = 10), color = "grey55", hjust = 0.5, face = "bold")
  )


ggsave(here::here("images/sy23_24/fac_feedback_wordcloud.png"),
       width = 23, height = 14, bg = "black"
)


ongoing_coaching <- TeachingLab::get_ongoing_coaching(year = "22_23")

coaching_plot_agree <- ongoing_coaching |>
  mutate(month = lubridate::month(RecordedDate, label = TRUE, abbr = FALSE)) |>
  dplyr::select(
    month,
    `They demonstrated deep knowledge of the content they coach` = coach_ongoing_feed_1,
    `Their coaching is clear` = coach_ongoing_feed_2,
    `They seem fully prepared for the coaching sessions` = coach_ongoing_feed_3,
    `They effectively build a safe learning environment` = coach_ongoing_feed_4,
    `They make necessary adjustments based on my needs` = coach_ongoing_feed_5
  ) |>
  tidyr::pivot_longer(!month, names_to = "Question", values_to = "Response") |>
  tidyr::drop_na(Response) |>
  dplyr::group_by(Question, Response, month) |>
  dplyr::count(sort = T) |>
  dplyr::ungroup() |>
  dplyr::group_by(Question, month) |>
  dplyr::mutate(
    Percent = round(100 * n / sum(n), 2),
    Response = factor(Response, levels = c(
      "1 - Strongly disagree",
      "2 - Disagree",
      "3 - Neither agree nor disagree",
      "4 - Agree",
      "5 - Strongly agree"
    )),
    Question = stringr::str_wrap(Question, width = 20)
  )

monthly_coaching_plot_agree <- coaching_plot_agree |>
  dplyr::filter(Response %in% c("(4) Agree", "(5) Strongly agree",
                                "4 - Agree", "5 - Strongly agree",
                                "Agree", "Strongly agree")) |>
  group_by(Question, month) |>
  summarise(Percent = sum(Percent)) |>
  dplyr::mutate(
    month = factor(month, levels = c("September", "October", "November",
                                     "December", "January", "February",
                                     "March", "April", "May",
                                     "June")))

monthly_coaching_plot_agree |>
  ggplot2::ggplot(ggplot2::aes(
    x = month,
    y = Percent,
    color = Question,
    group = Question
  )) +
  ggplot2::geom_line(size = 3, alpha = 0.8) +
  # geomtextpath::geom_textline(aes(label = Question), 
  #                             alpha = 0.8, size = 4, fontface = "bold",
  #                             linewidth = 4, spacing = 70, hjust = 1
  #                             ) +
  # facet_wrap( ~ Question) +
  ggrepel::geom_text_repel(data = monthly_coaching_plot_agree |> dplyr::filter(month == max(month)), 
                           mapping = aes(label = Question),
                           fontface = "bold",
                           nudge_x = 2) +
  ggplot2::labs(
    x = "", y = "",
    title = glue::glue("Coaching Participant Feedback % that agree or strongly agree (n = {sum(n_size_agree$n, na.rm = T)})"),
    fill = ""
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(),
    color = "none"
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    expand = c(0.14, 0),
    limits = c(70, 100)
  ) +
  scale_color_manual(values = tl_palette(color = "blue", n = 5)) +
  theme_tl() +
  ggplot2::theme(
    axis.text.y = ggtext::element_markdown(
      size = 13,
      margin = ggplot2::margin(r = -55)
    ),
    axis.text.x = element_text(size = 11),
    plot.title = ggplot2::element_text(size = 25, face = "bold"),
    legend.margin = ggplot2::margin(-30, 0, 0, -10),
    legend.text = ggplot2::element_text(size = 15)
  )

ggsave(here::here("images/sy23_24/coaching_feedback_time.png"),
       bg = "white",
       width = 14,
       height = 10)

session_feedback <- TeachingLab::get_session_survey(year = "22_23")

second_fac <- session_feedback |>
  mutate(month = lubridate::month(RecordedDate, label = TRUE, abbr = FALSE)) |>
  dplyr::select(
    month,
    `They demonstrated deep knowledge of the content they facilitated` = fac_feedback_2_1,
    `They facilitated the content clearly` = fac_feedback_2_2,
    `They effectively built a safe learning community` = fac_feedback_2_3,
    `They were fully prepared for the session` = fac_feedback_2_4,
    `They responded to the group's needs` = fac_feedback_2_5
  )
### Get first facilitator combined with second agree per question for end of session survey ###
plot_agree <- session_feedback |>
  mutate(month = lubridate::month(RecordedDate, label = TRUE, abbr = FALSE)) |>
  dplyr::select(
    month,
    `They demonstrated deep knowledge of the content they facilitated` = fac_feedback_1,
    `They facilitated the content clearly` = fac_feedback_2,
    `They effectively built a safe learning community` = fac_feedback_3,
    `They were fully prepared for the session` = fac_feedback_4,
    `They responded to the group's needs` = fac_feedback_5
  ) |>
  dplyr::bind_rows(second_fac) |>
  tidyr::pivot_longer(!month, names_to = "Question", values_to = "Response") |>
  tidyr::drop_na(Response) |>
  dplyr::group_by(Question, Response, month) |>
  dplyr::count(sort = T) |>
  dplyr::ungroup() |>
  dplyr::group_by(Question, month) |>
  dplyr::mutate(
    Percent = round(100 * n / sum(n), 2),
    Response = factor(Response, levels = c(
      "(1) Strongly disagree",
      "(2) Disagree",
      "(3) Neither agree nor disagree",
      "(4) Agree",
      "(5) Strongly agree"
    )),
    Question = stringr::str_wrap(Question, width = 20)
  )

monthly_session_plot_agree <- plot_agree |>
  dplyr::filter(Response %in% c("(4) Agree", "(5) Strongly agree",
                                "4 - Agree", "5 - Strongly agree",
                                "Agree", "Strongly agree")) |>
  group_by(Question, month) |>
  summarise(Percent = sum(Percent)) |>
  dplyr::mutate(
    month = factor(month, levels = c("September", "October", "November",
                                     "December", "January", "February",
                                     "March", "April", "May",
                                     "June")))

### Calculate n size ###
n_size_agree <- session_feedback |>
  dplyr::count(sort = T)

### Make ggplot of session survey agree percent ###
monthly_session_plot_agree |>
  drop_na(month) |>
  ggplot2::ggplot(ggplot2::aes(
    x = month,
    y = Percent,
    color = Question,
    group = Question
  )) +
  ggplot2::geom_line(size = 3, alpha = 0.8) +
  # geomtextpath::geom_textline(aes(label = Question), 
  #                             alpha = 0.8, size = 4, fontface = "bold",
  #                             linewidth = 4, spacing = 70, hjust = 1
  #                             ) +
  # facet_wrap( ~ Question) +
  ggrepel::geom_text_repel(data = monthly_session_plot_agree |> ungroup() |> dplyr::filter(month == max(month, na.rm = T)), 
                           mapping = aes(label = Question),
                           fontface = "bold",
                           nudge_x = 2) +
  ggplot2::labs(
    x = "", y = "",
    title = glue::glue("Participant Feedback % that agree or strongly agree (n = {format(sum(n_size_agree$n, na.rm = T), big.mark = ',')})"),
    fill = ""
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(),
    color = "none"
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    expand = c(0.14, 0),
    limits = c(70, 100)
  ) +
  scale_color_manual(values = tl_palette(color = "blue", n = 5)) +
  theme_tl() +
  ggplot2::theme(
    axis.text.y = ggtext::element_markdown(
      size = 13,
      margin = ggplot2::margin(r = -55)
    ),
    axis.text.x = element_text(size = 11),
    plot.title = ggplot2::element_text(size = 25, face = "bold"),
    legend.margin = ggplot2::margin(-30, 0, 0, -10),
    legend.text = ggplot2::element_text(size = 15)
  )

ggsave(here::here("images/sy23_24/facilitator_feedback_time.png"),
       bg = "white",
       width = 14,
       height = 10)