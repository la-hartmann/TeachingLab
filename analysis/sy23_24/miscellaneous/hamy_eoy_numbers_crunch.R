library(gt)
library(tidyverse)
library(TeachingLab)

course_survey <- TeachingLab::get_course_survey(year = "22_23")

course_survey |>
  group_by(site) |>
  summarise(nps = calc_nps(nps),
            n = n()) |>
  drop_na(nps) |>
  gt::gt() |>
  data_color(
    columns = c(nps, n),
    palette = "ggsci::blue_material"
  ) |>
  tab_header(title = md("**NPS by Site**")) |>
  gt_theme_tl() |>
  gtsave("~/Downloads/image1.png")

TeachingLab::get_end_coaching(year = "22_23") |>
  group_by(site) |>
  summarise(coach_nps = calc_nps(coach_nps),
            n = n()) |>
  drop_na(coach_nps) |>
  gt::gt() |>
  data_color(
    columns = c(coach_nps, n),
    palette = "ggsci::blue_material"
  ) |>
  tab_header(title = md("**Coaching NPS by Site**")) |>
  gt_theme_tl() |>
  gtsave("~/Downloads/image2.png")

contact_lead |>
  select(site, mid_year_likert_qs_6) |>
  group_by(site) |>
  summarise(`% positive` = pos(mid_year_likert_qs_6)) |>
  gt::gt() |>
  tab_header(title = md("**% Positive Scores: \"Teaching Lab helped us advance our goals\"**")) |>
  gt::fmt_percent(scale_values = F) |>
  gt_theme_tl() |>
  gtsave("~/Downloads/image1.png")


n_size_1 <- sum(!is.na(educator_survey$`mindsets_ts_1_1`))
n_size_2 <- sum(!is.na(followup_educator$`mindsets_ts_1_1`))

teacher_mindsets <- educator_survey |>
  bind_rows(followup_educator) |>
  # mutate(id = paste0(tolower(initials), dob)) |>
  # filter(id %in% id_in_both) |>
  select(-contains("DO")) |>
  select(site, prepost, contains("mindsets_ts")) |>
  pivot_longer(!c(prepost, site), names_to = "name", values_to = "value") |>
  mutate(name = str_replace_all(name, c(
    "mindsets_ts_1_2" = "The gap in the achievement among students of different races is about poverty, not race",
    "mindsets_ts_1_3" = "I think about my own background and experiences and how those affect my instruction",
    "mindsets_ts_1_4" = "I try to keep in mind the limits of my students’ ability and give them assignments that I know they can do so that they do not become discouraged",
    "mindsets_ts_1_5" = "Before students are asked to engage in complex learning tasks, they need to have a solid grasp of basic skills",
    "mindsets_ts_1_6" = "It is not fair to ask students who are struggling with English to take on challenging academic assignments",
    "mindsets_ts_1_7" = "Teachers should provide all students the opportunity to work with grade-level texts and tasks",
    "mindsets_ts_1_8" = "Students of all ethnic or cultural backgrounds can be successful in my classroom",
    "mindsets_ts_1_9" = "Students of all ethnic or cultural backgrounds are capable of solving problems by using critical thinking in my classroom",
    "mindsets_ts_1_10" = "Students of all ethnic or cultural backgrounds are able to meet the expectations for higher order skills in my classroom",
    "mindsets_ts_1_11" = "Grouping students of different levels of achievement for instruction may benefit some students, but it can undermine the progress that could otherwise be made by higher achieving students",
    "mindsets_ts_1_12" = "Students who come into my classroom behind grade level will have a hard time succeeding",
    "mindsets_ts_1_13" = "Students have a certain amount of intelligence, and they can’t really do much to change it",
    "mindsets_ts_1_14" = "Intelligence is something about students that they can’t change very much",
    "mindsets_ts_1_15" = "Students can learn new things, but they can’t really change their basic intelligence",
    "mindsets_ts_1_1" = "I am color blind when it comes to my teaching - I don’t think of my students in terms of their race or ethnicity"
  ))) |>
  drop_na(value) |>
  group_by(name, value, prepost, site) |>
  count(sort = T) |>
  ungroup()

if (sum(teacher_mindsets$prepost == "Post") >= 1) {
  teacher_mindsets |>
    mutate(
      pos_neg = case_when(
        str_detect(name, "race") ~ "negative",
        name %in% c(
          "Before students are asked to engage in complex learning tasks, they need to have a solid grasp of basic skills",
          "Grouping students of different levels of achievement for instruction may benefit some students, but it can undermine the progress that could otherwise be made by higher achieving students",
          "I try to keep in mind the limits of my students’ ability and give them assignments that I know they can do so that they do not become discouraged",
          "It is not fair to ask students who are struggling with English to take on challenging academic assignments",
          "Students who come into my classroom behind grade level will have a hard time succeeding",
          "Students can learn new things, but they can’t really change their basic intelligence",
          "Students have a certain amount of intelligence, and they can’t really do much to change it",
          "Intelligence is something about students that they can’t change very much"
        ) ~ "negative",
        TRUE ~ "positive"
      ),
      ` ` = case_when(
        str_detect(name, "race|own background") ~ "Recognition of Race & Culture",
        name %in% c(
          "Students of all ethnic or cultural backgrounds can be successful in my classroom",
          "Students of all ethnic or cultural backgrounds are capable of solving problems by using critical thinking in my classroom",
          "Students of all ethnic or cultural backgrounds are able to meet the expectations for higher order skills in my classroom",
          "Students who come into my classroom behind grade level will have a hard time succeeding"
        ) ~ "Need for Remediation",
        name %in% c(
          "Before students are asked to engage in complex learning tasks, they need to have a solid grasp of basic skills",
          "Grouping students of different levels of achievement for instruction may benefit some students, but it can undermine the progress that could otherwise be made by higher achieving students",
          "I try to keep in mind the limits of my students’ ability and give them assignments that I know they can do so that they do not become discouraged",
          "It is not fair to ask students who are struggling with English to take on challenging academic assignments",
          "Teachers should provide all students the opportunity to work with grade-level texts and tasks"
        ) ~ "High Expectations",
        name %in% c(
          "Students can learn new things, but they can’t really change their basic intelligence",
          "Students have a certain amount of intelligence, and they can’t really do much to change it",
          "Intelligence is something about students that they can’t change very much"
        ) ~ "Growth Mindsets"
      ),
      score_multiplier = case_when(
        value == "1 - Strongly disagree" & pos_neg == "negative" ~ 1,
        value == "1 - Strongly disagree" & pos_neg == "positive" ~ 0,
        value == "2 - Disagree" & pos_neg == "negative" ~ 0.75,
        value == "2 - Disagree" & pos_neg == "positive" ~ 0.25,
        value == "3 - Neither agree nor disagree" & pos_neg == "negative" ~ 0.5,
        value == "3 - Neither agree nor disagree" & pos_neg == "positive" ~ 0.5,
        value == "4 - Agree" & pos_neg == "negative" ~ 0.25,
        value == "4 - Agree" & pos_neg == "positive" ~ 0.75,
        value == "5 - Strongly agree" & pos_neg == "negative" ~ 0,
        value == "5 - Strongly agree" & pos_neg == "positive" ~ 1
      ),
      name = if_else(pos_neg == "negative", paste0("<p style='color:red;'>", name, "</p>"), name)
    ) |>
    filter(` ` != "Need for Remediation") |>
    group_by(name, prepost, site) |>
    mutate(score = (n * score_multiplier) / sum(n)) |>
    summarise(
      score = sum(score),
      ` ` = min(` `)
    ) |>
    ungroup() |>
    group_by(` `, prepost, site) |>
    summarise(score = mean(score)) |>
    ungroup() |>
    pivot_wider(names_from = prepost, values_from = score) |>
    relocate(Post, .after = Pre) |>
    group_by(site) |>
    summarise(Pre = mean(Pre), Post = mean(Post)) |>
    gt::gt() |>
    gt::fmt_percent(c(Pre, Post),
                    scale_values = TRUE
    ) |>
    # gt::fmt_markdown(` `) |>
    gt::data_color(
      columns = c(Pre, Post),
      fn = scales::col_bin(
        palette = c(
          TeachingLab::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
        ),
        domain = c(0, 1),
        bins = c(0, 0.39, 0.79, 1)
      )
    ) |>
    tab_header(md("**Equitable Mindsets Scores by Partner**")) |>
    TeachingLab::gt_theme_tl(align = "left") |>
    gtsave("~/Downloads/image2.png")
    # gt::tab_footnote(footnote = paste0("n = ", n_size_1), locations = cells_column_labels("Pre")) |>
    # gt::tab_footnote(footnote = paste0("n = ", n_size_2), locations = cells_column_labels("Post"))
  
  
knowledge_assessments |>
  group_by(site, prepost) |>
  summarise(percent = mean(percent)) |>
  ungroup() |>
  pivot_wider(names_from = prepost, values_from = percent) |>
  gt::gt() |>
  fmt_percent(columns = c(pre, post),
              scale_values = T) |>
  TeachingLab::gt_theme_tl() |>
  gtsave("~/Downloads/image3.png")


educator_survey |>
  bind_rows(followup_educator) |>
  select(prepost, site, contains("ts_crse_after")) |>
  pivot_longer(!c(prepost, site), names_to = "name", values_to = "value") |>
  mutate(name = str_replace_all(name, c(
    "ts_crse_after_7/30_1" = "I adapt instruction to meet the needs of my students",
    "ts_crse_after_7/30_2" = "I identify ways that the school culture (e.g., values, norms, and practices) is different from my students’ home culture",
    "ts_crse_after_7/30_3" = "I use my students’ prior knowledge to help them make sense of new information",
    "ts_crse_after_7/30_4" = "I revise instructional material to include a better representation of cultural groups",
    "ts_crse_after_7/30_5" = "Use my students’ cultural background to help make learning meaningful.",
    "ts_crse_after_7/30_6" = "Use examples that are familiar to students from diverse cultural backgrounds.",
    "ts_crse_after_7/30_7" = "Take time to learn about the cultures represented by students in my classroom.",
    "ts_crse_after_7/30_8" = "Teach the curriculum to students with unfinished learning",
    "ts_crse_after_7/30_9" = "Teach the curriculum to students who are from historically marginalized groups"
  ))) |>
  filter(value != "N/A - I do not observe teachers in my role.") |>
  group_by(name, value, prepost, site) |>
  count(sort = T) |>
  ungroup() |>
  drop_na(value) |>
  mutate(name = html_wrap(name, 25)) |>
  group_by(name, prepost, site) |>
  mutate(Percent = round(100 * n / sum(n), 2)) |>
  filter(value %in% c("5- Very often", "4- Often")) |>
  ungroup() |>
  group_by(name, prepost, site) |>
  summarise(
    Percent = sum(Percent)
  ) |>
  ungroup() |>
  group_by(site, prepost) |>
  summarise(
    Percent = mean(Percent)
  ) |>
  ungroup() |>
  pivot_wider(names_from = prepost, values_from = Percent) |>
  gt::gt() |>
  gt::fmt_percent(c(Pre, Post),
                  scale_values = FALSE
  ) |>
  # gt::fmt_markdown(` `) |>
  gt::data_color(
    columns = c(Pre, Post),
    fn = scales::col_bin(
      palette = c(
        TeachingLab::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
      ),
      domain = c(0, 100),
      bins = c(0, 39.9, 79.9, 100)
    )
  ) |>
  tab_header(md("**CRSE % Selected Often/Very Often by Partner**")) |>
  gt_theme_tl() |>
  gtsave("~/Downloads/image4.png")
  

student_work_grades |>
  tidyr::separate_rows(`Submitted Grade/s`, sep = ", ") |>
  dplyr::filter(!`Submitted Grade/s` %in% c("Duplicate", "Not legible", "Not on grade level", "Skipped",
                                            "Not\nlegible", "NA", NA, "Not\non\ngrade\nlevel", 
                                            "Task does NOT ask students to explain their mathematical thinking",
                                            "Task does NOT ask students to respond or write from evidence")) |>
  dplyr::filter(!`Submitted Grade/s` %in% c("Duplicate", "Not legible", "Not on grade level", "Skipped",
                                            "Not\nlegible", "NA", NA, "Not\non\ngrade\nlevel", "DUPLICATE") &
                  Prepost != "DUPLICATE") |>
  dplyr::group_by(`Submitted Grade/s`, Site, Prepost) |>
  dplyr::count(sort = T) |>
  dplyr::ungroup() |>
  dplyr::group_by(Site, Prepost) |>
  dplyr::mutate(
    percent = (n/sum(n))
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(`Submitted Grade/s` == "2") |>
  dplyr::select(Site, Prepost, percent) |>
  dplyr::filter(Site != "Other" & Site != "NA") |>
  pivot_wider(names_from = Prepost, values_from = percent) |>
  gt::gt() |>
  gt::fmt_percent(c(Pre, Post),
                  scale_values = TRUE
  ) |>
  gt::data_color(
    columns = c(Pre, Post),
    fn = scales::col_bin(
      palette = c(
        TeachingLab::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
      ),
      domain = c(0, 100),
      bins = c(0, 39.9, 79.9, 100)
    )
  ) |>
  tab_header(md("**Student Work % Proficient by Partner**")) |>
  gt::sub_missing(columns = everything(), missing_text = "No data") |>
  gt_theme_tl() |>
  gtsave("~/Downloads/image5.png")

agree_select <- c("4 - Often", "5 - Always", "4 - Somewhat agree", "5 - Agree",
                  "4 - Mostly true", "5 - Totally true")

student_survey |>
  dplyr::select(tidyselect::contains("crse"), tidyselect::contains("teacher_student_rel"),
                tidyselect::contains("self_efficacy"), tidyselect::contains("happiness_belonging"),
                tidyselect::contains("being_challenged"), prepost, site) |>
  dplyr::group_by(prepost, site) |>
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ TeachingLab::tl_select_percent(.x, agree_select))) |>
  tidyr::drop_na(prepost) |>
  pivot_longer(!c(prepost, site)) |>
  group_by(prepost, site) |>
  summarise(value = 100 * mean(value, na.rm = T)) |>
  ungroup() |>
  pivot_wider(names_from = prepost, values_from = value) |>
  gt::gt() |>
  gt::fmt_percent(c(Pre, Post),
                  scale_values = FALSE
  ) |>
  gt::data_color(
    columns = c(Pre, Post),
    fn = scales::col_bin(
      palette = c(
        TeachingLab::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
      ),
      domain = c(0, 100),
      bins = c(0, 39.9, 79.9, 100)
    )
  ) |>
  tab_header(md("**Student Survey % Positive Selections**")) |>
  gt::sub_missing(columns = everything(), missing_text = "No data") |>
  gt_theme_tl() #|>
  # gtsave("~/Downloads/image6.png")

