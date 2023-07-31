library(tidyverse)
library(gt)

# Data Cleaning Done Previously in 2021 cleaning rmd
ed_survey <- read_rds(here::here("data/sy19_20/full_2021.rds"))

# Positive and Negative Vectors for Mindsets Scoring

positive_vector <- c("4", "5")
negative_vector <- c("1", "2")


# Mindsets scores
mindsets_index <- tibble(question_pre = c("prerace1", "prerace2", 
                                          "prehigh2", "prehigh3", "prehigh4", "prehigh1", "pregrowth1", 
                                          "pregrowth2", "preacc1", "preacc2", "preacc3"),
                         question_post = c("postrace1", "postrace2", 
                             "posthigh2", "posthigh3", "posthigh4", "posthigh1", "postgrowth1", 
                             "postgrowth2", "postacc1", "postacc2", "postacc3"),
                coding = list("negative", "negative", "negative", "negative", "negative", "positive",
                              "negative", "negative", "positive", "positive", "positive"))

mindsets_df <- pmap_df(list(mindsets_index$question_pre, mindsets_index$question_post, mindsets_index$coding), 
                       ~ score_question_mindsets(ed_survey, question_pre = ..1, question_post = ..2, coding = ..3, na_remove = F, likert = 5)) %>%
  mutate(question = str_remove(mindsets_index$question_pre, "pre")) %>%
  relocate(question, .before = 1)

# School Environment Scores
environment_index <- tibble(question_pre = c("preschool1", "preschool2", "preschool3", "preschool4"),
                question_post = c("postschool1", "postschool2", "postschool3", "postschool4"),
                coding = list(positive_vector, positive_vector, positive_vector, positive_vector))

environment_df <- pmap_df(list(environment_index$question_pre, environment_index$question_post, environment_index$coding), 
                       ~ score_question_improved(ed_survey, question_pre = ..1, question_post = ..2, coding = ..3, middle_value = "3")) %>%
  select(question, score_pre = pre_percent, n1, score_post = post_percent, n2)

# ELA Questions

ela_index <- tibble(question_pre = c("preelalit1a", "preelalit1b", "preelalit1c", "preelalit1d", 
                                     "preelagen1a", "preelagen1b", "preelagen1c", "preelagen1d", 
                                              "preelagen2", "preelafluency1a", "preelafluency1b", "preelafluency1c", 
                                              "preelafluency1d", "preelafluency2", "preelatext1", "preelatext2a", 
                                              "preelatext2b", "preelatext2c", "preelatext2d", "preelaevi1a", 
                                              "preelaevi1b", "preelaevi1c", "preelaevi1d", "preelaevi2", 
                                              "preelaknow1", "preelaknow2", "preelasupp1", "preelasupp2a", 
                                              "preelasupp2b", "preelasupp2c", "preelasupp2d"),
                             question_post = c("postelalit1a", "postelalit1b", "postelalit1c", "postelalit1d",
                                               "postelagen1a", "postelagen1b", "postelagen1c", "postelagen1d", 
                                               "postelagen2", "postelafluency1a", "postelafluency1b", "postelafluency1c", 
                                               "postelafluency1d", "postelafluency2", "postelatext1", "postelatext2a", 
                                               "postelatext2b", "postelatext2c", "postelatext2d", "postelaevi1a", 
                                               "postelaevi1b", "postelaevi1c", "postelaevi1d", "postelaevi2", 
                                               "postelaknow1", "postelaknow2", "postelasupp1", "postelasupp2a", 
                                               "postelasupp2b", "postelasupp2c", "postelasupp2d"),
                             coding = list("Equitable",
                                           "Equitable",
                                           "Not equitable",
                                           "Not equitable",
                                           "Yes", 
                                           "Yes", 
                                           "No", 
                                           "No", 
                                           "A complex text that is worthy of reading multiple times.", 
                                           "TRUE", 
                                           "TRUE", 
                                           "FALSE", 
                                           "FALSE", 
                                           "Students independently read aloud texts at their reading level.", 
                                           "Ability to read complex text independently and proficiently.", 
                                           "Yes", 
                                           "Yes", 
                                           "No", 
                                           "No", 
                                           "TRUE", 
                                           "TRUE", 
                                           "FALSE", 
                                           "FALSE", 
                                           "Students pull out evidence from the text to explain their thinking in response to questions.", 
                                           "Students with low reading ability and a lot of knowledge about the food chain.", 
                                           "Have students read a series of additional texts at a variety of complexity levels on the topic.", 
                                           "Provide students with lower reading abilities an audio version of the main text to listen to before reading the main text in class.",
                                           "Yes", 
                                           "Yes", 
                                           "No", 
                                           "No"))

ela_df <- pmap_df(list(ela_index$question_pre, ela_index$question_post, ela_index$coding), 
                  ~ score_question_improved(ed_survey, question_pre = ..1, question_post = ..2, coding = ..3, middle_value = "3")) %>%
  select(question, score_pre = pre_percent, n1, score_post = post_percent, n2)

# Math Questions

math_index <- tibble(question_pre = c("mathgen1a.x", "mathgen1b.x", "mathgen1c.x", "mathgen1d.x", 
                                      "mathgen2.x", "mathgen3a.x", "mathgen3b.x", "mathgen3c.x", "mathgen3d.x", 
                                      "matheq1.x", "matheq2a.x", "matheq2b.x", "matheq2c.x", "matheq2d.x", 
                                      "matheq3.x", "mathsupp1.x", "mathsupp2a.x", "mathsupp2b.x", "mathsupp2c.x", 
                                      "mathsupp2d.x", "matheff1a.x", "matheff1b.x", "matheff1c.x", 
                                      "matheff1d.x", "matheff2.x", "matheff3.x"),
                     question_post = c("mathgen1a.y", "mathgen1b.y", "mathgen1c.y", "mathgen1d.y", 
                                       "mathgen2.y", "mathgen3a.y", "mathgen3b.y", "mathgen3c.y", "mathgen3d.y", 
                                       "matheq1.y", "matheq2a.y", "matheq2b.y", "matheq2c.y", "matheq2d.y", 
                                       "matheq3.y", "mathsupp1.y", "mathsupp2a.y", "mathsupp2b.y", "mathsupp2c.y", 
                                       "mathsupp2d.y", "matheff1a.y", "matheff1b.y", "matheff1c.y", 
                                       "matheff1d.y", "matheff2.y", "matheff3.y"),
                     coding = list("Yes",
                                   "Yes",
                                   "No",
                                   "No",
                                   "Procedural knowledge should be built from conceptual understanding.",
                                   "TRUE",
                                   "TRUE",
                                   "FALSE",
                                   "FALSE",
                                   "Creating opportunities for students to practice saying out loud how they solved for a problem.",
                                   "TRUE",
                                   "TRUE",
                                   "FALSE",
                                   "FALSE",
                                   "Enables teachers to make in-the-moment decisions on how to respond to students with questions and prompts that probe, scaffold, and extend to meet various learning needs.",
                                   "Identifying unfinished learning leading up to the current topic and teach 1-2 lessons targeting those prerequisites at the beginning of the topic.",
                                   "Yes",
                                   "Yes",
                                   "No",
                                   "No",
                                   "TRUE",
                                   "TRUE",
                                   "FALSE",
                                   "FALSE",
                                   "What’s the first step?",
                                   "Explicitly teaching students how to use certain representations."))

math_df <- pmap_df(list(math_index$question_pre, math_index$question_post, math_index$coding), 
                   ~ score_question_improved(ed_survey, question_pre = ..1, question_post = ..2, 
                                             coding = ..3, middle_value = "3")) %>%
  select(question, score_pre = pre_percent, n1, score_post = post_percent, n2)

# Lab Leaders

lab_index <- tibble(question_pre = c("preobs1",
                                     "preobs2",
                                     "preobs3"),
                    question_post = c("postobs1",
                                      "postobs2",
                                      "postobs3"),
                    coding = list(positive_vector, positive_vector, positive_vector))

lab_df <- pmap_df(list(lab_index$question_pre, lab_index$question_post, lab_index$coding), 
                            ~ score_question_improved(ed_survey, question_pre = ..1, question_post = ..2, 
                                                      coding = ..3, middle_value = "3")) %>%
  select(question, score_pre = pre_percent, n1, score_post = post_percent, n2)

# Admins

admin_index <- tibble(question_pre = c("postadmin1",
                                       "postadmin2",
                                       "postadmin3"),
                      question_post = c("postadmin4",
                                        "postadmin5",
                                        "postadmin6"),
                      coding = list(c("Often, Almost always"), c("Often, Almost always"), c("Often, Almost always")))

admin_df <- pmap_df(list(admin_index$question_pre, admin_index$question_post, admin_index$coding), 
                  ~ score_question_improved(ed_survey, question_pre = ..1, question_post = ..2, 
                                            coding = ..3, middle_value = "Sometimes")) %>%
  select(question, score_pre = pre_percent, n1, score_post = post_percent, n2)

# Teacher Practices

# teacher_index <- tibble(question_pre = c("postadmin1",
#                                  "postadmin2",
#                                  "postadmin3"),
#                 question_post = c("postadmin4",
#                                   "postadmin5",
#                                   "postadmin6"),
#                 coding = list(c("Often", "Almost always"), c("Often", "Almost always"), c("Often", "Almost always")))
# 
# teacher_df <- pmap_df(list(teacher_index$question_pre, teacher_index$question_post, teacher_index$coding), 
#                                        ~ score_question_improved(ed_survey, question_pre = ..1, 
#                                                                  question_post = ..2, coding = ..3, middle_value = "3")) %>%
#   distinct(question, .keep_all = T)


questions <- c("I am color blind when it comes to my teaching - I don’t think of my students in terms of their race or ethnicity.",
  "The gap in the achievement among students of different races is about poverty, not race",
  "I try to keep in mind the limits of my students’ ability and give them assignments that I know they can do so that they do not become discouraged.",
  "Before students are asked to engage in complex learning tasks, they need to have a solid grasp of basic skills.",
  "It is not fair to ask students who are struggling with English to take on challenging academic assignments.",
  "Teachers should provide all students the opportunity to work with grade -level texts and tasks.",
  "To be honest, students have a certain amount of intelligence, and they really can’t do much to change it.",
  "Your intelligence is something about you that you can’t change very much.",
  "If a child doesn’t learn something the first time, I will try another way.",
  "If I provide the proper scaffolds, all students in my class will be able to succeed with grade-level texts and tasks.",
  "I can develop the skills needed to produce meaningful students learning.",
  "I trust my fellow teachers in the school.",
  "I feel connected to my fellow teachers in the school.",
  "I have influence over the  professional learning that I receive through my school or district.",
  "I am confident that I am implementing the curriculum in a way that maximizes positive impact for student learning.” Note: We are referring to the curriculum that Teaching Lab provided support on only.",
  rep("Mr. Kaichi teaches first grade, and many of his students are not reading at first-grade level. Below are some approaches that teachers use to try to bring students up to grade-level in early grades. Some of these approaches are equitable instructional practices - they will meet students where they are and advance their learning by giving them what they need - and some are not. For each practice, indicate whether it is “Equitable” or “Not equitable”", 4),
  rep("Which of the following are literacy instructional shifts, and which are not?", 4),
  "When designing literacy lessons, teachers should start with which of the following?",
  rep("Which of the following statements are true about the relationship between reading fluency and reading comprehension, and which are false?", 4),
  "Which of the following is NOT an effective strategy for improving student fluency?",
  "Which of the following is the single biggest differentiator of college and career-readiness?",
  rep("Which of the following approaches for selecting texts for whole-class reading instruction are aligned with post-shifts literacy instruction and which are not", 4),
  rep("Which of the following statements are true about reading the same complex text multiple times?", 4),
  "Which of the following describes something students might do during close reading of complex texts?",
  "Mrs. Richards’ students have a range of reading proficiency and knowledge about the food chain. When reading a grade-level complex text about this topic, which group of students is most likely to perform better on comprehension questions?",
  "How could Mrs. Richards best prepare students to build knowledge about the topic of the food chain?",
  "The main text that the students in Ms. Blackwell’s class is about to read is likely to be very difficult for the majority of the class. Which of the following is a strategy that Ms. Blackwell could use with her students with lower reading abilities?",
  rep("Which of the following describe strategies for supporting struggling readers, and which do not?", 4),
  rep("Which of the following statements describe math instructional shifts associated with college-and-career readiness standards, and which do not?", 4),
  "Which of the following statements accurately describes the connection between conceptual understanding and procedural knowledge?",
  rep("Which of the following statements are true about the research into types of math instruction, and which are false?", 4),
  "Equitable instruction in math includes which of the following?",
  rep("Which of the following statements are true about the principles behind the Mathematical Language Routines, and which are false?", 4),
  "Please complete this statement: Eliciting and using student thinking is an equitable teaching practice because this practice...",
  "Which of the following actions BEST describes equitable instructional strategies for supporting students with unfinished learning in math?",
  rep("Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students.", 4),
  rep("Which of the following statements about learning goals are true, and which are false?", 4),
  "Which of the following questions will give you the LEAST amount of insight into what the student is thinking?",
  "Which of the following is the LEAST effective method of helping students understand and make connections between representations?",
  "Whether the lesson is focused on a high-quality text or task\"",
  "Whether the questions and tasks adress the analytical thinking required by the grade-level standard",
  "Whether all students have opportunities to engage in the work of the lesson") %>% as_tibble() %>% select(`Full Question` = 1) %>%
  mutate(`Full Question` = str_replace_all(str_wrap(`Full Question`, width = 50), "\n", "<br>"))

answers <- c("Strongly disagree - Strongly Agree",
             "Strongly disagree - Strongly Agree",
             "Strongly disagree - Strongly Agree",
             "Strongly disagree - Strongly Agree",
             "Strongly disagree - Strongly Agree",
             "Strongly disagree - Strongly Agree",
             "Strongly disagree - Strongly Agree",
             "Strongly disagree - Strongly Agree",
             "Strongly disagree - Strongly Agree",
             "Strongly disagree - Strongly Agree",
             "Strongly disagree - Strongly Agree",
             "Strongly disagree - Strongly Agree",
             "Strongly disagree - Strongly Agree",
             "Strongly disagree - Strongly Agree",
             "Strongly disagree - Strongly Agree",
             "Equitable",
             "Equitable",
             "Not equitable",
             "Not equitable",
             "Yes",
             "Yes",
             "No",
             "No",
             "A complex text that is worthy of reading multiple times.",
             "True",
             "True",
             "False",
             "False",
             "Students independently read aloud texts at their reading level",
             "Ability to read complex text independently and proficiently",
             "Yes",
             "Yes",
             "No",
             "No",
             "True",
             "True",
             "False",
             "False",
             "Students pull out evidence from the text to explain their thinking in response to questions.",
             "Students with low reading ability and a lot of knowledge about the food chain.",
             "Have students read a series of additional texts at a variety of complexity levels on the topic",
             "Provide students with lower reading abilities an audio version of the main text to listen to before reading the main text in class.",
             "Yes",
             "Yes",
             "No",
             "No",
             "Yes",
             "Yes",
             "No",
             "No",
             "Procedural knowledge should be built from conceptual understanding.",
             "True",
             "True",
             "False",
             "False",
             "Creating opportunities for students to practice saying out loud how they solved for a problem.",
             "True",
             "True",
             "False",
             "False",
             "Enables teachers to make in-the-moment decisions on how to respond to students with questions and prompts that probe, scaffold, and extend to meet various learning needs.",
             "Identifying unfinished learning leading up to the current topic and teach 1-2 lessons targeting those prerequisites at the beginning of the topic.",
             "Yes",
             "Yes",
             "No",
             "No",
             "True",
             "True",
             "False",
             "False",
             "What’s the first step?",
             "Explicitly teaching students how to use certain representations.",
             "Almost never - Almost always",
             "Almost never - Almost always",
             "Almost never - Almost always") %>% as_tibble() %>% select(Answer = 1) %>%
  mutate(Answer = str_replace_all(str_wrap(Answer, width = 50), "\n", "<br>"))





# GT arrow customization
rank_chg <- function(change_dir){
  if (change_dir == "increase") {
    logo_out <- fontawesome::fa("arrow-up", fill = "#98AFC7")
  } else if (change_dir == "decrease"){
    logo_out <- fontawesome::fa("arrow-down", fill = "#800000")
  } else if (change_dir == "equal"){
    logo_out <- "<strong>≈"
  }
  
  logo_out %>% 
    as.character() %>% 
    gt::html()
  
}

# Bind Together to Make Item by Item Effectiveness Ranking
(itemized_table <- bind_rows(mindsets_df %>% mutate(group = "Mindsets"), environment_df %>% mutate(group = "School Environment"), 
                             ela_df %>% mutate(group = "ELA"), math_df %>% mutate(group = "Math"), lab_df %>% mutate(group = "Lab")) %>%
  mutate(question = str_remove_all(question, "post|.x")) %>%
  mutate(rank_change = case_when(score_pre < score_post ~ "increase",
                                 score_post < score_pre ~ "decrease",
                                 abs(score_pre - score_post) < 3 ~ "equal")) %>%
  mutate(rank_change = purrr::map(rank_change, ~ rank_chg(change_dir = .x))) %>%
  bind_cols(questions, answers) %>%
  gt(groupname_col = "group") %>%
  tab_style(
    style = list(
      cell_fill("black"),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_row_groups()
  ) %>% 
  tab_style(
    style = cell_text(color = "darkgrey", weight = "bold"),
    locations = cells_stub()
  ) %>%
  tab_header(html('<img src = "https://images.squarespace-cdn.com/content/v1/5e9e07cd7714da42bfa89f6e/1587415133627-QY4HDP76E7QRG9UU4NDL/Teaching+Lab+Logo+HORIZONTAL.png?format=1500w", width = "75", height = "26"> <strong> &nbsp&nbsp&nbsp&nbsp&nbsp 2020-2021 Scores by Question &nbsp&nbsp&nbsp&nbsp&nbsp </strong> <img src = "https://images.squarespace-cdn.com/content/v1/5e9e07cd7714da42bfa89f6e/1587415133627-QY4HDP76E7QRG9UU4NDL/Teaching+Lab+Logo+HORIZONTAL.png?format=1500w", width = "75", height = "26">')) %>%
  fmt_percent(
    columns = c(score_pre, score_post),
    decimals = 2,
    scale_values = F
  ) %>%
  fmt_markdown(columns = c("Full Question", "Answer")) %>%
  cols_label(
    question = md("**Question**"),
    `score_pre` = md("**Fall Score**"),
    `score_post` = md("**Spring Score**"),
    `n1` = html("<strong>n<sub>1"),
    `n2` = html("<strong>n<sub>2"),
    `rank_change` = md("**Score Change**"),
    `Full Question` = md("**Full Questions**"),
    `Answer` = md("**Answers**"),
  ) %>%
  data_color(
    columns = c(score_pre, score_post),
    colors = scales::col_numeric(
      palette = c("#FFFFFF", "#04ABEB"),
      domain = NULL
    )
  ) %>%
  gt_theme_tl())
  
# Save as html and png
walk(c(".png", ".html"), ~ gtsave(itemized_table, here::here(paste0("Images/SY20-21 Tables/all_scores", .x))))
  