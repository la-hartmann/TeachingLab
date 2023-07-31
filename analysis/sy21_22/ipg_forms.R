library(tidyverse)
library(googlesheets4)
library(TeachingLab)
library(gt)
library(ggtext)
devtools::load_all()

ipg_forms <- read_sheet("https://docs.google.com/spreadsheets/d/1L33wVpPERyUQdG8WO3sZiyjnHzPvDL91O4yVUQTN14A/edit#gid=1455024681")

ipg_forms %>%
  mutate(day = lubridate::date(Timestamp)) %>%
  group_by(day) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_col(aes(x = day, y = n)) +
  labs(title = "Daily Count of Responses", x = "", y = "n") +
  theme_tl()

ipg_forms %>%
  mutate(day = lubridate::date(Timestamp)) %>%
  filter(day > as.Date("2021-07-01")) %>%
  group_by(day) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = day, y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -1) +
  labs(title = "Daily Count of Responses", x = "", y = "n",
       subtitle = "Total Responses since October 2021: 52") +
  theme_tl()
ggsave(here::here("images/ipg_forms/pic.png"), width = 10, height = 10, bg = "white")


ipg_forms_filter <- ipg_forms %>%
  mutate(day = lubridate::date(Timestamp)) %>%
  filter(day > as.Date("2021-07-01"))

ipg_forms_filter %>% 
  filter(str_detect(`Name of Site (Parish, District, Network)`, "District 9|D9|NYCDOE")) %>%
  group_by(day) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = day, y = n)) +
  geom_col(fill = "#04ABEB") +
  geom_text(aes(label = n), vjust = -1) +
  labs(title = "Daily Count of Responses", x = "", y = "n",
       subtitle = "Total Responses since October 2021: 52") +
  theme_tl()
ggsave(here::here("images/ipg_forms/pic2.png"), width = 10, height = 10, bg = "white")

ipg_forms_filter_d9 <- ipg_forms_filter %>% 
  filter(str_detect(`Name of Site (Parish, District, Network)`, "District 9|D9|NYCDOE")) %>%
  mutate_all( ~ as.character(.x)) %>%
  janitor::remove_empty(which = "cols")


# CA1A and CA1B
ipg_forms_filter_d9 %>%
  group_by(`CA1a. A majority of the lesson is spent listening to, reading, writing, or speaking about text(s)....57`) %>%
  count(sort = T)


# Rigor
ipg_forms_filter_d9 %>%
  group_by(`What aspect(s) of Rigor are targeted in the standard(s) addressed in this lesson?...142`) %>%
  count(sort = T) %>%
  ungroup() %>%
  gt::gt() %>%
  gt_theme_tl() %>%
  gtsave(here::here("images/ipg_forms/table1.png"))

ipg_forms_filter_d9 %>%
  group_by(`What aspect(s) of Rigor are targeted in the standard(s) addressed in this lesson?...81`) %>%
  count(sort = T) %>%
  ungroup() %>%
  gt::gt() %>%
  gt_theme_tl() %>%
  gtsave(here::here("images/ipg_forms/table2.png"))

# CA2A and CA2B
ipg_forms_filter_d9 %>%
  group_by(`CA3c. The teacher poses questions and problems that prompt students to explain their thinking about the content of the lesson; Students share their thinking about the content of the lesson beyond just stating answers....152`) %>%
  count(sort = T) %>%
  gt::gt() %>%
  gt_theme_tl() %>%
  gtsave(here::here("images/ipg_forms/table3.png"))

# FOR DOING MATH KEEP IN MIND ALMOST ALL ARE SHORTENED MATH, AND ONE IS FULL FORM



####### ELA #######
ela_data <- ipg_forms_filter_d9 %>%
  filter(str_detect(`IPG Rubric`, "K-2")) %>%
  janitor::remove_empty(which = "cols") %>%
  select(-c(33:35)) %>%
  pivot_longer(!c(1:7)) %>%
  drop_na(value) %>%
  group_by(name, value) %>%
  summarise(n = n()) %>%
  ungroup(value) %>%
  mutate(percent = 100*n/sum(n))

# AC1
# ela_data %>%
#   filter(name == "AC1. The following area(s) of focus may be observed; please check all that apply:...13") %>%
#   mutate(name = str_remove_all(name, "\\.\\.\\.[:digit:]"),
#          value = str_wrap(value, 30)) %>%
#   ggplot(aes(x = fct_reorder(value, percent), y = percent)) +
#   geom_col() +
#   coord_flip() +
#   labs(caption = glue::glue("n = {sum(.$n)}")) +
#   theme_tl()

####### Math #######
math_data <- ipg_forms_filter_d9 %>%
  filter(str_detect(`IPG Rubric`, "Math")) %>%
  janitor::remove_empty(which = "cols") %>%
  select(-c(54:55), -`Your Name`) %>%
  pivot_longer(!c(1:7)) %>%
  drop_na(value) %>%
  dplyr::filter(value != "Not observed") %>%
  filter(value != "NULL") %>%
  mutate(name = str_remove_all(name, "\\.+[:digit:]+")) %>% # Remove all with one or more period followed by one or more digits
  group_by(name, value) %>%
  summarise(n = n()) %>%
  ungroup(value) %>%
  mutate(percent = round(100*n/sum(n)))

# Rigor #1
ipg_plot(data = math_data, 
         name = "What aspect(s) of Rigor are targeted in the standard(s) addressed in this lesson?",
         width = 7,
         height = 7,
         wrap = 40,
         save_name = "math_rigor_1",
         split = T)
# Rigor #2
ipg_plot(data = math_data, 
         name = "What aspect(s) of Rigor are targeted in this lesson?",
         width = 8,
         height = 7,
         wrap = 45,
         save_name = "math_rigor_2",
         split = T)

devtools::load_all()
## Core Action 1
# a
ipg_plot(data = math_data, 
         name = "CA1a. The enacted lesson focuses on the grade-level cluster(s), grade-level content standard(s), or part(s) thereof",
         width = 8,
         height = 7,
         wrap = 40,
         save_name = "math_core_1")
# b
ipg_plot(data = math_data, 
         name = "CA1b. The enacted lesson appropriately relates new content to math content within or across grades",
         width = 8,
         height = 7,
         wrap = 40,
         save_name = "math_core_1b")
# c
ipg_plot(data = math_data, 
         name = "CA1c. The enacted lesson intentionally targets the aspect(s) of Rigor (conceptual understanding, procedural skill and fluency, application) called for by the standard(s) being addressed",
         width = 8,
         height = 7,
         wrap = 40,
         save_name = "math_core_1c")
## Core Action 2
# a
ipg_plot(data = math_data, 
         name = "CA2a. The teacher makes the mathematics of the lesson explicit through the use of explanations, representations, tasks, and/or examples",
         width = 11,
         height = 12,
         save_name = "math_core_2a",
         wrap = 38,
         sizing = 1.6,
         numeric = T,
         factor_level = "ca2a")

# b
ipg_plot(data = math_data, 
         name = "CA2b. The teacher strengthens all students’ understanding of the content by strategically sharing students’ representations and/or solution methods",
         width = 11,
         height = 12,
         save_name = "math_core_2b",
         wrap = 37, 
         sizing = 1.6,
         numeric = T,
         factor_level = "ca2b")

# c
ipg_plot(data = math_data, 
         name = "CA2c. The teacher deliberately checks for understanding throughout the lesson to surface misconceptions and opportunities for growth, and adapts the lesson according to student understanding",
         width = 10,
         height = 8,
         save_name = "math_core_2c",
         wrap = 55)

# c
ipg_plot(data = math_data, 
         name = "CA2d. The teacher facilitates the summary of the mathematics with references to student work and discussion in order to reinforce the purpose of the lesson",
         width = 10,
         height = 8,
         save_name = "math_core_2d",
         wrap = 55)

## Core Action 3
# a
ipg_plot(data = math_data, 
         name = "CA3a. The teacher provides opportunities for all students to work with and practice grade-level (or course-level) problems and exercises; Students work with and practice grade-level (or course-level) problems and exercises",
         width = 10,
         height = 8,
         save_name = "math_core_3a",
         wrap = 55, 
         numeric = T,
         factor_level = "ca3a")
# b
ipg_plot(data = math_data, 
         name = "CA3b. The teacher cultivates reasoning and problem solving by allowing students to productively struggle; Students persevere in solving problems in the face of difficulty",
         width = 10,
         height = 8,
         save_name = "math_core_3b",
         wrap = 55)
# c
ipg_plot(data = math_data, 
         name = "CA3c. The teacher poses questions and problems that prompt students to explain their thinking about the content of the lesson; Students share their thinking about the content of the lesson beyond just stating answers",
         width = 10,
         height = 12,
         save_name = "math_core_3c",
         wrap = 55,
         sizing = 1.15,
         numeric = T)
# d
ipg_plot(data = math_data, 
         name = "CA3d. The teacher creates the conditions for student conversations where students are encouraged to talk about each other’s thinking; Students talk and ask questions about each other’s thinking, in order to clarify or improve their own mathematical understanding",
         width = 10,
         height = 12,
         save_name = "math_core_3d",
         wrap = 50, 
         sizing = 1.15,
         numeric = T)
# e
ipg_plot(data = math_data, 
         name = "CA3e. The teacher connects and develops students’ informal language and mathematical ideas to precise mathematical language and ideas; Students use increasingly precise mathematical language and ideas",
         width = 10,
         height = 8,
         save_name = "math_core_3e",
         wrap = 55)

# ela
ela_data <- ipg_forms_filter_d9 %>%
  filter(str_detect(`IPG Rubric`, "K-2")) %>%
  janitor::remove_empty(which = "cols") %>%
  select(-c(33:35)) %>%
  pivot_longer(!c(1:7)) %>%
  drop_na(value) %>%
  dplyr::filter(value != "Not observed") %>%
  filter(value != "NULL") %>%
  mutate(name = str_remove_all(name, "\\.+[:digit:]+")) %>% # Remove all with one or more period followed by one or more digits
  group_by(name, value) %>%
  summarise(n = n()) %>%
  ungroup(value) %>%
  mutate(percent = round(100*n/sum(n)))
## AC1
ipg_plot(data = ela_data, 
         name = "AC1. The following area(s) of focus may be observed; please check all that apply:",
         width = 11,
         height = 10,
         save_name = "ela_ac1",
         wrap = 40,
         sizing = 1.5,
         split = T)
## AC1_2
ipg_plot(data = ela_data, 
         name = "AC1. The foundational skill(s) observed in the lesson reflects grade-level standards",
         width = 6,
         height = 6,
         save_name = "ela_ac1_2",
         wrap = 40,
         numeric = T,
         factor_level = "ac1")
## AC2
ipg_plot(data = ela_data, 
         name = "AC2. The foundational skill(s) observed in the lesson is part of a systematic scope and sequence",
         width = 7,
         height = 7,
         save_name = "ela_ac2",
         wrap = 52,
         numeric = T,
         factor_level = "ac1")
## AD1
ipg_plot(data = ela_data, 
         name = "AD1. Teacher collects student data (formal and/or informal). Opportunities were taken:",
         width = 10,
         height = 8,
         save_name = "ela_ad1",
         wrap = 55)
## AD2
ipg_plot(data = ela_data, 
         name = "AD2. Teacher responds to data and adjusts instruction accordingly to support students. Opportunities were taken:",
         width = 10,
         height = 8,
         save_name = "ela_ad2",
         wrap = 55)
## SP2
ipg_plot(data = ela_data, 
         name = "SP2. Students practice targeted skills in the context of decodable text",
         width = 10,
         height = 8,
         save_name = "ela_sp2",
         wrap = 55)
## TD1
ipg_plot(data = ela_data, 
         name = "TD1. Foundational skill(s) instruction is explicit, clear, and correct",
         width = 7,
         height = 7,
         save_name = "ela_td1",
         wrap = 55,
         numeric = T,
         factor_level = "ac1")

## TD2
ipg_plot(data = ela_data, 
         name = "TD2. When appropriate, instruction and materials provide opportunities to connect acquisition of foundational skills to making meaning from reading and listening",
         width = 8,
         height = 7,
         save_name = "ela_td2",
         wrap = 55,
         numeric = T,
         factor_level = "ac1")

## TD3
ipg_plot(data = ela_data, 
         name = "TD3. The lesson includes adequate time for aligned teacher instruction and student practice of targeted skill(s)",
         width = 8,
         height = 7,
         save_name = "ela_td3",
         wrap = 55,
         numeric = T,
         factor_level = "ac1")

## TD4
ipg_plot(data = ela_data, 
         name = "TD4. The elements of the lesson are presented in an engaging and child-friendly manner",
         height = 7,
         width = 8,
         save_name = "ela_td4",
         wrap = 55,
         numeric = T)



math_data %>%
  ungroup() %>%
  filter(str_detect(name, "Notes")) %>%
  select(-n, -percent) %>%
  gt::gt() %>%
  tab_header("Math IPG Forms Notes") %>%
  cols_label(name = "Notes", 
             value = "Written Note") %>%
  TeachingLab::gt_theme_tl() %>%
  gtsave(here::here("images/ipg_forms/math_notes.png"))

ela_data %>%
  ungroup() %>%
  filter(str_detect(name, "Notes")) %>%
  select(-n, -percent) %>%
  gt::gt() %>%
  tab_header("ELA IPG Forms Notes") %>%
  cols_label(name = "Notes", 
             value = "Written Note") %>%
  TeachingLab::gt_theme_tl() %>%
  gtsave(here::here("images/ipg_forms/ela_notes.png"))
