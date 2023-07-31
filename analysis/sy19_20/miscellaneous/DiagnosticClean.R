library(tidyverse)
library(visdat)
library(ggtext)
library(googlesheets4)

col <- colorRampPalette(c("#040404", "#04ABEB"))

data <- read_csv(here("Data/Single response questions in diagnostic survey.csv"))

# remove_vector <- c(
#   '<p><span id="docs-internal-guid-56978e54-7fff-17de-540c-74c7ed4747ba"><b>',
#   '</b></span><br></p>',
#   '<p><span id="docs-internal-guid-e28046a7-7fff-bf45-dc13-29abdea644b0"><b>',
#   '<p><span id="docs-internal-guid-54e91075-7fff-4d86-6614-488c17ffe087"><b id="docs-internal-guid-a8b2e328-7fff-1ce7-a05e-2047423fd689">',
#   '<p><span id="docs-internal-guid-48140e55-7fff-d2d3-a605-69864c529e8c"><span data-sheets-value="{&quot;1&quot;:2,&quot;2&quot;:&quot;Which of the following actions BEST describes equitable instructional strategies for supporting students with unfinished learning in math?&quot;}" data-sheets-userformat="{&quot;2&quot;:29056,&quot;10&quot;:1,&quot;11&quot;:4,&quot;15&quot;:&quot;Calibri&quot;,&quot;16&quot;:11,&quot;17&quot;:1}"><b>',
#   '</b></span></span><br></p>',
#   '<p><b id="docs-internal-guid-372bf957-7fff-d32d-e862-25e700c64d08"><b id="docs-internal-guid-edfd1be6-7fff-07b3-808b-4da51e593455">',
#   '<p><b id="docs-internal-guid-c29a3b05-7fff-1ead-a75a-9e6e16f1f2d4">',
#   '</b><br></p>'
# )

# Function to clean out html strings from questions
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

data_clean <- data %>%
  select(id, `response id`, `question content`, choice) %>%
  mutate(Question = cleanFun(`question content`)) %>%
  mutate(Question = str_remove(Question, "&nbsp;")) %>%
  mutate(Question = str_trim(Question)) %>%
  select(-`question content`, -id)


diagnostic_pivot <- data_clean %>%
  pivot_wider(names_from = "Question", values_from = "choice") %>%
  mutate(`response id` = as.character(`response id`))

vis_miss(diagnostic_pivot) +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 65)) +
  # scale_fill_manual(values = col(2)) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 7.5, face = "bold", margin = margin(0,0,0,0)),
        axis.text.x = element_text(angle = 0),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

data2 <- read_sheet("https://docs.google.com/spreadsheets/d/1DhVmQWqOzXab1EARPuleWSdZirNNuAcENhG2Wn2IVwU/edit#gid=309350695",
                    sheet = "Form Responses 1",
                    skip = 1)

replace_numeric <- c(
  "5" = "Strongly agree",
  "4" = "Agree",
  "3" = "Neither agree nor disagree",
  "2" = "Disagree",
  "1" = "Strongly disagree"
)

replace_columns <- c(
  "Which subject area do you teach/ lead? If you teach/support both, please select the one where you expect to work with Teaching Lab this year." = "Which subject area do you/ do you expect to work with Teaching Lab on this year?",
  "Which of the following best describes your primary role?" = "Which of the following best describes your role?",
  "The gap in the achievement among students of different races is about poverty, not race" = "The gap in the achievement among students of different races is about poverty, not race.",
  "Before students are asked to engage in complex learning tasks, they need to have a solid grasp of basic skills." = "Before students are asked to engage in complex learning tasks, they need to have a solid grasp of basic skills.",
  "It is not fair to ask students who are struggling with English to take on challenging academic assignments." = "It is not fair to ask students who are struggling with English to take on challenging academic assignments.",
  "Teachers should provide all students the opportunity to work with grade-level texts and tasks." = "Teachers should provide all students the opportunity to work with grade-level texts and tasks.",
  "To be honest, students have a certain amount of intelligence, and they really can’t do much to change it." = "To be honest, students have a certain amount of intelligence, and they really can’t do much to change it.",
  "Your intelligence is something about you that you can’t change very much." = "Your intelligence is something about you that you can’t change very much.",
  "I am confident that I am implementing the curriculum in a way that maximizes positive impact for student learning." = "I am confident that I am implementing the curriculum in a way that maximizes positive impact for student learning.Note - The curriculum refers to what Teaching Lab is supporting you on this year."
)

data2_clean <- data2 %>%
  select(-Timestamp) %>%
  mutate(across(where(is.numeric), ~ str_replace_all(.x, replace_numeric))) %>%
  mutate(`To be honest, students have a certain amount of intelligence, and they really can’t do much to change it.` = coalesce(`To be honest, students have a certain amount of intelligence, and they really can’t do much to change it....11`,
                                                                                                                                `To be honest, students have a certain amount of intelligence, and they really can’t do much to change it....93`),
         `Teachers should provide all students the opportunity to work with grade-level texts and tasks.` = coalesce(`Teachers should provide all students the opportunity to work with grade-level texts and tasks....10`,
                                                                                                                     `Teachers should provide all students the opportunity to work with grade-level texts and tasks....92`),
         `Your intelligence is something about you that you can’t change very much.` = coalesce(`Your intelligence is something about you that you can’t change very much....12`,
                                                                                                `Your intelligence is something about you that you can’t change very much....94`),
         `I can develop the skills needed to produce meaningful students learning.` = coalesce(`I can develop the skills needed to produce meaningful students learning....15`,
                                                                                               `I can develop the skills needed to produce meaningful students learning....97`)) %>%
  select(-10, -92, -9, -91, -11, -93, -14, -96) %>%
  rename(`response id` = `Your work email address.`) %>%
  rename_with( ~ str_replace_all(.x, replace_columns)) %>%
  rename_with( ~ str_remove(.x, "\\.\\.\\.[:digit:]")) %>%
  rename_with( ~ str_remove(.x, "\\.\\.\\.\\.[:digit:]")) %>%
  rename_with( ~ str_remove(.x, "\\.[:digit:]")) %>%
  rename_with( ~ str_remove(.x, "\\.[:digit:]")) %>%
  janitor::remove_empty("cols") %>%
  rename_with( ~ str_remove(.x, "[:digit:]$"))

test_join <- full_join(diagnostic_pivot, data2_clean)

vis_miss(test_join) +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 150)) +
  # scale_fill_manual(values = col(2)) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8, face = "bold", margin = margin(0,0,0,0)),
        axis.text.x = element_text(angle = 0),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  ggsave(here::here("Images/MissingDataDiagnostic.png"), width = 1792/300*5, height = 1067/300*5, dpi = 300)

# Write to google sheet

if (exists(ss)) {
  googledrive::drive_trash(ss)
}
# Spreadsheet creation
ss <- gs4_create("Joined Diagnostic Data", sheets = "Data")
# Write to sheet
test_join %>%
  write_sheet(ss, sheet = "Data")


# Column name comparison
# moodle_names <- colnames(diagnostic_pivot)
# sheets_names <- colnames(data2_clean)
# 
# moodle_names[moodle_names %!in% sheets_names]

















