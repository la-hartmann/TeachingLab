# Educator Survey #
educator_survey <- read.csv("data/educator_survey.csv") |>
  data_setup()
followup_educator <- read.csv("data/followup_educator.csv") |>
  data_setup()

# Participant Feedback #
session_survey <- read.csv("data/session_survey.csv") |>
  data_setup()
course_survey <- read.csv("data/course_survey.csv") |>
  data_setup()
ongoing_coaching <- read.csv("data/ongoing_coaching.csv") |>
  data_setup()
end_coaching <- read.csv("data/end_coaching.csv") |>
  data_setup()

# IPG Forms #
ipg_forms <- read.csv("data/ipg_forms.csv")

# Knowledge Assessments #
knowledge_assessments <- readRDS("data/knowledge_assessments.rds")

sites_list <- c("All Sites", sort(unique(c(unique(session_survey$site), unique(course_survey$site), 
                       unique(ongoing_coaching$site), unique(end_coaching$site),
                       unique(educator_survey$site), unique(ipg_forms$site)))))

### Add Calibri Fonts
font_add(family = "Calibri", regular = "www/Calibri.ttf")
font_add(family = "Calibri Bold", regular = "www/Calibri Bold.ttf")
font_add(family = "Roboto", regular = "www/Roboto-Black.ttf")
###

