library(tidyverse)
library(TeachingLab)

misssissippi_sheet_pre <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1yrqXouJ84glL-4uH7Nw-47HqhzQP1jINDgRy8nCUaxs/edit#gid=777182936",
                                     sheet = "SCORED_pre")
misssissippi_sheet_post <- read_sheet("https://docs.google.com/spreadsheets/d/1yrqXouJ84glL-4uH7Nw-47HqhzQP1jINDgRy8nCUaxs/edit#gid=777182936",
                                     sheet = "SCORED_post")

grade_sheet <- function(sheet) {
  
  scores <- sheet |>
    drop_na(`Score (0-2)`) |>
    pull(`Score (0-2)`)
  
  100 * round(sum(scores == 2, na.rm = T)/length(scores), 2)
}

grade_sheet(misssissippi_sheet_pre)
grade_sheet(misssissippi_sheet_post)

nm_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/14SsiJIPbtASz9WZMvXdWsleu5NKLjST1dh8ITpBU2XI/edit#gid=777182936",
           sheet = "6-8 SCORED")

nm_sheet |>
  drop_na(`Score (0-2)`) |>
  group_by(Batch) |>
  summarise(score = round(sum(`Score (0-2)` == 2, na.rm = T)/length(`Score (0-2)`), 2))

nm_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/14SsiJIPbtASz9WZMvXdWsleu5NKLjST1dh8ITpBU2XI/edit#gid=777182936",
                       sheet = "9-12 SCORED")

nm_sheet |>
  drop_na(`Score (0-2)`) |>
  group_by(Batch) |>
  summarise(score = round(sum(`Score (0-2)` == 2, na.rm = T)/length(`Score (0-2)`), 2))

student_work_scores <- x
crse_scores <- y
mindsets_matched <- z
teacher_content_knowledge_standard_deviation
increase_in_evidence_based_practices