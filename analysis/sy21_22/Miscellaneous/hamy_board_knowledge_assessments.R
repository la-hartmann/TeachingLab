library(tidyverse)
library(TeachingLab)

knowledge_assessments <- readr::read_rds(here::here("data/mid_year_reports/knowledge_assessments.rds"))



walk(unique(knowledge_assessments$know_assess), ~
      TeachingLab::know_assess_summary(data = knowledge_assessments, 
                                       know_assess = .x,
                                       summary_path = "hamy_board_summary"))
