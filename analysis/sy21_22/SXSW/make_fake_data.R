library(tidyverse)
devtools::load_all()
library(TeachingLab)

# fake_data <- tibble::tibble(
#   before_p = 77,
#   after_p = 98,
#   before_n = 107,
#   after_n = 25
# )

knowledge_assessments <- readr::read_rds(here::here("data/mid_year_reports/knowledge_assessments.rds"))

fake_data <- knowledge_assessments %>%
  dplyr::filter(site == "Delaware Department of Education, DE")

all_knowledge_assessments <- c("School Leaders: ELA",
                               "ELA General: Cycle of Inquiry - Complex Text",
                               "ELA General: Cycle of Inquiry - Speaking & Listening",
                               "ELA: Bootcamp - Foundational Skills Bootcamp Skills (K-2)",
                               "ELA: Bootcamp - General",
                               "ELA: CRSE PLC",
                               "ELA: Cycle of Inquiry - Curriculum Flex Foundational Skills",
                               "ELA: Guidebooks Cycle of Inquiry 1",
                               "ELA: Guidebooks Cycle of Inquiry 2",
                               "ELA: Guidebooks Diverse Learners Bootcamp - Leader",
                               "ELA: Guidebooks Diverse Learners Bootcamp - Teacher",
                               "ELA: Guidebooks Diverse Learners Bootcamp Writing",
                               "ELA: Guidebooks Diverse Learners Cycle of Inquiry - Fluency",
                               "ELA: Guidebooks Diverse Learners Cycle of Inquiry - Vocabulary",
                               "ELA: HQIM & Enrichment",
                               "ELA: School Leader Coaching Series",
                               "Math: Accelerating Learning",
                               "Math: Accelerating Learning - EIC",
                               "Math: Bootcamp",
                               "Math: Bootcamp - Curriculum Flexible",
                               "Math: Bootcamp - EIC",
                               "Math: Cycle of Inquiry I - Eliciting Student Thinking",
                               "Math: Cycle of Inquiry I - Eliciting Student Thinking - Curriculum Flexible",
                               "Math: Cycle of Inquiry II - Making Math Visible",
                               "Math: Cycle of Inquiry V- Sequencing and Connecting Representations")

map(all_knowledge_assessments, ~ TeachingLab::fake_know_assess_summary(know_assess = .x))
