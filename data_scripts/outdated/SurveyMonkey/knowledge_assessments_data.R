################################ Data Loading Script for Knowledge Assessments Dashboard ################################

################################################## Get the Data #########################################################
# options(sm_oauth_token = Sys.getenv("knowledge_token"))
# 
# all_knowledge_assessments <- TeachingLab::get_knowledge_assessments(update = TRUE, year = "21_22")
# 
# readr::write_rds(
#   all_knowledge_assessments,
#   here::here("data/sy21_22/knowledge_assessments.rds")
# )
# 
# questions_and_answers <- list.files(here::here("dashboards/KnowledgeAssessments/data/questions_and_answers"),
#            full.names = T)
# 
# all_questions_and_answers <- purrr::map_dfr(questions_and_answers, readr::read_rds)
# 
# readr::write_rds(all_questions_and_answers, "data/sy21_22/all_knowledge_questions_and_answers.rds")
