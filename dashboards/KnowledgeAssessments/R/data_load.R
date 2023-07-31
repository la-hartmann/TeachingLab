#### Read in Data ####

ela_general_bootcamp <- readr::read_rds("data/processed/ela_general_bootcamp.rds")
ela_foundational_skills <- readr::read_rds("data/processed/ela_foundational_skills.rds")
ela_guidebooks_diverse_learners_bootcamp_leader <- readr::read_rds("data/processed/ela_guidebooks_diverse_learners_bootcamp_leader.rds")
ela_guidebooks_diverse_learners_bootcamp_teacher <- readr::read_rds("data/processed/ela_guidebooks_diverse_learners_bootcamp_teacher.rds")
ela_guidebooks_diverse_learners_bootcamp_writing <- readr::read_rds("data/processed/ela_guidebooks_diverse_learners_bootcamp_writing.rds")
ela_school_leaders <- readr::read_rds("data/processed/ela_school_leaders.rds")
el_ela_hqim_enrichment <- readr::read_rds("data/processed/el_ela_hqim_enrichment.rds")
math_bootcamp_eic <- readr::read_rds("data/processed/math_bootcamp_eic.rds")
math_bootcamp <- readr::read_rds("data/processed/math_bootcamp.rds")
math_cycle_inquiry_iv <- readr::read_rds("data/processed/math_cycle_inquiry_iv.rds")
math_cycle_of_inquiry_i <- readr::read_rds("data/processed/math_cycle_of_inquiry_i.rds")














# library(surveymonkey)
# library(magrittr)
# 
# options(sm_oauth_token = "wD.rd9HKenA2QV2Z2zV.kJwL7533YR3TcbP0Ii7--tHadLRlID-hv5Kz8oAVvHsKXUSn9KRnzz31DcKqb8vcLMqjuHjYz7r3vW7kQj3TZ3oboSG5mvxi5ZijlFhL8ylm")
# 
# # surveys <- surveymonkey::browse_surveys()
# ids_surveys <- tibble::tibble(
#   title = c("ELA: Guidebooks Diverse Learners Bootcamp - Teacher",
#             "ELA: CRSE PLC", "Math: Cycle of Inquiry V- Sequencing and Connecting Representations",
#             "ELA: Bootcamp - Foundational Skills Bootcamp Skills (K-2)",
#             "ELA: Guidebooks Diverse Learners Bootcamp Writing", "Math: Bootcamp",
#             "ELA: Bootcamp - General", "Math: Cycle of Inquiry I - Eliciting Student Thinking",
#             "ELA: Guidebooks Diverse Learners Bootcamp - Leader", "School Leaders: ELA",
#             "Math: Bootcamp - EIC", "Math: Accelerating Learning", "ELA: HQIM & Enrichment",
#             "ELA General: Cycle of Inquiry - Complex Text", "ELA: Guidebooks Diverse Learners Cycle of Inquiry - Vocabulary",
#             "ELA: Guidebooks Diverse Learners Cycle of Inquiry - Fluency",
#             "ELA: Guidebooks Cycle of Inquiry 1", "ELA: Guidebooks Cycle of Inquiry 2"),
#   id = c("310008951", "312484554", "311404789", "309842602",
#          "310775522", "309842333", "309800566", "311433379", "311069987",
#          "312485414", "309893890", "310768681", "310009771", "311404498",
#          "310776879", "310776199", "310778066", "310777524")
# ) %>%
#   dplyr::mutate(count = dplyr::row_number())
# 
# fetch_survey_2 <- function(id, name) {
#   #### Get survey object ####
#   survey <- surveymonkey::fetch_survey_obj(id = id)
#   #### Parse survey ####
#   survey_parsed <- survey %>%
#     surveymonkey::parse_survey()
#   #### Assign all to environment with format surveyname ####
#   assign(value = survey_parsed, x = paste0("survey", name), envir = .GlobalEnv)
#   #### Compile dataframe of all the surveys new names and add ####
#   name_df <- tibble::tibble(names = paste0("survey", name)) %>%
#     dplyr::mutate(count = name) %>%
#     dplyr::left_join(ids_surveys, by = "count") %>%
#     dplyr::mutate(title = stringr::str_replace_all(title, " ", "")) %>%
#     dplyr::mutate(title = stringr::str_replace_all(title, ":", ""))
#   print(name_df)
#   #### Write to data folder with original name####
#   purrr::map2(.x = name_df$names, .y = name_df$title, ~ readr::write_rds(x = get(.x), file = paste0(here::here("dashboards/KnowledgeAssessments/data/"), .y, ".rds")))
# }
# 
# purrr::map2(.x = ids_surveys$id, .y = ids_surveys$count, ~ fetch_survey_2(id = .x, name = .y))

##### Temporary #####
# diagnostic <- readr::read_rds(here::here("dashboards/KnowledgeAssessments/data/diagnostic.rds"))
diagnostic <- readr::read_rds("data/sy21_22/diagnostic.rds")

#### Survey 1: ELA General Bootcamp ####
# ela_general_bootcamp <- readr::read_rds(here::here("dashboards/KnowledgeAssessments/data/ELABootcamp-General.rds"))
# ela_general_bootcamp <- readr::read_rds("data/ELABootcamp-General.rds")
# 
# ela_general_bootcamp_correct <- tibble::tibble(question = c("Which of the following are literacy instructional shifts? Select all that apply. - Regular practice with complex texts and their academic language.", 
#                                                             "Which of the following are literacy instructional shifts? Select all that apply. - Building knowledge through content-rich non-fiction.", 
#                                                             "Which of the following are literacy instructional shifts? Select all that apply. - Equal balance of text-based writing and writing from personal experiences.", 
#                                                             "Which of the following are literacy instructional shifts? Select all that apply. - Regular opportunities for direct instruction on reading comprehension strategies.", 
#                                                             "When designing literacy lessons, teachers should start with which of the following?", 
#                                                             "Which of the following is the single biggest differentiator of college and career-readiness?", 
#                                                             "Which of the following approaches for selecting texts for whole-class reading instruction are aligned with post-shifts literacy instruction? Select all that apply. - Selecting a text that is at or above the grade-level complexity.", 
#                                                             "Which of the following approaches for selecting texts for whole-class reading instruction are aligned with post-shifts literacy instruction? Select all that apply. - Selecting a text that is rich in meaning.", 
#                                                             "Which of the following approaches for selecting texts for whole-class reading instruction are aligned with post-shifts literacy instruction? Select all that apply. - Selecting an adapted version of the main text for below-grade-level readers.", 
#                                                             "Which of the following approaches for selecting texts for whole-class reading instruction are aligned with post-shifts literacy instruction? Select all that apply. - Selecting a text for the class based on student interest.", 
#                                                             "Which of the following describe strategies for supporting struggling readers? Select all that apply. - Read the complex text aloud for students.", 
#                                                             "Which of the following describe strategies for supporting struggling readers? Select all that apply. - Read aloud a simple article to build knowledge of the topic while students follow along.", 
#                                                             "Which of the following describe strategies for supporting struggling readers? Select all that apply. - Have students read the full text multiple times to find the main idea.", 
#                                                             "Which of the following describe strategies for supporting struggling readers? Select all that apply. - Ask simpler questions about the same text."),
#                                                   coding = c("Regular practice with complex texts and their academic language.",
#                                                              "Building knowledge through content-rich non-fiction.", 
#                                                              "incorrect",
#                                                              "incorrect",
#                                                              "A complex text that is worthy of reading multiple times.",
#                                                              "Ability to read complex text independently and proficiently.",
#                                                              "Selecting a text that is at or above the grade-level complexity.",
#                                                              "Selecting a text that is rich in meaning.",
#                                                              "incorrect",
#                                                              "incorrect",
#                                                              "Read the complex text aloud for students.",
#                                                              "Read aloud a simple article to build knowledge of the topic while students follow along.",
#                                                              "incorrect",
#                                                              "incorrect"))
# ela_general_bootcamp_percent <- purrr::map2_df(ela_general_bootcamp_correct$question, 
#                                         ela_general_bootcamp_correct$coding, ~ TeachingLab::score_question(data = ela_general_bootcamp, 
#                                                                                               question = .x,
#                                                                                               coding = .y,
#                                                                                               grouping = "Please select your site (district, parish, network, or school)"))
# #### Making Survey 2 ELA General Cycle of Inquiry ####
# #### Not complete as of 10-29-31, will need updating on questions, answers ####
# # ela_cycle_of_inquiry <- readr::read_rds(here::here("dashboards/KnowledgeAssessments/data/ELAGeneralCycleofInquiry-ComplexText.rds"))
# ela_cycle_of_inquiry <- readr::read_rds("data/ELAGeneralCycleofInquiry-ComplexText.rds")
# 
# ela_cycle_of_inquiry_correct <- tibble::tibble(question = c("For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Print concepts", 
#                                                "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Phonological awareness", 
#                                                "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Vocabulary development", 
#                                                "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Fluency", 
#                                                "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Reading comprehension", 
#                                                "A structured phonics program is important in K-2 for the following reasons EXCEPT:", 
#                                                "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Utilize a variety of ongoing assessment data to determine the focus of instruction for small groups", 
#                                                "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Group students by their ongoing phase of development with regard to the foundational skills", 
#                                                "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Only provide foundational skills instruction during small group time", 
#                                                "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Adhere to a same structure of number of groups and members of groups for the entirety of the year"),
#                                   coding = c("Print Concepts", "Phonological awareness", "incorrect", "Fluency", "incorrect",
#                                              "It prompts students to use context clues and pictures to decode words",
#                                              "Utilize a variety of ongoing assessment data to determine the focus of instruction for small groups",
#                                              "Group students by their ongoing phase of development with regard to the foundational skills",
#                                              "incorrect",
#                                              "incorrect"))
# ela_cycle_of_inquiry_percent <- purrr::map2_df(ela_cycle_of_inquiry_correct$question, 
#                                                ela_cycle_of_inquiry_correct$coding, ~ TeachingLab::score_question(data = ela_cycle_of_inquiry, 
#                                                                                               question = .x,
#                                                                                               coding = .y,
#                                                                                               grouping = "Please select your site (district, parish, network, or school)"))
# 
# #### Making Survey 3 ELA Foundational Skills Bootcamp ####
# # ela_foundational_skills <- readr::read_rds(here::here("dashboards/KnowledgeAssessments/data/ELABootcamp-FoundationalSkillsBootcampSkills(K-2).rds"))
# ela_foundational_skills <- readr::read_rds("data/ELABootcamp-FoundationalSkillsBootcampSkills(K-2).rds")
# 
# ela_foundational_skills_correct <- tibble::tibble(question = c("For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Print concepts", 
#                                                                "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Phonological awareness", 
#                                                                "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Vocabulary development", 
#                                                                "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Fluency", 
#                                                                "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Reading comprehension", 
#                                                                "A structured phonics program is important in K-2 for the following reasons EXCEPT:", 
#                                                                "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Utilize a variety of ongoing assessment data to determine the focus of instruction for small groups", 
#                                                                "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Group students by their ongoing phase of development with regard to the foundational skills", 
#                                                                "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Only provide foundational skills instruction during small group time", 
#                                                                "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Adhere to a same structure of number of groups and members of groups for the entirety of the year"),
#                                                   coding = c("Print Concepts", "Phonological awareness", "incorrect", "Fluency", "incorrect",
#                                                              "It prompts students to use context clues and pictures to decode words",
#                                                              "Utilize a variety of ongoing assessment data to determine the focus of instruction for small groups",
#                                                              "Group students by their ongoing phase of development with regard to the foundational skills",
#                                                              "incorrect",
#                                                              "incorrect"))
# ela_foundational_skills_percent <- purrr::map2_df(ela_foundational_skills_correct$question, 
#                                                   ela_foundational_skills_correct$coding, ~ TeachingLab::score_question(data = ela_foundational_skills, 
#                                                                                                                         question = .x,
#                                                                                                                         coding = .y,
#                                                                                                                         grouping = "Please select your site (district, parish, network, or school)"))
# #### Making Survey 4 ELA Guidebooks Cycle of Inquiry 1 ####
# #### Not complete as of 10-29-31, will need updating on questions, answers ####
# # ela_guidebooks_cycle1 <- readr::read_rds(here::here("dashboards/KnowledgeAssessments/data/ELAGuidebooksCycleofInquiry1.rds"))
# ela_guidebooks_cycle1 <- readr::read_rds("data/ELAGuidebooksCycleofInquiry1.rds")
# 
# ela_guidebooks_cycle1_correct <- tibble::tibble(question = c("For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Print concepts", 
#                                                                "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Phonological awareness", 
#                                                                "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Vocabulary development", 
#                                                                "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Fluency", 
#                                                                "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Reading comprehension", 
#                                                                "A structured phonics program is important in K-2 for the following reasons EXCEPT:", 
#                                                                "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Utilize a variety of ongoing assessment data to determine the focus of instruction for small groups", 
#                                                                "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Group students by their ongoing phase of development with regard to the foundational skills", 
#                                                                "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Only provide foundational skills instruction during small group time", 
#                                                                "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Adhere to a same structure of number of groups and members of groups for the entirety of the year"),
#                                                   coding = c("Print Concepts", "Phonological awareness", "incorrect", "Fluency", "incorrect",
#                                                              "It prompts students to use context clues and pictures to decode words",
#                                                              "Utilize a variety of ongoing assessment data to determine the focus of instruction for small groups",
#                                                              "Group students by their ongoing phase of development with regard to the foundational skills",
#                                                              "incorrect",
#                                                              "incorrect"))
# ela_guidebooks_cycle1_percent <- purrr::map2_df(ela_guidebooks_cycle1_correct$question, 
#                                                 ela_guidebooks_cycle1_correct$coding, ~ TeachingLab::score_question(data = ela_guidebooks_cycle1, 
#                                                                                                                         question = .x,
#                                                                                                                         coding = .y,
#                                                                                                                         grouping = "Please select your site (district, parish, network, or school)"))
# 
# #### Making Survey 5 ELA Guidebooks Cycle of Inquiry 2 ####
# #### Not complete as of 10-29-31, will need updating on questions, answers ####
# # ela_guidebooks_cycle2 <- readr::read_rds(here::here("dashboards/KnowledgeAssessments/data/ELAGuidebooksCycleofInquiry2.rds"))
# ela_guidebooks_cycle2 <- readr::read_rds("data/ELAGuidebooksCycleofInquiry2.rds")
# 
# ela_guidebooks_cycle2_correct <- tibble::tibble(question = c("For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Print concepts", 
#                                                              "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Phonological awareness", 
#                                                              "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Vocabulary development", 
#                                                              "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Fluency", 
#                                                              "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Reading comprehension", 
#                                                              "A structured phonics program is important in K-2 for the following reasons EXCEPT:", 
#                                                              "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Utilize a variety of ongoing assessment data to determine the focus of instruction for small groups", 
#                                                              "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Group students by their ongoing phase of development with regard to the foundational skills", 
#                                                              "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Only provide foundational skills instruction during small group time", 
#                                                              "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Adhere to a same structure of number of groups and members of groups for the entirety of the year"),
#                                                 coding = c("Print Concepts", "Phonological awareness", "incorrect", "Fluency", "incorrect",
#                                                            "It prompts students to use context clues and pictures to decode words",
#                                                            "Utilize a variety of ongoing assessment data to determine the focus of instruction for small groups",
#                                                            "Group students by their ongoing phase of development with regard to the foundational skills",
#                                                            "incorrect",
#                                                            "incorrect"))
# ela_guidebooks_cycle2_percent <- purrr::map2_df(ela_guidebooks_cycle2_correct$question, 
#                                                 ela_guidebooks_cycle2_correct$coding, ~ TeachingLab::score_question(data = ela_guidebooks_cycle2, 
#                                                                                                                     question = .x,
#                                                                                                                     coding = .y,
#                                                                                                                     grouping = "Please select your site (district, parish, network, or school)"))
# 
# 
# #### Making Survey 6 ELA Guidebooks Diverse Learners: Bootcamp - Leader ####
# # ela_guidebooks_diverse_learners_leader <- readr::read_rds(here::here("dashboards/KnowledgeAssessments/data/ELAGuidebooksDiverseLearnersBootcamp-Leader.rds"))
# ela_guidebooks_diverse_learners_leader <- readr::read_rds("data/ELAGuidebooksDiverseLearnersBootcamp-Leader.rds")
# 
# ela_guidebooks_diverse_learners_leader_correct <- tibble::tibble(
#   question = c("Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Some students need targeted additional support outside of their ELA block.", 
#                "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students who need it should have practice with the text before they engage with that text in their ELA block.", 
#                "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students who need it should be front-loaded with reading comprehension strategies before engaging with a complex grade-level text.", 
#                "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students need to build up all general basic skills before they can take part in Tier 1 instruction.", 
#                "Through observing her students, Mrs. Richards concluded that eight of her students may not know the elements of a written response. There is a culminating writing task that all students will need to complete next week. How can she BEST plan to support these eight students so that they will be successful on this writing task?"),
#   coding = c("Some students need targeted additional support outside of their ELA block.",
#              "Students who need it should have practice with the text before they engage with that text in their ELA block.",
#              "incorrect",
#              "incorrect",
#              "Plan with the interventionist, Mr. Liu, to have the students work with him to analyze some exemplar written responses for key elements a few days before the writing task is assigned."))
# 
# ela_guidebooks_diverse_learners_leader_percent <- purrr::map2_df(ela_guidebooks_diverse_learners_leader_correct$question, 
#                                                                  ela_guidebooks_diverse_learners_leader_correct$coding, ~ TeachingLab::score_question(data = ela_guidebooks_diverse_learners_leader, 
#                                                                                                                     question = .x,
#                                                                                                                     coding = .y,
#                                                                                                                     grouping = "Please select your site (district, parish, network, or school)."))
# #### Making Survey 7 ELA Guidebooks Diverse Learners: Bootcamp - Teacher ####
# # ela_guidebooks_diverse_learners_teacher <- readr::read_rds(here::here("dashboards/KnowledgeAssessments/data/ELAGuidebooksDiverseLearnersBootcamp-Teacher.rds"))
# ela_guidebooks_diverse_learners_teacher <- readr::read_rds("data/ELAGuidebooksDiverseLearnersBootcamp-Teacher.rds")
# 
# ela_guidebooks_diverse_learners_teacher_correct <- tibble::tibble(
#   question = c("Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Some students need targeted additional support outside of their ELA block.", 
#                "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students who need it should have practice with the text before they engage with that text in their ELA block.", 
#                "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students who need it should be front-loaded with reading comprehension strategies before engaging with a complex grade-level text.", 
#                "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students need to build up all general basic skills before they can take part in Tier 1 instruction.", 
#                "Through observing her students, Mrs. Richards concluded that eight of her students may not know the elements of a written response. There is a culminating writing task that all students will need to complete next week. How can she BEST plan to support these eight students so that they will be successful on this writing task?", 
#                "Which of the following observations describe practices for establishing an inclusive and student centered classroom culture? Select all that apply. - Students nominate a set of discussion agreements to use in their class.", 
#                "Which of the following observations describe practices for establishing an inclusive and student centered classroom culture? Select all that apply. - The teacher holds a mediation conversation for two students after witnessing one student making a derogatory comment toward the other.", 
#                "Which of the following observations describe practices for establishing an inclusive and student centered classroom culture? Select all that apply. - Students research one of three 19th century American heroes: Abraham Lincoln, Thomas Edison, and Mark Twain.", 
#                "Which of the following observations describe practices for establishing an inclusive and student centered classroom culture? Select all that apply. - A classroom norm is that “All students must make at least one comment during a class discussion.”", 
#                "What is the ideal use case of the Diverse Learners Planning Guide?"),
#   coding = c("Some students need targeted additional support outside of their ELA block.",
#              "Students who need it should have practice with the text before they engage with that text in their ELA block.",
#              "incorrect",
#              "incorrect",
#              "Plan with the interventionist, Mr. Liu, to have the students work with him to analyze some exemplar written responses for key elements a few days before the writing task is assigned.",
#              "Students nominate a set of discussion agreements to use in their class.",
#              "The teacher holds a mediation conversation for two students after witnessing one student making a derogatory comment toward the other.",
#              "incorrect",
#              "incorrect",
#              "To help teachers plan to use the supports and allow for students to preview the skills that diverse learners might need to engage with grade-level texts and tasks ahead of whole-class instruction."))
# 
# ela_guidebooks_diverse_learners_teacher_percent <- purrr::map2_df(ela_guidebooks_diverse_learners_teacher_correct$question, 
#                                                                  ela_guidebooks_diverse_learners_teacher_correct$coding, ~ TeachingLab::score_question(data = ela_guidebooks_diverse_learners_teacher, 
#                                                                                                                                                       question = .x,
#                                                                                                                                                       coding = .y,
#                                                                                                                                                       grouping = "Please select your site (district, parish, network, or school)."))
# 
# #### Making Survey 8 ELA Guidebooks Diverse Learners: Bootcamp - Writing ####
# # ela_guidebooks_diverse_learners_writing <- readr::read_rds(here::here("dashboards/KnowledgeAssessments/data/ELAGuidebooksDiverseLearnersBootcampWriting.rds"))
# ela_guidebooks_diverse_learners_writing <- readr::read_rds("data/ELAGuidebooksDiverseLearnersBootcampWriting.rds")
# 
# ela_guidebooks_diverse_learners_writing_correct <- tibble::tibble(
#   question = c("Which of the following are true about how most students become better writers? Select all that apply. - Students need to be explicitly taught how to write.", 
#                "Which of the following are true about how most students become better writers? Select all that apply. - Students should  plan out what they’re going to write before beginning to write.", 
#                "Which of the following are true about how most students become better writers? Select all that apply. - Students can become good writers by reading complex texts.", 
#                "Which of the following are true about how most students become better writers? Select all that apply. - Students should have isolated grammar lessons so that they can apply grammar rules in their writing.", 
#                "Which of the following are true about how most students become better writers? Select all that apply. - I’m not sure", 
#                "All of the following are effective ways a teacher can help students write a stronger sentence EXCEPT:"),
#   coding = c("Students need to be explicitly taught how to write.",
#              "Students should  plan out what they’re going to write before beginning to write.",
#              "incorrect",
#              "incorrect",
#              "incorrect",
#              "Identify incorrect uses of punctuation and correct them."))
# 
# ela_guidebooks_diverse_learners_writing_percent <- purrr::map2_df(ela_guidebooks_diverse_learners_writing_correct$question, 
#                                                                   ela_guidebooks_diverse_learners_writing_correct$coding, ~ TeachingLab::score_question(data = ela_guidebooks_diverse_learners_writing, 
#                                                                                                                                                         question = .x,
#                                                                                                                                                         coding = .y,
#                                                                                                                                                         grouping = "Please select your site (district, parish, network, or school)"))
# 
# #### Making Survey 9 ELA Guidebooks Diverse Learners: Bootcamp - Fluency ####
# #### Not complete as of 10-29-31, will need updating on questions, answers ####
# # ela_guidebooks_diverse_learners_fluency <- readr::read_rds(here::here("dashboards/KnowledgeAssessments/data/ELAGuidebooksDiverseLearnersCycleofInquiry-Fluency.rds"))
# ela_guidebooks_diverse_learners_fluency <- readr::read_rds("data/ELAGuidebooksDiverseLearnersCycleofInquiry-Fluency.rds")
# 
# ela_guidebooks_diverse_learners_fluency_correct <- tibble::tibble(
#   question = c("Which of the following are true about how most students become better writers? Select all that apply. - Students need to be explicitly taught how to write.", 
#                "Which of the following are true about how most students become better writers? Select all that apply. - Students should  plan out what they’re going to write before beginning to write.", 
#                "Which of the following are true about how most students become better writers? Select all that apply. - Students can become good writers by reading complex texts.", 
#                "Which of the following are true about how most students become better writers? Select all that apply. - Students should have isolated grammar lessons so that they can apply grammar rules in their writing.", 
#                "Which of the following are true about how most students become better writers? Select all that apply. - I’m not sure", 
#                "All of the following are effective ways a teacher can help students write a stronger sentence EXCEPT:"),
#   coding = c("Students need to be explicitly taught how to write.",
#              "Students should  plan out what they’re going to write before beginning to write.",
#              "incorrect",
#              "incorrect",
#              "incorrect",
#              "Identify incorrect uses of punctuation and correct them."))
# 
# ela_guidebooks_diverse_learners_writing_percent <- purrr::map2_df(ela_guidebooks_diverse_learners_fluency_correct$question, 
#                                                                   ela_guidebooks_diverse_learners_fluency_correct$coding, ~ TeachingLab::score_question(data = ela_guidebooks_diverse_learners_fluency, 
#                                                                                                                                                         question = .x,
#                                                                                                                                                         coding = .y,
#                                                                                                                                                         grouping = "Please select your site (district, parish, network, or school)"))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # ela_general_bootcamp[[25]] %>% unique() %>% purrr::keep( ~ !is.na(.x)) %>% clipr::write_clip() %>% print()
# 
# 
# # score_question(data = ela_foundational_skills, 
# #                question = colnames(ela_foundational_skills)[14],
# #                coding = "Print concepts",
# #                grouping = "Please select your site (district, parish, network, or school)")
