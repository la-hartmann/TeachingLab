library(tidyverse)
course_survey_replacement <- c("Brownington Central School" = "Brownington Central School, VT",
                               "Building 21 - Allentown" = "Building 21 - Allentown, PA",
                               "Calcasieu Parish" = "Calcasieu Parish, LA",
                               "Freire Charter Schools" = "Freire Charter Schools, PA/DE",
                               "Horizon Charter Schools" = "Horizon Charter Schools, CA",
                               "Kankakee School District" = "Kankakee School District, IL",
                               "Lafayette Parish" = "Lafayette Parish, LA",
                               "Louisiana Department of Education" = "Louisiana Department of Education, LA",
                               "North Bronx School of Empowerment" = "North Bronx School of Empowerment, NY",
                               "NYC District 10 - PS 386" = "NYC District 10 - PS 386, NY",
                               "NYC District 11 - District-wide" = "NYC District 11 - District-wide, NY",
                               "NYC District 11 - IS 287" = "NYC District 11 - IS 287, NY",
                               "NYC District 11 - IS 326" = "NYC District 11 - IS 355, NY",
                               "NYC District 11 - IS 462" = "NYC District 11 - IS 462, NY",
                               "NYC District 11 - MS 127" = "NYC District 11 - MS 127, NY",
                               "NYC District 11 - MS 144" = "NYC District 11 - MS 144, NY",
                               "NYC District 11 - MS 180" = "NYC District 11 - MS 180, NY",
                               "NYC District 11 - PS 103" = "NYC District 11 - PS 103, NY",
                               "NYC District 11 - PS 111" = "NYC District 11 - PS 111, NY",
                               "NYC District 11 - PS 112" = "NYC District 11 - PS 112, NY",
                               "NYC District 11 - PS 121" = "NYC District 11 - PS 121, NY",
                               "NYC District 11 - PS 16" = "NYC District 11 - PS 16, NY",
                               "NYC District 11 - PS 160" = "NYC District 11 - PS 160, NY",
                               "NYC District 11 - PS 175" = "NYC District 11 - PS 175, NY",
                               "NYC District 11 - PS 19" = "NYC District 11 - PS 19, NY",
                               "NYC District 11 - PS 41" = "NYC District 11 - PS 41, NY",
                               "NYC District 11 - PS 468" = "NYC District 11 - PS 468, NY",
                               "NYC District 11 - PS 483" = "NYC District 11 - PS 483, NY",
                               "NYC District 11 - PS 76" = "NYC District 11 - PS 76, NY",
                               "NYC District 11 - PS 83" = "NYC District 11 - PS 83, NY",
                               "NYC District 11 - PS 96" = "NYC District 11 - PS 96, NY",
                               "NYC District 11 - PS/MS 194" = "NYC District 11 - PS/MS 194, NY",
                               "NYC District 11 - PS/MS 498" = "NYC District 11 - PS/MS 498, NY",
                               "NYC District 12 - EMST-IS 190" = "NYC District 12 - EMST-IS 190, N",
                               "NYC District 12 - MS 286" = "NYC District 12 - MS 286, NY",
                               "NYC District 6 - MS311" = "NYC District 6 - MS311, NY",
                               "Orleans Central Supervisory Union" = "Orleans Central Supervisory Union, VT",
                               "Pointe Coupee Parish" = "Pointe Coupee Parish, LA",
                               "San Diego Unified School District" = "San Diego Unified School District, CA",
                               "West Contra Costa USD - Murphy Elementary" = "West Contra Costa USD - Murphy Elementary, CA",
                               "West Contra Costa USD" = "West Contra Costa USD, CA",
                               "Wisconsin Department of Education, WI" = "Wisconsin Department of Public Instruction")

diagnostic_replacement <- c("NYC District 12 - EMST-IS 190, NY" = "NYC District 12 - EMST-IS 190, N",
                            "Rochester City School District - School 3" = "Rochester City School District",
                            "Rochester City School District - School 5" = "Rochester City School District",
                            "Rochester City School District - School 8" = "Rochester City School District",
                            "Rochester City School District - School 45" = "Rochester City School District",
                            "Rochester City School District - School 12" = "Rochester City School District",
                            "Rochester City School District - Wilson Foundation" = "Rochester City School District",
                            "Rochester City School District - Monroe Lower" = "Rochester City School District",
                            "Rochester City School District - Franklin Lower" = "Rochester City School District",
                            "Rochester City School District, NY" = "Rochester City School District",
                            "Wisconsin Department of Education, WI" = "Wisconsin Department of Public Instruction")


course_survey <- readr::read_rds(here::here("data/sy21_22/course_survey_21_22.rds")) %>%
  dplyr::mutate(`Select your site (district, parish, network, or school).` = str_replace_all(`Select your site (district, parish, network, or school).`,
                                                                                      course_survey_replacement))

write_rds(course_survey, here::here("data/mid_year_reports/course_survey_21_22data.rds"))

## State level diagnostic
wisconsin_diagnostic <- surveymonkey::fetch_survey_obj(310477252) %>%
  surveymonkey::parse_survey() %>%
  janitor::clean_names()

#### EDITS HERE ####
# diagnostic_names <- colnames(readr::read_rds(here::here(data/sy21_22/diagnostic.rds)))
# colnames(wisconsin_diagnostic)[which(colnames(wisconsin_diagnostic) %in% diagnostic_names)]

## General diagnostic and bind rows together

diagnostic_survey <- readr::read_rds(here::here("data/sy21_22/diagnostic.rds")) %>%
  full_join(wisconsin_diagnostic) %>%
  mutate(your_site_district_parish_network_or_school_br_br = str_replace_all(your_site_district_parish_network_or_school_br_br,
                                                                             diagnostic_replacement))

write_rds(diagnostic_survey, here::here("data/mid_year_reports/diagnostic.rds"))

### School Leaders: ELA ###
TeachingLab::save_processed_data2(
  data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/SchoolLeadersELA.rds"),
  q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/ela_school_leaders.rds"),
  correct = c(
    "Regular practice with complex texts and their academic language.",
    "Building knowledge through content-rich non-fiction.",
    "What can you infer from Dr. King’s letter about the letter that he received?",
    "In “The Lion, the Witch, and the Wardrobe”, how and why does Edmund change?",
    "It focuses on observations aligned to the ELA and literacy instructional shifts.",
    "It is a coaching tool that supports identifying equitable literacy practices.",
    "The feedback is focused on teacher moves such as the questions the teachers posed as students read the text.",
    "The interaction includes role-playing for the teacher to practice a new strategy which directly addresses a challenge from the observation."
  ),
  save_name = "ela_school_leaders"
)

### ELA General: Cycle of Inquiry - Complex Text ###
TeachingLab::save_processed_data2(
  data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/ELAGeneralCycleofInquiry-ComplexText.rds"),
  q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/ela_cycle_inquiry_complex_text.rds"),
  correct = c(
    "They expect all students, regardless of their reading proficiency or performance, to engage with grade-level texts.",
    "They emphasize text complexity throughout the grades, even in the early years when most students cannot decode.",
    "The length of the text",
    "Fluently reading the text aloud for them",
    "Supplementing the text with a simpler one on the same topic",
    "Emphasis on strategies for making inferences"
  ),
  save_name = "ela_cycle_inquiry_complex_text"
)

### ELA General: Cycle of Inquiry - Speaking & Listening ###
TeachingLab::save_processed_data2(
  data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/ELAGeneralCycleofInquiry-Speaking&Listening.rds"),
  q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/ela_cycle_inquiry_speaking_listening_correct.rds"),
  correct = c(
    "They ensure every student has a voice and their ideas are heard and recognized as being valuable.",
    "Every student is engaged and held accountable for his or her learning.",
    "They provide every student with the opportunity to process their ideas with their peers.",
    "Most of the conversation takes place between students.",
    "The students analyze chunks of text to see how they fit together."
  ),
  save_name = "ela_cycle_inquiry_speaking_listening"
)

### ELA Foundational Skills: Bootcamp ###
TeachingLab::save_processed_data2(
  data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/ELABootcamp-FoundationalSkillsBootcampSkills(K-2).rds"),
  q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/ela_foundational_skills.rds"),
  correct = c(
    "Print concepts",
    "Phonological awareness",
    "Fluency",
    "It prompts students to use context clues and pictures to decode words",
    "Utilize a variety of ongoing assessment data to determine the focus of instruction for small groups",
    "Group students by their ongoing phase of development with regard to the foundational skills"
  ),
  save_name = "ela_foundational_skills"
)

### ELA General Bootcamp ###
TeachingLab::save_processed_data2(
  data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/ELABootcamp-General.rds"),
  q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/ela_general_bootcamp.rds"),
  correct = c(
    "Regular practice with complex texts and their academic language.",
    "Building knowledge through content-rich non-fiction.",
    "A complex text that is worthy of reading multiple times.",
    "Ability to read complex text independently and proficiently.",
    "Selecting a text that is at or above the grade-level complexity.",
    "Selecting a text that is rich in meaning.",
    "Read the complex text aloud for students.",
    "Read aloud a simple article to build knowledge of the topic while students follow along."
  ),
  save_name = "ela_general_bootcamp"
)

### ELA: Cycle of Inquiry - Curriculum Flex Foundational Skills ###
TeachingLab::save_processed_data2(
  data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/ELACycleofInquiry-CurriculumFlexFoundationalSkills.rds"),
  q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/ela_cycle_inquiry_curriculum_flex.rds"),
  correct = c(
    "Print concepts",
    "Phonological awareness",
    "Fluency",
    "It focuses on observations aligned to the Science of Reading and effective foundational skills instruction.",
    "It supports identifying equitable literacy practices in the foundational skills.",
    "The feedback is focused on teacher moves such as the questions the teachers posed as students read the text.",
    "The interaction includes role-playing for the teacher to practice a new strategy which directly addresses a challenge from the observation."
  ),
  save_name = "ela_cycle_inquiry_curriculum_flex"
)

### ELA Guidebooks Diverse Learners: Bootcamp - Leader ###
TeachingLab::save_processed_data2(
  data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/ELAGuidebooksDiverseLearnersBootcamp-Leader.rds"),
  q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners.rds"),
  correct = c(
    "Some students need targeted additional support outside of their ELA block.",
    "Students who need it should have practice with the text before they engage with that text in their ELA block.",
    "Plan with the interventionist, Mr. Liu, to have the students work with him to analyze exemplar written responses for key elements a few days before the writing task is assigned."
  ),
  save_name = "ela_guidebooks_diverse_learners_bootcamp_leader"
)

### ELA Guidebooks Diverse Learners: Bootcamp - Teacher ###
TeachingLab::save_processed_data2(
  data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/ELAGuidebooksDiverseLearnersBootcamp-Teacher.rds"),
  q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_bootcamp_teacher.rds"),
  correct = c(
    "Some students need targeted additional support outside of their ELA block.",
    "Students who need it should have practice with the text before they engage with that text in their ELA block.",
    "Plan with the interventionist, Mr. Liu, to have the students work with him to analyze exemplar written responses for key elements a few days before the writing task is assigned.",
    "Students nominate a set of discussion agreements to use in their class.",
    "The teacher holds a mediation conversation for two students after witnessing one student making a derogatory comment toward the other.",
    "To help teachers plan to use the supports and allow for students to preview the skills that diverse learners might need to engage with grade-level texts and tasks ahead of whole-class instruction."
  ),
  save_name = "ela_guidebooks_diverse_learners_bootcamp_teacher"
)

### ELA Guidebooks Diverse Learners: Bootcamp - Writing ###
TeachingLab::save_processed_data2(
  data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/ELAGuidebooksDiverseLearnersBootcampWriting.rds"),
  q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_bootcamp_writing.rds"),
  correct = c(
    "Students need to be explicitly taught how to write.",
    "Students should  plan out what they’re going to write before beginning to write.",
    "Identify incorrect uses of punctuation and correct them."
  ),
  save_name = "ela_guidebooks_diverse_learners_bootcamp_writing"
)

### ELA: Guidebooks Diverse Learners Cycle of Inquiry - Vocabulary ###
TeachingLab::save_processed_data2(
  data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/ELAGuidebooksDiverseLearnersCycleofInquiry-Vocabulary.rds"),
  q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_vocabulary.rds"),
  correct = c(
    "Words likely to appear in cross-disciplinary complex texts the students will read in the future.",
    "Words that are part of a semantic network.",
    "Adult-child conversational exchanges.",
    "Make instructional decisions based on evidence of student work.",
    "Identify gaps that should be addressed.",
    "Frayer Model: having students create definitions, examples, nonexamples, characteristics, and a visual representation of a new vocabulary word connecting new learning about that word to what they already know."
  ),
  save_name = "ela_guidebooks_diverse_learners_vocabulary"
)

### ELA EL: HQIM & Enrichment ###
TeachingLab::save_processed_data2(
  data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/ELAHQIM&Enrichment.rds"),
  q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/el_ela_hqim_enrichment.rds"),
  correct = c(
    "Gifted learners have special needs in the classroom that fall into these categories: Cognitive, Creative, Affective, Behavioral.",
    "Sequencing the layers of Depth and Complexity to support key standards yields the highest-impact enrichment.",
    "The Depth and Complexity Framework is a differentiation tool that can raise the thinking level for all students.",
    "Includes universal screening of all students, multiple tiers of instruction and support services, and integrated data collection and assessment systems to inform decisions at each tier of instruction"
  ),
  save_name = "el_ela_hqim_enrichment"
)

########## Math Section ##########
### Math: Accelerating Learning ###
TeachingLab::save_processed_data2(
  data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/MathAcceleratingLearning.rds"),
  q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/math_accelerating_learning.rds"),
  correct = c(
    "Identifying unfinished learning leading up to the current topic and teach 1-2 lessons targeting those prerequisites at the beginning of the topic.",
    "Pull the 6 students for a small group to discuss the connections between different strategies they’ve used in previous grades.",
    "Plan an activity with multiple entry points to engage the whole class in.",
    "Trying to address every gap a student has",
    "Choosing content for intervention based solely on students’ weakest area",
    "Stick to grade-level content and instructional rigor"
  ),
  save_name = "math_accelerating_learning"
)

### Math: Accelerating Learning: EIC ###
TeachingLab::save_processed_data(
  data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/MathAcceleratingLearning.rds"),
  q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/math_accelerating_learning.rds"),
  correct = c(
    "Identifying unfinished learning leading up to the current topic and teach 1-2 lessons targeting those prerequisites at the beginning of the topic.",
    "Pull the 6 students for a small group to discuss the connections between different strategies they’ve used in previous grades.",
    "Plan an activity with multiple entry points to engage the whole class in.",
    "Trying to address every gap a student has",
    "Choosing content for intervention based solely on students’ weakest area",
    "Stick to grade-level content and instructional rigor"
  ),
  save_name = "math_accelerating_learning_eic"
)

### Math: Bootcamp EIC ###
TeachingLab::save_processed_data2(
  data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/MathBootcamp-EIC.rds"),
  q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/math_bootcamp.rds"),
  correct = c(
    "Going deeper into fewer math topics.",
    "Making connections between math topics across grades.",
    "Unguided problem solving lessons are the least effective type of math lesson.",
    "Building deep understanding with fewer math topics is more effective than covering a broader range of math topics.",
    "Creating opportunities for students to practice saying out loud how they solved for a problem.",
    "Creating opportunities",
    "Students do not need to understand English completely before they can start making sense of math instruction in English.",
    "An appropriate scaffold for language development is to allow students to make charts, diagrams, and other visual representations of their understanding."
  ),
  save_name = "math_bootcamp_eic"
)

### Math: Bootcamp ###
TeachingLab::save_processed_data2(
  data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/MathBootcamp.rds"),
  q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/math_bootcamp.rds"),
  correct = c(
    "Going deeper into fewer math topics.",
    "Making connections between math topics across grades.",
    "Unguided problem solving lessons are the least effective type of math lesson.",
    "Building deep understanding with fewer math topics is more effective than covering a broader range of math topics.",
    "Creating opportunities for students to practice saying out loud how they solved for a problem.",
    "Students do not need to understand English completely before they can start making sense of math instruction in English.",
    "An appropriate scaffold for language development is to allow students to make charts, diagrams, and other visual representations of their understanding."
  ),
  save_name = "math_bootcamp"
)

### Math Cycle of Inquiry I - Eliciting Student Thinking ###
TeachingLab::save_processed_data2(
  data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/MathCycleofInquiryI-ElicitingStudentThinking.rds"),
  q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/math_cycle_inquiry_1_elicit_student_thinking.rds"),
  correct = c(
    "How do we know that y=3x+2 represents a linear relationship?",
    "What key features of this graph tells us that the line represents a proportional relationship?",
    "How did Tessa think about this word problem?",
    "What connections do you see between Tessa and Fede’s strategies?",
    "The next day, a teacher leads a discussion analyzing a few different students’ solutions to the previous day’s problem set and connecting solutions to a learning goal.",
    "A plan for how to students should solve each step of a task",
    "Intentionally asking focusing questions of students who may have been historically pushed out of math success in the past",
    "Analyzing video or audio recordings of one’s own instruction regularly for bias",
    "Sharing frustrations"
  ),
  save_name = "math_cycle_of_inquiry_i"
)

### Math: Cycle of Inquiry V- Sequencing and Connecting Representations ###
TeachingLab::save_processed_data2(
  data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/MathCycleofInquiryV-SequencingandConnectingRepresentations.rds"),
  q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/math_cycle_inquiry_5.rds"),
  correct = c(
    "Anticipate, Monitor, Select, Sequence, Connect",
    "Emphasize the importance of planning to create an engaging mathematical discussion",
    "We lead students to a pre-planned mathematical idea during Connecting.",
    "We use student data to inform our planning during Anticipation.",
    "We can gather a variety of student work during Selecting.",
    "Presents tasks that offer multiple entry points",
    "Structures collaboration to use varying math knowledge and skills to solve complex problems",
    "How do I identify and support mathematical contributions from students with different strengths and levels of confidence?"
  ),
  save_name = "math_cycle_inquiry_iv"
)



el_ela_hqim_enrichment <- read_rds(here::here("data/mid_year_reports/knowledge_assessments/el_ela_hqim_enrichment.rds")) %>%
  mutate(know_assess = "el_ela_hqim_enrichment")
ela_cycle_inquiry_complex_text <- read_rds(here::here("data/mid_year_reports/knowledge_assessments/ela_cycle_inquiry_complex_text.rds")) %>%
  mutate(know_assess = "ela_cycle_inquiry_complex_text")
ela_cycle_inquiry_curriculum_flex <- read_rds(here::here("data/mid_year_reports/knowledge_assessments/ela_cycle_inquiry_curriculum_flex.rds")) %>%
  mutate(know_assess = "ela_cycle_inquiry_curriculum_flex")
ela_cycle_inquiry_speaking_listening <- read_rds(here::here("data/mid_year_reports/knowledge_assessments/ela_cycle_inquiry_speaking_listening.rds")) %>%
  mutate(know_assess = "ela_cycle_inquiry_speaking_listening")
ela_foundational_skills <- read_rds(here::here("data/mid_year_reports/knowledge_assessments/ela_foundational_skills.rds")) %>%
  mutate(know_assess = "ela_foundational_skills")
ela_general_bootcamp <- read_rds(here::here("data/mid_year_reports/knowledge_assessments/ela_general_bootcamp.rds")) %>%
  mutate(know_assess = "ela_general_bootcamp")
ela_guidebooks_diverse_learners_bootcamp_leader <- read_rds(here::here("data/mid_year_reports/knowledge_assessments/ela_guidebooks_diverse_learners_bootcamp_leader.rds")) %>%
  mutate(know_assess = "ela_guidebooks_diverse_learners_bootcamp_leader")
ela_guidebooks_diverse_learners_bootcamp_teacher <- read_rds(here::here("data/mid_year_reports/knowledge_assessments/ela_guidebooks_diverse_learners_bootcamp_teacher.rds")) %>%
  mutate(know_assess = "ela_guidebooks_diverse_learners_bootcamp_teacher")
ela_guidebooks_diverse_learners_bootcamp_writing <- read_rds(here::here("data/mid_year_reports/knowledge_assessments/ela_guidebooks_diverse_learners_bootcamp_writing.rds")) %>%
  mutate(know_assess = "ela_guidebooks_diverse_learners_bootcamp_writing")
ela_guidebooks_diverse_learners_vocabulary <- read_rds(here::here("data/mid_year_reports/knowledge_assessments/ela_guidebooks_diverse_learners_vocabulary.rds")) %>%
  mutate(know_assess = "ela_guidebooks_diverse_learners_vocabulary")
ela_school_leaders <- read_rds(here::here("data/mid_year_reports/knowledge_assessments/ela_school_leaders.rds")) %>%
  mutate(know_assess = "ela_school_leaders")
math_bootcamp_eic <- read_rds(here::here("data/mid_year_reports/knowledge_assessments/math_bootcamp_eic.rds")) %>%
  mutate(know_assess = "math_bootcamp_eic")
math_bootcamp <- read_rds(here::here("data/mid_year_reports/knowledge_assessments/math_bootcamp.rds")) %>%
  mutate(know_assess = "math_bootcamp")
math_cycle_inquiry_iv <- read_rds(here::here("data/mid_year_reports/knowledge_assessments/math_cycle_inquiry_iv.rds")) %>%
  mutate(know_assess = "math_cycle_inquiry_iv")
math_cycle_of_inquiry_i <- read_rds(here::here("data/mid_year_reports/knowledge_assessments/math_cycle_of_inquiry_i.rds")) %>%
  mutate(know_assess = "math_cycle_of_inquiry_i")
math_accelerating_learning <- read_rds(here::here("data/mid_year_reports/knowledge_assessments/math_cycle_of_inquiry_i.rds")) %>%
  mutate(know_assess = "math_accelerating_learning")

all_knowledge_assessments <- el_ela_hqim_enrichment %>%
  full_join(ela_foundational_skills) %>%
  full_join(ela_foundational_skills) %>%
  full_join(ela_general_bootcamp) %>%
  full_join(ela_guidebooks_diverse_learners_bootcamp_leader) %>%
  full_join(ela_guidebooks_diverse_learners_bootcamp_teacher) %>%
  full_join(ela_guidebooks_diverse_learners_bootcamp_writing) %>%
  full_join(ela_school_leaders) %>%
  full_join(math_bootcamp_eic) %>%
  full_join(math_bootcamp) %>%
  full_join(math_cycle_inquiry_iv) %>%
  full_join(math_cycle_of_inquiry_i) %>%
  mutate(site = str_replace_all(site, c("Rochester City School District - School 3" = "Rochester City School District",
                                        "Rochester City School District - School 8" = "Rochester City School District",
                                        "Rochester City School District - School 12" = "Rochester City School District",
                                        "Rochester City School District - Franklin Lower" = "Rochester City School District",
                                        "EMST-IS 190" = "NYC District 12 - EMST-IS 190, N",
                                        "Wisconsin Department of Education, WI" = "Wisconsin Department of Public Instruction"))) %>%
  mutate(question = str_remove_all(question, "_\\d"))

write_rds(all_knowledge_assessments, 
          here::here("data/mid_year_reports/knowledge_assessments.rds"))










