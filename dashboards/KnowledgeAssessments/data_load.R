# Load for pipe
library(magrittr)

################################ Data Loading Script for Knowledge Assessments Dashboard ################################

################################################## Get the Data #########################################################

options(sm_oauth_token = "wD.rd9HKenA2QV2Z2zV.kJwL7533YR3TcbP0Ii7--tHadLRlID-hv5Kz8oAVvHsKXUSn9KRnzz31DcKqb8vcLMqjuHjYz7r3vW7kQj3TZ3oboSG5mvxi5ZijlFhL8ylm")

# surveys <- surveymonkey::browse_surveys()
ids_surveys <- tibble::tibble(
  title = c("ELA: Guidebooks Diverse Learners Bootcamp - Teacher",
            "ELA: CRSE PLC", "Math: Cycle of Inquiry V- Sequencing and Connecting Representations",
            "ELA: Bootcamp - Foundational Skills Bootcamp Skills (K-2)",
            "ELA: Guidebooks Diverse Learners Bootcamp Writing", "Math: Bootcamp",
            "ELA: Bootcamp - General", "Math: Cycle of Inquiry I - Eliciting Student Thinking",
            "ELA: Guidebooks Diverse Learners Bootcamp - Leader", "School Leaders: ELA",
            "Math: Bootcamp - EIC", "Math: Accelerating Learning", "ELA: HQIM & Enrichment",
            "ELA General: Cycle of Inquiry - Complex Text", "ELA: Guidebooks Diverse Learners Cycle of Inquiry - Vocabulary",
            "ELA: Guidebooks Diverse Learners Cycle of Inquiry - Fluency",
            "ELA: Guidebooks Cycle of Inquiry 1", "ELA: Guidebooks Cycle of Inquiry 2"),
  id = c("310008951", "312484554", "311404789", "309842602",
         "310775522", "309842333", "309800566", "311433379", "311069987",
         "312485414", "309893890", "310768681", "310009771", "311404498",
         "310776879", "310776199", "310778066", "310777524")
) %>%
  dplyr::mutate(count = dplyr::row_number())

fetch_survey_2 <- function(id, name) {
  #### Get survey object ####
  survey <- surveymonkey::fetch_survey_obj(id = id)
  #### Parse survey ####
  survey_parsed <- survey %>%
    surveymonkey::parse_survey()
  #### Assign all to environment with format surveyname ####
  assign(value = survey_parsed, x = paste0("survey", name), envir = .GlobalEnv)
  #### Compile dataframe of all the surveys new names and add ####
  name_df <- tibble::tibble(names = paste0("survey", name)) %>%
    dplyr::mutate(count = name) %>%
    dplyr::left_join(ids_surveys, by = "count") %>%
    dplyr::mutate(title = stringr::str_replace_all(title, " ", "")) %>%
    dplyr::mutate(title = stringr::str_replace_all(title, ":", ""))
  print(name_df)
  #### Write to data folder with original name####
  purrr::map2(.x = name_df$names, .y = name_df$title, ~ readr::write_rds(x = get(.x), file = paste0(here::here("dashboards/KnowledgeAssessments/data/"), .y, ".rds")))
}

purrr::map2(.x = ids_surveys$id, .y = ids_surveys$count, ~ fetch_survey_2(id = .x, name = .y))
########################################################################################################################



### ELA General Bootcamp ###
ela_general_bootcamp_correct <- tibble::tibble(question = c("Which of the following are literacy instructional shifts? Select all that apply. - Regular practice with complex texts and their academic language.", 
                                                            "Which of the following are literacy instructional shifts? Select all that apply. - Building knowledge through content-rich non-fiction.", 
                                                            "Which of the following are literacy instructional shifts? Select all that apply. - Equal balance of text-based writing and writing from personal experiences.", 
                                                            "Which of the following are literacy instructional shifts? Select all that apply. - Regular opportunities for direct instruction on reading comprehension strategies.", 
                                                            "When designing literacy lessons, teachers should start with which of the following?", 
                                                            "When designing literacy lessons, teachers should start with which of the following?", 
                                                            "When designing literacy lessons, teachers should start with which of the following?", 
                                                            "When designing literacy lessons, teachers should start with which of the following?", 
                                                            "Which of the following is the single biggest differentiator of college and career-readiness?", 
                                                            "Which of the following is the single biggest differentiator of college and career-readiness?", 
                                                            "Which of the following is the single biggest differentiator of college and career-readiness?", 
                                                            "Which of the following is the single biggest differentiator of college and career-readiness?", 
                                                            "Which of the following approaches for selecting texts for whole-class reading instruction are aligned with post-shifts literacy instruction? Select all that apply. - Selecting a text that is at or above the grade-level complexity.", 
                                                            "Which of the following approaches for selecting texts for whole-class reading instruction are aligned with post-shifts literacy instruction? Select all that apply. - Selecting a text that is rich in meaning.", 
                                                            "Which of the following approaches for selecting texts for whole-class reading instruction are aligned with post-shifts literacy instruction? Select all that apply. - Selecting an adapted version of the main text for below-grade-level readers.", 
                                                            "Which of the following approaches for selecting texts for whole-class reading instruction are aligned with post-shifts literacy instruction? Select all that apply. - Selecting a text for the class based on student interest.", 
                                                            "Which of the following describe strategies for supporting struggling readers? Select all that apply. - Read the complex text aloud for students.", 
                                                            "Which of the following describe strategies for supporting struggling readers? Select all that apply. - Read aloud a simple article to build knowledge of the topic while students follow along.", 
                                                            "Which of the following describe strategies for supporting struggling readers? Select all that apply. - Have students read the full text multiple times to find the main idea.", 
                                                            "Which of the following describe strategies for supporting struggling readers? Select all that apply. - Ask simpler questions about the same text."),
                                               answer = c("Regular practice with complex texts and their academic language.",
                                                          "Building knowledge through content-rich non-fiction.", 
                                                          "Equal balance of text-based writing and writing from personal experiences.",
                                                          "Regular opportunities for direct instruction on reading comprehension strategies.",
                                                          "A complex text that is worthy of reading multiple times.",
                                                          "A strategy they want students to implement (main idea, predicting, etc.).",
                                                          "A standard (identify author’s purpose).",
                                                          "I’m not sure.",
                                                          "Ability to read fluently and accurately.",
                                                          "Ability to read and write at grade-level independently.",
                                                          "Ability to read complex text independently and proficiently.",
                                                          "I’m not sure.",
                                                          "Selecting a text that is at or above the grade-level complexity.",
                                                          "Selecting a text that is rich in meaning.",
                                                          "Selecting an adapted version of the main text for below-grade-level readers.",
                                                          "Selecting a text for the class based on student interest.",
                                                          "Read the complex text aloud for students.",
                                                          "Read aloud a simple article to build knowledge of the topic while students follow along.",
                                                          "Have students read the full text multiple times to find the main idea.",
                                                          "Ask simpler questions about the same text."))
readr::write_rds(ela_general_bootcamp_correct,
          here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/ela_general_bootcamp.rds"))

TeachingLab::save_processed_data(data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/ELABootcamp-General.rds"),
                    q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/ela_general_bootcamp.rds"),
                    correct = c("Regular practice with complex texts and their academic language.",
                                "Building knowledge through content-rich non-fiction.",
                                "A complex text that is worthy of reading multiple times.",
                                "Ability to read complex text independently and proficiently.",
                                "Selecting a text that is at or above the grade-level complexity.",
                                "Selecting a text that is rich in meaning.",
                                "Read the complex text aloud for students.",
                                "Read aloud a simple article to build knowledge of the topic while students follow along."),
                    save_name = "ela_general_bootcamp")

### ELA Foundational Skills: Bootcamp ###
ela_foundational_skills_correct <- tibble::tibble(question = c("For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Print concepts", 
                                                               "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Phonological awareness", 
                                                               "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Vocabulary development", 
                                                               "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Fluency", 
                                                               "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Reading comprehension", 
                                                               "A structured phonics program is important in K-2 for the following reasons EXCEPT:", 
                                                               "A structured phonics program is important in K-2 for the following reasons EXCEPT:", 
                                                               "A structured phonics program is important in K-2 for the following reasons EXCEPT:", 
                                                               "A structured phonics program is important in K-2 for the following reasons EXCEPT:", 
                                                               "A structured phonics program is important in K-2 for the following reasons EXCEPT:", 
                                                               "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Utilize a variety of ongoing assessment data to determine the focus of instruction for small groups", 
                                                               "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Group students by their ongoing phase of development with regard to the foundational skills", 
                                                               "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Only provide foundational skills instruction during small group time", 
                                                               "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Adhere to a same structure of number of groups and members of groups for the entirety of the year"),
                                                  answer = c("Print Concepts", 
                                                             "Phonological awareness", 
                                                             "Vocabulary development", 
                                                             "Fluency", 
                                                             "Reading comprehension",
                                                             "It is the most effective approach for the most students to learn how to read",
                                                             "It supports the development of foundational reading skills in a focused, systematic way",
                                                             "It explicitly teaches the sound/spelling patterns of English in a sequence",
                                                             "It prompts students to use context clues and pictures to decode words",
                                                             "I am not sure.",
                                                             "Utilize a variety of ongoing assessment data to determine the focus of instruction for small groups",
                                                             "Group students by their ongoing phase of development with regard to the foundational skills",
                                                             "Only provide foundational skills instruction during small group time",
                                                             "Adhere to a same structure of number of groups and members of groups for the entirety of the year"))

readr::write_rds(ela_foundational_skills_correct, 
                 here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/ela_foundational_skills.rds"))

TeachingLab::save_processed_data(data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/ELABootcamp-FoundationalSkillsBootcampSkills(K-2).rds"),
                    q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/ela_foundational_skills.rds"),
                    correct = c("Print concepts",
                                "Phonological awareness",
                                "Fluency",
                                "It prompts students to use context clues and pictures to decode words",
                                "Utilize a variety of ongoing assessment data to determine the focus of instruction for small groups",
                                "Group students by their ongoing phase of development with regard to the foundational skills"),
                    save_name = "ela_foundational_skills")

### ELA Guidebooks Diverse Learners: Bootcamp - Leader ###
ela_guidebooks_diverse_learners_leader_correct <- tibble::tibble(
  question = c("Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Some students need targeted additional support outside of their ELA block.", 
               "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students who need it should have practice with the text before they engage with that text in their ELA block.", 
               "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students who need it should be front-loaded with reading comprehension strategies before engaging with a complex grade-level text.", 
               "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students need to build up all general basic skills before they can take part in Tier 1 instruction.", 
               "Through observing her students, Mrs. Richards concluded that eight of her students may not know the elements of a written response. There is a culminating writing task that all students will need to complete next week. How can she BEST plan to support these eight students so that they will be successful on this writing task?",
               "Through observing her students, Mrs. Richards concluded that eight of her students may not know the elements of a written response. There is a culminating writing task that all students will need to complete next week. How can she BEST plan to support these eight students so that they will be successful on this writing task?",
               "Through observing her students, Mrs. Richards concluded that eight of her students may not know the elements of a written response. There is a culminating writing task that all students will need to complete next week. How can she BEST plan to support these eight students so that they will be successful on this writing task?",
               "Through observing her students, Mrs. Richards concluded that eight of her students may not know the elements of a written response. There is a culminating writing task that all students will need to complete next week. How can she BEST plan to support these eight students so that they will be successful on this writing task?",
               "Through observing her students, Mrs. Richards concluded that eight of her students may not know the elements of a written response. There is a culminating writing task that all students will need to complete next week. How can she BEST plan to support these eight students so that they will be successful on this writing task?"),
  answer = c("Some students need targeted additional support outside of their ELA block.",
             "Students who need it should have practice with the text before they engage with that text in their ELA block.",
             "Students who need it should be front-loaded with reading comprehension strategies before engaging with a complex grade-level text.",
             "Students need to build up all general basic skills before they can take part in Tier 1 instruction.",
             "Plan with the interventionist, Mr. Liu, to have the students work with him to analyze some exemplar written responses for key elements a few days before the writing task is assigned.",
             "Design a new task that does not require students to include the elements of a written response.",
             "Give the students the culminating task in advance so that they have more time to independently plan their response.",
             "As their peers begin drafting the writing task, pull these eight students into a small group and give them a mini lesson on key elements of a written response.",
             "I’m not sure"))

readr::write_rds(ela_guidebooks_diverse_learners_leader_correct, 
                 here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners.rds"))

TeachingLab::save_processed_data(data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/ELAGuidebooksDiverseLearnersBootcamp-Leader.rds"),
                    q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners.rds"),
                    correct = c("Some students need targeted additional support outside of their ELA block.",
                                "Students who need it should have practice with the text before they engage with that text in their ELA block.",
                                "Plan with the interventionist, Mr. Liu, to have the students work with him to analyze exemplar written responses for key elements a few days before the writing task is assigned."),
                    save_name = "ela_guidebooks_diverse_learners_bootcamp_leader")

### ELA Guidebooks Diverse Learners: Bootcamp - Teacher ###
ela_guidebooks_diverse_learners_teacher_correct <- tibble::tibble(
  question = c("Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Some students need targeted additional support outside of their ELA block.", 
               "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students who need it should have practice with the text before they engage with that text in their ELA block.", 
               "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students who need it should be front-loaded with reading comprehension strategies before engaging with a complex grade-level text.", 
               "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students need to build up all general basic skills before they can take part in Tier 1 instruction.", 
               "Through observing her students, Mrs. Richards concluded that eight of her students may not know the elements of a written response. There is a culminating writing task that all students will need to complete next week. How can she BEST plan to support these eight students so that they will be successful on this writing task?", 
               "Through observing her students, Mrs. Richards concluded that eight of her students may not know the elements of a written response. There is a culminating writing task that all students will need to complete next week. How can she BEST plan to support these eight students so that they will be successful on this writing task?",
               "Through observing her students, Mrs. Richards concluded that eight of her students may not know the elements of a written response. There is a culminating writing task that all students will need to complete next week. How can she BEST plan to support these eight students so that they will be successful on this writing task?",
               "Through observing her students, Mrs. Richards concluded that eight of her students may not know the elements of a written response. There is a culminating writing task that all students will need to complete next week. How can she BEST plan to support these eight students so that they will be successful on this writing task?",
               "Through observing her students, Mrs. Richards concluded that eight of her students may not know the elements of a written response. There is a culminating writing task that all students will need to complete next week. How can she BEST plan to support these eight students so that they will be successful on this writing task?",
               "Which of the following observations describe practices for establishing an inclusive and student centered classroom culture? Select all that apply. - Students nominate a set of discussion agreements to use in their class.", 
               "Which of the following observations describe practices for establishing an inclusive and student centered classroom culture? Select all that apply. - The teacher holds a mediation conversation for two students after witnessing one student making a derogatory comment toward the other.", 
               "Which of the following observations describe practices for establishing an inclusive and student centered classroom culture? Select all that apply. - Students research one of three 19th century American heroes: Abraham Lincoln, Thomas Edison, and Mark Twain.", 
               "Which of the following observations describe practices for establishing an inclusive and student centered classroom culture? Select all that apply. - A classroom norm is that “All students must make at least one comment during a class discussion.”", 
               "What is the ideal use case of the Diverse Learners Planning Guide?",
               "What is the ideal use case of the Diverse Learners Planning Guide?",
               "What is the ideal use case of the Diverse Learners Planning Guide?",
               "What is the ideal use case of the Diverse Learners Planning Guide?"),
  answer = c("Some students need targeted additional support outside of their ELA block.",
             "Students who need it should have practice with the text before they engage with that text in their ELA block.",
             "Students who need it should be front-loaded with reading comprehension strategies before engaging with a complex grade-level text.",
             "Students need to build up all general basic skills before they can take part in Tier 1 instruction.",
             "Plan with the interventionist, Mr. Liu, to have the students work with him to analyze some exemplar written responses for key elements a few days before the writing task is assigned.",
             "Design a new task that does not require students to include the elements of a written response.",
             "Give the students the culminating task in advance so that they have more time to independently plan their response.",
             "As their peers begin drafting the writing task, pull these eight students into a small group and give them a mini lesson on key elements of a written response.",
             "I’m not sure",
             "Students nominate a set of discussion agreements to use in their class.",
             "The teacher holds a mediation conversation for two students after witnessing one student making a derogatory comment toward the other.",
             "Students research one of three 19th century American heroes: Abraham Lincoln, Thomas Edison, and Mark Twain.",
             "A classroom norm is that \"All students must make at least one comment during a class discussion.\"",
             "To help teachers plan to use the supports and allow for students to preview the skills that diverse learners might need to engage with grade-level texts and tasks ahead of whole-class instruction.",
             "To help teachers plan for Tier-1 instruction that supports diverse learners to engage with grade-level texts and tasks.",
             "To help teachers plan for whole-group instruction that addresses the most pressing unfinished learning of the majority of the students in the class.",
             "I’m not sure"))

readr::write_rds(ela_guidebooks_diverse_learners_teacher_correct, 
                 here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_bootcamp_teacher.rds"))

TeachingLab::save_processed_data(data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/ELAGuidebooksDiverseLearnersBootcamp-Teacher.rds"),
                    q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_bootcamp_teacher.rds"),
                    correct = c("Some students need targeted additional support outside of their ELA block.",
                                "Students who need it should have practice with the text before they engage with that text in their ELA block.",
                                "Plan with the interventionist, Mr. Liu, to have the students work with him to analyze exemplar written responses for key elements a few days before the writing task is assigned.",
                                "Students nominate a set of discussion agreements to use in their class.",
                                "The teacher holds a mediation conversation for two students after witnessing one student making a derogatory comment toward the other.",
                                "To help teachers plan to use the supports and allow for students to preview the skills that diverse learners might need to engage with grade-level texts and tasks ahead of whole-class instruction."),
                    save_name = "ela_guidebooks_diverse_learners_bootcamp_teacher")

### ELA Guidebooks Diverse Learners: Bootcamp - Writing ###
ela_guidebooks_diverse_learners_writing_correct <- tibble::tibble(
  question = c("Which of the following are true about how most students become better writers? Select all that apply. - Students need to be explicitly taught how to write.", 
               "Which of the following are true about how most students become better writers? Select all that apply. - Students should  plan out what they’re going to write before beginning to write.", 
               "Which of the following are true about how most students become better writers? Select all that apply. - Students can become good writers by reading complex texts.", 
               "Which of the following are true about how most students become better writers? Select all that apply. - Students should have isolated grammar lessons so that they can apply grammar rules in their writing.", 
               "Which of the following are true about how most students become better writers? Select all that apply. - I’m not sure", 
               "All of the following are effective ways a teacher can help students write a stronger sentence EXCEPT:",
               "All of the following are effective ways a teacher can help students write a stronger sentence EXCEPT:",
               "All of the following are effective ways a teacher can help students write a stronger sentence EXCEPT:",
               "All of the following are effective ways a teacher can help students write a stronger sentence EXCEPT:",
               "All of the following are effective ways a teacher can help students write a stronger sentence EXCEPT:"),
  answer = c("Students need to be explicitly taught how to write.",
             "Students should  plan out what they’re going to write before beginning to write.",
             "Students can become good writers by reading complex texts.",
             "Students should have isolated grammar lessons so that they can apply grammar rules in their writing.",
             "I’m not sure",
             "Identify incorrect uses of punctuation and correct them.",
             "Identify fragments and change them into a complete sentence.",
             "Use 5Ws and H questions to write a summary sentence.",
             "Incorporate conjunctions to respond to a text-dependent question.",
             "I’m not sure"))

readr::write_rds(ela_guidebooks_diverse_learners_writing_correct,
          here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_bootcamp_writing.rds"))

TeachingLab::save_processed_data(data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/ELAGuidebooksDiverseLearnersBootcampWriting.rds"),
                    q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_bootcamp_writing.rds"),
                    correct = c("Students need to be explicitly taught how to write.",
                                "Students should  plan out what they’re going to write before beginning to write.",
                                "Identify incorrect uses of punctuation and correct them."),
                    save_name = "ela_guidebooks_diverse_learners_bootcamp_writing")

### ELA EL: HQIM & Enrichment ###
ela_hqim_enrichment_correct <- tibble::tibble(
  question = c("Which of the following statements describes gifted learners?", 
               "Which of the following statements describes gifted learners?",
               "Which of the following statements describes gifted learners?",
               "Which of the following statements describes gifted learners?",
               "Which of the following statements describe the Depth and Complexity framework? Select all that apply. - The Depth and Complexity Framework applies specifically to the content area of ELA.", 
               "Which of the following statements describe the Depth and Complexity framework? Select all that apply. - Sequencing the layers of Depth and Complexity to support key standards yields the highest-impact enrichment.", 
               "Which of the following statements describe the Depth and Complexity framework? Select all that apply. - The Depth and Complexity Framework is a differentiation tool that can raise the thinking level for all students.", 
               "Which of the following statements describe the Depth and Complexity framework? Select all that apply. - Teachers should attend to all aspects and layers of the Depth and Complexity framework in any given instructional unit to ensure the highest leverage enrichment experience for students.", 
               "The Multi-Tiered System of Support (MTSS) is a framework that:",
               "The Multi-Tiered System of Support (MTSS) is a framework that:",
               "The Multi-Tiered System of Support (MTSS) is a framework that:",
               "The Multi-Tiered System of Support (MTSS) is a framework that:"),
  answer = c("Gifted learners are always high-achieving.",
             "Gifted learners will display characteristics and traits in all four categories: Cognitive, Creative, Affective, Behavioral.",
             "Gifted learners have special needs in the classroom that fall into these categories: Cognitive, Creative, Affective, Behavioral.",
             "I’m not sure",
             "The Depth and Complexity Framework applies specifically to the content area of ELA.",
             "Sequencing the layers of Depth and Complexity to support key standards yields the highest-impact enrichment.",
             "The Depth and Complexity Framework is a differentiation tool that can raise the thinking level for all students.",
             "Teachers should attend to all aspects and layers of the Depth and Complexity framework in any given instructional unit to ensure the highest leverage enrichment experience for students.",
             "Replaces Response to Intervention or RTI",
             "Identifies students in need of pull-out instruction",
             "Includes universal screening of all students, multiple tiers of instruction and support services, and integrated data collection and assessment systems to inform decisions at each tier of instruction",
             "I’m not sure"
  ))

readr::write_rds(ela_hqim_enrichment_correct, 
          here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/el_ela_hqim_enrichment.rds"))

TeachingLab::save_processed_data(data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/ELAHQIM&Enrichment.rds"),
                    q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/el_ela_hqim_enrichment.rds"),
                    correct = c("Gifted learners have special needs in the classroom that fall into these categories: Cognitive, Creative, Affective, Behavioral.",
                                "Sequencing the layers of Depth and Complexity to support key standards yields the highest-impact enrichment.",
                                "The Depth and Complexity Framework is a differentiation tool that can raise the thinking level for all students.",
                                "Includes universal screening of all students, multiple tiers of instruction and support services, and integrated data collection and assessment systems to inform decisions at each tier of instruction"),
                    save_name = "el_ela_hqim_enrichment")

########## Special Section, Same Questions for Math: Bootcamp EIC, and Math Bootcamp ##########
### Math: Bootcamp EIC ###
math_bootcamp_correct <- tibble::tibble(
  question = c("Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Going deeper into fewer math topics.", 
               "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Making connections between math topics across grades.", 
               "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Prioritizing conceptual understanding over procedural skills.", 
               "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Creating opportunities for students to work on math skills above their grade-level.", 
               "Which of the following statements are true about the research into types of math instruction? Select all that apply. - Unguided problem solving lessons are the least effective type of math lesson.", 
               "Which of the following statements are true about the research into types of math instruction? Select all that apply. - Building deep understanding with fewer math topics is more effective than covering a broader range of math topics.", 
               "Which of the following statements are true about the research into types of math instruction? Select all that apply. - Math instruction that covers more math topics leads to better student performance on assessments.", 
               "Which of the following statements are true about the research into types of math instruction? Select all that apply. - Direct instruction (I do, We do, You do) is the most effective type of math instruction.", 
               "Equitable instruction in math includes which of the following?", 
               "Equitable instruction in math includes which of the following?", 
               "Equitable instruction in math includes which of the following?", 
               "Equitable instruction in math includes which of the following?", 
               "Equitable instruction in math includes which of the following?", 
               "Which of the following statements are true about the principles behind the Mathematical Language Routines? Select all that apply. - Students do not need to understand English completely before they can start making sense of math instruction in English.", 
               "Which of the following statements are true about the principles behind the Mathematical Language Routines? Select all that apply. - An appropriate scaffold for language development is to allow students to make charts, diagrams, and other visual representations of their understanding.", 
               "Which of the following statements are true about the principles behind the Mathematical Language Routines? Select all that apply. - Students who are not proficient in academic English should work with simplified math tasks until they develop stronger language skills.", 
               "Which of the following statements are true about the principles behind the Mathematical Language Routines? Select all that apply. - To demonstrate their mathematical knowledge, students must be able to use formal definitions and vocabulary accurately."),
  answer = c("Going deeper into fewer math topics.", 
             "Making connections between math topics across grades.",
             "Prioritizing conceptual understanding over procedural skills.",
             "Creating opportunities for students to work on math skills above their grade-level.",
             "Unguided problem solving lessons are the least effective type of math lesson.",
             "Building deep understanding with fewer math topics is more effective than covering a broader range of math topics.",
             "Math instruction that covers more math topics leads to better student performance on assessments.",
             "Direct instruction (I do, We do, You do) is the most effective type of math instruction.",
             "Giving simplified tasks to students who have unfinished learning.",
             "Following a strict pacing schedule for covering new materials and focusing only on grade-level work.",
             "Teaching the procedures and “tricks” such as using the “butterfly method” to compare fractions.",
             "Creating opportunities for students to practice saying out loud how they solved for a problem.",
             "I’m not sure.",
             "Students do not need to understand English completely before they can start making sense of math instruction in English.",
             "An appropriate scaffold for language development is to allow students to make charts, diagrams, and other visual representations of their understanding.",
             "Students who are not proficient in academic English should work with simplified math tasks until they develop stronger language skills.",
             "To demonstrate their mathematical knowledge, students must be able to use formal definitions and vocabulary accurately."
  ))

readr::write_rds(math_bootcamp_correct, 
          here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/math_bootcamp.rds"))

TeachingLab::save_processed_data(data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/MathBootcamp-EIC.rds"),
                    q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/math_bootcamp.rds"),
                    correct = c("Going deeper into fewer math topics.",
                                "Making connections between math topics across grades.",
                                "Unguided problem solving lessons are the least effective type of math lesson.",
                                "Building deep understanding with fewer math topics is more effective than covering a broader range of math topics.",
                                "Creating opportunities for students to practice saying out loud how they solved a problem.",
                                "Students do not need to understand English completely before they can start making sense of math instruction in English.",
                                "An appropriate scaffold for language development is to allow students to make charts, diagrams, and other visual representations of their understanding."),
                    save_name = "math_bootcamp_eic")

### Math: Bootcamp ###
TeachingLab::save_processed_data(data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/MathBootcamp.rds"),
                    q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/math_bootcamp.rds"),
                    correct = c("Going deeper into fewer math topics.",
                                "Making connections between math topics across grades.",
                                "Unguided problem solving lessons are the least effective type of math lesson.",
                                "Building deep understanding with fewer math topics is more effective than covering a broader range of math topics.",
                                "Creating opportunities for students to practice saying out loud how they solved a problem.",
                                "Students do not need to understand English completely before they can start making sense of math instruction in English.",
                                "An appropriate scaffold for language development is to allow students to make charts, diagrams, and other visual representations of their understanding."),
                    save_name = "math_bootcamp")

### Math Cycle of Inquiry I - Eliciting Student Thinking ###
math_cycle_inquiry_1_eic_correct <- tibble::tibble(
  question = c("Which of the following are focusing questions? Select all that apply. - What kind of equation does y=3x+2 represent?", 
               "Which of the following are focusing questions? Select all that apply. - Notice that this line passes through (0,0) on the graph. What kind of relationship does this show?", 
               "Which of the following are focusing questions? Select all that apply. - How do we know that y=3x+2 represents a linear relationship?", 
               "Which of the following are focusing questions? Select all that apply. - What key features of this graph tells us that the line represents a proportional relationship?", 
               "Which of the following are focusing questions? Select all that apply. - I’m not sure",
               "A teacher is planning a 15-minute re-engagement lesson. Which of the following best models an effective re-engagement lesson?",
               "A teacher is planning a 15-minute re-engagement lesson. Which of the following best models an effective re-engagement lesson?",
               "A teacher is planning a 15-minute re-engagement lesson. Which of the following best models an effective re-engagement lesson?",
               "A teacher is planning a 15-minute re-engagement lesson. Which of the following best models an effective re-engagement lesson?",
               "A teacher is planning a 15-minute re-engagement lesson. Which of the following best models an effective re-engagement lesson?",
               "All of the following are skills that a teacher needs to equitably elicit student thinking EXCEPT:",
               "All of the following are skills that a teacher needs to equitably elicit student thinking EXCEPT:",
               "All of the following are skills that a teacher needs to equitably elicit student thinking EXCEPT:",
               "All of the following are skills that a teacher needs to equitably elicit student thinking EXCEPT:",
               "All of the following are skills that a teacher needs to equitably elicit student thinking EXCEPT:",
               "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Providing opportunities for students to answer each other’s questions", 
               "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Calling on all students regardless of their race or background to answer questions", 
               "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Intentionally asking focusing questions of students who may have been historically pushed out of math success in the past", 
               "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Analyzing video or audio recordings of one’s own instruction regularly for bias", 
               "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - I’m not sure",
               "“Language of Commitment and Personal Responsibility” does NOT include which of the following?",
               "“Language of Commitment and Personal Responsibility” does NOT include which of the following?",
               "“Language of Commitment and Personal Responsibility” does NOT include which of the following?",
               "“Language of Commitment and Personal Responsibility” does NOT include which of the following?",
               "“Language of Commitment and Personal Responsibility” does NOT include which of the following?",
               "Which of the following are focusing questions? Select all that apply. - How did Tessa think about this word problem?", 
               "Which of the following are focusing questions? Select all that apply. - What are some of the key words and numbers in the story?", 
               "Which of the following are focusing questions? Select all that apply. - What connections do you see between Tessa and Fede’s strategies?", 
               "Which of the following are focusing questions? Select all that apply. - What equation will I solve in this word problem?", 
               "Which of the following are focusing questions? Select all that apply. - I’m not sure.",
               "A teacher is planning a 15-minute re-engagement lesson. Which of the following best models an effective re-engagement lesson?_2", 
               "A teacher is planning a 15-minute re-engagement lesson. Which of the following best models an effective re-engagement lesson?_2", 
               "A teacher is planning a 15-minute re-engagement lesson. Which of the following best models an effective re-engagement lesson?_2", 
               "A teacher is planning a 15-minute re-engagement lesson. Which of the following best models an effective re-engagement lesson?_2", 
               "A teacher is planning a 15-minute re-engagement lesson. Which of the following best models an effective re-engagement lesson?_2", 
               "All of the following are skills that a teacher needs to equitably elicit student thinking EXCEPT:_2", 
               "All of the following are skills that a teacher needs to equitably elicit student thinking EXCEPT:_2",
               "All of the following are skills that a teacher needs to equitably elicit student thinking EXCEPT:_2",
               "All of the following are skills that a teacher needs to equitably elicit student thinking EXCEPT:_2",
               "All of the following are skills that a teacher needs to equitably elicit student thinking EXCEPT:_2",
               "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Providing opportunities for students to answer each other’s questions_2", 
               "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Calling on all students regardless of their race or background to answer questions_2", 
               "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Intentionally asking focusing questions of students who may have been historically pushed out of math success in the past_2", 
               "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Analyzing video or audio recordings of one’s own instruction regularly for bias_2", 
               "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - I’m not sure_2", 
               "“Language of Commitment and Personal Responsibility” does NOT include which of the following?_2",
               "“Language of Commitment and Personal Responsibility” does NOT include which of the following?_2",
               "“Language of Commitment and Personal Responsibility” does NOT include which of the following?_2",
               "“Language of Commitment and Personal Responsibility” does NOT include which of the following?_2",
               "“Language of Commitment and Personal Responsibility” does NOT include which of the following?_2"),
  answer = c("What kind of equation does y=3x+2 represent?", 
             "Notice that this line passes through (0,0) on the graph. What kind of relationship does this show?",
             "How do we know that y=3x+2 represents a linear relationship?",
             "What key features of this graph tells us that the line represents a proportional relationship?",
             "I’m not sure",
             "The teacher teaches a shorter version of the original lesson with the problems worded differently, offering a refresher of the concept in the same way.",
             "The next day, a teacher leads a discussion analyzing a few different students’ solutions to the previous day’s problem set and connecting solutions to a learning goal.",
             "The teacher gives students back their graded problem set from the day before and students have the opportunity to correct them.",
             "The teacher models how to solve a problem that most students got incorrect the day before and then students work in small groups to correct their errors.",
             "I’m not sure",
             "A plan for how to students should solve each step of a task",
             "Deep understanding of the content and how it fits into the learning progression",
             "Knowledge of the various solutions students could come up with",
             "Knowledge of misconceptions underlying different solutions",
             "I'm not sure",
             "Providing opportunities for students to answer each other’s questions",
             "Calling on all students regardless of their race or background to answer questions",
             "Intentionally asking focusing questions of students who may have been historically pushed out of math success in the past",
             "Analyzing video or audio recordings of one’s own instruction regularly for bias",
             "I’m not sure",
             "Explicitly expressing what we stand for",
             "Sharing frustrations",
             "Raising questions for oneself",
             "Directing attention to places where we have maximum influence",
             "I’m not sure",
             "How did Tessa think about this word problem?",
             "What are some of the key words and numbers in the story?",
             "What connections do you see between Tessa and Fede’s strategies?",
             "What equation will I solve in this word problem?",
             "I’m not sure.",
             "The teacher teaches a shorter version of the original lesson with the problems worded differently, offering a refresher of the concept in the same way.",
             "The next day, a teacher leads a discussion analyzing a few different students’ solutions to the previous day’s problem set and connecting solutions to a learning goal.",
             "The teacher gives students back their graded problem set from the day before and students have the opportunity to correct them.",
             "The teacher models how to solve a problem that most students got incorrect the day before and then students work in small groups to correct their errors.",
             "I’m not sure",
             "A plan for how to students should solve each step of a task",
             "Deep understanding of the content and how it fits into the learning progression",
             "Knowledge of the various solutions students could come up with",
             "Knowledge of misconceptions underlying different solutions",
             "I'm not sure",
             "Providing opportunities for students to answer each other’s questions",
             "Calling on all students regardless of their race or background to answer questions",
             "Intentionally asking focusing questions of students who may have been historically pushed out of math success in the past",
             "Analyzing video or audio recordings of one’s own instruction regularly for bias",
             "I’m not sure",
             "Explicitly expressing what we stand for",
             "Sharing frustrations",
             "Raising questions for oneself",
             "Directing attention to places where we have maximum influence",
             "I’m not sure"
  ))

readr::write_rds(math_cycle_inquiry_1_eic_correct, 
          here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/math_cycle_inquiry_1.rds"))

TeachingLab::save_processed_data(data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/MathCycleofInquiryI-ElicitingStudentThinking.rds"),
                    q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/math_cycle_inquiry_1.rds"),
                    correct = c("How do we know that y=3x+2 represents a linear relationship?",
                                "What key features of this graph tells us that the line represents a proportional relationship?",
                                "How did Tessa think about this word problem?",
                                "What connections do you see between Tessa and Fede’s strategies?",
                                "The next day, a teacher leads a discussion analyzing a few different students’ solutions to the previous day’s problem set and connecting solutions to a learning goal.",
                                "A plan for how to students should solve each step of a task",
                                "Intentionally asking focusing questions of students who may have been historically pushed out of math success in the past",
                                "Analyzing video or audio recordings of one’s own instruction regularly for bias",
                                "Sharing frustrations"),
                    save_name = "math_cycle_of_inquiry_i")

### Math: Cycle of Inquiry V- Sequencing and Connecting Representations ###
math_cycle_inquiry_5_scr_correct <- tibble::tibble(
  question = c("Which order represents the general progression in a mathematics discussion using the Five Practices?", 
               "Which order represents the general progression in a mathematics discussion using the Five Practices?",
               "Which order represents the general progression in a mathematics discussion using the Five Practices?",
               "Which order represents the general progression in a mathematics discussion using the Five Practices?",
               "Which order represents the general progression in a mathematics discussion using the Five Practices?",
               "Which order represents the general progression in a mathematics discussion using the Five Practices?",
               "Which of the following best summarizes the purpose of the Five Practices for Orchestrating Mathematics Discussions?", 
               "Which of the following best summarizes the purpose of the Five Practices for Orchestrating Mathematics Discussions?",
               "Which of the following best summarizes the purpose of the Five Practices for Orchestrating Mathematics Discussions?",
               "Which of the following best summarizes the purpose of the Five Practices for Orchestrating Mathematics Discussions?",
               "Which of the following best summarizes the purpose of the Five Practices for Orchestrating Mathematics Discussions?",
               "Which of the following statements are always true about the Five Practices for Orchestrating Mathematics Discussions? Select all that apply. - We show student work one at a time during Sequencing.", 
               "Which of the following statements are always true about the Five Practices for Orchestrating Mathematics Discussions? Select all that apply. - We lead students to a pre-planned mathematical idea during Connecting.", 
               "Which of the following statements are always true about the Five Practices for Orchestrating Mathematics Discussions? Select all that apply. - We use student data to inform our planning during Anticipation.", 
               "Which of the following statements are always true about the Five Practices for Orchestrating Mathematics Discussions? Select all that apply. - We begin thinking about student misconceptions during Monitoring.", 
               "Which of the following statements are always true about the Five Practices for Orchestrating Mathematics Discussions? Select all that apply. - We can gather a variety of student work during Selecting.", 
               "Which of the following statements are always true about the Five Practices for Orchestrating Mathematics Discussions? Select all that apply. - I’m not sure", 
               "A lesson representative of “Leveraging multiple mathematical competencies” does which of the following? Select all that apply. - Presents tasks that offer multiple entry points", 
               "A lesson representative of “Leveraging multiple mathematical competencies” does which of the following? Select all that apply. - Requires students to show mastery of skills prior to engaging in more complex problem solving", 
               "A lesson representative of “Leveraging multiple mathematical competencies” does which of the following? Select all that apply. - Gives ambivalent value to flexibility, reasoning, and persistence", 
               "A lesson representative of “Leveraging multiple mathematical competencies” does which of the following? Select all that apply. - Structures collaboration to use varying math knowledge and skills to solve complex problems", 
               "A lesson representative of “Leveraging multiple mathematical competencies” does which of the following? Select all that apply. - I’m not sure.", 
               "Which question best represents what teachers would ask themselves as they plan a lesson to ”Leveraging Multiple Math Competencies”?",
               "Which question best represents what teachers would ask themselves as they plan a lesson to ”Leveraging Multiple Math Competencies”?",
               "Which question best represents what teachers would ask themselves as they plan a lesson to ”Leveraging Multiple Math Competencies”?",
               "Which question best represents what teachers would ask themselves as they plan a lesson to ”Leveraging Multiple Math Competencies”?",
               "Which question best represents what teachers would ask themselves as they plan a lesson to ”Leveraging Multiple Math Competencies”?"),
  answer = c("Monitor, Anticipate, Select, Sequence, Connect",
             "Anticipate, Monitor, Sequence, Select, Connect",
             "Anticipate, Monitor, Select, Sequence, Connect",
             "Connect, Anticipate, Monitor, Select, Sequence",
             "Monitor, Anticipate, Sequence, Select, Connect",
             "I’m not sure",
             "Provide a structure for students to follow during an engaging mathematical discussion",
             "Highlight teaching moves that can create an engaging mathematical discussion",
             "Give students clear entry points for participating in an engaging mathematical discussion",
             "Emphasize the importance of planning to create an engaging mathematical discussion",
             "I’m not sure",
             "We show student work one at a time during Sequencing. ",
             "We lead students to a pre-planned mathematical idea during Connecting.",
             "We use student data to inform our planning during Anticipation.",
             "We begin thinking about student misconceptions during Monitoring. ",
             "We can gather a variety of student work during Selecting.",
             "I’m not sure",
             "Presents tasks that offer multiple entry points",
             "Requires students to show mastery of skills prior to engaging in more complex problem solving ",
             "Gives ambivalent value to flexibility, reasoning, and persistence ",
             "Structures collaboration to use varying math knowledge and skills to solve complex problems",
             "I’m not sure.",
             "How do I identify and support mathematical contributions from students with different strengths and levels of confidence?",
             "How do I support students in closely examining the math concept?",
             "How can I effectively communicate with families the strengths and needs of students to affirm their math identities and promote math learning?",
             "What impact have race and racism had on my mathematics lessons?",
             "I’m not sure."))

readr::write_rds(math_cycle_inquiry_5_scr_correct,
          here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/math_cycle_inquiry_5.rds"))

TeachingLab::save_processed_data(data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/MathCycleofInquiryV-SequencingandConnectingRepresentations.rds"),
                    q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/math_cycle_inquiry_5.rds"),
                    correct = c("Anticipate, Monitor, Select, Sequence, Connect",
                                "Emphasize the importance of planning to create an engaging mathematical discussion",
                                "We lead students to a pre-planned mathematical idea during Connecting.",
                                "We use student data to inform our planning during Anticipation.",
                                "We can gather a variety of student work during Selecting.",
                                "Presents tasks that offer multiple entry points",
                                "Structures collaboration to use varying math knowledge and skills to solve complex problems",
                                "How do I identify and support mathematical contributions from students with different strengths and levels of confidence?"),
                    save_name = "math_cycle_inquiry_iv")

### School Leaders: ELA ###
ela_school_leaders_correct <- tibble::tibble(
  question = c("Which of the following are literacy instructional shifts? Select all that apply. - Regular practice with complex texts and their academic language.", 
               "Which of the following are literacy instructional shifts? Select all that apply. - Building knowledge through content-rich non-fiction.", 
               "Which of the following are literacy instructional shifts? Select all that apply. - Equal balance of text-based writing and writing from personal experiences. ", 
               "Which of the following are literacy instructional shifts? Select all that apply. - Regular opportunities for direct instruction on reading comprehension strategies.", 
               "Which of the following are literacy instructional shifts? Select all that apply. - I'm not sure.", 
               "Which of the following are examples of text-specific questions? Select all that apply. - What can you infer from Dr. King’s letter about the letter that he received?", 
               "Which of the following are examples of text-specific questions? Select all that apply. - In “The Lion, the Witch, and the Wardrobe”, how and why does Edmund change?", 
               "Which of the following are examples of text-specific questions? Select all that apply. - In “Casey at the Bat,” Casey strikes out. Describe a time when you failed at something.", 
               "Which of the following are examples of text-specific questions? Select all that apply. - In “Letter from a Birmingham Jail,” Dr. King discusses nonviolent protests. Write about a time when you wanted to fight against something that you felt was unfair.", 
               "Which of the following statements are true about the purpose of the Instructional Practice Guide (IPG)? Select all that apply. - It focuses on observations aligned to the ELA and literacy instructional shifts", 
               "Which of the following statements are true about the purpose of the Instructional Practice Guide (IPG)? Select all that apply. - It is a coaching tool that supports identifying equitable literacy practices.", 
               "Which of the following statements are true about the purpose of the Instructional Practice Guide (IPG)? Select all that apply. - It can be used to diagnose unfinished learning needs of students.", 
               "Which of the following statements are true about the purpose of the Instructional Practice Guide (IPG)? Select all that apply. - It is a tool that can be used to determine teacher effectiveness in formal observations.", 
               "Which of the following statements are true about the purpose of the Instructional Practice Guide (IPG)? Select all that apply. - I'm not sure.", 
               "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - The feedback is focused on teacher moves such as the questions the teachers posed as students read the text.", 
               "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - The interaction includes role-playing for the teacher to practice a new strategy which directly addresses a challenge from the observation.", 
               "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - The feedback makes clear the supervisor’s opinion about the teacher’s instruction pedagogy.", 
               "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - The feedback is delivered via email so that the teacher can respond in writing.", 
               "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - I'm not sure."),
  answer = c("Regular practice with complex texts and their academic language.",
             "Building knowledge through content-rich non-fiction.",
             "Equal balance of text-based writing and writing from personal experiences.",
             "Regular opportunities for direct instruction on reading comprehension strategies.",
             "I’m not sure.",
             "What can you infer from Dr. King’s letter about the letter that he received?",
             "In “The Lion, the Witch, and the Wardrobe”, how and why does Edmund change?",
             "In “Casey at the Bat,” Casey strikes out. Describe a time when you failed at something.",
             "In “Letter from a Birmingham Jail,” Dr. King discusses nonviolent protests. Write about a time when you wanted to fight against something that you felt was unfair.",
             "It focuses on observations aligned to the ELA and literacy instructional shifts",
             "It is a coaching tool that supports identifying equitable literacy practices.",
             "It can be used to diagnose unfinished learning needs of students.",
             "It is a tool that can be used to determine teacher effectiveness in formal observations.",
             "I’m not sure.",
             "The feedback is focused on teacher moves such as the questions the teachers posed as students read the text.",
             "The interaction includes role-playing for the teacher to practice a new strategy which directly addresses a challenge from the observation.",
             "The feedback makes clear the supervisor’s opinion about the teacher’s instruction pedagogy.",
             "The feedback is delivered via email so that the teacher can respond in writing.",
             "I’m not sure."
))

write_rds(ela_school_leaders_correct,
          here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/ela_school_leaders.rds"))

TeachingLab::save_processed_data(data = here::here("dashboards/KnowledgeAssessments/data/unprocessed/SchoolLeadersELA.rds"),
                    q_and_a = here::here("dashboards/KnowledgeAssessments/data/questions_and_answers/ela_school_leaders.rds"),
                    correct = c("Regular practice with complex texts and their academic language.",
                                "Building knowledge through content-rich non-fiction.",
                                "What can you infer from Dr. King’s letter about the letter that he received?",
                                "In “The Lion, the Witch, and the Wardrobe”, how and why does Edmund change?",
                                "It focuses on observations aligned to the ELA and literacy instructional shifts.",
                                "It is a coaching tool that supports identifying equitable literacy practices.",
                                "The feedback is focused on teacher moves such as the questions the teachers posed as students read the text.",
                                "The interaction includes role-playing for the teacher to practice a new strategy which directly addresses a challenge from the observation."),
                    save_name = "ela_school_leaders")

################################################################################################################################






