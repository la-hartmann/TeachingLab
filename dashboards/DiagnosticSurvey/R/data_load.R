library(magrittr)

### Load the Data ###
# diagnostic <- readr::read_rds(here::here("dashboards/DiagnosticSurvey/data/diagnostic.rds"))
diagnostic <- readr::read_rds("data/sy21_22/diagnostic.rds")

#### Grade Column Names ####
grades <- c(
  "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_k",
  "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_1",
  "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_2",
  "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_3",
  "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_4",
  "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_5",
  "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_6",
  "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_7",
  "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_8",
  "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_9",
  "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_10",
  "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_11",
  "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_12"
)
################################################################################################
#### Curricula Column Names ####
teacher_curricula_use <- c(
  "how_often_do_you_use_the_following_curricula_for_your_instruction_on_average_ckla",
  "how_often_do_you_use_the_following_curricula_for_your_instruction_on_average_el",
  "how_often_do_you_use_the_following_curricula_for_your_instruction_on_average_guidebooks_learn_zillion_guidebooks",
  "how_often_do_you_use_the_following_curricula_for_your_instruction_on_average_curricula_i_create_myself",
  "how_often_do_you_use_the_following_curricula_for_your_instruction_on_average_curricula_my_school_or_district_has_created",
  "how_often_do_you_use_the_following_curricula_for_your_instruction_on_average_another_published_ela_curriculum_please_specify_below",
  "how_often_do_you_use_the_following_curricula_for_your_instruction_on_average_engage_ny_eureka",
  "how_often_do_you_use_the_following_curricula_for_your_instruction_on_average_illustrative_mathematics",
  "how_often_do_you_use_the_following_curricula_for_your_instruction_on_average_zearn",
  "how_often_do_you_use_the_following_curricula_for_your_instruction_on_average_curricula_i_create_myself_2",
  "how_often_do_you_use_the_following_curricula_for_your_instruction_on_average_curricula_my_school_or_district_has_created_2",
  "how_often_do_you_use_the_following_curricula_for_your_instruction_on_average_another_published_math_curriculum_please_specify_below"
)

other_curricula <- c(
  "how_often_do_you_use_the_following_curricula_for_your_instruction_on_average_other_please_specify_2",
  "how_often_do_you_use_the_following_curricula_for_your_instruction_on_average_other_please_specify"
)
#### Curricula Overall Frequency ####
curricula_average <- diagnostic %>%
  select(teacher_curricula_use) %>%
  pivot_longer(everything()) %>%
  drop_na(value) %>%
  group_by(value) %>%
  count(sort = T) %>%
  ungroup() %>%
  mutate(percent = 100 * n / sum(n)) %>%
  slice(1)
################################################################################################
#### Mindsets Column Names ####
teacher_mindsets <- c(
  "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_i_am_color_blind_when_it_comes_to_my_teaching_i_don_t_think_of_my_students_in_terms_of_their_race_or_ethnicity",
  "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_the_gap_in_the_achievement_among_students_of_different_races_is_about_poverty_not_race",
  "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_i_think_about_my_own_background_and_experiences_and_how_those_affect_my_instruction",
  "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_i_try_to_keep_in_mind_the_limits_of_my_students_ability_and_give_them_assignments_that_i_know_they_can_do_so_that_they_do_not_become_discouraged",
  "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_before_students_are_asked_to_engage_in_complex_learning_tasks_they_need_to_have_a_solid_grasp_of_basic_skills",
  "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_it_is_not_fair_to_ask_students_who_are_struggling_with_english_to_take_on_challenging_academic_assignments",
  "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_teachers_should_provide_all_students_the_opportunity_to_work_with_grade_level_texts_and_tasks"
)
#### Teacher Mindsets Overall Average ####
teacher_mindset_average <- diagnostic %>%
  select(teacher_mindsets) %>%
  pivot_longer(everything()) %>%
  drop_na(value) %>%
  mutate(pos_neg = ifelse(name %in% c(teacher_mindsets[1:2], teacher_mindsets[4:7]), "negative", "positive")) %>%
  group_by(value, pos_neg) %>%
  count(sort = T) %>%
  ungroup() %>%
  mutate(
    value = readr::parse_number(as.character(value)),
    value = if_else(pos_neg == "negative",
                    str_replace_all(as.character(value), c("2" = "four",
                                                          "1" = "five",
                                                          "5" = "1",
                                                          "4" = "2")
                                                 ),
                    as.character(value)
                    ),
    value = if_else(pos_neg == "negative",
                    str_replace_all(as.character(value),
                                    c("four" = "4",
                                      "five" = "5")
                                    ),
                    as.character(value)
                    ),
    value = readr::parse_number(value)
  ) %>%
  summarise(average = round(weighted.mean(x = value, w = n), digits = 2))
################################################################################################
#### Teacher CRSE Practices Column Names ####
teacher_crse_practices <- c(
  "please_rate_your_confidence_on_the_following_items_br_i_am_able_to_adapt_instruction_to_meet_the_needs_of_my_students",
  "please_rate_your_confidence_on_the_following_items_br_i_am_able_to_identify_ways_that_the_school_culture_e_g_values_norms_and_practices_is_different_from_my_students_home_culture",
  "please_rate_your_confidence_on_the_following_items_br_i_am_able_to_use_my_students_prior_knowledge_to_help_them_make_sense_of_new_information",
  "please_rate_your_confidence_on_the_following_items_br_i_am_able_to_revise_instructional_material_to_include_a_better_representation_of_cultural_groups",
  "please_rate_your_confidence_on_the_following_items_br_i_am_able_to_teach_the_curriculum_to_students_with_unfinished_learning",
  "please_rate_your_confidence_on_the_following_items_br_i_am_able_to_teach_the_curriculum_to_students_who_are_from_historically_marginalized_groups"
)
#### Teacher CRSE Practices Overall Average ####
teacher_crse_average <- diagnostic %>%
  select(teacher_crse_practices) %>%
  pivot_longer(everything()) %>%
  drop_na(value) %>%
  group_by(value) %>%
  count(sort = T) %>%
  ungroup() %>%
  mutate(value = readr::parse_number(as.character(value))) %>%
  summarise(average = round(weighted.mean(x = value, w = n), digits = 2))
################################################################################################
#### School Environment Column Names ####
teacher_school_environment <- c(
  "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_i_trust_my_fellow_teachers_in_the_school",
  "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_i_feel_connected_to_my_fellow_teachers_in_the_school",
  "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_i_have_influence_over_the_professional_learning_that_i_receive_through_my_school_or_district",
  "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_i_am_confident_that_i_am_implementing_the_curriculum_in_a_way_that_maximizes_positive_impact_for_student_learning_br_note_we_are_referring_to_the_curriculum_that_teaching_lab_will_provide_support_on_only"
)
#### School Environment Overall Average ####
teacher_school_environment_average <- diagnostic %>%
  select(teacher_school_environment) %>%
  pivot_longer(everything()) %>%
  drop_na(value) %>%
  group_by(value) %>%
  count(sort = T) %>%
  ungroup() %>%
  mutate(value = readr::parse_number(as.character(value))) %>%
  summarise(average = round(weighted.mean(x = value, w = n), digits = 2))
################################################################################################
#### Administrator Mindsets Column Names ####
administrator_mindsets <- c(
  "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_teachers_should_be_color_blind_when_it_comes_to_their_teaching_they_shouldnt_think_of_students_in_terms_of_their_race_or_ethnicity",
  "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_the_gap_in_the_achievement_among_students_of_different_races_is_about_poverty_not_race_2",
  "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_i_think_about_my_own_background_and_experiences_and_how_those_affect_my_instructional_leadership",
  "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_teachers_should_try_to_keep_in_mind_the_limits_of_students_ability_and_give_them_assignments_that_they_know_they_can_do_so_that_they_do_not_become_discouraged",
  "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_before_students_are_asked_to_engage_in_complex_learning_tasks_they_need_to_have_a_solid_grasp_of_basic_skills_2",
  "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_it_is_not_fair_to_ask_students_who_are_struggling_with_english_to_take_on_challenging_academic_assignments_2",
  "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_teachers_should_provide_all_students_the_opportunity_to_work_with_grade_level_texts_and_tasks_2"
)
#### Admin Mindsets Overall Average ####
administrator_mindsets_average <- diagnostic %>%
  select(administrator_mindsets) %>%
  pivot_longer(everything()) %>%
  drop_na(value) %>%
  mutate(pos_neg = ifelse(name %in% c(administrator_mindsets[1:2], administrator_mindsets[4:7]), "negative", "positive")) %>%
  group_by(value, pos_neg) %>%
  count(sort = T) %>%
  ungroup() %>%
  mutate(
    value = readr::parse_number(as.character(value)),
    value = if_else(pos_neg == "negative",
                    str_replace_all(as.character(value), c("2" = "four",
                                                           "1" = "five",
                                                           "5" = "1",
                                                           "4" = "2")
                    ),
                    as.character(value)
    ),
    value = if_else(pos_neg == "negative",
                    str_replace_all(as.character(value),
                                    c("four" = "4",
                                      "five" = "5")
                    ),
                    as.character(value)
    ),
    value = readr::parse_number(value)
  ) %>%
  summarise(average = round(weighted.mean(x = value, w = n), digits = 2))
################################################################################################
#### Administrator Support CRT Column Names ####
administrator_support_crt <- c(
  "please_rate_the_extent_to_which_you_believe_you_can_support_teachers_in_the_following_areas_br_i_believe_i_can_directly_or_indirectly_support_teachers_to_adapt_instruction_to_meet_the_needs_of_their_students",
  "please_rate_the_extent_to_which_you_believe_you_can_support_teachers_in_the_following_areas_br_i_believe_i_can_directly_or_indirectly_support_teachers_to_identify_ways_that_the_school_culture_e_g_values_norms_and_practices_is_different_from_their_students_home_culture",
  "please_rate_the_extent_to_which_you_believe_you_can_support_teachers_in_the_following_areas_br_i_believe_i_can_directly_or_indirectly_support_teachers_to_use_their_students_prior_knowledge_to_help_them_make_sense_of_new_information",
  "please_rate_the_extent_to_which_you_believe_you_can_support_teachers_in_the_following_areas_br_i_believe_i_can_directly_or_indirectly_support_teachers_to_revise_instructional_material_to_include_a_better_representation_of_cultural_groups",
  "please_rate_the_extent_to_which_you_believe_you_can_support_teachers_in_the_following_areas_br_i_believe_i_can_directly_or_indirectly_support_teachers_to_teach_the_curriculum_to_students_with_unfinished_learning",
  "please_rate_the_extent_to_which_you_believe_you_can_support_teachers_in_the_following_areas_br_i_believe_i_can_directly_or_indirectly_support_teachers_to_teach_the_curriculum_to_students_who_are_from_historically_marginalized_groups"
)
#### Administrator Support CRT Overall Average ####
administrator_support_crt_average <- diagnostic %>%
  select(administrator_support_crt) %>%
  pivot_longer(everything()) %>%
  drop_na(value) %>%
  group_by(value) %>%
  count(sort = T) %>%
  ungroup() %>%
  mutate(value = readr::parse_number(as.character(value))) %>%
  summarise(average = round(weighted.mean(x = value, w = n), digits = 2))
################################################################################################
#### Administrator Observational Practices ####
administrator_observational_practices <- c(
  "how_often_does_your_observation_of_teacher_practice_focus_on_the_following_whether_the_lesson_is_focused_on_a_high_quality_text_or_task",
  "how_often_does_your_observation_of_teacher_practice_focus_on_the_following_whether_the_questions_and_tasks_address_the_analytical_thinking_required_by_the_grade_level_standards",
  "how_often_does_your_observation_of_teacher_practice_focus_on_the_following_whether_all_students_have_opportunities_to_engage_in_the_work_of_the_lesson"
)
#### Admin Observational Overall Average
administrator_observational_average <- diagnostic %>%
  select(administrator_observational_practices) %>%
  pivot_longer(everything()) %>%
  drop_na(value) %>%
  group_by(value) %>%
  count(sort = T) %>%
  ungroup() %>%
  mutate(value = readr::parse_number(as.character(value))) %>%
  summarise(average = round(weighted.mean(x = value, w = n), digits = 2))
################################################################################################








