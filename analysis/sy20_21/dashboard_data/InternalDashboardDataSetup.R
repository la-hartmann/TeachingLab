# library(tidyverse)
# 
# teaching_df_readin <- read_rds(here("Data/Dashboard Data/original_df.rds")) # Read in the data
# # teaching_df <- read_rds("~/Teaching Lab/Coding/TeachingLab/PieCharter/Data/original_df.rds")
# # Relevant columns
# oldcols <- c(
#   "Professional training session",
#   "Select your site (district, parish, or network).",
#   # "Select the best description for your role.",
#   # "Select the grade-band(s) you focused on.",
#   "I am satisfied with the overall quality of today's professional learning session.",
#   "Today's topic was relevant for my role.",
#   "The activities of today's session were well-designed to help me learn.",
#   "How likely are you to apply this learning to your practice in the next 4-6 weeks?",
#   "Select the name of your first facilitator.",
#   "S/he facilitated the content clearly....12",
#   "S/he effectively built a community of learners....13",
#   "Did you have a second facilitator?",
#   "Select the name of your second facilitator.",
#   "S/he facilitated the content clearly....16",
#   "S/he effectively built a community of learners....17",
#   "How likely are you to recommend this professional learning to a colleague or friend?"
# ) # Original column names
# 
# newcols <- str_to_title(c(
#   "Professional training session",
#   "District, parish, or network",
#   # "What is the best description for your role?",
#   # "What grade band(s) do you focus on?",
#   "% satisfied with the overall quality of today's professional learning session",
#   "% Who say today's topic was relevant for my role",
#   "% Who say activities of today's session were well-designed to help me learn",
#   "How likely are you to apply this learning to your practice in the next 4-6 weeks?",
#   "Name of your first facilitator",
#   "S/he facilitated the content clearly (first facilitator)",
#   "S/he effectively built a community of learners (first facilitator)",
#   "Did you have a second facilitator?",
#   "Name of your second facilitator.",
#   "S/he facilitated the content clearly (second facilitator)",
#   "S/he effectively built a community of learners (second facilitator)",
#   "How likely are you to recommend this professional learning to a colleague or friend?"
# )) # New column names
# 
# # Session Name Replace
# 
# # session_replace <- c("Accelerating Learning in Math" = "EngageNY - Accelerating Learning in Math",
# #                      "B21 IM Inquiry Cycle III: Facilitating Student Discourse" = "IM - Inquiry Cycle III: Facilitating Math Discourse", # Not sure about this one but it is the only IM Inquiry Cycle III
# #                      "Bennington K-2 EL Modules Bootcamp" = "EL - Bootcamp: Skills (K-2)",
# #                      "Brownington EL Cycle 1" = "EL - Inquiry Cycle 1: Speaking & Listening (K-8)", # LITERALLY NO IDEA HERE
# #                      "Calcasieu Diverse Learners Fluency Cycle" = "Guidebooks - Diverse Learners Cycle: Fluency",
# #                      "Calcasieu Diverse Learners Teacher Bootcamp" = "Guidebooks - Diverse Learners Teacher Bootcamp",
# #                      "Calcasieu Diverse Learners Vocabulary Cycle" = "Guidebooks - Diverse Learners Cycle: Vocabulary",
# #                      "Cleveland Cohort 1 Cycle 1" = "", # LITERALLY NO IDEA HERE
# #                      "Cleveland Cohort 1 EL Modules Bootcamp" = "EL - Bootcamp: Modules (K-8)",
# #                      "Cleveland Cohort 2 Cycle 1" = "", # LITERALLY NO IDEA HERE
# #                      "Cleveland Cohort 2 EL Modules Bootcamp" = "EL - Bootcamp: Modules (K-8)",
# #                      "Zearn Summer Learning Recovery Series" = "Zearn - Summer PLC", 
# #                      "Wisconsin HQPL Fellowship: Quality Matters Part 2" = "State-level - WI - HQPL Fellowship (Course 2)", 
# #                      "WI Statewide HQPL Fellowship Meeting 4" = "State-level - WI - HQPL Fellowship (Course 2)", 
# #                      "WI Statewide HQPL Fellowship Meeting 2" = "State-level - WI - HQPL Fellowship (Course 1)", 
# #                      "WCC EL K-2 Skills Bootcamp" = "EL - Bootcamp: Skills (K-2)", 
# #                      "Tangipahoa Diverse Learners Fluency Cycle - Cohort 2" = "Guidebooks - Diverse Learners Cycle: Fluency", 
# #                      "Tangipahoa Diverse Learners Fluency Cycle - Cohort 1" = "Guidebooks - Diverse Learners Cycle: Fluency", 
# #                      "Tangi Cohort 2 GB HS Cycle 2" = "Guidebooks - High School Cycle 2 (Modules 6 + 7)", 
# #                      "Tangi Cohort 2 GB HS Cycle 1 (Modules 4-5)" = "Guidebooks - High School Cycle 1 (Modules 4 + 5)", 
# #                      "Tangi Cohort 2 GB Diverse Learners Bootcamp" = "Guidebooks - Diverse Learners Teacher Bootcamp", # Could also be Guidebooks - Diverse Learners Bootcamp: Writing
# #                      "Tangi Cohort 1 GB HS Cycle 2" = "Guidebooks - High School Cycle 2 (Modules 6 + 7)", 
# #                      "Tangi Cohort 1 GB HS  Cycle 1 (Modules 4-5)" = "Guidebooks - High School Cycle 1 (Modules 4 + 5)", 
# #                      "Tangi Cohort 1 GB Diverse Learners Bootcamp" = "Guidebooks - Diverse Learners Bootcamp: Writing", # Could also be Guidebooks - Diverse Learners Teacher Bootcamp
# #                      "State-Level WI Statewide HQPL Fellowship Meeting 4" = "State-level - WI - HQPL Fellowship (Course 2)", 
# #                      "State-Level WI Statewide HQPL Fellowship Meeting 2" = "State-level - WI - HQPL Fellowship (Course 1)", 
# #                      "State-Level Nebraska IM-PL Fellowship Virtual Series" = "State-level - NE HQPL Fellowship", 
# #                      "School Leaders: Lafayette SL-4" = "", # LITERALLY NO IDEA, could be either guidebooks, im, or el
# #                      "School Leaders: Lafayette SL-3" = "",  #LITERALLY NO IDEA, could be either guidebooks, im, or el
# #                      "School Leaders: IM SL-2" = "IM - School Leaders Modules 1-3", 
# #                      "School Leaders: IM SL-1" = "IM - School Leaders Modules 1-3", 
# #                      "School Leaders: GuideBooks SL-4" = "Guidebooks - School Leaders Module 4", 
# #                      "School Leaders: GuideBooks SL-3" = "Guidebooks - School Leaders Module 3", 
# #                      "School Leaders: GuideBooks SL-2" = "Guidebooks - School Leaders Module 2", 
# #                      "School Leaders: GuideBooks SL-1" = "Guidebooks - School Leaders Module 1",
# #                      "School Leaders: GuideBooks Leaders Workshop" = "", # LITERALLY NO IDEA
# #                      "School Leaders: EL SL-4" = "EL - School Leaders Module 4", 
# #                      "School Leaders: EL SL-3" = "EL - School Leaders Module 3",
# #                      "School Leaders: EL Leaders Workshop, Day 1" = "EL - School Leaders FSOT Module 1", # Probably??
# #                      "School Leaders: EL Leaders Workshop, Day" = "EL - School Leaders FSOT Module 1", # Probably??
# #                      "PS 89  EL Modules Cycle 1" = "", # SEVERAL EL OPTIONS HERE
# #                      "Pointe Coupee IM Inquiry Cycle IV: Checking for Understanding" = "IM - Inquiry Cycle IV: Checking for Understanding", 
# #                      "Pointe Coupee IM Inquiry Cycle III: Facilitating Student Discourse" = "IM - Inquiry Cycle III: Facilitating Math Discourse", 
# #                      "Nebraska IM-PL Fellowship Virtual Series" = "State-level - NE HQPL Fellowship", 
# #                      "NA Nebraska IM-PL Fellowship Virtual Series" = "State-level - NE HQPL Fellowship", 
# #                      "NA Math Curriculum Flexible: Day 7 Making Math Visible IC2 Close" = "State-level - Math Curriculum Flexible (Course 2)", 
# #                      "NA Math Curriculum Flexible: Day 6 Making Math Visible Inquiry Cycle 2 Open" = "State-level - Math Curriculum Flexible (Course 2)", 
# #                      "NA Math Curriculum Flexible: Day 5 Eliciting Student Thinking through Questioning Inquiry Cycle 1 Close" = "State-level - Math Curriculum Flexible (Course 1)", 
# #                      "NA Math Curriculum Flexible: Day 3 Eliciting Student Thinking through Questioning Inquiry Cycle 1 Open" = "State-level - Math Curriculum Flexible (Course 1)", 
# #                      "NA IM School Leaders Module 3" = "IM - School Leaders Modules 1-3", 
# #                      "NA IM Inquiry Cycle IV: Checking for Understanding" = "IM - Inquiry Cycle IV: Checking for Understanding", 
# #                      "NA IM Inquiry Cycle II: Making Math Visible" = "IM - Inquiry Cycle II: Making Math Visible", 
# #                      "NA Guidebooks Steering Committee" = "", # LITERALLY NO IDEA HERE
# #                      "NA Guidebooks School Leaders Coaching" = "All - Leader Coaching", # Probably??
# #                      "NA Guidebooks Diverse Learners: BootCamp: Writing" = "Guidebooks - Diverse Learners Bootcamp: Writing", 
# #                      "NA Guidebooks District Leaders Coaching" = "All - Leader Coaching", # Probably?? 
# #                      "NA ELA Curriculum Flexible: Day 6 Launch of Evidence Based Writing Cycle" = "State-level - ELA Curriculum Flexible (Course 2)", 
# #                      "NA ELA Curriculum Flexible: Day 5 Close of Text Complexity Cycle\"" = "State-level - ELA Curriculum Flexible (Course 1)", 
# #                      "NA EL Skills Inquiry Cycle 1: Data Informed Instruction (K-2)" = "EL - Skills Cycle 1: Using Data to Inform Foundational Skills Instruction", 
# #                      "NA EL School Leaders Module 3" = "EL - School Leaders Module 3", 
# #                      "NA EL School Leaders Module 1" = "EL - School Leaders Module 4", 
# #                      "NA EL Inquiry Cycle 2: Approaches to Complex Text (K-8)" = "EL - Inquiry Cycle 2: Approaches to Complex Text (K-8)", 
# #                      "NA EL Inquiry Cycle 1: Speaking & Listening (K-8)" = "EL - Inquiry Cycle 1: Speaking & Listening (K-8)", 
# #                      "Math Curriculum Flexible: Day 7 Making Math Visible IC2 Close" = "State-level - Math Curriculum Flexible (Course 2)", 
# #                      "Math Curriculum Flexible: Day 6 Making Math Visible Inquiry Cycle 2 Open" = "State-level - Math Curriculum Flexible (Course 2)", 
# #                      "Math Curriculum Flexible: Day 6" = "State-level - Math Curriculum Flexible (Course 2)", 
# #                      "Math Curriculum Flexible: Day 5 Eliciting Student Thinking through Questioning Inquiry Cycle 1 Close" = "State-level - Math Curriculum Flexible (Course 2)", 
# #                      "Math Curriculum Flexible: Day 4 Observing and Coaching Grounded in the IPG" = "State-level - Math Curriculum Flexible (Course 2)", 
# #                      "Math Curriculum Flexible: Day 3 Eliciting Student Thinking through Questioning Inquiry Cycle 1 Open" = "State-level - Math Curriculum Flexible (Course 2)", 
# #                      "Math Curriculum Flexible Day 2" = "State-level - Math Curriculum Flexible (Course 2)", 
# #                      "Math Curriculum Flexible Day 1" = "State-level - Math Curriculum Flexible (Course 2)", 
# #                      "LFT Guidebooks Cycle 1 HS Cohort" = "Guidebooks - High School Cycle 1 (Modules 4 + 5)", 
# #                      "LFT Guidebooks Cycle 1 ES-MS Cohort" = "Guidebooks - Cycle 1: Close Reading with Speaking & Listening (Modules 3 + 4)", # Probably?? 
# #                      "Legacy MS Guidebooks Diverse Learners - Fluency Cycle" = "Guidebooks - Diverse Learners Cycle: Fluency", 
# #                      "Legacy EngageNY 6-12 Inquiry Cycle II: Making Math Visible" = "EngageNY - 6-12 Inquiry Cycle II: Making Math Visible", 
# #                      "Legacy EngageNY 6-12 Inquiry Cycle I: Eliciting Student Thinking" = "EngageNY - 6-12 Inquiry Cycle I: Eliciting Student Thinking", 
# #                      "Legacy ELA School Leader Series" = "", # LITERALLY NO IDEA HERE
# #                      "Legacy Cohort 2: EL Modules Cycle 2" = "", # LITERALLY NO IDEA HERE could be inquiry or skills
# #                      "Legacy Cohort 2 EL Cycle 1: Speaking & Listening" = "EL - Inquiry Cycle 1: Speaking & Listening (K-8)", # Probably?? 
# #                      "Legacy Cohort 1: EL Modules Cycle 2" = "", # LITERALLY NO IDEA HERE could be inquiry or skills
# #                      "Legacy Cohort 1 EL Cycle 1: Speaking & Listening" = "EL - Inquiry Cycle 1: Speaking & Listening (K-8)", # Probably??
# #                      "Lafayette Guidebooks Inquiry Cycle 2 (Grades 3-8)" = "Guidebooks - Cycle 2: Developing Writing & Language Skills (Modules 5 + 6)", # Probably?? 
# #                      "Lafayette GB HS Cycle 2" = "Guidebooks - High School Cycle 2 (Modules 6 + 7)", 
# #                      "Kankakee MS IM Supported Planning", 
# #                      "Kankakee MS IM Inquiry Cycle III: Facilitating Student Discourse", 
# #                      "Kankakee HS IM Supported Planning" = "",  # LITERALLY NO IDEA HERE
# #                      "Kankakee HS IM Inquiry Cycle III: Facilitating Student Discourse" = "", # LITERALLY NO IDEA HERE
# #                      "Jacob  Elementary EL Cycle 1" = "", # LITERALLY NO IDEA HERE 
# #                      "Innovation Network Fellowship, Meeting 4, Day 2" = "", # LITERALLY NO IDEA, doesn't exist?? 
# #                      "Innovation Network Fellowship, Meeting 4, Day 1" = "", # LITERALLY NO IDEA, doesn't exist?? 
# #                      "Innovation Network Fellowship, Meeting 2" = "", # LITERALLY NO IDEA, doesn't exist?? 
# #                      "Innovation Network Fellowship, Meeting 1" = "", # LITERALLY NO IDEA, doesn't exist?? 
# #                      "IM Unit Unpacking" = "", # LITERALLY NO IDEA, doesn't exist?? 
# #                      "IM Student Thinking, Day 2" = "IM - Inquiry Cycle I: Eliciting Student Thinking", # Probably??
# #                      "IM Student Thinking, Day 1" = "IM - Inquiry Cycle I: Eliciting Student Thinking", # Probably??
# #                      "IM Student Conversations, Day 1" = "", # LITERALLY NO IDEA, doesn't exist?? 
# #                      "IM School Leaders Module 3" = "IM - School Leaders Modules 1-3", 
# #                      "IM Making Math Visible, Day 2" = "IM - Inquiry Cycle II: Making Math Visible", 
# #                      "IM Making Math Visible, Day 1" = "IM - Inquiry Cycle II: Making Math Visible", 
# #                      "IM Inquiry Cycle IV: Checking for Understanding" = "IM - Inquiry Cycle IV: Checking for Understanding", 
# #                      "IM Inquiry Cycle III: Facilitating Math Discourse" = "IM - Inquiry Cycle III: Facilitating Math Discourse", 
# #                      "IM Inquiry Cycle II: Making Math Visible" = "IM - Inquiry Cycle II: Making Math Visible", 
# #                      "IM Inquiry Cycle I: Eliciting Student Thinking" = "IM - Inquiry Cycle I: Eliciting Student Thinking", 
# #                      "IM Coaching" = "All - Teacher Coaching", # Probably??
# #                      "IM BootCamp, Day 3: Springfield EZ" = "IM - Bootcamp", 
# #                      "IM Bootcamp, Day 3: B21" = "IM - Bootcamp", 
# #                      "IM Bootcamp, Day 3" = "IM - Bootcamp",
# #                      "IM Bootcamp, Day 2: STRIVE/CPS" = "IM - Bootcamp", 
# #                      "IM BootCamp, Day 2: Springfield EZ" = "IM - Bootcamp", 
# #                      "IM Bootcamp, Day 2: Freire" = "IM - Bootcamp", 
# #                      "IM Bootcamp, Day 2: B21" = "IM - Bootcamp", 
# #                      "IM BootCamp, Day 2" = "IM - Bootcamp", 
# #                      "IM Bootcamp, Day 2" = "IM - Bootcamp", 
# #                      "IM Bootcamp, Day 1: STRIVE/CPS" = "IM - Bootcamp", 
# #                      "IM BootCamp, Day 1: Springfield EZ" = "IM - Bootcamp", 
# #                      "IM Bootcamp, Day 1: Freire" = "IM - Bootcamp", 
# #                      "IM Bootcamp, Day 1: B21" = "IM - Bootcamp", 
# #                      "IM Bootcamp, Day 1" = "IM - Bootcamp", 
# #                      "Illustrative Mathematics Zearn Summer Learning Recovery Series" = "Zearn - Summer PLC", # Probably??
# #                      "Illustrative Mathematics Math Curriculum Flexible: Day 8 Student Discourse IC3 Open" = "State-level - Math Curriculum Flexible (Course 2)", 
# #                      "Illustrative Mathematics Math Curriculum Flexible: Day 6" = "State-level - Math Curriculum Flexible (Course 2)", 
# #                      "Illustrative Mathematics Math Curriculum Flexible: Day 5 Eliciting Student Thinking through Questioning Inquiry Cycle 1 Close" = "State-level - Math Curriculum Flexible (Course 1)", 
# #                      "Illustrative Mathematics Math Curriculum Flexible: Day 4 Observing and Coaching Grounded in the IPG" = "State-level - Math Curriculum Flexible (Course 1)", 
# #                      "Illustrative Mathematics Math Curriculum Flexible: Day 3 Eliciting Student Thinking through Questioning Inquiry Cycle 1 Open" = "State-level - Math Curriculum Flexible (Course 1)", 
# #                      "Illustrative Mathematics Math Curriculum Flexible Day 2" = "State-level - Math Curriculum Flexible (Course 1)", 
# #                      "Illustrative Mathematics Math Curriculum Flexible Day 1" = "State-level - Math Curriculum Flexible (Course 1)", 
# #                      "Illustrative Mathematics IM Unit Unpacking" = "", # LITERALLY NO IDEA, doesn't exist?? 
# #                      "Illustrative Mathematics IM School Leaders Module 3" = "IM - School Leaders Modules 1-3", 
# #                      "Illustrative Mathematics IM Lab Leaders Session 3" = "IM - Lab Leaders (Year 2)", 
# #                      "Illustrative Mathematics IM Inquiry Cycle III: Facilitating Math Discourse" = "IM - Inquiry Cycle III: Facilitating Math Discourse", 
# #                      "Illustrative Mathematics IM Inquiry Cycle II: Making Math Visible" = "IM - Inquiry Cycle II: Making Math Visible", 
# #                      "Illustrative Mathematics IM Inquiry Cycle I: Eliciting Student Thinking" = "IM - Inquiry Cycle I: Eliciting Student Thinking", 
# #                      "Illustrative Mathematics Illustrative Mathematics Bootcamp" = "IM - Bootcamp", 
# #                      "Illustrative Mathematics EngageNY K-5 Inquiry Cycle I: Eliciting Student Thinking" = "EngageNY - 6-12 Inquiry Cycle I: Eliciting Student Thinking", 
# #                      "Illustrative Mathematics EngageNY K-5 Bootcamp" = "EngageNY - K-5 Bootcamp", 
# #                      "Illustrative Mathematics EngageNY 6-12 Bootcamp" = "EngageNY - 6-12 Bootcamp", 
# #                      "Illustrative Mathematics DE Curriculum Flexible Cycle 3 - Opening" = "State-level - Math Curriculum Flexible (Course 2)",
# #                      "Illustrative Mathematics DE Curriculum Flexible Cycle 3 - Closing" = "State-level - Math Curriculum Flexible (Course 2)", 
# #                      "Illustrative Mathematics DE Curriculum Flexible Cycle 2 - Opening" = "State-level - Math Curriculum Flexible (Course 2)", 
# #                      "Illustrative Mathematics DE Curriculum Flexible Cycle 2 - Closing" = "State-level - Math Curriculum Flexible (Course 2)", 
# #                      "Illustrative Mathematics DE Curriculum Flexible Cycle 1 - Opening" = "State-level - Math Curriculum Flexible (Course 1)", 
# #                      "Illustrative Mathematics DE Curriculum Flexible Cycle 1 - Closing, Pt. 2" = "State-level - Math Curriculum Flexible (Course 1)", 
# #                      "Illustrative Mathematics DE Curriculum Flexible Cycle 1 - Closing" = "State-level - Math Curriculum Flexible (Course 1)", 
# #                      "Illustrative Mathematics DE Curriculum Flexible Bootcamp Day 2" = "IM - Bootcamp", # Probably??
# #                      "Illustrative Mathematics DE Curriculum Flexible Bootcamp" = "SIM - Bootcamp", # Probably??
# #                      "Illustrative Mathematics Bootcamp" = "IM - Bootcamp", 
# #                      "Illustrative Mathematics Accelerating Learning in Math" = "IM - Accelerating Learning in Math", 
# #                      "Guidebooks Writing PLC" = "Guidebooks - Diverse Learners Bootcamp: Writing", # Probably??
# #                      "Guidebooks Unit Unpacking" = "Guidebooks - Unit Unpacking", 
# #                      "Guidebooks Steering Committee" = "", # LITERALLY NO IDEA HERE
# #                      "Guidebooks High School Cycle: CM4-5" = "Guidebooks - High School Cycle 1 (Modules 4 + 5)", 
# #                      "Guidebooks High School Bootcamp" = "Guidebooks - High School Bootcamp (Modules 1, 2, 3)", # Probably?? Could be modules 0,1,2
# #                      "Guidebooks Diverse Learners: BootCamp: Writing" = "Guidebooks - Diverse Learners Bootcamp: Writing", 
# #                      "Guidebooks Diverse Learners Writing BootCamp" = "Guidebooks - Diverse Learners Bootcamp: Writing", 
# #                      "Guidebooks Diverse Learners Vocabulary, Day 2" = "Guidebooks - Diverse Learners Cycle: Vocabulary", 
# #                      "Guidebooks Diverse Learners Vocabulary, Day 1" = "Guidebooks - Diverse Learners Cycle: Vocabulary", 
# #                      "Guidebooks Diverse Learners Teacher Bootcamp" = "Guidebooks - Diverse Learners Teacher Bootcamp", 
# #                      "Guidebooks Diverse Learners Leader Bootcamp" = "Guidebooks - DL Leader Bootcamp", 
# #                      "Guidebooks Diverse Learners Fluency, Day 2" = "Guidebooks - Diverse Learners Cycle: Fluency", 
# #                      "Guidebooks Diverse Learners Fluency, Day 1" = "Guidebooks - Diverse Learners Cycle: Fluency", 
# #                      "Guidebooks Diverse Learners Cycle: Vocabulary" = "Guidebooks - Diverse Learners Cycle: Vocabulary", 
# #                      "Guidebooks Diverse Learners Cycle: Fluency" = "Guidebooks - Diverse Learners Cycle: Fluency", 
# #                      "Guidebooks Diverse Learners BootCamp, Day 2" = "Guidebooks - DL Leader Bootcamp", # Probably??
# #                      "Guidebooks Diverse Learners BootCamp, Day 1: Leaders" = "Guidebooks - DL Leader Bootcamp", 
# #                      "Guidebooks Diverse Learners BootCamp, Day 1" = "Guidebooks - DL Leader Bootcamp", 
# #                      "Guidebooks Coaching" = "All - Leader Coaching", # Probably?? 
# #                      "Guidebooks CM-6", # LITERALLY NO IDEA HERE
# #                      "Guidebooks CM-5", # LITERALLY NO IDEA HERE
# #                      "Guidebooks CM-4", # LITERALLY NO IDEA HERE
# #                      "Guidebooks CM-3", # LITERALLY NO IDEA HERE
# #                      "Guidebooks CM-2", # LITERALLY NO IDEA HERE
# #                      "Guidebooks CM-1", # LITERALLY NO IDEA HERE
# #                      "Guidebooks CM-0", # LITERALLY NO IDEA HERE
# #                      "Guidebooks CLM-9", # LITERALLY NO IDEA HERE
# #                      "Guidebooks CLM-8", # LITERALLY NO IDEA HERE
# #                      "Guidebooks Abbreviated Bootcamp" = "Guidebooks - Abbreviated Bootcamp", 
# #                      "General: Math" = "", # LITERALLY NO IDEA HERE
# #                      "General: ELA", # LITERALLY NO IDEA HERE
# #                      "Freire IM Inquiry Cycle IV: Checking for Understanding" = "IM - Inquiry Cycle IV: Checking for Understanding", 
# #                      "Fort Dodge School Leader Skills Module" = "",  # LITERALLY NO IDEA HERE
# #                      "Fort Dodge K-2 Skills Cycle 1: Using Data to Inform Instruction" = "EL - Skills Cycle 1: Using Data to Inform Foundational Skills Instruction", 
# #                      "Fort Dodge EL Skills Cycle 2" = "EL - Skills Cycle 2: Establishing a Self-Managed Classroom", 
# #                      "FDMS: EL Modules Cycle 2" = "", # LITERALLY NO IDEA HERE
# #                      "FDMS EL School Leader Series" = "", # LITERALLY NO IDEA HERE
# #                      "FDMS EL Cycle 1: Speaking & Listening to Deepen Student Understanding of Complex Texts and Topics" = "State-level - ELA Curriculum Flexible (Course 2)", 
# #                      "Evangeline K Cohort: EL Cycle 1" = "", # LITERALLY NO IDEA HERE, could be inquiry or skill cycle
# #                      "Evangeline G2 Cohort: EL Cycle 1" = "", # LITERALLY NO IDEA HERE, could be inquiry or skill cycle
# #                      "Evangeline G1 Cohort: EL Cycle 1" = "", # LITERALLY NO IDEA HERE, could be inquiry or skill cycle
# #                      "EngageNY K-5 Inquiry Cycle I: Eliciting Student Thinking" = "EngageNY - K-5 Inquiry Cycle I: Eliciting Student Thinking",
# #                      "EngageNY K-5 Bootcamp" = "EngageNY - K-5 Bootcamp", 
# #                      "EngageNY 6-12 Bootcamp" = "EngageNY - 6-12 Bootcamp", 
# #                      "ELA Curriculum Flexible: Day 6 Launch of Evidence Based Writing Cycle" = "State-level - ELA Curriculum Flexible (Course 2)", 
# #                      "ELA Curriculum Flexible: Day 5 Close of Text Complexity Cycle" = "State-level - ELA Curriculum Flexible (Course 1)", 
# #                      "ELA Curriculum Flexible: Day 4 Observing and Coaching Grounded in the IPG" = "State-level - ELA Curriculum Flexible (Course 1)", 
# #                      "ELA Curriculum Flexible: Day 3 Launch of Text Complexity Cycle" = "State-level - ELA Curriculum Flexible (Course 1)", 
# #                      "ELA Curriculum Flexible Day 2" = "State-level - ELA Curriculum Flexible (Course 1)", 
# #                      "EL Virtual Bootcamp" = "", # LITERALLY NO IDEA HERE
# #                      "EL Unit Unpacking" = "", # LITERALLY NO IDEA HERE, unpacking only exists 
# #                      "EL Skills Inquiry Cycle 1: Data Informed Instruction (K-2)" = "EL - Bootcamp: Skills (K-2)", 
# #                      "EL School Leaders Module 2" = "EL - School Leaders Module 2", 
# #                      "EL Lab Leaders PLC 3" = "All - Lab Leader Coaching ", # Probably??, no EL lab leaders as far as I can tell
# #                      "EL Lab Leaders PLC 2" = "All - Lab Leader Coaching ", # Probably??
# #                      "EL Lab Leaders PLC 1" = "All - Lab Leader Coaching ", # Probably??
# #                      "EL K-5 Text-based Writing, Day 2" = "EL - Inquiry Cycle 3: Text-Based Writing (K-8)", # Probably?? 
# #                      "EL K-5 Text-based Writing, Day 1" = "EL - Inquiry Cycle 3: Text-Based Writing (K-8)", # Probably?? 
# #                      "EL K-5 Speaking & Listening, Day 2" = "EL - Inquiry Cycle 1: Speaking & Listening (K-8)", # Probably?? 
# #                      "EL K-5 Speaking & Listening, Day 1" = "EL - Inquiry Cycle 1: Speaking & Listening (K-8)", # Probably?? 
# #                      "EL K-5 Close Reading & Complex Texts, Day 2" = "", # LITERALLY NO IDEA HERE, doesn't exist??
# #                      "EL K-5 Close Reading & Complex Texts, Day 1" = "", # LITERALLY NO IDEA HERE, doesn't exist??
# #                      "EL K-5 BootCamp, Day 2: Robin Hood" = "", # LITERALLY NO IDEA HERE 
# #                      "EL K-5 BootCamp, Day 2" = "", # LITERALLY NO IDEA HERE 
# #                      "EL K-5 Bootcamp, Day 1: Robin Hood" = "", # LITERALLY NO IDEA HERE 
# #                      "EL K-5 BootCamp, Day 1" = "", # LITERALLY NO IDEA HERE
# #                      "EL Inquiry Cycle 2: Approaches to Complex Text (K-8)" = "EL - Inquiry Cycle 2: Approaches to Complex Text (K-8)", 
# #                      "EL Inquiry Cycle 1: Speaking & Listening (K-8)" = "EL - Inquiry Cycle 1: Speaking & Listening (K-8)", 
# #                      "EL Guidebooks Unit Unpacking" = "Guidebooks - Unit Unpacking", 
# #                      "EL Guidebooks High School Cycle: CM4-5" = "Guidebooks - High School Cycle 1 (Modules 4 + 5)", 
# #                      "EL Guidebooks High School Bootcamp" = "Guidebooks - High School Bootcamp (Modules 1, 2, 3)", 
# #                      "EL Guidebooks Diverse Learners Teacher Bootcamp" = "Guidebooks - Diverse Learners Teacher Bootcamp", 
# #                      "EL Guidebooks Diverse Learners Leader Bootcamp" = "Guidebooks - DL Leader Bootcamp", 
# #                      "EL Guidebooks Diverse Learners Cycle: Vocabulary" = "Guidebooks - Diverse Learners Cycle: Vocabulary", 
# #                      "EL Guidebooks Diverse Learners Cycle: Fluency" = "Guidebooks - Diverse Learners Cycle: Fluency", 
# #                      "EL Guidebooks Abbreviated Bootcamp" = "Guidebooks - Abbreviated Bootcamp", 
# #                      "EL ELA Curriculum Flexible: Day 5 Close of Text Complexity Cycle" = "State-level - ELA Curriculum Flexible (Course 1)", 
# #                      "EL ELA Curriculum Flexible: Day 4 Observing and Coaching Grounded in the IPG" = "State-level - ELA Curriculum Flexible (Course 1)", 
# #                      "EL ELA Curriculum Flexible: Day 3 Launch of Text Complexity Cycle" = "State-level - ELA Curriculum Flexible (Course 1)", 
# #                      "EL ELA Curriculum Flexible Day 2" = "State-level - ELA Curriculum Flexible (Course 1)", 
# #                      "EL ELA Curriculum Flex: Day 8 Launch of Speaking & Listening Cycle" = "State-level - ELA Curriculum Flexible (Course 2)", 
# #                      "EL EL Unit Unpacking" = "", # LITERALLY NO IDEA, doesn't exist?? 
# #                      "EL EL Skills Inquiry Cycle 1: Data Informed Instruction (K-2)" = "EL - Skills Cycle 1: Using Data to Inform Foundational Skills Instruction", # Probably?? 
# #                      "EL EL School Leaders Module 3" = "EL - School Leaders Module 3", 
# #                      "EL EL School Leaders Module 2" = "EL - School Leaders Module 2", 
# #                      "EL EL School Leaders Module 1" = "EL - School Leaders Module 1", 
# #                      "EL EL Inquiry Cycle 1: Speaking & Listening (K-8)" = "EL - Inquiry Cycle 1: Speaking & Listening (K-8)", 
# #                      "EL EL Bootcamp: Skills (K-2)" = "EL - Bootcamp: Skills (K-2)", 
# #                      "EL EL Bootcamp: Modules (K-8)" = "EL - Bootcamp: Modules (K-8)", 
# #                      "EL EL Bootcamp: ALL Block (3-5)" = "EL - Bootcamp: ALL Block (3-5)", 
# #                      "EL DE Curriculum Flexible Cycle 3 - Opening" = "State-level - ELA Curriculum Flexible (Course 2)", 
# #                      "EL DE Curriculum Flexible Cycle 2 - Opening" = "State-level - ELA Curriculum Flexible (Course 2)", 
# #                      "EL DE Curriculum Flexible Cycle 2 - Closing" = "State-level - ELA Curriculum Flexible (Course 2)", 
# #                      "EL DE Curriculum Flexible Cycle 1 - Opening" = "State-level - ELA Curriculum Flexible (Course 1)", 
# #                      "EL DE Curriculum Flexible Cycle 1 - Closing, Pt. 2" = "State-level - ELA Curriculum Flexible (Course 1)", 
# #                      "EL DE Curriculum Flexible Cycle 1 - Closing" = "State-level - ELA Curriculum Flexible (Course 1)", 
# #                      "EL DE Curriculum Flexible Bootcamp Day 2" = "", # LITERALLY NO IDEA, doesn't seem to exist
# #                      "EL Bootcamp: Skills (K-2)" = "EL - Bootcamp: Skills (K-2)", 
# #                      "EL Bootcamp: Modules (K-8)" = "EL - Bootcamp: Modules (K-8)", 
# #                      "EL Bootcamp: ALL Block (3-5)" = "EL - Bootcamp: ALL Block (3-5)", 
# #                      "EBR DL Cycle 2: Fluency Cohort 1" = "Guidebooks - Diverse Learners Cycle: Fluency", # Probably??
# #                      "DeSoto Skills Cycle 1: Kindergarten Cohort" = "EL - Skills Cycle 1: Using Data to Inform Foundational Skills Instruction", # Probably?? 
# #                      "DeSoto Skills Cycle 1: Grades 1-2 Cohort" = "EL - Skills Cycle 1: Using Data to Inform Foundational Skills Instruction", # Probably?? 
# #                      "DeSoto EL Skills Cycle 2" = "EL - Skills Cycle 2: Establishing a Self-Managed Classroom", 
# #                      "DeSoto EL School Leader Series" = "EL - School Leader Module 5: Foundational Skills", # Probably?? 
# #                      "DE Curriculum Flexible Cycle 3 - Opening" = "State-level - ELA Curriculum Flexible (Course 2)", 
# #                      "DE Curriculum Flexible Cycle 3 - Closing" = "State-level - ELA Curriculum Flexible (Course 2)", 
# #                      "DE Curriculum Flexible Cycle 2 - Opening" = "State-level - ELA Curriculum Flexible (Course 2)", 
# #                      "DE Curriculum Flexible Cycle 2 - Closing" = "State-level - ELA Curriculum Flexible (Course 2)", 
# #                      "DE Curriculum Flexible Cycle 1 - Opening" = "State-level - ELA Curriculum Flexible (Course 1)", 
# #                      "DE Curriculum Flexible Cycle 1 - Closing, Pt. 2" = "State-level - ELA Curriculum Flexible (Course 1)", 
# #                      "DE Curriculum Flexible Cycle 1 - Closing" = "State-level - ELA Curriculum Flexible (Course 1)", 
# #                      "DE Curriculum Flexible Bootcamp Day 2" = "", # LITERALLY NO IDEA, doesn't seem to exist
# #                      "DE Curriculum Flexible Bootcamp" = "", # LITERALLY NO IDEA, doesn't seem to exist
# #                      "Cleveland Eureka Math Bootcamp K-5 - Cohort 3" = "IM - Bootcamp", # Probably??
# #                      "Cleveland Eureka Math Bootcamp K-5 - Cohort 2" = "IM - Bootcamp", # Probably??
# #                      "Cleveland Eureka Math Bootcamp K-5 - Cohort 1" = "IM - Bootcamp", # Probably??
# #                      "Cleveland Eureka Math Bootcamp 6-8 - Cohort 6" = "IM - Bootcamp", # Probably??
# #                      "Cleveland Eureka Math Bootcamp 6-8 - Cohort 5" = "IM - Bootcamp", # Probably??
# #                      "Cleveland Cohort 4 EL Modules Bootcamp" = "EL - Bootcamp: Modules (K-8)", 
# #                      "Cleveland Cohort 3 EL Modules Bootcamp" = "EL - Bootcamp: Modules (K-8)", 
# #                      "Cleveland Cohort 3 Cycle 1" = "", # LITERALLY NO IDEA 
# #                      "Cleveland Cohort 2 EL Modules Bootcamp" = "EL - Bootcamp: Modules (K-8)")
# 
# network_replace <- c("NYC District 11 - District-wide, NY" = "NYC District 11 - District wide, NY",
#                      "NYC District 11 - IS 355, NY" = "NYC District 11, IS 355",
#                      "-(?! )" = "NA",
#                      "OTHER" = "NA",
#                      "Robinhood Schools (NYC), NY" = "Robinhood Schools (NYC)",
#                      "Delaware DOE" = "Delaware Department of Education, DE",
#                      "Calcasieu Parish, LA" = "Calcasieu Parish",
#                      "Building 21 - PA" = "Building 21",
#                      "Building 21 NA Allentown, PA" = "Building 21",
#                      "Building 21 NA Philadelphia, PA" = "Building 21",
#                      "Building 21 NA PA" = "Building 21",
#                      "Cincinnati Public Schools/ STRIVE, OH" = "Cincinnati Public Schools/ STRIVE",
#                      "Cleveland Metropolitan School District, OH" = "Cleveland Metropolitan School District",
#                      "Freire Charter Schools, PA/DE" = "Freire Charter Schools",
#                      "Iberia Parish, LA" = "Iberia Parish",
#                      "Louisiana State Content Leader Training, LA" = "Louisiana State Content Leader Training",
#                      "NYC District 11 - PS 96, NY" = "NYC District 11, PS 96",
#                      "Tangipahoa Parish, LA" = "Tangipahoa Parish",
#                      "Washington Parish, LA" = "Washington Parish",
#                      "West Contra Costa USD, CA" = "West Contra Costa USD",
#                      "Fort Dodge Schools, IA" = "Fort Dodge Schools",
#                      "Lafayette Parish, LA" = "Lafayette Parish",
#                      "DeSoto Parish, LA" = "DeSoto Parish",
#                      "East Baton Rouge Parish, LA" = "East Baton Rouge Parish",
#                      "ESU 2, NE" = "ESU 2, Nebraska",
#                      "Ft. Dodge Schools" = "Fort Dodge Schools",
#                      "Pointe Coupee Parish, LA" = "Pt. Coupee Parish",
#                      "Robinhood Schools NA STEAM Bridge, NY" = "Robinhood Schools STEAM Bridge, NY")
# 
# # Small data clean
# teaching_df_readin <- teaching_df_readin %>%
#   select(-`Select the grade-band(s) you focused on.`,
#          -`Select the best description for your role.`) %>%
#   rename_with(~ newcols[which(oldcols == .x)], .cols = oldcols) %>%
#   mutate(`Date for the session` = lubridate::ymd(`Date for the session`)) %>%
#   mutate(Portfolio = case_when(!str_detect(`Professional Training Session`, c("EL|IM|Guidebooks|GuideBooks")) == T ~ "State-Level",
#                                str_detect(`Professional Training Session`, "IM") == T ~ "Illustrative Mathematics",
#                                str_detect(`Professional Training Session`, "Guidebooks|GuideBooks") == T ~ "Guidebooks",
#                                str_detect(`Professional Training Session`, "EL") == T ~ "EL"))#,
#          # `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "Grades 6-8;Grades 9-12", "Grades 6-8, Grades 9-12"),
#          # `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "All grades K-12", "All Grades"),
#          # `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "Grades 9-12, All grades K-12", "All Grades"),
#          # `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "Grades K-2, Grades 3-5, Grades 6-8, Grades 9-12, All grades K-12", "All Grades"),
#          # `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "Grades K-2, Grades 3-5, Grades 6-8, Grades 9-12", "All Grades"),
#          # `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "All Grades, All Grades", "All Grades"))
# 
# # teaching_df <- teaching_df_readin
# 
# # Making the reviews of multiple facilitators in a session into one
# # Split the data
# community_content_second <- teaching_df_readin %>%
#   filter(`Did You Have A Second Facilitator?` == "Yes") %>%
#   dplyr::select(!c(`S/He Facilitated The Content Clearly (First Facilitator)`, 
#                    `S/He Effectively Built A Community Of Learners (First Facilitator)`,
#                    `Name Of Your First Facilitator`)) %>%
#   rename(`S/He Facilitated The Content Clearly` = `S/He Facilitated The Content Clearly (Second Facilitator)`,
#          `S/He Effectively Built A Community Of Learners` = `S/He Effectively Built A Community Of Learners (Second Facilitator)`,
#          `Name Of Your First Facilitator` = `Name Of Your Second Facilitator.`)
# 
# # Name replacement vector
# name_replace <- c("Octavia" = "Octavia Nixon", "Vaishali" = "Vaishali Joshi", "Ryan C" = "Ryan Colon",
#                   "Holli" = "Holli Fears", "Addie" = "Addie Kelley", "Adrienne" = "Adrienne Williams",
#                   "Anita" = "Anita Walls", "Brad" = "Brad Haggerty", "Christi" = "Christi Denning", "Christi Herrigna" = "Christi Denning",
#                   "Erin" = "Erin Abraham", "Evan" = "Evan Rushton", "Jalinda" = "Jalinda Soto",
#                   "John" = "John Silverthorne", "Justin" = "Justin Endicott", "Katie" = "Katie Endicott",
#                   "Lauren" = "Lauren Myer", "Lindsay" = "Lindsay Tomlinson", "Lindsey" = "Lindsey Tomlinson",
#                   "Liza" = "Liza Zarifi", "Mandi" = "Mandi Van Dellen", "Mandy" = "Mandy Flora", "Meredith" = "Meredith Starks",
#                   "Rod" = "Rod Naquin", "Sarah" = "Sarah Tierney", "Sheena" = "Sheena Lights", "Ryan S" = "Ryan Mateo Sharnbroich",
#                   "Spencer" = "Spencer Russell", "Stacy" = "Stacy Weldon", "Stephanie" = "Stephanie Carpenter",
#                   "Tamala" = "Tamala Wiley", "Tara" = "Tara McDonald", "Tia" = "Tiayana Marks", "Zoe" = "Zoe Rind",
#                   "Fitz" = "Andrea Fitzgerald", "Hannah" = "Hannah Lawler", "Auddie" = "Auddie Mastroleo",
#                   "Jill" = "Jill Syvret", "Farren" = "Farren Liben", "Rebecca" = "Rebecca Stephenson",
#                   "Leigh" = "Leigh Topp", "Jen" = "Jen Slavick", "Bennison" = "Bennison Ntsakey", "Tracey" = "Tracey Waters",
#                   "Sable" = "Sable Mensah", "Brett" = "Brett Shield", "Amy Baker-Sheridan" = "Non-TL Facilitators",
#                   "Pamala Alfaro" = "Non-TL Facilitators", "Renee Parsley" = "Non-TL Facilitators")
# 
# # Bind it to original dataframe
# teaching_df_merge <- teaching_df_readin %>%
#   dplyr::select(-c(`S/He Facilitated The Content Clearly (Second Facilitator)`,
#                    `S/He Effectively Built A Community Of Learners (Second Facilitator)`,
#                    `Name Of Your Second Facilitator.`,
#                    -`Did You Have A Second Facilitator?`)) %>%
#   rename(`S/He Effectively Built A Community Of Learners` = `S/He Effectively Built A Community Of Learners (First Facilitator)`,
#          `S/He Facilitated The Content Clearly` = `S/He Facilitated The Content Clearly (First Facilitator)`) %>%
#   bind_rows(community_content_second) %>%
#   rename(`Name Of Your Facilitator` = `Name Of Your First Facilitator`) %>%
#   mutate(`Name Of Your Facilitator` = str_replace_all(`Name Of Your Facilitator`, name_replace))
# 
# # Moodle data merge
# moodle_data <- read_rds(here("Data/Dashboard Data/moodle_export_reformat.rds")) %>%
#   mutate(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?` = as.numeric(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`))
# # Sheets Data Merge
# sheets_data <- read_rds(here("Data/Dashboard Data/sheets_data_merge.rds")) %>%
#   mutate(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?` = as.numeric(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`),
#          `Date for the session` = as.Date(`Date for the session`))
# 
# teaching_df <- full_join(teaching_df_merge, moodle_data) %>%
#   full_join(sheets_data) %>%
#   mutate(across(c(4:18, 20:24), ~ replace_na(., "No Response"))) %>%
#   mutate(across(c(4:18, 20:24), ~ replace_na(., "No Response"))) %>%
#   mutate(across(c(4:18, 20:24), ~ str_replace_all(.x, "NULL", "No Response"))) %>%
#   select(-Compare, -Concatcomments) %>%
#   mutate(`District, Parish, Or Network` = str_replace_all(`District, Parish, Or Network`, network_replace)) %>%
#   mutate(`District, Parish, Or Network` = str_remove_all(`District, Parish, Or Network`, ", NY")) %>%
#   mutate(`Professional Training Session` = str_replace_all(`Professional Training Session`, "NA Guidebooks Abbreviated Bootcamp", "Guidebooks Abbreviated Bootcamp"))
# 
# 
# write_rds(teaching_df, here("Data/Dashboard Data/dashboard_data.rds"))
# write_rds(teaching_df, here("Data/dashboard_data.rds"))
# 
# write_rds(teaching_df, here("ParticipantFeedback/Data/dashboard_data.rds"))
# 
