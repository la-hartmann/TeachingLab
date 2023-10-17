library(dplyr)
library(googlesheets4)
library(purrr)
library(qualtRics)

ipg_forms <- qualtRics::fetch_survey(
  surveyID = "SV_0BSnkV9TVXK1hjw",
  verbose = TRUE,
  force_request = TRUE,
  start_date = as.Date("2023-08-01"),
  include_display_order = FALSE
) |>
  dplyr::filter(Finished == TRUE)

ipg_forms_selected <- ipg_forms |>
  dplyr::select(-c(StartDate,
                   EndDate,
                   Status,
                   IPAddress,
                   Progress,
                   `Duration (in seconds)`,
                   Finished,
                   ResponseId,
                   RecipientLastName,
                   RecipientFirstName,
                   RecipientEmail,
                   ExternalReference,
                   LocationLatitude,
                   LocationLongitude,
                   DistributionChannel,
                   UserLanguage,
                   QLTI_Valid,
                   QLTI_App,
                   QLTI_RequestToken,
                   QLTI_ErrorToken))

overall_ipg_write <- ipg_forms_selected |>
  dplyr::rename(`Please write in your name. (coach)` = coach_other,
                `Please write in your name. (not coach)` = rater_name,
                `Learning Goal (WI):` = wi_learning_goal,
                `Learning Goal (Math)` = k12_m_learning_goal,
                `Core Action 1 Notes (Math)` = k12_m_ca1_notes,
                `Core Action 2 Notes (Math)` = k12_m_ca2_notes,
                `Core Action 3 Notes (Math)` = k12_m_ca3_notes,
                `Teacher name - Selected Choice 1` = IPRT_1_4,
                `Teacher name - Selected Choice 2` = IPRT_1_5,
                `Teacher name - Selected Choice 3` = IPRT_1_6,
                `Teacher name - Selected Choice 4` = IPRT_1_7,
                `Teacher name - Selected Choice 5` = IPRT_1_8,
                `Teacher name - Selected Choice 6` = IPRT_1_9,
                `Teacher name - Selected Choice 7` = IPRT_1_10,
                `Do you need to complete an additional Scoring Form for another 15 minute observation segment? (1)` = IPRT_1_35,
                `Do you need to complete an additional Scoring Form for another 15 minute observation segment? (2)` = IPRT_2_19,
                `Do you need to complete an additional Scoring Form for another 15 minute observation segment? (3)` = IPRT_3_19,
                `Do you need to complete an additional Scoring Form for another 15 minute observation segment? (4)` = IPRT_4_19,
                `Do you need to complete an additional Scoring Form for another 15 minute observation segment? (5)` = IPRT_5_19,
                `Teacher provides asset-based, relevant, and specific feedback on student responses. (Scaffolding)` = ma_scaff_7,
                `Selected Date` = 2) |>
  mutate(across(c("Please write in your name. (coach)",
                  "Please write in your name. (not coach)",
                  "Learning Goal (WI):",
                  "Learning Goal (Math)",
                  "Core Action 1 Notes (Math)",
                  "Core Action 2 Notes (Math)",
                  "Core Action 3 Notes (Math)",
                  "Teacher name - Selected Choice 1",
                  "Teacher name - Selected Choice 2",
                  "Teacher name - Selected Choice 3",
                  "Teacher name - Selected Choice 4",
                  "Teacher name - Selected Choice 5",
                  "Teacher name - Selected Choice 6",
                  "Teacher name - Selected Choice 7",
                  "Do you need to complete an additional Scoring Form for another 15 minute observation segment? (1)",
                  "Do you need to complete an additional Scoring Form for another 15 minute observation segment? (2)",
                  "Do you need to complete an additional Scoring Form for another 15 minute observation segment? (3)",
                  "Do you need to complete an additional Scoring Form for another 15 minute observation segment? (4)",
                  "Do you need to complete an additional Scoring Form for another 15 minute observation segment? (5)",
                  "Teacher provides asset-based, relevant, and specific feedback on student responses. (Scaffolding)",
                  "ma_twps_time",
                  "Selected Date"), ~ as.character(.x))) |>
  TeachingLab::relabel_qualtrics_df() |>
  mutate(across(where(is.factor), ~ na_if(as.character(.x), "NA"))) |>
  janitor::remove_empty("cols")

Sys.sleep(15)

already_written <- read_sheet("1mRZThJuslwVGXyWNY2Z-gAgh0FZKaJM1LIHRwhRvMg8",
                              "All data")

sheet_append(data = overall_ipg_write |> filter(`Recorded Date` > max(already_written$`Recorded Date`)),
            ss = "1mRZThJuslwVGXyWNY2Z-gAgh0FZKaJM1LIHRwhRvMg8",
            sheet = "All data"#,
            # col_names = TRUE,
            # reformat = FALSE,
            # range = cell_limits(c(1, 1), c(nrow(overall_ipg_write) + 1, ncol(overall_ipg_write)))
            )

# get_new_data_to_write <- function(data, sheet) {
#   
#   max_date <- max(sheet$RecordedDate, na.rm = TRUE)
#   
#   data |>
#     dplyr::filter(RecordedDate > max_date) |>
#     TeachingLab::relabel_qualtrics_df()
# }

ipg_split_data <- ipg_forms_selected |>
  dplyr::filter(RecordedDate > max(already_written$`Recorded Date`)) |>
  dplyr::rename(`Please write in your name. (coach)` = coach_other,
                `Please write in your name. (not coach)` = rater_name,
                `Learning Goal (WI):` = wi_learning_goal,
                `Learning Goal (Math)` = k12_m_learning_goal,
                `Core Action 1 Notes (Math)` = k12_m_ca1_notes,
                `Core Action 2 Notes (Math)` = k12_m_ca2_notes,
                `Core Action 3 Notes (Math)` = k12_m_ca3_notes,
                `Teacher name - Selected Choice 1` = IPRT_1_4,
                `Teacher name - Selected Choice 2` = IPRT_1_5,
                `Teacher name - Selected Choice 3` = IPRT_1_6,
                `Teacher name - Selected Choice 4` = IPRT_1_7,
                `Teacher name - Selected Choice 5` = IPRT_1_8,
                `Teacher name - Selected Choice 6` = IPRT_1_9,
                `Teacher name - Selected Choice 7` = IPRT_1_10,
                `Do you need to complete an additional Scoring Form for another 15 minute observation segment? (1)` = IPRT_1_35,
                `Do you need to complete an additional Scoring Form for another 15 minute observation segment? (2)` = IPRT_2_19,
                `Do you need to complete an additional Scoring Form for another 15 minute observation segment? (3)` = IPRT_3_19,
                `Do you need to complete an additional Scoring Form for another 15 minute observation segment? (4)` = IPRT_4_19,
                `Do you need to complete an additional Scoring Form for another 15 minute observation segment? (5)` = IPRT_5_19,
                `Teacher provides asset-based, relevant, and specific feedback on student responses. (Scaffolding)` = ma_scaff_7,
                `Selected Date` = 2) |>
  mutate(across(c("Please write in your name. (coach)",
                  "Please write in your name. (not coach)",
                  "Learning Goal (WI):",
                  "Learning Goal (Math)",
                  "Core Action 1 Notes (Math)",
                  "Core Action 2 Notes (Math)",
                  "Core Action 3 Notes (Math)",
                  "Teacher name - Selected Choice 1",
                  "Teacher name - Selected Choice 2",
                  "Teacher name - Selected Choice 3",
                  "Teacher name - Selected Choice 4",
                  "Teacher name - Selected Choice 5",
                  "Teacher name - Selected Choice 6",
                  "Teacher name - Selected Choice 7",
                  "Do you need to complete an additional Scoring Form for another 15 minute observation segment? (1)",
                  "Do you need to complete an additional Scoring Form for another 15 minute observation segment? (2)",
                  "Do you need to complete an additional Scoring Form for another 15 minute observation segment? (3)",
                  "Do you need to complete an additional Scoring Form for another 15 minute observation segment? (4)",
                  "Do you need to complete an additional Scoring Form for another 15 minute observation segment? (5)",
                  "Teacher provides asset-based, relevant, and specific feedback on student responses. (Scaffolding)",
                  "ma_twps_time",
                  "Selected Date"), ~ as.character(.x))) |>
  TeachingLab::relabel_qualtrics_df() |>
  janitor::remove_empty("cols") |>
  dplyr::mutate(across(where(is.factor), ~ na_if(as.character(.x), "NA"))) |>
  (\(.) split(., .$`Please select the tool for this observation.`))() |>
  purrr::discard(~ nrow(.x) == 0) |>
  map(~ janitor::remove_empty(.x, "cols"))

names(ipg_split_data) -> ipg_split_names
ipg_split_names[which(str_detect(ipg_split_names, "K-12: ELA"))] <- "K-12: ELA/Literacy IPG"

# written_data <- map(ipg_split_names,
#                     ~ read_sheet("1mRZThJuslwVGXyWNY2Z-gAgh0FZKaJM1LIHRwhRvMg8",
#                                  sheet = .x
#                     ))

# ipg_split_data$`GPS Think Write Pair Share` <- ipg_split_data$`GPS Think Write Pair Share` |>
#   select(-ma_twps_time)

map2(
  ipg_split_data,
  ipg_split_names,
  ~ sheet_append("1mRZThJuslwVGXyWNY2Z-gAgh0FZKaJM1LIHRwhRvMg8",
                sheet = .y,
                data = .x
  )
)
# 
# if (nrow(selected_ipg_forms) >= 1) {
#   selected_ipg_forms |>
#     googlesheets4::sheet_append(
#       ss = "1mRZThJuslwVGXyWNY2Z-gAgh0FZKaJM1LIHRwhRvMg8",
#       sheet = "ipg_forms"
#     )
# }
