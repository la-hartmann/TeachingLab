

extra_data <- test |>
  dplyr::select(c(date_created,
                  `Select the name of your facilitator._2`,
                  `Select the name of your facilitator._2`,
                  `Select the name of your facilitator. - Other (please specify)_2`,
                  `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._2`,
                  `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._2`,
                  `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._2`,
                  `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._2`,
                  `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._2`,
                  `What grade(s) do you teach, support, and/or lead? You can select more than one. - K`,
                  `What grade(s) do you teach, support, and/or lead? You can select more than one. - 1`,
                  `What grade(s) do you teach, support, and/or lead? You can select more than one. - 2`,
                  `What grade(s) do you teach, support, and/or lead? You can select more than one. - 3`,
                  `What grade(s) do you teach, support, and/or lead? You can select more than one. - 4`,
                  `What grade(s) do you teach, support, and/or lead? You can select more than one. - 5`,
                  `What grade(s) do you teach, support, and/or lead? You can select more than one. - 6`,
                  `What grade(s) do you teach, support, and/or lead? You can select more than one. - 7`,
                  `What grade(s) do you teach, support, and/or lead? You can select more than one. - 8`,
                  `What grade(s) do you teach, support, and/or lead? You can select more than one. - 9`,
                  `What grade(s) do you teach, support, and/or lead? You can select more than one. - 10`,
                  `What grade(s) do you teach, support, and/or lead? You can select more than one. - 11`,
                  `What grade(s) do you teach, support, and/or lead? You can select more than one. - 12`,
                  `What grade(s) do you teach, support, and/or lead? You can select more than one. - Other (please specify)`)) |>
  arrange(date_created) |>
  dplyr::select(-date_created)



extra_data_final <- extra_data |>
  bind_rows(as.data.frame(matrix(NA, nrow = 1538, ncol = ncol(extra_data), 
                                 dimnames = list(NULL, colnames(extra_data)))))

session_survey_21_22 |>
  arrange(Date) |>
  bind_cols(extra_data_final) -> new_sess_survey_21_22

new_sess_survey_21_22 |> write_csv("data/sy21_22/session_survey.csv")
