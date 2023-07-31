#### SESSION SURVEY ####

options(sm_oauth_token = "6zpcKriMLjBWVEHno8VWb4Uvclqotpq0H53HudGcfcyLc6aW0vxfm-M3e.REqngqrQ7vw1HPB92gxQprqcGH7IFXI1u64xNU.PLchF79sIqyhoTsuHyTAchN2yfLvBvU")


session_survey <- readr::read_rds("data/sy21_22/session_facilitator_surveymonkey.rds") %>%
  dplyr::mutate(date_created = lubridate::date(date_created)) %>%
  dplyr::mutate(`Select your course.` = dplyr::coalesce(`Select your course.`, `Select your course._2`, `Select your course._3`,
                                                        `Select your course._4`, `Select your course._5`, `Select your course._6`)) %>%
  dplyr::mutate(Date = lubridate::mdy(`Select the date for this session. - \n    Date / Time\n`)) %>%
  dplyr::mutate(Facilitator = dplyr::coalesce(`Select the name of your facilitator.`,
                                              `Select the name of your facilitator._2`,
                                              `Select the name of your facilitator._3`,
                                              `Select the name of your facilitator. - Other (please specify)_3`,
                                              `Select the name of your facilitator. - Other (please specify)_2`,
                                              `Select the name of your facilitator. - Other (please specify)_3`),
                Facilitation_Feedback = dplyr::coalesce(`What additional feedback do you have about their facilitation skills?`,
                                                        `What additional feedback do you have about their facilitation skills?_2`,
                                                        `What additional feedback do you have about their facilitation skills?_3`)) %>%
  dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.` =
                  dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.`,
                                  `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._2`,
                                  `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._3`)) %>%
  dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.` =
                  dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.`,
                                  `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._2`,
                                  `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._3`)) %>%
  dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.` =
                  dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.`,
                                  `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._2`,
                                  `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._3`)) %>%
  dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.` =
                  dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.`,
                                  `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._2`,
                                  `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._3`)) %>%
  dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.` =
                  dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.`,
                                  `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._2`,
                                  `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._3`))