participant_feedback <- fetch_survey(surveyID = "SV_djt8w6zgigaNq0C", 
                                     verbose = TRUE)

ids <- participant_feedback |>
  mutate(id = paste0(tolower(Initials), DOB)) |>
  pull(id)

ids

"lyt0818" %in% ids

participant_feedback |>
  mutate(id = paste0(tolower(Initials), DOB)) |>
  filter(id == "snw0218") |>
  select(RecordedDate, Site) |>
  view()
