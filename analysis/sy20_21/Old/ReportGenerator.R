
params_list <- list(
  partner = list(
    "Evangeline Parish, LA", "Louisiana State Content Leader Training, LA",
    "Lafayette Parish, LA", "East Baton Rouge Parish, LA", "Building 21 - Philadelphia, PA",
    "Freire Charter Schools, PA/DE", "Pointe Coupee Parish, LA",
    "Building 21 - Allentown, PA", "NYC District 11 - District-wide, NY",
    "NYC District 11 - PS 89, NY", "NYC District 11 - PS 21, NY",
    "NYC District 11 - IS 355, NY", "NYC District 11 - PS 96, NY",
    "NYC District 11 - PS 87, NY", "Legacy Early College, SC", "NYC District 11 - PS 103, NY",
    "Brownington Central School, VT", "NYC District 11 - PS 189, NY",
    "NYC District 11 - PS 498, NY", "Kankakee School District, IL",
    "Tangipahoa Parish, LA", "Mississippi Department of Education, MS",
    "Delaware Department of Education, DE"
  ),
  matched = list("matched", "unmatched")
)

groups <- crossing(partner = params_list$partner, matched = params_list$matched)

partner <- groups$partner
matched <- groups$matched

walk2(partner, matched, ~ rmarkdown::render(
  input = here::here("Rmd/2021Reports/Template/LoopingFilteredFeedback.Rmd"),
  output_file = paste0("2021 Report_", .y, "_", .x),
  output_dir = here::here("Rmd/2021Reports/Generated"),
  params = list(partner = .x, matched = .y)
))
