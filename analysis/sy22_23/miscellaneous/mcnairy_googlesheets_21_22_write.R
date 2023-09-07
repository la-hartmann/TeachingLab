library(TeachingLab)

ipg_forms_21_22 <- TeachingLab::get_ipg_forms(year = "21_22")

final_ipg_21_22 <- ipg_forms_21_22 |>
  filter(`Name of Site (Parish, District, Network)` == "McNairy County, TN") |>
  janitor::remove_empty("cols")

final_ipg_21_22 |>
  googlesheets4::write_sheet()
