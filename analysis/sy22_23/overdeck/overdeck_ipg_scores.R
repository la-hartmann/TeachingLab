ipg_forms |>
  mutate(math_ela = ifelse(!str_detect(`IPG Rubric`, "Math|math"), "ELA", "Math"),
         `Timeline of Obs` = ifelse(is.na(`Timeline of Obs`), "Observation 2", `Timeline of Obs`)) |>
  group_by(`Timeline of Obs`, math_ela) |>
  summarise(
    across(all_of(ca1), ~ grade_ipg(.x)),
    across(all_of(ca2), ~ grade_ipg(.x, type = "numeric")),
    across(all_of(ca3), ~ grade_ipg(.x, type = "numeric")),
    across(all_of(td), ~ grade_ipg(.x, type = "numeric")),
    across(all_of(sp), ~ grade_ipg(.x, type = "numeric")),
    across(all_of(ac), ~ grade_ipg(.x, type = "numeric"))
  ) |>
  drop_na(`Timeline of Obs`) |>
  # mutate(across(everything(), ~ as.character(.x))) |>
  pivot_longer(!c(math_ela, `Timeline of Obs`)) |>
  drop_na(value) |>
  mutate(value = as.numeric(value),
         ca = case_when(str_detect(name, "Ca1|CA1|ca1|CA\\.1") ~ "Core Action 1",
                        str_detect(name, "Ca2|CA2|ca2|CA\\.2") ~ "Core Action 2",
                        str_detect(name, "Ca3|CA3|ca3|CA\\.3") ~ "Core Action 3")) |>
  drop_na(ca) |>
  group_by(math_ela, `Timeline of Obs`, ca) |>
  summarise(percent = mean(value, na.rm = T)) |>
  pivot_wider(names_from = `Timeline of Obs`, values_from = percent) |>
  view()
