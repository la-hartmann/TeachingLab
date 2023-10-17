student_work |> 
  filter(subject == "ELA") |>
  select(contains("grade")) |> 
  TeachingLab::relabel_qualtrics_df() |> 
  summarise(across(everything(), ~ sum(!is.na(.x)))) |>
  pivot_longer(everything()) |>
  gt::gt() |>
  gt::tab_header("ELA") |>
  gt_theme_tl() |>
  gt::gtsave("~/Downloads/grades1.png")

student_work |> 
  filter(subject == "Math") |>
  select(contains("grade")) |> 
  TeachingLab::relabel_qualtrics_df() |> 
  summarise(across(everything(), ~ sum(!is.na(.x)))) |>
  pivot_longer(everything()) |>
  gt::gt() |>
  gt::tab_header("Math") |>
  gt_theme_tl() |>
  gt::gtsave("~/Downloads/grades2.png")
