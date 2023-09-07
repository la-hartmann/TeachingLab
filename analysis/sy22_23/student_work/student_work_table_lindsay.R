library(gt)
library(tidyverse)
library(qualtRics)

student_work <- qualtRics::fetch_survey("SV_6nwa9Yb4OyXLji6")

student_work |>
  filter(!is.na(network4) | !is.na(network7) | !is.na(network12)) |>
  group_by(subject) |>
  count()

student_work_filtered <- student_work |>
  filter(Finished == TRUE) |>
  mutate(grade_band = case_when(!is.na(`grade_level_3`) ~ "3-5",
                                !is.na(`grade_level_4`) ~ "3-5",
                                !is.na(`grade_level_5`) ~ "3-5",
                                !is.na(`grade_level_6`) ~ "6-8",
                                !is.na(`grade_level_7`) ~ "6-8",
                                !is.na(`grade_level_8`) ~ "6-8",
                                !is.na(`grade_level_9`) ~ "9-12",
                                !is.na(`grade_level_10`) ~ "9-12",
                                !is.na(`grade_level_11`) ~ "9-12",
                                !is.na(`grade_level_12`) ~ "9-12",
                                !is.na(`grade_level_13`) ~ "Other")) |>
  mutate(subject = na_if(as.character(subject), "NA"),
         subject_area = case_when(str_detect(class, "Math|math") ~ "Math",
                                  str_detect(class, "ELA|ela|Ela|Literacy|lit") ~ "ELA",
                                  str_detect(class, "NA") ~ "Other"),
         subject_area_2 = coalesce(subject, subject_area)) |>
  group_by(grade_band, subject_area_2) |>
  summarise(n = n(),
            students = sum(`# of students_1`))
  
student_work_filtered |>
  mutate(subject_area_2 = replace_na(subject_area_2, "Other"),
         subject_area_2 = factor(subject_area_2, levels = c("Math", "ELA", "Other"))) |>
  dplyr::group_by(subject_area_2) |>
  dplyr::arrange(subject_area_2) |>
  gt::gt() |>
  fmt_number(
    columns = c(n, students),
    decimals = 0
  ) |>
  data_color(
    columns = c(n, students),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>%
        as.character(),
      domain = NULL
    )
  ) |>
  gt::summary_rows(
    groups = TRUE,
    columns = c(n, students),
    fns = list(
      total = "sum"
    ),
    formatter = fmt_number
  ) |>
  TeachingLab::gt_theme_tl() |>
  gt::gtsave("~/Downloads/student_work_subject_area_grade.png")

# view(student_work)


student_work |>
  filter(Finished == TRUE) |>
  select(Date = RecordedDate, teacher_or_coach, teacher_name, initials, dob, site,
         district9, district11, rochester,
         class, `# of students_1`, File_Name, contains("Grade")) |>
  googlesheets4::write_sheet()
