library(echarts4r)
library(tidyverse)


knowledge_assessments_detailed <- readr::read_rds("dashboards/KnowledgeAssessments2022-2023/data/knowledge_assessments_22_23_detailed.rds")


percentFormat <- htmlwidgets::JS('function (value) {
  var percentVal = Math.round(value.value[0] * 100);
  var percent = percentVal + "%";
  //var percent = value.value[0] + "%";
  return percent;
}')

# test |>
#   filter(know_assess == "ELA: Bootcamp - General" & site == "LA_Pointe Coupee Parish") |>
#   group_by(prepost) |>
#   summarise(score = mean(percent))

data_for_chart <- knowledge_assessments_detailed |>
  ungroup() |>
  filter(know_assess == "ELA: Bootcamp - General" & site == "LA_Pointe Coupee Parish") |>
  mutate(question = str_wrap(question, 60)) |>
  group_by(question, prepost) |>
  summarise(score = round(mean(score), digits = 2)) |>
  group_by(prepost) |>
  print()

data_for_chart |>
  e_chart(x = question) |>
  e_bar(score, name = c("Before", "After"), 
        showBackground = TRUE,
        backgroundStyle = list(
          color = 'rgba(220, 220, 220, 0.8)'
        ),
        label = list(
          show = TRUE,
          position = "right",
          formatter = percentFormat
        )) |>
  # e_bar(score, name = "After", showBackground = TRUE,
  #       backgroundStyle = list(
  #         color = 'rgba(220, 220, 220, 0.8)'
  #       ),
  #       label = list(
  #         show = TRUE,
  #         position = "right",
  #         formatter = percentFormat
  #       )) |>
  # e_labels(position = "right",
  #          formatter = "{c}%",
  #          fontFamily = "Calibri") |>
  e_color(color = c("#d17df7", "#55BBC7")) |>
  e_title(text = "{a|Score per Question Before and After}", 
          textAlign = "center", 
          left = "50%", 
          fontFamily = "Calibri",
          textStyle = list(
            rich = "a: {
            color: '#04abeb'
        }"
          )) |>
  e_legend(bottom = "auto", show = TRUE) |>
  # e_x_axis(
  #   inverse = TRUE,
  #   axisLabel = list(inside = TRUE),
  #   axisTick = list(show = FALSE),
  #   axisLine = list(show = FALSE),
  #   fontFamily = "Calibri"
  # ) |>
  e_y_axis(
    formatter = e_axis_formatter("percent", digits = 0),
    max = 1, 
    fontFamily = "Calibri"
  ) |>
  e_flip_coords()




