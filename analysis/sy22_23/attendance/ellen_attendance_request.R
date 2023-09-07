attendance <- read_sheet("https://docs.google.com/spreadsheets/d/15mSsAWPq-d0s-syNHKhgUPf4iHYr-zP0uslxKcRheIc/edit#gid=1866560154",
                         sheet = 1)

attendance |>
  filter(`Please select your site (district, parish, network, or school)` == "NY_D9" #&
           # between(Timestamp, as.Date("2022-10-11"), as.Date("2022-10-13"))
         ) |>
  mutate(mdy = format(Timestamp, "%Y-%m-%d"),
         Course = coalesce(`What course did you take?...9`,
                             `What course did you take?...10`,
                             `What course did you take?...11`,
                             `What course did you take?...12`,
                             `What course did you take?...13`,
                             `What course did you take?...14`,
                             `What course did you take?...15`,
                             `What course did you take?...16`,
                             `What course did you take?...17`,
                             `What course did you take?...18`,
                             `What course did you take?...19`,
                             `What course did you take?...20`,
                             `What course did you take?...21`,
                             `What course did you take?...22`,
                             `What course did you take?...23`,
                             `What course did you take?...24`,
                             `What course did you take?...25`,
                             `What course did you take?...26`,
                             `What course did you take?...27`,
                             `What course did you take?...28`,
                             `What course did you take?...29`,
                             `What course did you take?...30`,
                             `What course did you take?...31`,
                             `What course did you take?...32`,
                             `What course did you take?...33`,
                             `What course did you take?...34`,
                             `What course did you take?...35`,
                             `What course did you take?...36`,
                             `What course did you take?...37`,
                             `What course did you take?...38`,
                             `What course did you take?...39`,
                             `What course did you take?...40`,
                             `What course did you take?...41`,
                             `What course did you take?...42`,
                             `What course did you take?...43`,
                             `What course did you take?...44`,
                             `What course did you take?...45`,
                             `What course did you take?...46`,
                             `What course did you take?...47`,
                             `What course did you take?...48`,
                             `What course did you take?...49`,
                             `What course did you take?...50`,
                             `What course did you take?...51`,
                             `What course did you take?...52`,
                             `What course did you take?...53`,
                             `What course did you take?...54`,
                             `What course did you take?...55`,
                             `What course did you take?...56`,
                             `What course did you take?...60`,
                             `What course did you take?...61`,
                             `What course did you take?...62`,
                             `What course did you take?...63`,
                             `What course did you take?...64`,
                             `What course did you take?...65`,
                             `What course did you take?...66`,
                             `What course did you take?...67`,
                             `What course did you take?...68`,
                             `What course did you take?...69`,
                             `What course did you take?...70`,
                             `What course did you take?...71`,
                             `What course did you take?...72`,
                             `What course did you take?...73`,
                             `What course did you take?...74`,
                             `What course did you take?...75`,
                             `What course did you take?...76`,
                             `What course did you take?...77`,
                             `What course did you take?...78`,
                             `What course did you take?...79`,
                             `What course did you take?...80`,
                             `What course did you take?...81`,
                             `What course did you take?...82`,
                             `What course did you take?...83`,
                             `What course did you take?...84`,
                             `What course did you take?...85`,
                             `What course did you take?...86`,
                             `What course did you take?...87`,
                             `What course did you take?...88`,
                             `What course did you take?...89`,
                             `What course did you take?...90`,
                             `What course did you take?...91`,
                             `What course did you take?...92`,
                             `What course did you take?...93`,
                             `What course did you take?...94`,
                             `What course did you take?...95`,
                             `What course did you take?...96`,
                             `What course did you take?...97`,
                             `What course did you take?...98`,
                             `What course did you take?...99`,
                             `What course did you take?...100`,
                             `What course did you take?...101`,
                             `What course did you take?...102`,
                             `What course did you take?...103`,
                             `What course did you take?...104`,
                             `What course did you take?...105`,
                             `What course did you take?...106`,
                             `What course did you take?...107`,
                             `What course did you take?...108`)) |>
  select(Timestamp, mdy, Course, Roles) |>
  filter(str_detect(Course, "Foundational Skills|Accelerated Learning")) |>
  group_by(Course, mdy, Roles) |>
  count() |>
  ungroup() |>
  group_by(Course) |>
  arrange(mdy) |>
  rename(Date = mdy) |>
  gt::gt() |>
  gt::data_color(
    columns = c(n),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) |>
        as.character(),
      domain = NULL
    )
  ) |>
  TeachingLab::gt_theme_tl() |>
  gt::gtsave(here::here("Images/Attendance/d9_foundational_accelerated_with_roles.png"))
