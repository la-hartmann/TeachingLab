ipg_scoring_direct <- ipg_forms_direct_to_ts |>
  mutate(
    direct_to_ts_obs = str_replace_all(direct_to_ts_obs, c(
      "Ongoing" = "Mid-year (middle of service, if applicable)",
      "Other" = "Mid-year (middle of service, if applicable)"
    ))
  ) |>
  filter(direct_to_ts_obs != "Other") |>
  mutate(direct_to_ts_obs = factor(direct_to_ts_obs,
                                   levels = c(
                                     "Baseline (first observation of the year)",
                                     "Mid-year (middle of service, if applicable)",
                                     "End of year (last observation of the year)"
                                   )
  ))

### Mix of yes-no and 1-4 Scale Scoring ###
k_12_math <- ipg_scoring_direct |>
  select(
    site,
    direct_to_ts_obs,
    k12_m_ca1a, k12_m_ca1b, k12_m_ca1c, # Core Action 1 yes-no
    k12_m_ca2a, k12_m_ca2b, k12_m_ca2c, k12_m_ca2d, # Core Action 2 1-4
    k12_m_ca3a, k12_m_ca3b, k12_m_ca3c, k12_m_ca3d, k12_m_ca3e
  ) |> # Core Action 3 1-4
  mutate(
    across(!direct_to_ts_obs, ~ as.character(.x))
  ) |>
  filter(if_all(c(k12_m_ca1a, k12_m_ca1b, k12_m_ca1c), ~ !is.na(.x))) |>
  group_by(direct_to_ts_obs, site) |>
  summarise(
    across(c(k12_m_ca1a, k12_m_ca1b, k12_m_ca1c), ~ grade_ipg(.x, type = "character")),
    across(c(
      k12_m_ca2a, k12_m_ca2b, k12_m_ca2c, k12_m_ca2d,
      k12_m_ca3a, k12_m_ca3b, k12_m_ca3c, k12_m_ca3d, k12_m_ca3e
    ), ~ grade_ipg(.x, type = "numeric"))
  ) |>
  pivot_longer(!c(direct_to_ts_obs, site), names_to = "Name", values_to = "value") |>
  mutate(`Core Action` = case_when(
    str_detect(Name, "ca1") ~ "Core Action 1",
    str_detect(Name, "ca2") ~ "Core Action 2",
    str_detect(Name, "ca3") ~ "Core Action 3"
  )) |>
  group_by(direct_to_ts_obs, site) |>
  summarise(`Core Action` = "Overall",
            value = mean(value, na.rm = TRUE)) |>
  mutate(direct_to_ts_obs = factor(direct_to_ts_obs,
                                   levels = c(
                                     "Baseline (first observation of the year)",
                                     "Mid-year (middle of service, if applicable)",
                                     "End of year (last observation of the year)"
                                   )
  ))
  
ipg_math_n_size <- ipg_scoring_direct |>
  filter(!is.na(as.character(k12_m_ca1a))) |>
  group_by(direct_to_ts_obs, site) |>
  count() |>
  ungroup() |>
  mutate(n = as.character(n)) |>
  with(setNames(str_wrap(paste0(direct_to_ts_obs, " (n = ", n, ")"), width = 35), direct_to_ts_obs))
  
math_plot <- k_12_math |>
  mutate(`Core Action` = str_replace_all(`Core Action`, "Core Action", "CA")) |>
  ggplot(aes(x = `Core Action`, y = value, fill = value)) +
  geom_col() +
  geom_text(aes(label = paste0(round(value), "%")),
            fontface = "bold",
            vjust = -1
  ) +
  facet_wrap(~direct_to_ts_obs,
             labeller = as_labeller(
               ipg_math_n_size
             )
  ) +
  scale_fill_continuous(colorRampPalette(tl_palette("blue", n = 6))) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100)
  ) +
  labs(
    x = "", y = "",
    title = "% Positive Indicators K-12 Math IPG"
  ) +
  theme_tl() +
  theme(
    strip.text = element_text(face = "bold", hjust = 0.5),
    plot.title = element_text(face = "bold", family = "Calibri Bold")
  )

### Mix of yes-no and 1-4 scoring ###
k_12_ela <- ipg_scoring_direct |>
  select(
    site,
    direct_to_ts_obs,
    k12_ela_ca1a, k12_ela_ca1b, k12_ela_ca1c, # Core Action 1, Yes/No
    k12_ela_ca2a, k12_ela_ca2b, k12_ela_ca2c, k12_ela_ca2d, # Core Action 2, 1-4
    k12_ela_ca3a, k12_ela_ca3b, k12_ela_ca3c, k12_ela_ca3d, k12_ela_ca3e, k12_ela_ca3f
  ) |> # Core Action 3, 1-4
  mutate(
    across(!direct_to_ts_obs, ~ as.character(.x))
  ) |>
  filter(if_all(c(k12_ela_ca1a, k12_ela_ca1b, k12_ela_ca1c), ~ !is.na(.x))) |>
  group_by(direct_to_ts_obs, site) |>
  summarise(
    across(c(k12_ela_ca1a, k12_ela_ca1b, k12_ela_ca1c), ~ grade_ipg(.x, type = "character")),
    across(c(
      k12_ela_ca2a, k12_ela_ca2b, k12_ela_ca2c, k12_ela_ca2d,
      k12_ela_ca3a, k12_ela_ca3b, k12_ela_ca3c, k12_ela_ca3d, k12_ela_ca3e, k12_ela_ca3f
    ), ~ grade_ipg(.x, type = "numeric"))
  ) |>
  pivot_longer(!c(direct_to_ts_obs, site), names_to = "name", values_to = "value") |>
  mutate(`Core Action` = case_when(
    str_detect(name, "ca1") ~ "Core Action 1",
    str_detect(name, "ca2") ~ "Core Action 2",
    str_detect(name, "ca3") ~ "Core Action 3"
  )) |>
  group_by(direct_to_ts_obs, site) |>
  summarise(`Core Action` = "Overall",
            value = mean(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(direct_to_ts_obs = factor(direct_to_ts_obs,
                                   levels = c(
                                     "Baseline (first observation of the year)",
                                     "Mid-year (middle of service, if applicable)",
                                     "End of year (last observation of the year)"
                                   )
  ))
  
ipg_ela_n_size <- ipg_scoring_direct |>
  filter(!is.na(as.character(k12_ela_ca1a))) |>
  group_by(direct_to_ts_obs, site) |>
  count() |>
  ungroup() |>
  mutate(n = as.character(n)) |>
  with(setNames(str_wrap(paste0(direct_to_ts_obs, " (n = ", n, ")"), width = 35), direct_to_ts_obs))

ela_plot <- k_12_ela |>
  mutate(`Core Action` = str_replace_all(`Core Action`, "Core Action", "CA")) |>
  ggplot(aes(x = `Core Action`, y = value, fill = value)) +
  geom_col() +
  geom_text(aes(label = paste0(round(value), "%")),
            fontface = "bold",
            vjust = -1
  ) +
  facet_wrap(~direct_to_ts_obs,
             labeller = as_labeller(
               ipg_ela_n_size
             )
  ) +
  scale_fill_continuous(colorRampPalette(tl_palette("blue", n = 6))) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100)
  ) +
  labs(
    x = "", y = "",
    title = "% Positive Indicators K-12 ELA IPG"
  ) +
  theme_tl() +
  theme(
    strip.text = element_text(face = "bold", hjust = 0.5),
    plot.title = element_text(face = "bold", family = "Calibri Bold")
  )

### Mix of 1-4 and 1-3 scoring ###
fsot <- ipg_scoring_direct |>
  select(
    site,
    direct_to_ts_obs,
    fsot_ac1, fsot_ac2, # AC1/AC2, 1-4, Q69 is not 1-4
    fsot_td1, fsot_td2, fsot_td3, fsot_td4, # TD, 1-4
    fsot_sp1, fsot_sp2, fsot_sp3, fsot_sp4, # SP, 1-4
    fsot_ad1, fsot_ad2
  ) |> # AD1/AD2, 1-3
  mutate(
    across(everything(), ~ as.character(.x))
  ) |>
  filter(if_all(c(fsot_ac1, fsot_ac2, fsot_td1, fsot_td2), ~ !is.na(.x))) |>
  group_by(direct_to_ts_obs, site) |>
  summarise(
    across(c(
      fsot_ac1, fsot_ac2,
      fsot_td1, fsot_td2, fsot_td3, fsot_td4,
      fsot_sp1, fsot_sp2, fsot_sp3, fsot_sp4
    ), ~ grade_ipg(.x, type = "numeric")),
    across(c(fsot_ad1, fsot_ad2), ~ grade_ipg(.x, type = "numeric_low"))
  ) |>
  pivot_longer(!c(direct_to_ts_obs, site), names_to = "name", values_to = "value") |>
  arrange(name) |>
  mutate(`Core Action` = case_when(
    str_detect(name, "fsot_ac") ~ "Aligned\nContent",
    str_detect(name, "fsot_ad") ~ "Assessment &\nDifferentiation",
    str_detect(name, "fsot_sp") ~ "Student\nPractice",
    str_detect(name, "fsot_td") ~ "Teacher-Directed\nInstruction"
  )) |>
  group_by(direct_to_ts_obs, site) |>
  summarise(`Core Action` = "Overall",
            value = mean(value, na.rm = TRUE)) |>
  ungroup()

fsot_n_size <- ipg_scoring_direct |>
  filter(!is.na(as.character(fsot_ac1))) |>
  group_by(direct_to_ts_obs, site) |>
  count() |>
  ungroup() |>
  mutate(n = as.character(n)) |>
  with(setNames(str_wrap(paste0(direct_to_ts_obs, " (n = ", n, ")"), width = 25), direct_to_ts_obs))

fsot_plot <- fsot |>
  mutate(`Core Action` = factor(`Core Action`, levels = c(
    "Aligned\nContent",
    "Assessment &\nDifferentiation",
    "Student\nPractice",
    "Teacher-Directed\nInstruction",
    "Overall"
  ))) |>
  drop_na(value) |>
  mutate(direct_to_ts_obs = factor(direct_to_ts_obs, levels = c(
    "Baseline (first observation of the year)",
    "Mid-year (middle of service, if applicable)",
    "End of year (last observation of the year)"
  ))) |>
  ggplot(aes(x = `Core Action`, y = value, fill = value)) +
  geom_col() +
  geom_text(aes(label = paste0(round(value), "%")),
            fontface = "bold",
            vjust = -0.5
  ) +
  facet_wrap(~direct_to_ts_obs,
             labeller = as_labeller(
               fsot_n_size
             )
  ) +
  scale_fill_continuous() +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100)
  ) +
  labs(
    x = "", y = "",
    title = "% Positive Indicators Foundational\nSkills Observational Tool"
  ) +
  theme_tl() +
  theme(
    strip.text = element_text(face = "bold", hjust = 0.5),
    plot.title = element_text(face = "bold", family = "Calibri Bold")
  )

k_12_math |>
  mutate(n = case_when(
    direct_to_ts_obs == "Baseline (first observation of the year)" ~ sum(!is.na(ipg_forms_direct_to_ts$k12_m_ca1a[ipg_forms_direct_to_ts$direct_to_ts_obs == "Baseline (first observation of the year)"])),
    direct_to_ts_obs == "Mid-year (middle of service, if applicable)" ~ sum(!is.na(ipg_forms_direct_to_ts$k12_m_ca1a[ipg_forms_direct_to_ts$direct_to_ts_obs == "Mid-year (middle of service, if applicable)"])),
    direct_to_ts_obs == "End of year (last observation of the year)" ~ sum(!is.na(ipg_forms_direct_to_ts$k12_m_ca1a[ipg_forms_direct_to_ts$direct_to_ts_obs == "End of year (last observation of the year)"]))
  )) |>
  bind_rows(k_12_ela |>
              mutate(n = case_when(
                direct_to_ts_obs == "Baseline (first observation of the year)" ~ sum(!is.na(ipg_forms_direct_to_ts$k12_ela_ca1a[ipg_forms_direct_to_ts$direct_to_ts_obs == "Baseline (first observation of the year)"])),
                direct_to_ts_obs == "Mid-year (middle of service, if applicable)" ~ sum(!is.na(ipg_forms_direct_to_ts$k12_ela_ca1a[ipg_forms_direct_to_ts$direct_to_ts_obs == "Mid-year (middle of service, if applicable)"])),
                direct_to_ts_obs == "End of year (last observation of the year)" ~ sum(!is.na(ipg_forms_direct_to_ts$k12_ela_ca1a[ipg_forms_direct_to_ts$direct_to_ts_obs == "End of year (last observation of the year)"]))
              ))) |>
  bind_rows(fsot |>
              mutate(n = case_when(
                direct_to_ts_obs == "Baseline (first observation of the year)" ~ sum(!is.na(ipg_forms_direct_to_ts$fsot_ac1[ipg_forms_direct_to_ts$direct_to_ts_obs == "Baseline (first observation of the year)"])),
                direct_to_ts_obs == "Mid-year (middle of service, if applicable)" ~ sum(!is.na(ipg_forms_direct_to_ts$fsot_ac1[ipg_forms_direct_to_ts$direct_to_ts_obs == "Mid-year (middle of service, if applicable)"])),
                direct_to_ts_obs == "End of year (last observation of the year)" ~ sum(!is.na(ipg_forms_direct_to_ts$fsot_ac1[ipg_forms_direct_to_ts$direct_to_ts_obs == "End of year (last observation of the year)"]))
              ))) |>
  filter(`Core Action` == "Overall") |>
  group_by(direct_to_ts_obs, site) |>
  summarise(value = weighted.mean(x = value, w = n, na.rm = T)) |>
  ungroup() |>
  drop_na(direct_to_ts_obs) |>
  mutate(direct_to_ts_obs = factor(direct_to_ts_obs, levels = c(
    "Baseline (first observation of the year)",
    "Mid-year (middle of service, if applicable)",
    "End of year (last observation of the year)"
  ))) |>
  arrange(direct_to_ts_obs) |>
  dplyr::filter(site == "NY_D9") |>
  mutate(n = table(ipg_scoring_direct$direct_to_ts_obs[ipg_scoring_direct$site == "NY_D9"])) |>
  ggplot(aes(x = direct_to_ts_obs, y = value)) +
  geom_col(fill = "#04abeb") +
  geom_text(aes(label = paste0(round(value), "%\n (n = ", n, ")")),
            fontface = "bold",
            vjust = -1
  ) +
  # facet_wrap( ~ site) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100)
  ) +
  labs(
    x = "", y = "",
    title = "% Positive Indicators Across\nAll Observation Rubrics at NY_D9"
  ) +
  theme_tl()

ggsave("~/Downloads/ipg_plot_d9.png",
       width = 14,
       height = 11,
       bg = "white")

k_12_math |>
  mutate(n = case_when(
    direct_to_ts_obs == "Baseline (first observation of the year)" ~ sum(!is.na(ipg_forms_direct_to_ts$k12_m_ca1a[ipg_forms_direct_to_ts$direct_to_ts_obs == "Baseline (first observation of the year)"])),
    direct_to_ts_obs == "Mid-year (middle of service, if applicable)" ~ sum(!is.na(ipg_forms_direct_to_ts$k12_m_ca1a[ipg_forms_direct_to_ts$direct_to_ts_obs == "Mid-year (middle of service, if applicable)"])),
    direct_to_ts_obs == "End of year (last observation of the year)" ~ sum(!is.na(ipg_forms_direct_to_ts$k12_m_ca1a[ipg_forms_direct_to_ts$direct_to_ts_obs == "End of year (last observation of the year)"]))
  )) |>
  bind_rows(k_12_ela |>
              mutate(n = case_when(
                direct_to_ts_obs == "Baseline (first observation of the year)" ~ sum(!is.na(ipg_forms_direct_to_ts$k12_ela_ca1a[ipg_forms_direct_to_ts$direct_to_ts_obs == "Baseline (first observation of the year)"])),
                direct_to_ts_obs == "Mid-year (middle of service, if applicable)" ~ sum(!is.na(ipg_forms_direct_to_ts$k12_ela_ca1a[ipg_forms_direct_to_ts$direct_to_ts_obs == "Mid-year (middle of service, if applicable)"])),
                direct_to_ts_obs == "End of year (last observation of the year)" ~ sum(!is.na(ipg_forms_direct_to_ts$k12_ela_ca1a[ipg_forms_direct_to_ts$direct_to_ts_obs == "End of year (last observation of the year)"]))
              ))) |>
  bind_rows(fsot |>
              mutate(n = case_when(
                direct_to_ts_obs == "Baseline (first observation of the year)" ~ sum(!is.na(ipg_forms_direct_to_ts$fsot_ac1[ipg_forms_direct_to_ts$direct_to_ts_obs == "Baseline (first observation of the year)"])),
                direct_to_ts_obs == "Mid-year (middle of service, if applicable)" ~ sum(!is.na(ipg_forms_direct_to_ts$fsot_ac1[ipg_forms_direct_to_ts$direct_to_ts_obs == "Mid-year (middle of service, if applicable)"])),
                direct_to_ts_obs == "End of year (last observation of the year)" ~ sum(!is.na(ipg_forms_direct_to_ts$fsot_ac1[ipg_forms_direct_to_ts$direct_to_ts_obs == "End of year (last observation of the year)"]))
              ))) |>
  filter(`Core Action` == "Overall") |>
  group_by(direct_to_ts_obs, site) |>
  summarise(value = weighted.mean(x = value, w = n, na.rm = T)) |>
  ungroup() |>
  drop_na(direct_to_ts_obs) |>
  mutate(direct_to_ts_obs = factor(direct_to_ts_obs, levels = c(
    "Baseline (first observation of the year)",
    "Mid-year (middle of service, if applicable)",
    "End of year (last observation of the year)"
  ))) |>
  arrange(direct_to_ts_obs) |>
  dplyr::filter(site == "NY_D11") |>
  mutate(n = table(ipg_scoring_direct$direct_to_ts_obs[ipg_scoring_direct$site == "NY_D11"])) |>
  ggplot(aes(x = direct_to_ts_obs, y = value)) +
  geom_col(fill = "#04abeb") +
  geom_text(aes(label = paste0(round(value), "%\n (n = ", n, ")")),
            fontface = "bold",
            vjust = -1
  ) +
  # facet_wrap( ~ site) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100)
  ) +
  labs(
    x = "", y = "",
    title = "% Positive Indicators Across\nAll Observation Rubrics at NY_D11"
  ) +
  theme_tl()

ggsave("~/Downloads/ipg_plot_d11.png",
       width = 14,
       height = 11,
       bg = "white")

k_12_math |>
  mutate(n = case_when(
    direct_to_ts_obs == "Baseline (first observation of the year)" ~ sum(!is.na(ipg_forms_direct_to_ts$k12_m_ca1a[ipg_forms_direct_to_ts$direct_to_ts_obs == "Baseline (first observation of the year)"])),
    direct_to_ts_obs == "Mid-year (middle of service, if applicable)" ~ sum(!is.na(ipg_forms_direct_to_ts$k12_m_ca1a[ipg_forms_direct_to_ts$direct_to_ts_obs == "Mid-year (middle of service, if applicable)"])),
    direct_to_ts_obs == "End of year (last observation of the year)" ~ sum(!is.na(ipg_forms_direct_to_ts$k12_m_ca1a[ipg_forms_direct_to_ts$direct_to_ts_obs == "End of year (last observation of the year)"]))
  )) |>
  bind_rows(k_12_ela |>
              mutate(n = case_when(
                direct_to_ts_obs == "Baseline (first observation of the year)" ~ sum(!is.na(ipg_forms_direct_to_ts$k12_ela_ca1a[ipg_forms_direct_to_ts$direct_to_ts_obs == "Baseline (first observation of the year)"])),
                direct_to_ts_obs == "Mid-year (middle of service, if applicable)" ~ sum(!is.na(ipg_forms_direct_to_ts$k12_ela_ca1a[ipg_forms_direct_to_ts$direct_to_ts_obs == "Mid-year (middle of service, if applicable)"])),
                direct_to_ts_obs == "End of year (last observation of the year)" ~ sum(!is.na(ipg_forms_direct_to_ts$k12_ela_ca1a[ipg_forms_direct_to_ts$direct_to_ts_obs == "End of year (last observation of the year)"]))
              ))) |>
  bind_rows(fsot |>
              mutate(n = case_when(
                direct_to_ts_obs == "Baseline (first observation of the year)" ~ sum(!is.na(ipg_forms_direct_to_ts$fsot_ac1[ipg_forms_direct_to_ts$direct_to_ts_obs == "Baseline (first observation of the year)"])),
                direct_to_ts_obs == "Mid-year (middle of service, if applicable)" ~ sum(!is.na(ipg_forms_direct_to_ts$fsot_ac1[ipg_forms_direct_to_ts$direct_to_ts_obs == "Mid-year (middle of service, if applicable)"])),
                direct_to_ts_obs == "End of year (last observation of the year)" ~ sum(!is.na(ipg_forms_direct_to_ts$fsot_ac1[ipg_forms_direct_to_ts$direct_to_ts_obs == "End of year (last observation of the year)"]))
              ))) |>
  filter(`Core Action` == "Overall") |>
  group_by(direct_to_ts_obs, site) |>
  summarise(value = weighted.mean(x = value, w = n, na.rm = T)) |>
  ungroup() |>
  drop_na(direct_to_ts_obs) |>
  mutate(direct_to_ts_obs = factor(direct_to_ts_obs, levels = c(
    "Baseline (first observation of the year)",
    "Mid-year (middle of service, if applicable)",
    "End of year (last observation of the year)"
  ))) |>
  arrange(direct_to_ts_obs) |>
  dplyr::filter(site == "IL_Chicago Public Schools_Network 4") |>
  mutate(n = table(ipg_scoring_direct$direct_to_ts_obs[ipg_scoring_direct$site == "IL_Chicago Public Schools_Network 4"])[-2]) |>
  ggplot(aes(x = direct_to_ts_obs, y = value)) +
  geom_col(fill = "#04abeb") +
  geom_text(aes(label = paste0(round(value), "%\n (n = ", n, ")")),
            fontface = "bold",
            vjust = -1
  ) +
  # facet_wrap( ~ site) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100)
  ) +
  labs(
    x = "", y = "",
    title = "% Positive Indicators Across\nAll Observation Rubrics at IL_Chicago Public Schools_Network 4"
  ) +
  theme_tl()

ggsave("~/Downloads/ipg_plot_network4.png",
       width = 14,
       height = 11,
       bg = "white")

educator_survey <- TeachingLab::get_diagnostic_survey(year = "22_23")
followup_educator <- TeachingLab::get_followup_educator()

n_size_1 <- sum(!is.na(educator_survey$`ts_perceptions_sl_1`))
n_size_2 <- sum(!is.na(followup_educator$`ts_perceptions_sl_1`))

educator_survey |>
  bind_rows(followup_educator) |>
  # filter(id %in% ids_keep) |>
  select(prepost, site, contains("ts_perceptions_sl")) |>
  pivot_longer(!c(prepost, site), names_to = "name", values_to = "value") |>
  mutate(name = str_replace_all(name, c(
    "ts_perceptions_sl_1" = "My school leaders attend the professional development related to curriculum materials with us",
    "ts_perceptions_sl_2" = "My school leaders have created a shared vision for instruction that my curriculum-related professional development experiences is helping our school to implement",
    "ts_perceptions_sl_3" = "My school leaders make sure I have access to all the materials and resources I need to implement our adopted curriculum",
    "ts_perceptions_sl_4" = "My school leaders press me to implement the ideas I learn in curriculum-related professional development",
    "ts_perceptions_sl_5" = "My school leaders make time for my curriculum-related professional development.",
    "ts_perceptions_sl_6" = "Instructional guidance in my school conflicts with the approach taken in curriculum-aligned professional development",
    "ts_perceptions_sl_7" = "I sometimes feel pressure to teach in ways not aligned with the approach to instruction taken in curriculum-aligned professional development",
    "ts_perceptions_sl_8" = "Sometimes I feel like my school puts up barriers to implementing the things I learn in curriculum-related PL"
  ))) |>
  group_by(name, value, prepost, site) |>
  count(sort = T) |>
  ungroup() |>
  drop_na(value) |>
  mutate(name = html_wrap(name, 60)) |>
  group_by(name, prepost, site) |>
  mutate(
    Percent = round(100 * n / sum(n), 2),
    value = factor(value, levels = c(
      "Not at all",
      "Rarely",
      "Sometimes",
      "Often",
      "All the time"
    ))
  ) |>
  filter(value %in% c("Often", "All the time")) |>
  ungroup() |>
  group_by(name, prepost, site) |>
  summarise(
    Percent = sum(Percent),
    n = sum(n)
  ) |>
  dplyr::ungroup() |>
  dplyr::group_by(site, prepost) |>
  dplyr::summarise(Percent = mean(Percent)) |>
  pivot_wider(names_from = prepost, values_from = Percent) |>
  mutate(improve = Post - Pre) |>
  arrange(desc(improve)) |>
  drop_na(improve) |>
  gt::gt() |>
  gt::fmt_percent(c(Pre, Post, improve), scale_values = F) |>
  gt_theme_tl()

student_work_grades <- TeachingLab::get_student_work_grades(year = "22_23")

on_grade_level <- student_work_grades |>
  dplyr::select(`Submitted Grade/s`, `Student Work File`, Prepost, Site) |>
  dplyr::filter(!is.null(`Submitted Grade/s`) & Prepost != "DUPLICATE") |>
  dplyr::mutate(`Submitted Grade/s` = str_remove_all(`Submitted Grade/s`, "Not legible, |Task does NOT ask students to explain their mathematical thinking, |Task does NOT ask students to explain their mathematical thinking|Not legible|Skipped|Duplicate|Task does NOT ask students to respond or write from evidence|Not\nlegible")) |>
  tidyr::drop_na(`Submitted Grade/s`) |>
  tidyr::separate_rows(`Submitted Grade/s`, sep = ", ") |>
  dplyr::filter(`Submitted Grade/s` != "NA" & `Submitted Grade/s` != "") |>
  dplyr::mutate(`Grade Level` = if_else(`Submitted Grade/s` == "Not on grade level", 0, 1)) |>
  dplyr::group_by(`Grade Level`, Prepost, Site) |>
  dplyr::count(sort = T) |>
  dplyr::ungroup() |>
  dplyr::group_by(Prepost, Site) |>
  dplyr::mutate(`Percent` = round(100 * n / sum(n), 2)) |>
  suppressWarnings()

grade_breakdown <- student_work_grades |>
  dplyr::select(`Submitted Grade/s`, `Student Work File`, Prepost, Site) |>
  dplyr::filter(!is.null(`Submitted Grade/s`)) |>
  dplyr::mutate(`Submitted Grade/s` = str_remove_all(`Submitted Grade/s`, "Not legible, |Task does NOT ask students to explain their mathematical thinking, |Not on grade level, |Task does NOT ask students to explain their mathematical thinking|Not legible|Not\nlegible|NULL|NA, |NA|Skipped|Duplicate|Task does NOT ask students to respond or write from evidence|Not\non\ngrade\nlevel|Not on grade level"),
                Prepost = factor(Prepost, levels = c("Pre", "Post"))) |>
  tidyr::drop_na(`Submitted Grade/s`) |>
  tidyr::separate_rows(`Submitted Grade/s`, sep = ", ") |>
  dplyr::filter(`Submitted Grade/s` != " " & `Submitted Grade/s` != "" & Prepost != "DUPLICATE") |>
  dplyr::mutate(grade_percent = as.numeric(`Submitted Grade/s`) / 2) |>
  dplyr::group_by(`Submitted Grade/s`, Prepost, Site) |>
  dplyr::count(sort = T) |>
  dplyr::ungroup() |>
  dplyr::group_by(Prepost, Site) |>
  dplyr::mutate(percent = round(100 * (n / sum(n)), 2),
                Prepost = case_when(Prepost == "Pre" ~ paste0("Pre (n = ", format(sum(on_grade_level$n[on_grade_level$Prepost == "Pre"], na.rm = TRUE), big.mark = ","), ")"),
                                    Prepost == "Post" ~ paste0("Post (n = ", format(sum(on_grade_level$n[on_grade_level$Prepost == "Post"], na.rm = TRUE), big.mark = ","), ")")),
                Prepost = factor(Prepost)) |>
  suppressWarnings()

grade_breakdown |>
  dplyr::ungroup() |>
  dplyr::mutate(Prepost = ifelse(str_detect(Prepost, "Pre"), "Pre", "Post")) |>
  pivot_wider(id_cols = c(`Submitted Grade/s`, Site), names_from = Prepost, values_from = percent) |>
  drop_na(Post) |>
  mutate(Improve = Post - Pre) |>
  arrange(desc(Improve)) |>
  filter(`Submitted Grade/s` %in% c(1, 2)) |>
  gt::gt() |>
  gt::fmt_percent(c(Pre, Post, Improve), scale_values = F) |>
  gt_theme_tl() |>
  gtsave("~/Downloads/student_work_grades_improve.png")


zero_grade_pre <- ifelse("0" %in% grade_breakdown$`Submitted Grade/s`[str_detect(grade_breakdown$Prepost, "Pre")], 
                         glue::glue("<span style='color:#040404'>{grade_breakdown$percent[grade_breakdown$`Submitted Grade/s` == '0' & str_detect(grade_breakdown$Prepost, 'Pre')]}% received a 0.</span>"),
                         "no scores received a 0.")
one_grade_pre <- ifelse("1" %in% grade_breakdown$`Submitted Grade/s`[str_detect(grade_breakdown$Prepost, "Pre")], 
                        glue::glue("<span style='color:#02587A'>{grade_breakdown$percent[grade_breakdown$`Submitted Grade/s` == '1' & str_detect(grade_breakdown$Prepost, 'Pre')]}% received a 1,</span> and"),
                        "no scores received a 1.")
two_grade_pre <- ifelse("2" %in% grade_breakdown$`Submitted Grade/s`[str_detect(grade_breakdown$Prepost, "Pre")], 
                        glue::glue("<span style='color:#04abeb'>{grade_breakdown$percent[grade_breakdown$`Submitted Grade/s` == '2' & str_detect(grade_breakdown$Prepost, 'Pre')]}% received a 2,</span>"),
                        "no scores received a 2.")

zero_grade_post <- ifelse("0" %in% grade_breakdown$`Submitted Grade/s`[str_detect(grade_breakdown$Prepost, "Post")], 
                          glue::glue("<span style='color:#040404'>{grade_breakdown$percent[grade_breakdown$`Submitted Grade/s` == '0' & str_detect(grade_breakdown$Prepost, 'Post')]}% received a 0.</span>"),
                          "no scores received a 0.")
one_grade_post <- ifelse("1" %in% grade_breakdown$`Submitted Grade/s`[str_detect(grade_breakdown$Prepost, "Post")], 
                         glue::glue("<span style='color:#02587A'>{grade_breakdown$percent[grade_breakdown$`Submitted Grade/s` == '1' & str_detect(grade_breakdown$Prepost, 'Post')]}% received a 1,</span> and"),
                         "no scores received a 1.")
two_grade_post <- ifelse("2" %in% grade_breakdown$`Submitted Grade/s`[str_detect(grade_breakdown$Prepost, "Post")], 
                         glue::glue("<span style='color:#04abeb'>{grade_breakdown$percent[grade_breakdown$`Submitted Grade/s` == '2' & str_detect(grade_breakdown$Prepost, 'Post')]}% received a 2,</span>"),
                         "no scores received a 2.")

student_work_plot_2 <- grade_breakdown |>
  ggplot(aes(fill = `Submitted Grade/s`, values = n)) +
  waffle::geom_waffle(
    n_rows = 10,
    size = 1, colour = "white",
    make_proportional = TRUE,
    na.rm = TRUE,
    radius = grid::unit(2, "pt"),
    height = 0.9,
    width = 0.9
  ) +
  facet_wrap( ~ Prepost) +
  ggplot2::scale_fill_manual(values = c("0" = "#040404", "1" = "#02587A", "2" = "#00ACF0")) +
  ggplot2::labs(
    title = paste0("Student Performance On Grade-Level Tasks"),
    subtitle = glue::glue("<br>Of <b>pre</b> on grade-level tasks, {two_grade_pre} {one_grade_pre} {zero_grade_pre}<br>Of <b>post</b> on grade-level tasks, {two_grade_post} {one_grade_post} {zero_grade_post}")
  ) +
  ggplot2::theme_void(base_family = "Calibri") +
  ggplot2::theme(
    legend.position = "none",
    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 20),
    plot.subtitle = ggtext::element_markdown(
      hjust = 0.5, face = "italic",
      lineheight = 1.15,
      size = 15
    ),
    strip.text = element_text(face = "bold", size = 18)
  )
