library(ggfx)
library(ggtext)
library(patchwork)
library(ragg)
library(tidyverse)

nps <- get_participant_feedback(start_date = as.Date("2023-07-01")) |>
  filter(RecordedDate >= as.Date("2023-07-01")) |>
  select(RecordedDate, nps = coach_nps) |>
  drop_na(nps) |>
  mutate(id = row_number())

nps_labs <-
  nps |>
  group_by(RecordedDate) |>
  filter(id == min(id))

overall_nps <- calc_nps(nps$nps)

nps |>
  ggplot(aes(
    x = id,
    y = nps,
    color = RecordedDate,
    fill = RecordedDate
  )) +
  ## curves
  ggforce::geom_link(
    aes(
      x = id,
      xend = id,
      y = 0,
      yend = nps,
      color = RecordedDate,
      color = after_scale(colorspace::desaturate(color, .3)),
      alpha = nps
    ),
    n = 300,
    size = .25
  ) +
  ## triangles
  geom_point(
    aes(y = 0),
    shape = 17,
    size = .3
  ) +
  ## points
  geom_point(
    aes(
      y = nps,
      size = nps
    )
  ) +
  ## tick marks years
  geom_text(
    data = nps_labs,
    aes(y = 0, label = "|"),
    family = "Changa",
    fontface = "bold",
    size = 4,
    vjust = 1
  ) +
  # geom_richtext(
  #   data = nps_labs,
  #   aes(y = 0, label = glue::glue("<br>{floor(RecordedDate)}")),
  #   size = 5.5,
  #   family = "Oswald",
  #   fontface = "bold",
  #   fill = NA,
  #   label.color = NA,
  #   vjust = .85
  # ) +
  # geom_curve(
  #   data = tibble(x1 = 450, x2 = 400, y1 = 0, y2 = 0),
  #   aes(x = x1, y = y1, xend = x2, yend = y2),
  #   color = "white",
  #   arrow = arrow(length = unit(0.03, "npc")),
  #   inherit.aes = F
  # ) +
  geom_text(
    data = tibble(x = c(425, -15), y = c(1, 10), labs = c("1", "10")),
    aes(x = x, y = y, label = labs),
    inherit.aes = F,
    size = 7,
    family = "Oswald",
    color = "white"
  ) +
  geom_text(
    data = tibble(x = c(-30, 500), y = c(0, 0), labs = c("July, 2023", "October, 2023")),
    aes(x = x, y = y, label = labs),
    inherit.aes = F,
    size = 8,
    family = "Oswald",
    color = "white"
  ) +
  with_inner_glow(
    geom_text(
      data = tibble(x = -230, y = 0, label = as.character(overall_nps)),
      aes(x = x, y = y, label = label),
      inherit.aes = F,
      size = 35,
      family = "Oswald",
      color = "white"
    ),
    colour = '#04abeb',
    sigma = 10
  ) +
  geom_textbox(
    data = tibble(
      id = 580, nps = 11.75,
      label = "<b style='font-size:38pt;'>Teaching Lab FY23-24 NPS</b><br><br>Every single answer to \"How likely are you to recommend this PL to a friend or colleague?\" Teaching Lab has received in FY24, ordered by date, including the overall NPS rating highlighted centrally.<br>"
    ),
    aes(
      x = id,
      y = nps,
      label = label
    ),
    inherit.aes = F,
    size = 10,
    family = "Oswald",
    color = "grey70",
    lineheight = 1.3,
    width = unit(6.2, "inch"),
    hjust = 0,
    vjust = 0,
    fill = NA,
    box.colour = NA
  ) +
  coord_polar(theta = "y", clip = "off") +
  # coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(-250, 800), expand = c(0, 0)) + # limits = c(-2040, NA)) +
  scale_y_continuous(limits = c(0, 15), expand = c(0, 0)) + # limits = c(0, 156091.8)
  scico::scale_color_scico(palette = "vik", guide = F, direction = -1) +
  scico::scale_fill_scico(palette = "vik", guide = F, direction = -1) +
  scale_size(range = c(.001, 3), guide = F) +
  scale_alpha(range = c(0.1, 0.6), guide = F) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    plot.margin = margin(-260, -260, -300, -200)
  )

ggsave(here::here("images/sy23_24/newsletter/nps_october.png"), 
       width = 22, 
       height = 20, 
       device = ragg::agg_png)

nps2 <- get_participant_feedback(start_date = as.Date("2023-07-01")) |>
  filter(RecordedDate >= as.Date("2023-07-01")) |>
  select(RecordedDate, nps = coach_nps, nps_group = coach_nps_NPS_GROUP)

nps_groups <- nps2 |>
  mutate(month = lubridate::month(RecordedDate, label = TRUE, abbr = FALSE)) |>
  group_by(nps_group, month) |>
  summarise(n = n()) |>
  ungroup() |>
  drop_na(nps_group) |>
  group_by(month) |>
  mutate(percent = 100 * n/sum(n),
         color = ifelse(nps_group == "Detractor", "white", "black"),
         size = ifelse(nps_group == "Detractor", 8, 12))

p1 <- nps_groups |>
  filter(month != "July") |>
  ggplot(aes(x = month, y = percent, fill = nps_group)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  geom_text(position = position_stack(reverse = TRUE, vjust = 0.5),
            aes(label = paste0(round(percent, 2), "% ", nps_group, "s\n(n = ", n, ")"), color = color, group = nps_group, size = size),
            fontface = "bold",
            family = "Calibri") +
  coord_flip() +
  scale_fill_manual(values = tl_palette(n = 3, color = "blue")) +
  scale_color_identity() +
  scale_size_continuous(range = c(4, 12)) +
  labs(x = "", y = "") +
  theme_tl() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 13))


overall_nps <- nps2 |>
  mutate(month = lubridate::month(RecordedDate, label = TRUE, abbr = FALSE)) |>
  filter(month != "July") |>
  group_by(month) |>
  summarise(nps = calc_nps(nps)) |>
  mutate(nps = as.character(nps))

p2 <- overall_nps |>
  ggplot() +
  with_inner_glow(
    geom_text(
      aes(x = 0, y = month, label = nps),
      inherit.aes = F,
      size = 35,
      family = "Oswald",
      color = "white"
    ),
    colour = '#04abeb',
    sigma = 15
  ) +
  theme_void()

p1 + p2 +
  plot_layout(widths = c(3, 1))

ggsave(here::here("images/sy23_24/newsletter/nps_october_v2.png"), 
       width = 22, 
       height = 15, 
       device = ragg::agg_png)

ipg_forms <- get_ipg_forms() |>
  filter(!direct_to_ts_obs %in% c("Mid-year (middle of service, if applicable)",
                                  "Other",
                                  "Ongoing",
                                  "End of year (last observation of the year)"))

make_ipg_fsot_summary_chart <- function() {
  
  if (interactive()) {
    data <- ipg_forms
    base_color <- "#040404"
    end_color <- "#04abeb"
  } else {
    data <- params$data
    base_color <- params$base_color
    end_color <- params$end_color
  }
  
  grade_ipg <- function(x, type = "character") {
    x <- x[!is.na(x)]
    x <- x[!is.null(x)]
    x <- x[!str_detect(x, "Not Observed|Not observed|NULL|NA")]
    
    if (type == "character") {
      x <- 100 * (sum(stringr::str_detect(x, "Yes"), na.rm = T)) /
        (sum(stringr::str_detect(x, "No"), na.rm = T) + sum(str_detect(x, "Yes"), na.rm = T))
    } else if (type == "numeric") {
      x <- 100 * (sum(stringr::str_detect(x, "3|4"), na.rm = T)) /
        (sum(!stringr::str_detect(x, "3|4"), na.rm = T) + sum(str_detect(x, "3|4"), na.rm = T))
    } else if (type == "numeric_low") {
      x <- 100 * (sum(stringr::str_detect(x, "2|3"), na.rm = T)) /
        (sum(!stringr::str_detect(x, "2|3"), na.rm = T) + sum(str_detect(x, "2|3"), na.rm = T))
    }
  }
  
  ipg_adjust <- data |>
    dplyr::mutate(
      direct_to_ts_obs = stringr::str_replace_all(direct_to_ts_obs, c(
        "Ongoing" = "Mid-year (middle of service, if applicable)"#,
        # "Other" = "Mid-year (middle of service, if applicable)"
      ))
    ) |>
    dplyr::filter(direct_to_ts_obs != "Other") |>
    tidyr::drop_na(direct_to_ts_obs) |>
    dplyr::mutate(direct_to_ts_obs = factor(direct_to_ts_obs,
                                            levels = c(
                                              "Baseline (first observation of the year)",
                                              "Mid-year (middle of service, if applicable)",
                                              "End of year (last observation of the year)"
                                            )
    ))
  
  fsot <- ipg_adjust |>
    dplyr::select(
      direct_to_ts_obs,
      fsot_ac1, fsot_ac2, # AC1/AC2, 1-4, Q69 is not 1-4
      fsot_td1, fsot_td2, fsot_td3, fsot_td4, # TD, 1-4
      fsot_sp1, fsot_sp2, fsot_sp3, fsot_sp4, # SP, 1-4
      fsot_ad1, fsot_ad2
    ) |> # AD1/AD2, 1-3
    dplyr::mutate(
      dplyr::across(!direct_to_ts_obs, ~ as.character(.x))
    ) |>
    dplyr::filter(if_all(c(fsot_ac1, fsot_ac2, fsot_td1, fsot_td2), ~ !is.na(.x))) |>
    dplyr::group_by(direct_to_ts_obs) |>
    dplyr::summarise(
      dplyr::across(c(fsot_ac1, fsot_ac2, fsot_td1, fsot_td2), ~ grade_ipg(.x, type = "character")),
      dplyr::across(c(
        fsot_ac1, fsot_ac2,
        fsot_td1, fsot_td2, fsot_td3, fsot_td4,
        fsot_sp1, fsot_sp2, fsot_sp3, fsot_sp4
      ), ~ grade_ipg(.x, type = "numeric")),
      dplyr::across(c(fsot_ad1, fsot_ad2), ~ grade_ipg(.x, type = "numeric_low"))
    ) |>
    tidyr::pivot_longer(!direct_to_ts_obs, names_to = "name", values_to = "value") |>
    dplyr::arrange(name) |>
    (\(.) dplyr::mutate(., `Core Action` = c(
      rep("Aligned\nContent", nrow(.) / 6),
      rep("Assessment &\nDifferentiation", nrow(.) / 6),
      rep("Student\nPractice", nrow(.) / 3),
      rep("Teacher-Directed\nInstruction", nrow(.) / 3)
    )))() |>
    dplyr::group_by(`Core Action`, direct_to_ts_obs) |>
    dplyr::summarise(value = mean(value)) |>
    dplyr::ungroup() |>
    (\(.) dplyr::add_row(., `Core Action` = "Overall", direct_to_ts_obs = "Baseline (first observation of the year)", .before = 1))() |>
    (\(.) dplyr::add_row(., `Core Action` = "Overall", direct_to_ts_obs = "Mid-year (middle of service, if applicable)", .before = 1))() |>
    (\(.) dplyr::add_row(., `Core Action` = "Overall", direct_to_ts_obs = "End of year (last observation of the year)", .before = 1))() |>
    dplyr::group_by(direct_to_ts_obs) |>
    dplyr::mutate(value = ifelse(is.na(value), mean(value, na.rm = T), value)) |>
    dplyr::ungroup() |>
    dplyr::mutate(direct_to_ts_obs = factor(direct_to_ts_obs,
                                            levels = c(
                                              "Baseline (first observation of the year)",
                                              "Mid-year (middle of service, if applicable)",
                                              "End of year (last observation of the year)"
                                            )
    ),
    `Core Action` = factor(`Core Action`, levels = c("Overall", "Aligned\nContent",
                                                     "Assessment &\nDifferentiation", "Student\nPractice",
                                                     "Teacher-Directed\nInstruction"))) |>
    tidyr::drop_na(value)
  
  ipg_fsot_n_size <- ipg_adjust |>
    filter(!is.na(as.character(fsot_ac1))) |>
    dplyr::group_by(direct_to_ts_obs) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::mutate(n = as.character(n)) |>
    with(setNames(stringr::str_wrap(paste0(direct_to_ts_obs, " (n = ", n, ")"), width = 35), direct_to_ts_obs))
  
  unique_color_n <- length(unique(fsot$`Core Action`))
  
  fsot_plot <- fsot |>
    ggplot(aes(x = `Core Action`, y = value, fill = `Core Action`)) +
    geom_col() +
    geom_text(aes(label = paste0(round(value), "%")),
              fontface = "bold",
              vjust = -1
    ) +
    facet_wrap(~direct_to_ts_obs,
               labeller = as_labeller(
                 ipg_fsot_n_size
               )
    ) +
    scale_fill_manual(values = tlShiny::tl_palette2(n = unique_color_n, base_color_start = base_color, end_color_start = end_color)) +
    scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      limits = c(0, 100)
    ) +
    labs(
      x = "", y = "",
      title = "% Positive Indicators FSOT IPG"
    ) +
    tlShiny::theme_tl() +
    theme(
      strip.text = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(size = "13"),
      plot.title = element_text(face = "bold", family = "Calibri Bold")
    )
  
  fsot_plot
  
}

make_ipg_math_summary_chart <- function() {
  
  if (interactive()) {
    data <- ipg_forms
    base_color <- "#040404"
    end_color <- "#04abeb"
  } else {
    data <- params$data
    base_color <- params$base_color
    end_color <- params$end_color
  }
  
  grade_ipg <- function(x, type = "character") {
    x <- x[!is.na(x)]
    x <- x[!is.null(x)]
    x <- x[!str_detect(x, "Not Observed|Not observed|NULL|NA")]
    
    if (type == "character") {
      x <- 100 * (sum(stringr::str_detect(x, "Yes"), na.rm = T)) /
        (sum(stringr::str_detect(x, "No"), na.rm = T) + sum(str_detect(x, "Yes"), na.rm = T))
    } else if (type == "numeric") {
      x <- 100 * (sum(stringr::str_detect(x, "3|4"), na.rm = T)) /
        (sum(!stringr::str_detect(x, "3|4"), na.rm = T) + sum(str_detect(x, "3|4"), na.rm = T))
    } else if (type == "numeric_low") {
      x <- 100 * (sum(stringr::str_detect(x, "2|3"), na.rm = T)) /
        (sum(!stringr::str_detect(x, "2|3"), na.rm = T) + sum(str_detect(x, "2|3"), na.rm = T))
    }
  }
  
  ipg_adjust <- data |>
    dplyr::mutate(
      direct_to_ts_obs = stringr::str_replace_all(direct_to_ts_obs, c(
        "Ongoing" = "Mid-year (middle of service, if applicable)"#,
        # "Other" = "Mid-year (middle of service, if applicable)"
      ))
    ) |>
    dplyr::filter(direct_to_ts_obs != "Other") |>
    tidyr::drop_na(direct_to_ts_obs) |>
    dplyr::mutate(direct_to_ts_obs = factor(direct_to_ts_obs,
                                            levels = c(
                                              "Baseline (first observation of the year)",
                                              "Mid-year (middle of service, if applicable)",
                                              "End of year (last observation of the year)"
                                            )
    ))
  
  k_12_math <- ipg_adjust |>
    dplyr::select(
      direct_to_ts_obs,
      k12_m_ca1a, k12_m_ca1b, k12_m_ca1c, # Core Action 1 yes-no
      k12_m_ca2a, k12_m_ca2b, k12_m_ca2c, k12_m_ca2d, # Core Action 2 1-4
      k12_m_ca3a, k12_m_ca3b, k12_m_ca3c, k12_m_ca3d, k12_m_ca3e
    ) |> # Core Action 3 1-4
    dplyr::mutate(
      dplyr::across(!direct_to_ts_obs, ~ as.character(.x))
    ) |>
    dplyr::filter(if_all(c(k12_m_ca1a, k12_m_ca1b, k12_m_ca1c), ~ !is.na(.x))) |>
    dplyr::group_by(direct_to_ts_obs) |>
    dplyr::summarise(
      dplyr::across(c(k12_m_ca1a, k12_m_ca1b, k12_m_ca1c), ~ grade_ipg(.x, type = "character")),
      dplyr::across(c(
        k12_m_ca2a, k12_m_ca2b, k12_m_ca2c, k12_m_ca2d,
        k12_m_ca3a, k12_m_ca3b, k12_m_ca3c, k12_m_ca3d, k12_m_ca3e
      ), ~ grade_ipg(.x, type = "numeric"))
    ) |>
    tidyr::pivot_longer(!direct_to_ts_obs, names_to = "Name", values_to = "value") |>
    dplyr::mutate(`Core Action` = case_when(
      stringr::str_detect(Name, "ca1") ~ "Core Action 1",
      stringr::str_detect(Name, "ca2") ~ "Core Action 2",
      stringr::str_detect(Name, "ca3") ~ "Core Action 3"
    )) |>
    dplyr::group_by(`Core Action`, direct_to_ts_obs) |>
    dplyr::summarise(value = mean(value)) |>
    dplyr::ungroup() |>
    (\(.) dplyr::add_row(., `Core Action` = "Overall", direct_to_ts_obs = "Baseline (first observation of the year)", .before = 1))() |>
    (\(.) dplyr::add_row(., `Core Action` = "Overall", direct_to_ts_obs = "Mid-year (middle of service, if applicable)", .before = 1))() |>
    (\(.) dplyr::add_row(., `Core Action` = "Overall", direct_to_ts_obs = "End of year (last observation of the year)", .before = 1))() |>
    dplyr::group_by(direct_to_ts_obs) |>
    dplyr::mutate(value = ifelse(is.na(value), mean(value, na.rm = T), value)) |>
    dplyr::ungroup() |>
    dplyr::mutate(direct_to_ts_obs = factor(direct_to_ts_obs,
                                            levels = c(
                                              "Baseline (first observation of the year)",
                                              "Mid-year (middle of service, if applicable)",
                                              "End of year (last observation of the year)"
                                            )
    )) |>
    tidyr::drop_na(value)
  
  ipg_math_n_size <- ipg_adjust |>
    dplyr::filter(!is.na(as.character(k12_m_ca1a))) |>
    dplyr::group_by(direct_to_ts_obs) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::mutate(n = as.character(n)) |>
    with(setNames(stringr::str_wrap(paste0(direct_to_ts_obs, " (n = ", n, ")"), width = 35), direct_to_ts_obs))
  
  unique_color_n <- length(unique(k_12_math$`Core Action`))
  
  math_plot <- k_12_math |>
    ggplot(aes(x = `Core Action`, y = value, fill = `Core Action`)) +
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
    scale_fill_manual(values = tlShiny::tl_palette2(n = unique_color_n, base_color_start = base_color, end_color_start = end_color)) +
    scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      limits = c(0, 100)
    ) +
    labs(
      x = "", y = "",
      title = "% Positive Indicators K-12 Math IPG"
    ) +
    tlShiny::theme_tl() +
    theme(
      strip.text = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(size = "13"),
      plot.title = element_text(face = "bold", family = "Calibri Bold")
    )
  
  math_plot
  
}

make_ipg_ela_summary_chart <- function() {
  
  if (interactive()) {
    data <- ipg_forms
    base_color <- "#040404"
    end_color <- "#04abeb"
  } else {
    data <- params$data
    base_color <- params$base_color
    end_color <- params$end_color
  }
  
  grade_ipg <- function(x, type = "character") {
    x <- x[!is.na(x)]
    x <- x[!is.null(x)]
    x <- x[!str_detect(x, "Not Observed|Not observed|NULL|NA")]
    
    if (type == "character") {
      x <- 100 * (sum(stringr::str_detect(x, "Yes"), na.rm = T)) /
        (sum(stringr::str_detect(x, "No"), na.rm = T) + sum(str_detect(x, "Yes"), na.rm = T))
    } else if (type == "numeric") {
      x <- 100 * (sum(stringr::str_detect(x, "3|4"), na.rm = T)) /
        (sum(!stringr::str_detect(x, "3|4"), na.rm = T) + sum(str_detect(x, "3|4"), na.rm = T))
    } else if (type == "numeric_low") {
      x <- 100 * (sum(stringr::str_detect(x, "2|3"), na.rm = T)) /
        (sum(!stringr::str_detect(x, "2|3"), na.rm = T) + sum(str_detect(x, "2|3"), na.rm = T))
    }
  }
  
  ipg_adjust <- data |>
    dplyr::mutate(
      direct_to_ts_obs = stringr::str_replace_all(direct_to_ts_obs, c(
        "Ongoing" = "Mid-year (middle of service, if applicable)"#,
        # "Other" = "Mid-year (middle of service, if applicable)"
      ))
    ) |>
    dplyr::filter(direct_to_ts_obs != "Other") |>
    tidyr::drop_na(direct_to_ts_obs) |>
    dplyr::mutate(direct_to_ts_obs = factor(direct_to_ts_obs,
                                            levels = c(
                                              "Baseline (first observation of the year)",
                                              "Mid-year (middle of service, if applicable)",
                                              "End of year (last observation of the year)"
                                            )
    ))
  
  k_12_ela <- ipg_adjust |>
    dplyr::select(
      direct_to_ts_obs,
      k12_ela_ca1a, k12_ela_ca1b, k12_ela_ca1c, # Core Action 1, Yes/No
      k12_ela_ca2a, k12_ela_ca2b, k12_ela_ca2c, k12_ela_ca2d, # Core Action 2, 1-4
      k12_ela_ca3a, k12_ela_ca3b, k12_ela_ca3c, k12_ela_ca3d, k12_ela_ca3e, k12_ela_ca3f
    ) |> # Core Action 3, 1-4
    dplyr::mutate(
      dplyr::across(!direct_to_ts_obs, ~ as.character(.x))
    ) |>
    dplyr::filter(if_all(c(k12_ela_ca1a, k12_ela_ca1b, k12_ela_ca1c), ~ !is.na(.x))) |>
    dplyr::group_by(direct_to_ts_obs) |>
    dplyr::summarise(
      dplyr::across(c(k12_ela_ca1a, k12_ela_ca1b, k12_ela_ca1c), ~ grade_ipg(.x, type = "character")),
      dplyr::across(c(
        k12_ela_ca2a, k12_ela_ca2b, k12_ela_ca2c, k12_ela_ca2d,
        k12_ela_ca3a, k12_ela_ca3b, k12_ela_ca3c, k12_ela_ca3d, k12_ela_ca3e, k12_ela_ca3f
      ), ~ grade_ipg(.x, type = "numeric"))
    ) |>
    tidyr::pivot_longer(!direct_to_ts_obs, names_to = "Name", values_to = "value") |>
    dplyr::mutate(`Core Action` = case_when(
      stringr::str_detect(Name, "ca1") ~ "Core Action 1",
      stringr::str_detect(Name, "ca2") ~ "Core Action 2",
      stringr::str_detect(Name, "ca3") ~ "Core Action 3"
    )) |>
    dplyr::group_by(`Core Action`, direct_to_ts_obs) |>
    dplyr::summarise(value = mean(value)) |>
    dplyr::ungroup() |>
    (\(.) dplyr::add_row(., `Core Action` = "Overall", direct_to_ts_obs = "Baseline (first observation of the year)", .before = 1))() |>
    (\(.) dplyr::add_row(., `Core Action` = "Overall", direct_to_ts_obs = "Mid-year (middle of service, if applicable)", .before = 1))() |>
    (\(.) dplyr::add_row(., `Core Action` = "Overall", direct_to_ts_obs = "End of year (last observation of the year)", .before = 1))() |>
    dplyr::group_by(direct_to_ts_obs) |>
    dplyr::mutate(value = ifelse(is.na(value), mean(value, na.rm = T), value)) |>
    dplyr::ungroup() |>
    dplyr::mutate(direct_to_ts_obs = factor(direct_to_ts_obs,
                                            levels = c(
                                              "Baseline (first observation of the year)",
                                              "Mid-year (middle of service, if applicable)",
                                              "End of year (last observation of the year)"
                                            )
    )) |>
    tidyr::drop_na(value)
  
  ipg_ela_n_size <- ipg_adjust |>
    dplyr::filter(!is.na(as.character(k12_ela_ca1a))) |>
    dplyr::group_by(direct_to_ts_obs) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::mutate(n = as.character(n)) |>
    with(setNames(stringr::str_wrap(paste0(direct_to_ts_obs, " (n = ", n, ")"), width = 35), direct_to_ts_obs))
  
  unique_color_n <- length(unique(k_12_ela$`Core Action`))
  
  ela_plot <- k_12_ela |>
    ggplot(aes(x = `Core Action`, y = value, fill = `Core Action`)) +
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
    scale_fill_manual(values = tlShiny::tl_palette2(n = unique_color_n, base_color_start = base_color, end_color_start = end_color)) +
    scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      limits = c(0, 100)
    ) +
    labs(
      x = "", y = "",
      title = "% Positive Indicators K-12 ELA IPG"
    ) +
    tlShiny::theme_tl() +
    theme(
      strip.text = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(size = "13"),
      plot.title = element_text(face = "bold", family = "Calibri Bold")
    )
  
  ela_plot
  
}

make_ipg_ela_summary_chart()

ggsave(here::here("images/newsletter/ipg_visual1.png"),
       bg = "white",
       width = 12,
       height = 10)

make_ipg_math_summary_chart()

ggsave(here::here("images/newsletter/ipg_visual2.png"),
       bg = "white",
       width = 12,
       height = 10)


make_ipg_fsot_summary_chart()

ggsave(here::here("images/newsletter/ipg_visual3.png"),
       bg = "white",
       width = 12,
       height = 10)
