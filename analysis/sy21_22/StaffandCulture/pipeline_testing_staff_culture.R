gwc_1 <- read_rds("dashboards/StaffandCulture/data/gwc_1.rds")
gwc_group <- gwc_1$Question %>% unique()

data <- read_rds("dashboards/StaffandCulture/data/full_data.rds") %>%
  mutate(time = factor(time, levels = rev(c("March 2019", "July 2019", "February 2020", "November 2020", "July 2021"))))

# (data_1 <- 
    
data %>%
  select(
    gwc_group,
    `How do you identify racially or ethnically?`,
    time
  ) %>%
  mutate(across(`How do you identify racially or ethnically?`, ~ str_replace_all(.x, c("Yes" = "Manager",
                                                            "No" = "Not a manager",
                                                            "Other" = "Prefer not to answer")))) %>%
  pivot_longer(!c(`How do you identify racially or ethnically?`, time), names_to = "Question", values_to = "Rating") %>%
  drop_na(Rating) %>%
  filter(Question != "Are you part of the Leadership Team at Teaching Lab?" &
           Question != "How do you identify racially or ethnically?") %>%
  mutate(Question = TeachingLab::html_wrap(Question, n = 25)) %>%
  filter(Question == "I am satisfied with the<br>logistics of working<br>for Teaching Lab<br>(contracting, invoices,<br>reimbursements, payment,<br>etc.)" & time == "July 2021") %>%
  group_by(`How do you identify racially or ethnically?`, Question, Rating, time) %>%
  view()
  summarise(n = n()) %>%
  filter(Rating %in% c("4", "5"))
  ungroup() %>%
  group_by(Question, `How do you identify racially or ethnically?`, time) %>%
  mutate(n_group = sum(n)) %>%
  mutate(Percent = round(100*(n_group/sum(n_group)))) %>%
  mutate(n = sum(n)) %>%
  filter(Rating %in% c("4", "5")) %>%
  summarise(Percent = sum(Percent),
            n = n) %>%
  distinct(`How do you identify racially or ethnically?`, .keep_all = T) %>%
  filter(Question == "I am satisfied with the<br>logistics of working<br>for Teaching Lab<br>(contracting, invoices,<br>reimbursements, payment,<br>etc.)" & time == "July 2021") %>%
  identity())

data_1 %>%
  group_by(`Question`, time) %>%
  summarise(Percent = round(weighted.mean(Percent, n)),
            n = round(mean(n))) %>%
  mutate(`How do you identify racially or ethnically?` = "Overall")
