library(tidyverse)
data <- readr::read_rds(here::here("dashboards/RaceandCulture2/data/full_data.rds"))
# data <- data %>%
#   mutate(`What is your role?` = coalesce(`What is your role?`, `What is your role`),
#          `Are you part of the Leadership Team at Teaching Lab?` = coalesce(`Are you part of the Leadership Team at Teaching Lab?`,
#                                                                            `Are you part of the Leadership Team at Teaching Lab`)) %>%
#   select(-c(`What is your role`, `Are you part of the Leadership Team at Teaching Lab`))

# data <- data %>%
#   mutate(`How do you identify racially or ethnically?` = str_replace_all(`How do you identify racially or ethnically?`,
#                                                                          c("Person of color" = "As a person of color",
#                                                                            "Race: Prefer not to answer" = "Prefer not to answer",
#                                                                            "White person" = "As a white person")))
# data %>% readr::write_rds(here::here("dashboards/RaceandCulture2/data/full_data.rds"))

gwc_1 <- data %>% select(
  `I am satisfied with the logistics of working for Teaching Lab (contracting, invoices, reimbursements, payment, etc.).`,
  `I am satisfied with the logistics of working for Teaching Lab (contracting, invoices, reimbursements, payment, etc.)`,
  `I feel the work I am doing at Teaching Lab will lead to positive impact on students.`,
  `I expect to be working at Teaching Lab organization three years from now.`,
  `I am satisfied with the work-life balance I'm able to achieve while working for Teaching Lab.`,
  `I have access to the resources I need to do my job well at Teaching Lab.`,
  `I am satisfied with my manager at Teaching Lab.`,
  time
) %>%
  mutate_all( ~ as.character(.x)) %>%
  mutate(`I am satisfied with the logistics of working for Teaching Lab (contracting, invoices, reimbursements, payment, etc.)` = 
           coalesce(`I am satisfied with the logistics of working for Teaching Lab (contracting, invoices, reimbursements, payment, etc.).`,
                    `I am satisfied with the logistics of working for Teaching Lab (contracting, invoices, reimbursements, payment, etc.)`)) %>%
  select(-`I am satisfied with the logistics of working for Teaching Lab (contracting, invoices, reimbursements, payment, etc.).`) %>%
  group_by(time) %>%
  summarise(across(everything(), ~ round(100 * (sum(.x %in% c("4", "5"), na.rm = T)/length(which(!is.na(.x))))))) %>%
  pivot_longer(!time, names_to = "Question", values_to = "Percent")

gwc_1 %>% write_rds(here::here("dashboards/RaceandCulture2/data/gwc_1.rds"))

mngmnt <- data %>%
  select(contains("leadership") | contains("executive") | contains("management"),
         `My manager(s) are clear about where the organization is going.`, 
         `My manager(s) make their expectations clear.`,
         `My manager(s) include me on decisions that impact my work and work environment.`,
         `I can ask my manager(s) a question and get a straight answer.`,
         
         time) %>%
  mutate_all( ~ as.character(.x)) %>%
  transmute(`Are you part of the Leadership Team at Teaching Lab?` = coalesce(`Are you part of the Leadership Team at Teaching Lab?`,
                                                                           `Are you part of the Leadership Team at Teaching Lab`),
         `Our leadership team communicates well with the organization.` = coalesce(`Our leadership team communicates well with the organization.`,
                                                                                   `Our leadership team communicates well with the organization..`,
                                                                                   `Our Executive Team (CEO Sarah Johnson, COO Chris Daniels, MD - Content Design Vaishali Joshi, MD - Partnerships Sheena Lights, MD - Learning and Research HaMy Vu) communicates well with the organization.`,
                                                                                   `Our Executive Team (CEO Sarah Johnson, MD - Content Design Vaishali Joshi, MD - Partnerships Sheena Lights, MD - Learning and Research HaMy Vu) communicates well with the organization.`),
         `My manager(s) are clear about where the organization is going.` = `My manager(s) are clear about where the organization is going.`,
         `Are you part of the Leadership Team at Teaching Lab?` = `Are you part of the Leadership Team at Teaching Lab?`,
         `My manager(s) are clear about where the organization is going.` = `My manager(s) are clear about where the organization is going.`, 
         `My manager(s) make their expectations clear.` = `My manager(s) make their expectations clear.`,
         `My manager(s) include me on decisions that impact my work and work environment.` = `My manager(s) include me on decisions that impact my work and work environment.`,
         `I can ask my manager(s) a question and get a straight answer.` = `I can ask my manager(s) a question and get a straight answer.`,
         time = time) %>%
  group_by(time) %>%
  summarise(across(!c(`Are you part of the Leadership Team at Teaching Lab?`), ~ round(100 * (sum(.x %in% c("4", "5"), na.rm = T)/length(which(!is.na(.x))))))) %>%
  pivot_longer(!time, names_to = "Question", values_to = "Percent")
mngmnt %>% write_rds(here::here("dashboards/RaceandCulture2/data/mngmnt.rds"))

employee_engagment <- data %>%
  select(`I know what is expected of me at work.`,
         `I have the materials and equipment I need to do my work right.`,
         `At work, I have the opportunity to do what I do best every day.`,
         `In the last seven days, I have received recognition or praise for doing good work.`,
         `My supervisor, or someone at work, seems to care about me as a person.`,
         `There is someone at work who encourages my development.`,
         `At work, my opinions seem to count.`,
         `The mission or purpose of my organization makes me feel my job is important.`,
         `My associates (fellow employees) are committed to doing quality work.`,
         `I have a best friend at work.`,
         `In the last six months, someone at work has talked to me about my progress.`,
         `This last year, I have had opportunities at work to learn and grow.`,
         time) %>%
  group_by(time) %>%
  summarise(across(everything(), ~ round(100 * (sum(.x %in% c("4", "5"), na.rm = T)/length(which(!is.na(.x))))))) %>%
  pivot_longer(!time, names_to = "Question", values_to = "Percent")
employee_engagment %>% write_rds(here::here("dashboards/RaceandCulture2/data/employee_engagment.rds"))

tl_values <- data %>%
  select(
    `To what degree does the Teaching Lab community uphold our values of Habits`,
    `To what degree does the Teaching Lab community uphold our values of Head`,
    `To what degree does the Teaching Lab community uphold our values of Heart`,
    `To what degree does the Teaching Lab community uphold our values of Racial Equity`,
    time) %>%
  group_by(time) %>%
  summarise(across(everything(), ~ round(100 * (sum(.x %in% c("4", "5"), na.rm = T)/length(which(!is.na(.x))))))) %>%
  pivot_longer(!time, names_to = "Question", values_to = "Percent")
tl_values %>% write_rds(here::here("dashboards/RaceandCulture2/data/tl_values.rds"))

equity_inclusion <- data %>%
  select(
    `Our organization has an explicit commitment to inclusiveness.`,
    `Our organization has an explicit commitment to racial equity.`,
    `We have free and open expression of ideas, opinions and beliefs.`,
    `We have free and open expression of ideas, opinions and beliefs..`,
    `We have frequent conversations about race/ethnicity re: how we work.`,
    `We have frequent conversations about race/ethnicity re: how we work..`,
    `We have frequent conversations about how power and privilege impact our work at Teaching Lab.`,
    `I’m given the opportunity to contribute meaningfully in meetings.`,
    `I am given the opportunity to contribute meaningfully in meetings.`,
    `I trust that I will not be penalized if I ask for help or admit a mistake at work.`,
    `I can bring my "whole self" to work`,
    `I can bring my "whole self" to work.`,
    `Our organization has a racial equity strategy.`,
    `Team members are held accountable for racial equity.`,
    `Our Executive Team’s actions show that racial equity is a priority at Teaching Lab.`,
    `Our organization tries to remove bias in our hiring process`,
    `Career advancement is equally accessible for all`,
    `Our compensation systems are implemented fairly`,
    `I see a diverse group of employees advancing`,
    `I see a clear link between performance and opportunities for promotion`,
    `I see the same opportunity for advancement for all racial/ethnic identities`,
    `Our culture respects individuals and values differences`,
    `Our culture respects individuals and values differences.`,
    time
  ) %>%
  transmute(
    `We have frequent conversations about race/ethnicity re: how we work.` = coalesce(`We have frequent conversations about race/ethnicity re: how we work.`, `We have frequent conversations about race/ethnicity re: how we work..`),
    `We have free and open expression of ideas, opinions and beliefs.` = coalesce(`We have free and open expression of ideas, opinions and beliefs.`, `We have free and open expression of ideas, opinions and beliefs..`,),
    `I’m given the opportunity to contribute meaningfully in meetings.` = coalesce(`I’m given the opportunity to contribute meaningfully in meetings.`, `I am given the opportunity to contribute meaningfully in meetings.`),
    `I can bring my "whole self" to work.` = coalesce(`I can bring my "whole self" to work.`, `I can bring my "whole self" to work`),
    `Our organization has an explicit commitment to inclusiveness.`,
    `Our organization has an explicit commitment to racial equity.`,
    `Our organization has a racial equity strategy.`,
    `I trust that I will not be penalized if I ask for help or admit a mistake at work.`,
    `Team members are held accountable for racial equity.`,
    `Our Executive Team’s actions show that racial equity is a priority at Teaching Lab.`,
    `We have frequent conversations about how power and privilege impact our work at Teaching Lab.`,
    `Our organization tries to remove bias in our hiring process` = `Our organization tries to remove bias in our hiring process`,
    `Career advancement is equally accessible for all` = `Career advancement is equally accessible for all`,
    `Our compensation systems are implemented fairly` = `Our compensation systems are implemented fairly`,
    `I see a diverse group of employees advancing` = `I see a diverse group of employees advancing`,
    `I see a clear link between performance and opportunities for promotion` = `I see a clear link between performance and opportunities for promotion`,
    `I see the same opportunity for advancement for all racial/ethnic identities` = `I see the same opportunity for advancement for all racial/ethnic identities`,
    `Our culture respects individuals and values differences.` = coalesce(`Our culture respects individuals and values differences.`, `Our culture respects individuals and values differences`),
    time
  ) %>%
  group_by(time) %>%
  summarise(across(everything(), ~ round(100 * (sum(.x %in% c("4", "5"), na.rm = T)/length(which(!is.na(.x))))))) %>%
  pivot_longer(!time, names_to = "Question", values_to = "Percent")
equity_inclusion %>% write_rds(here::here("dashboards/RaceandCulture2/data/equity_inclusion.rds"))

slgs <- data %>%
  select(contains("SLG") | contains("Social Learning"), time) %>%
  group_by(time) %>%
  summarise(across(everything(), ~ round(100 * (sum(.x %in% c("4", "5"), na.rm = T)/length(which(!is.na(.x))))))) %>%
  pivot_longer(!time, names_to = "Question", values_to = "Percent")
slgs %>% write_rds(here::here("dashboards/RaceandCulture2/data/slgs.rds"))


data <- data %>%
  mutate_all( ~ as.character(.x)) %>%
  mutate(`I am satisfied with the logistics of working for Teaching Lab (contracting, invoices, reimbursements, payment, etc.)` = 
           coalesce(`I am satisfied with the logistics of working for Teaching Lab (contracting, invoices, reimbursements, payment, etc.).`,
                    `I am satisfied with the logistics of working for Teaching Lab (contracting, invoices, reimbursements, payment, etc.)`)) %>%
  select(-`I am satisfied with the logistics of working for Teaching Lab (contracting, invoices, reimbursements, payment, etc.).`) %>%
  mutate(`Our leadership team communicates well with the organization.` = coalesce(`Our leadership team communicates well with the organization.`,
                                                                                      `Our leadership team communicates well with the organization..`,
                                                                                      `Our Executive Team (CEO Sarah Johnson, COO Chris Daniels, MD - Content Design Vaishali Joshi, MD - Partnerships Sheena Lights, MD - Learning and Research HaMy Vu) communicates well with the organization.`,
                                                                                      `Our Executive Team (CEO Sarah Johnson, MD - Content Design Vaishali Joshi, MD - Partnerships Sheena Lights, MD - Learning and Research HaMy Vu) communicates well with the organization.`)) %>%
           select(-c(`Our leadership team communicates well with the organization..`,
                                                                             `Our Executive Team (CEO Sarah Johnson, COO Chris Daniels, MD - Content Design Vaishali Joshi, MD - Partnerships Sheena Lights, MD - Learning and Research HaMy Vu) communicates well with the organization.`,
                                                                             `Our Executive Team (CEO Sarah Johnson, MD - Content Design Vaishali Joshi, MD - Partnerships Sheena Lights, MD - Learning and Research HaMy Vu) communicates well with the organization.`)) %>%
  mutate(
    `We have frequent conversations about race/ethnicity re: how we work.` = coalesce(`We have frequent conversations about race/ethnicity re: how we work.`, `We have frequent conversations about race/ethnicity re: how we work..`),
    `We have free and open expression of ideas, opinions and beliefs.` = coalesce(`We have free and open expression of ideas, opinions and beliefs.`, `We have free and open expression of ideas, opinions and beliefs..`),
    `I’m given the opportunity to contribute meaningfully in meetings.` = coalesce(`I’m given the opportunity to contribute meaningfully in meetings.`, `I am given the opportunity to contribute meaningfully in meetings.`),
    `I can bring my "whole self" to work.` = coalesce(`I can bring my "whole self" to work.`, `I can bring my "whole self" to work`),
    `Our culture respects individuals and values differences.` = coalesce(`Our culture respects individuals and values differences.`, `Our culture respects individuals and values differences`),
    time
  ) %>%
  select(-c(`We have frequent conversations about race/ethnicity re: how we work..`,
            `We have free and open expression of ideas, opinions and beliefs..`,
            `I am given the opportunity to contribute meaningfully in meetings.`,
            `Our culture respects individuals and values differences`))
  
  
  
plotting <- data %>% 
  select(`How do you identify racially or ethnically?`,
         time,
         gwc_group) %>%
  group_by(time, `How do you identify racially or ethnically?`) %>%
  summarise(across(everything(), ~ round(100 * (sum(.x %in% c("4", "5"), na.rm = T) / length(which(!is.na(.x))))))) %>%
  pivot_longer(!c(time, `How do you identify racially or ethnically?`), names_to = "Question", values_to = "Percent") %>%
  mutate(Question = TeachingLab::html_wrap(Question, interval = 25))

ggplot(plotting, aes(factor(Question), Percent, fill = `How do you identify racially or ethnically?`)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = paste0(Percent, "%")), position = position_dodge(width = 1), vjust = -1.1) +
  facet_wrap( ~ time) +
  labs(x = "", y = "") +
  scale_fill_tl(n = 4) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 105)) +
  theme_tl() +
  theme(axis.text.x = element_markdown(lineheight = 1.1, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        strip.text = element_text(hjust = 0.5, face = "bold"))
  
  
  
