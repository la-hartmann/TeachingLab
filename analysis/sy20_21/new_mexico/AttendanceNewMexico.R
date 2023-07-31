library(googlesheets4)
library(tidyverse)
library(googledrive)

newmexico_data <- read_sheet("https://docs.google.com/spreadsheets/d/15mSsAWPq-d0s-syNHKhgUPf4iHYr-zP0uslxKcRheIc/edit#gid=1817148802")
participants_data <- read_sheet("https://docs.google.com/spreadsheets/d/1aj3MxmGRkN9CUABxOZUOhTqWpz96Flbidxc3hcp_Q_A/edit#gid=1112537703") %>%
  select(`Participant Name`, `Activity Number`)
	
newmexico_wide <- newmexico_data %>%
  mutate(`Your first and last name` = str_to_title(`Your first and last name`),
         `Your first and last name` = str_replace_all(`Your first and last name`, "Annamaria Trujillo (Mia)", "Mia Trujillo"),
         `Your first and last name` = str_replace(`Your first and last name`, "Annamaria Trujillo (Mia)", "Mia Trujillo")) %>%
  filter(`Name of district/parish/network/school` == "New Mexico Public Education Department, NM") %>%
  group_by(`Date for this training`) %>%
  # filter(`Your first and last name` %in% participants_data$`Participant Name`) %>%
  pivot_wider(values_from = "Your first and last name", names_from = "Date for this training")

name_replacement <- c("Mickey D. Click" = "Mickey Click",
                      "Annamaria Trujillo (Mia)" = "Mia Trujillo",
                      "Militza Zamoradegeisel" = "Militza Geisel",
                      "Samial B. Morerod" = "Sam Morerod",
                      "Thomas Barksdale" = "Tommy Barksdale")

just_dates <- newmexico_wide %>%
  select(13:18) %>%
  pivot_longer(everything(), names_to = "Date", values_to = "Name") %>%
  drop_na() %>%
  mutate(Name = str_replace_all(Name, name_replacement),
         Name = str_replace(Name, "Annetta(?! )", "Annetta Hadley"),
         Name = str_replace(Name, "(?<! )Longwill", "Maggie Longwill"),
         Name = str_replace(Name, "Alan(?! )", "Alan French"))
just_dates$Name[3] <- "Mia Trujillo"
just_dates_final <- left_join(just_dates, participants_data, by = c("Name" = "Participant Name")) %>%
  mutate(`Activity Number` = str_replace(`Activity Number`, "1,2", "1, 2"),
         Date = as.Date(Date),
         Attended = T) %>%
  arrange(desc(`Activity Number`, Date))


write_sheet(just_dates_final, ss = "https://docs.google.com/spreadsheets/d/1lCykVWBsOc0xLUFWTzdbw4mS8IqDtvVkI8sh76kw-Zg/edit#gid=0", sheet = 1)
