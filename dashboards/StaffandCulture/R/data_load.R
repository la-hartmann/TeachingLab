library(shiny)
library(shiny.semantic)
library(semantic.dashboard) # <-- Change this line to: library(semantic.dashboard)
library(sysfonts)
library(tidyverse)
library(TeachingLab)
library(tidytext)
library(bookdown)
library(ggtext)
library(grid)
library(shinybusy)
library(shinyWidgets)
font_add(family = "Calibri", regular = "www/Calibri.ttf")

round_even <- function(x) {
  2 * ceiling(x / 2)
}

# write_rds(discrete_data, here::here("dashboards/RaceandCulture/data/discrete_data.rds"))
data <- read_rds("data/full_data.rds") %>%
  mutate(time = factor(time, levels = rev(c("March 2019", "July 2019", "February 2020", "November 2020", "July 2021")))) #%>%
  # rename(`I am satisfied with the logistics of working for Teaching Lab.` = `I am satisfied with the logistics of working for Teaching Lab (contracting, invoices, reimbursements, payment, etc.)`)


gwc_1 <- read_rds("data/gwc_1.rds") #%>%
  # mutate(Question = str_replace_all(Question, c("I am satisfied with the logistics of working for Teaching Lab \\(contracting, invoices, reimbursements, payment, etc.\\)" = "I am satisfied with the logistics of working for Teaching Lab.")))
gwc_2 <- read_rds("data/gwc_plot2.rds")
mngmnt <- read_rds("data/mngmnt.rds")
employee_engagement <- read_rds("data/employee_engagment.rds")
equity_inclusion <- read_rds("data/equity_inclusion.rds")
tl_values <- read_rds("data/tl_values.rds")
slgs <- read_rds("data/slgs.rds")

gwc_group <- gwc_1$Question %>% unique()
mngmnt_group <- mngmnt$Question %>% unique()
employee_group <- employee_engagement$Question %>% unique()
equity_group <- equity_inclusion$Question %>% unique()
tl_values_group <- tl_values$Question %>% unique()
slgs_group <- slgs$Question %>% unique()
  

# july_2021 <- read_rds(here::here("dashboards/StaffandCulture/data/july_2021.rds"))
# 
# data <- read_rds(here::here("dashboards/StaffandCulture/data/full_data.rds")) %>%
#   filter(time != "July 2021") %>%
#   bind_rows(july_2021) %>%
#   mutate(time = factor(time, levels = c("March 2019", "July 2019", "February 2020", "November 2020", "July 2021")))
# write_rds(data, here::here("dashboards/StaffandCulture/data/full_data.rds"))





