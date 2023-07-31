library(tidyverse)
library(googlesheets4)
library(gt)
library(glue)

change_type <- c("Click here if you are able to lead facilitate only" = "Lead Facilitator",
                 "Click here if you are able to tech/support facilitate only" = "Tech/Support Facilitator",
                 "Click here if you are available for either role" = "Either")

facilitator_table <- read_sheet("https://docs.google.com/spreadsheets/d/1sow3ucqMQg9X-5ebHt6xfuvqRoKQ0t4NdOKHQNfXz10/edit?resourcekey#gid=1721007024",
                                sheet = "Form Responses 9") %>%
  mutate(across(c(3), ~ str_replace_all(.x, change_type)))

# facilitator_table_gt <- facilitator_table %>%
#   select(c(2, 3, 4, 6))

# number <- str_count(facilitator_table_gt$`Choose times:`[1], ",") + 1
# 
# into = map_chr(1:number, ~ paste0("Time ", .x))
# 
# facilitator_table_gt %>%
#   tidyr::separate(col = "Choose times:", sep = ", ", into = into)



# facilitator_table_gt %>% 
#   gt() %>%
#   cols_label(
#     `Please Enter Your Name:` = md("**Name**"),
#     `Choose Facilitation Role:` = md("**Preference**")
#   )

# 
# {facilitator_table$`Please Enter Your Name:`[3]} has signed up to {facilitator_table$`Choose Facilitation Role:`[3]} at {facilitator_table$`Please Confirm the Site:`[3]} at the following times: 
#   
#   {facilitator_table$`Choose times:`[3]}.
# 
# {if_else(!is.na(facilitator_table$`(Optional) Any additional notes you would like to share regarding your availability?`[3]), paste0('They also left the following comments: ', facilitator_table$`(Optional) Any additional notes you would like to share regarding your availability?`[3]), 'They did not leave any additional comments.')}
# 
# 
# 
# {facilitator_table$`Please Enter Your Name:`[4]} has signed up to {facilitator_table$`Choose Facilitation Role:`[4]} at {facilitator_table$`Please Confirm the Site:`[4]} at the following times: 
#   
#   {facilitator_table$`Choose times:`[4]}.
# 
# {if_else(!is.na(facilitator_table$`(Optional) Any additional notes you would like to share regarding your availability?`[4]), paste0('They also left the following comments: ', facilitator_table$`(Optional) Any additional notes you would like to share regarding your availability?`[4]), 'They did not leave any additional comments.')}
# 
# 
# 
# {facilitator_table$`Please Enter Your Name:`[5]} has signed up to {facilitator_table$`Choose Facilitation Role:`[5]} at {facilitator_table$`Please Confirm the Site:`[5]} at the following times: 
#   
#   {facilitator_table$`Choose times:`[5]}.
# 
# {if_else(!is.na(facilitator_table$`(Optional) Any additional notes you would like to share regarding your availability?`[5]), paste0('They also left the following comments: ', facilitator_table$`(Optional) Any additional notes you would like to share regarding your availability?`[5]), 'They did not leave any additional comments.')}
# 
# 
# 
# {facilitator_table$`Please Enter Your Name:`[6]} has signed up to {facilitator_table$`Choose Facilitation Role:`[6]} at {facilitator_table$`Please Confirm the Site:`[6]} at the following times: 
#   
#   {facilitator_table$`Choose times:`[6]}.
# 
# {if_else(!is.na(facilitator_table$`(Optional) Any additional notes you would like to share regarding your availability?`[6]), paste0('They also left the following comments: ', facilitator_table$`(Optional) Any additional notes you would like to share regarding your availability?`[6]), 'They did not leave any additional comments.')}


my_email_object2 <- compose_email(
  body = md(glue::glue(
    "
Hello!

{facilitator_table$`Please Enter Your Name:`[1]} has signed up to {facilitator_table$`Choose Facilitation Role:`[1]} at {facilitator_table$`Please Confirm the Site:`[1]} at the following times: 

{facilitator_table$`Choose times:`[1]}.

{if_else(!is.na(facilitator_table$`(Optional) Any additional notes you would like to share regarding your availability?`[1]), paste0('They also left the following comments: ', facilitator_table$`(Optional) Any additional notes you would like to share regarding your availability?`[1]), 'They did not leave any additional comments.')}



{facilitator_table$`Please Enter Your Name:`[2]} has signed up to {facilitator_table$`Choose Facilitation Role:`[2]} at {facilitator_table$`Please Confirm the Site:`[2]} at the following times: 

{facilitator_table$`Choose times:`[2]}.

{if_else(!is.na(facilitator_table$`(Optional) Any additional notes you would like to share regarding your availability?`[2]), paste0('They also left the following comments: ', facilitator_table$`(Optional) Any additional notes you would like to share regarding your availability?`[2]), 'They did not leave any additional comments.')}



Here's what the eventual table will look like.



| Names  | Preferences | Date/Time 1 | Date/Time 2 | Date/Time 3 | Additional Comments |
|--------|-------------|-------------|-------------|-------------|---------------------|
| Duncan | Lead        | Now         | Now         | Future      | None                |
| Dunk   | Tech        | Tomorrow    | Tomorrow    | Now         | Ready to get paid!  |
| Dunkin | Either      | Future      | Future      | Tomorrow    | I'm Duncan          |

Cheers,

Teaching Lab Staffing
")
)
)

# email <- c("brad.haggerty@teachinglab.org", "duncan.gates@teachinglab.org", "jenn.becker@teachinglab.org",
#            "julie.poluszejko@teachinglab.org", "kristen.economaki@teachinglab.org", "quintin.bostic@teachinglab.org",
#            "rene.arnold@teachinglab.org", "hamy.vu@teachinglab.org")

# email <- c("duncan.gates@teachinglab.org", "staffing@teachinglab.org")

email <- c("brad.haggerty@teachinglab.org", "duncan.gates@teachinglab.org", "jenn.becker@teachinglab.org",
           "julie.poluszejko@teachinglab.org", "kristen.economaki@teachinglab.org", "quintin.bostic@teachinglab.org",
           "rene.arnold@teachinglab.org", "hamy.vu@teachinglab.org")


# Create subject line
subject <- glue::glue("Facilitator Request Return for {paste(data$`site`, data$call_times, collapse = ', ')}")

# Send email
my_email_object2 %>%
  smtp_send(
    to = email,
    from = "staffing@teachinglab.org",
    subject = subject,
    credentials = creds_file(here("email_creds_staffing"))
  )


