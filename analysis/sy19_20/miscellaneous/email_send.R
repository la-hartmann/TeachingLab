library(blastula)
library(glue)

# Fake data for test emails
data <- read_rds("Data/new_data.rds")

# here("Rmd") %>% dir() %>% print()

# Create rmd to be sent
my_email_object <- render_email(
  input = "Rmd/email.rmd"
)

email <- "duncan.gates@teachinglab.org"
  

# Create subject line
subject <- glue::glue("Facilitator Request for {paste(data$call_times, collapse = ', ')}")

# Send email
my_email_object %>%
  smtp_send(
    to = email,
    from = "staffing@teachinglab.org",
    subject = subject,
    credentials = creds_file("email_creds_staffing")
  )
