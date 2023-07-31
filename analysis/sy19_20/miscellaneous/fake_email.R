library(blastula)
library(glue)

# Fake data for test emails
# data <- read_rds(here("Staffing/data.rds"))
data <- read_sheet("https://docs.google.com/spreadsheets/d/1nREEae-VxYPTXb-LzKuM3tgPkTA5UZmFxHHkRNCkNtQ/edit#gid=0",
                   sheet = 1, range = "A1:I2")

# here("Rmd") %>% dir() %>% print()

# Create rmd to be sent
my_email_object <- render_email(
  input = here("Rmd/email.rmd"),
  # output_options = list(
  #   runtime = "shiny"
  # )
)

email <- c("brad.haggerty@teachinglab.org", "duncan.gates@teachinglab.org", "jenn.becker@teachinglab.org",
           "julie.poluszejko@teachinglab.org", "kristen.economaki@teachinglab.org", "quintin.bostic@teachinglab.org",
           "rene.arnold@teachinglab.org", "hamy.vu@teachinglab.org")

# email <- c("duncan.gates@teachinglab.org", "staffing@teachinglab.org")


# Create subject line
subject <- glue::glue("Facilitator Request for {paste(data$call_times, collapse = ', ')}")

# Send email
my_email_object %>%
  smtp_send(
    to = email,
    from = "staffing@teachinglab.org",
    subject = subject,
    credentials = creds_file(here("email_creds_staffing"))
  )
