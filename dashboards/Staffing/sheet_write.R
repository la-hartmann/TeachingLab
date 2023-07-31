# Script for writing to google spreadsheet that is read in under email.rmd

# out of band (oob) option enables correctly loading auth page
options(httr_oob_default = TRUE,
        gargle_oob_default = TRUE)
# token <- gs_auth (cache = FALSE)
# saveRDS(token, file = "[PATH]/gdrive_token.rds")

# Read in token
# token <- read_rds(here::here("Tokens/gs4_token.rds"))

# Authorize google drive
drive_auth(path = "data/thermal-cathode-310719-1445194b99c7.json")
gs4_auth(token = drive_token()) # REMEMBER YOU JUST CHANGED THIS

# Register sheet
sheet <- gs4_find("Staffing Input Request")

# Read sheet
init_data <- read_sheet(ss = sheet$id, sheet = "Input", col_types = "c")

# Read new data
new_data <- read_rds("data/new_data.rds")

# Append to data
data <- bind_rows(new_data, init_data)

# Write data
write_sheet(ss = sheet$id, data = data, sheet = "Input")

# Read Sheet Back in and Save

all_input <- read_sheet(ss = sheet$id, sheet = "Input")

write_rds(all_input, here::here("data/dashboards/AllInput.rds"))


