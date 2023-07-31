library(cronR)

r <- cron_rscript("InternalDashboardDataSetup.R")

cron_add(r, frequency = "daily", at = "00:00", description = "Update data.")