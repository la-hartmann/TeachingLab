library(trackdown)

# Step 1: Upload File
# upload_file(file = here::here("Rmd/drive_test_doc.Rmd"))

# Step 2: Collaborate on Google Docs

# Step 3: Download File
trackdown::download_file(file = here::here("Rmd/drive_test_doc.Rmd"))

# Step 4: Update File
# Update the file in rmd and then proceed below
trackdown::update_file(file = here::here("Rmd/drive_test_doc.Rmd"), hide_code = T)

# Step 5: Render the File
# This function is simply a wrapper around `download_file()` that will additionally call `rmarkdown::render()` 
# once the document has been downloaded.
trackdown::render_file(file = here::here("Rmd/drive_test_doc.Rmd"))

