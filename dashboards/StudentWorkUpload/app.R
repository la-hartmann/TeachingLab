library(shinyforms)
quickform(
    title = "Upload your files here",
    description = "Describe your survey here",
    questions = list(
        list(id = "name",
             type = "shortanswer",
             question = "What is your name?",
             required = TRUE),
        list(id = "email",
             type = "text",
             question = "What is your email?",
             required = TRUE),
        list(id = "file",
             type = "text",
             question = "Please upload file here",
             required = TRUE)
    ),
    gmail = FALSE,
    folder = "shinyforms"
)