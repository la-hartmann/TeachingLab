library(reticulate)

# Install necessary packages #
# py_install(packages = c( "langchain", "openai", "pypdf", "bs4", "python-dotenv", "chromadb", "tiktoken"), envname = "r-reticulate")

# Using r-reticulate package #
use_virtualenv("r-reticulate", required = TRUE)
# langchain <- import("langchain")  # Assuming "langchain" is the correct module name

# Use already installed OpenAI API Key #
api_key_for_py <- r_to_py(Sys.getenv("OPENAI_API_KEY"))

# Added rubrics for math and ela to system manually #
# Activate python mode
reticulate::repl_python()

all_pages_in_r <- py$all_pages

all_pages_in_r[[1]]$metadata # See metadata in the first item

# Look at split docs
my_split_docs <- py$my_split_docs

# Maximum number of characters in a chunk
get_characters <- function(the_chunk) {
  x <- nchar(the_chunk$page_content)
  return(x)
}

purrr::map_int(my_split_docs, get_characters) |>
  max()


mm_relevant <- py$mm_docs
sim_relevant <- py$sim_docs

mm_relevant[[1]]$dict

py_run_string('print(qa_chain.run("What grade would you have given to this assignment"))')

py$student_work_text[[1]]
