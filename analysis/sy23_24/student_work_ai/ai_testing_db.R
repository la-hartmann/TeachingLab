
test_json <- jsonlite::read_json("data/student_work/ela_student_rubric_3_12.json")
tidyjson::json_schema(test_json, type = "value")

library(DBI)

con = dbConnect(RSQLite::SQLite(), dbname = 'data/student_work/chroma_db/chroma.sqlite3')

DBI::dbListTables(con) -> tables

tables_fields <- purrr::map(tables, ~ DBI::dbListFields(con, .x))

tibble::tibble(tables = tables,
       rows = purrr::map_dbl(tables, ~ nrow(DBI::dbReadTable(con, .x))))

DBI::dbReadTable(con, "collections")
