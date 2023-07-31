library(googlesheets4)

df <- read_sheet("https://docs.google.com/spreadsheets/d/1cELGsuEjXsb1e5oRdtdYof-Ul7jfdBXMOTYbIYd7nS4/edit#gid=0",
           sheet = 1,
           range = "A:B")

ela_courses_in_survey <- x
ela_courses_in_sheet <- y

setdiff(x, y)


math_courses_in_survey <- x
math_courses_in_sheet <- y

setdiff(x, y)


school_leaders_courses_in_survey <- x
school_leaders_courses_in_sheet <- y

setdiff(x, y)



hqim_courses_in_survey <- x
hqim_courses_in_sheet <- y

setdiff(x, y)


hqpl_courses_in_survey <- x
hqpl_courses_in_sheet <- y

setdiff(x, y)


lab_leaders_courses_in_survey <- x
lab_leaders_courses_in_sheet <- y

setdiff(x, y)


state_level_courses_in_survey <- x
state_level_courses_in_sheet <- y

setdiff(x, y)