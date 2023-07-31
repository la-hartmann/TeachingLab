# NAs dataframe
na_df <- tlShiny::na_df

ipg_forms <- readr::read_rds("data/ipg_data.rds")

###### ADD FILTER FOR TEACHER NAMES THAT THEY HAVE TO BE AFTER TODAY ######
###### MAKE TEACHER FILTER REACTIVE #######
ipg_plot_select_names <- readr::read_rds("data/ipg_plot_select_names.rds")
ipg_text_select_names <- readr::read_rds("data/ipg_text_select_names.rds")
