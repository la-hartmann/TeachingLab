#' @title Qualtrics Data Frame Relabel
#' @description Switches the labels and column names of a data frame, or just changes 
#' the column names to the labels
#' @param df the relevant dataframe
#' @param switch T/F whether or not to switch the names
#' @return a data.frame
#'
#' @examples
#' \dontrun{
#' qualtrics_survey |>
#'   relabel_qualtrics_df()
#' }
#' @export

relabel_qualtrics_df <- function(df, switch = TRUE) {
  
  if (switch == TRUE) {
    original_colnames <- colnames(df)
  }
  
  colnames_relabel <- purrr::map(df, ~ attr(.x, "label"))
  
  ### Handle attr that returns null by replacing with actual names from the dataframe ###
  need_actual_names <- which(purrr::map_dbl(colnames_relabel, ~ sum(is.null(.x))) >= 1)
  colnames_relabel[need_actual_names] <- colnames(df)[need_actual_names]
  
  colnames_relabel <- colnames_relabel |>
    flatten() |>
    map_chr( ~ .x[1])
  
  ### Handle duplicate columns ###
  if (sum(duplicated(colnames_relabel)) >= 1) {
    
    replace_tibble <- tibble::tibble(all_names = colnames_relabel,
                                     duplicates = ifelse(duplicated(colnames_relabel), colnames_relabel[duplicated(colnames_relabel)], NA)) |>
      dplyr::group_by(duplicates) |>
      dplyr::mutate(new_name = ifelse(!is.na(duplicates), paste0(duplicates, "_", dplyr::row_number()), NA)) |>
      dplyr::ungroup() |>
      dplyr::mutate(all_names = ifelse(!is.na(new_name), new_name, all_names))
    
    colnames_relabel <- replace_tibble$all_names
    
  }
  
  colnames(df) <- colnames_relabel
  
  ### Add original column names as attr ###
  if (switch == TRUE) {
    attr(df, "label") <- original_colnames
  }
  
  return(df)
  
}