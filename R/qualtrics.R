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
  
  need_actual_names <- which(purrr::map_dbl(colnames_relabel, ~ sum(is.null(.x))) >= 1)
  colnames_relabel[need_actual_names] <- colnames(df)[need_actual_names]
  
  colnames(df) <- colnames_relabel
  
  if (switch == TRUE) {
    attr(df, "label") <- original_colnames
  }
  
  return(df)
  
}