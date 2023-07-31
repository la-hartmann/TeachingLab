

percent_round <- function(x) {
  round(100 * (sum(x %in% c("4", "5"), na.rm = T) / length(which(!is.na(x)))))
}
