#' @title Fetch Missing Information
#' @description This function takes in a faculty's name and a column name, and returns the last recorded
#'              non NA value for that column for given faculty
#' @param first.name Faculty's first name
#' @param last.name Faculty's last name
#' @param x data set to search in
#' @param column column of interest
#'
#' @return last recorded non NA value for column of interest for given faculty
#'
#' @export
fetch_missing_information <- function(x, firstname, lastname, column){
  stopifnot(is.data.frame(x))
  stopifnot(column %in% names(x))

  ## First select only relevant rows from x
  df <- dplyr::filter(x, first.name == firstname & last.name == lastname)

  ## Now inspect the column of interest. We will drop all NA values, and then take the last one (if non NA value is present).
  ## Might have stored String NAs as "" -- fix this
  info <- df[[column]]
  info[which(info == "")] <- NA
  info <- info[which(! is.na(info))]


  ifelse(length(info) == 0, NA, tail(info, 1))
}
