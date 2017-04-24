#' @title Add Faculty Birth Year
#'
#' @description This function takes as input a data frame which includes the first.degree.year column
#'   associated with each faculty member. It returns that data frame along with
#'   a new column, "birth.year" which is calculated using the heuristic of year of first degree minus 22
#'
#' @param x data frame with first.degree.year column
#'
#' @return the input data frame along with a new column.
#'
#' @format \describe{
#'  \item{birth.year}{inferred birth year for faculty member}
#'  }
#'
#' @export
add_faculty_birth_year <- function(x) {

  stopifnot(is.data.frame(x))
  stopifnot("first.degree.year" %in% names(x))
  stopifnot(is.numeric(x$first.degree.year))

  ## Now add column using heuristic of year of first degree minus 22
  x$birth.year <- x$first.degree.year - 22

  x
}
