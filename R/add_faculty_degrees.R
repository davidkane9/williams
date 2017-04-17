#' @title Add Faculty Degrees
#'
#' @description This function takes as input a data frame which includes the raw
#'   text associated with each faculty member. It returns that data frame along with
#'   six new columns: first.degree, first.degree.year, first.degree.school, last.degree, last.degree.year and last.degree.school
#'
#' @param x data frame with raw.text column
#'
#' @return the input data frame along with two new columns.
#'
#' @format \describe{
#'  \item{first.degree}{First degree held by faculty; This is generally a bachelors degree}
#'  \item{first.degree.year}{Year the first degree was conferred}
#'  \item{first.degree.school}{School that conferred first degree}
#'  \item{last.degree}{First degree held by faculty; This is generally the highest degree held by the faculty.}
#'  \item{last.degree.year}{Year the last degree was conferred}
#'  \item{last.degree.school}{School that conferred last degree.}
#'  }
#'
#' @importFrom dplyr %>%
#'
#' @export
add_faculty_degrees <- function(x) {

  stopifnot(is.data.frame(x))
  stopifnot("raw.text" %in% names(x))
  stopifnot(is.character(x$raw.text))

  ## Grab all degree info: these are always succeeding the third '#' in the raw text
  ## Also, remove any leading/trailing white spaces
  degree.info <- stringr::str_split(x$raw.text, "#", simplify = TRUE)[ ,3] %>% stringr::str_trim()

  ## Now, each information about each degree is seperated by ";". Let's break that up.
  degrees <- stringr::str_split(degree.info, ";")

  ## Add information about first degree
  x$first.degree <- degrees %>% purrr::map_chr(1) %>% fetch_degree()
  x$first.degree.year <- degrees %>% purrr::map_chr(1) %>% fetch_year()
  x$first.degree.school <- degrees %>% purrr::map_chr(1) %>% fetch_school()

  ## Add information about last degree
  x$last.degree <- degrees %>% purrr::map_chr(tail, 1) %>% stringr::str_trim() %>% fetch_degree()
  x$last.degree.year <- degrees %>% purrr::map_chr(tail, 1) %>% fetch_year()
  x$last.degree.school <- degrees %>% purrr::map_chr(tail, 1) %>% fetch_school()

  x

}

## The following our standalone functions that help add the degree columns
## I have included them here (instead of their independent file) as they are only of
## relevance to add_faculty_degrees

## Given graduation details about a degree, this function finds the name of the degree
fetch_degree <- function(degree.info){
  stringr::str_split(degree.info, " ", simplify = TRUE)[ ,1] %>% stringr::str_trim()
}

## Given graduation details about a degree, this function finds the year the degree was conferred
fetch_year <- function(degree.info){
  # Get the parenthesis and what is inside
  year <- stringr::str_extract_all(degree.info, "\\([^()]+\\)")
  # Remove parenthesis
  year <- substring(year, 2, nchar(year)-1)
  as.numeric(year)
}

## Given graduation details about a degree, this function finds the name of school conferring the degree
fetch_school <- function(degree.info){
  stringr::str_split(degree.info, "\\)", simplify = TRUE)[,2] %>%  stringr::str_trim()
}

