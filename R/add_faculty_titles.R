#' @title Add Faculty Titles
#'
#' @description This function takes as input a data frame which includes the raw
#'   text associated with each faculty member. It returns that data frame along with
#'   a new columns for the facutly's title
#'
#' @param x data frame with raw.text column
#'
#' @return the input data frame along with a new columns.
#'
#' @format \describe{
#'  \item{title}{Faculty's title}
#'  }
#'
#' @importFrom dplyr %>%
#'
#' @export

add_faculty_titles <- function(x){

  stopifnot(is.data.frame(x))
  stopifnot("raw.text" %in% names(x))
  stopifnot(is.character(x$raw.text))

  ## Grab all titles: these are always between the first and second '#'
  ## in the raw text
  x$title <- stringr::str_split(x$raw.text, "#", simplify = TRUE)[ ,2] %>% stringr::str_trim()

  ## Some error checking
  stopifnot(all(x$title != ""))

  x
}
