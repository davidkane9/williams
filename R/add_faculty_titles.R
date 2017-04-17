#' @title Add Faculty Titles
#'
#' @description This function takes as input a data frame which includes the raw
#'   text associated with each faculty member. It returns that data frame along with
#'   2 new columns: for the facutly's title and faculty's rank
#'
#' @param x data frame with raw.text column
#'
#' @return the input data frame along with a new columns.
#'
#' @format \describe{
#'  \item{title}{Faculty's title}
#'  \item{rank}{Faculty's rank}
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

  ## Now add rank as one of "Librarian", "Instructor", "Fellow", "Lecturer", "Visiting Professor",
  ## "Assistant Professor", "Associate Professor", "Professor"
  x$rank <- ""
  x$rank[which(stringr::str_detect(x$title, "Fellow"))] <- "Fellow"
  x$rank[which(stringr::str_detect(x$title, "Librarian"))] <- "Librarian"
  x$rank[which(stringr::str_detect(x$title, "Instructor"))] <- "Instructor"
  x$rank[which(stringr::str_detect(x$title, "Lecturer") & x$rank == "")] <- "Lecturer"
  x$rank[which(stringr::str_detect(x$title, "Visiting") & x$rank == "")] <- "Visiting Professor"
  x$rank[which(stringr::str_detect(x$title, "Assistant Professor") & x$rank == "")] <- "Assistant Professor"
  x$rank[which(stringr::str_detect(x$title, "Associate Professor") & x$rank == "")] <- "Associate Professor"
  x$rank[which(stringr::str_detect(x$title, "Professor") & x$rank == "")] <- "Professor"


  ## Some error checking
  stopifnot(all(x$title != ""))

  x
}
