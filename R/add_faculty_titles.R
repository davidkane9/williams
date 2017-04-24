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
#'  \item{rank}{Faculty's rank; one of "Librarian", "Instructor", "Fellow", "Lecturer", "Visiting Professor",
#'  "Assistant Professor", "Associate Professor", "Professor"}
#'  \item{status}{Faculty's status; as one of "Visiting", "Part-time", Tenure-track", or "Tenured"}
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

  ## Now add rank as one of "Librarian", "Instructor", "Fellow", "Lecturer", "Artist-in-Residence","Visiting Professor",
  ## "Assistant Professor", "Associate Professor", "Professor"
  x$rank <- NA
  x$rank[which(stringr::str_detect(x$title, "Assistant Professor"))] <- "Assistant Professor"
  x$rank[which(stringr::str_detect(x$title, "Associate Professor") & is.na(x$rank))] <- "Associate Professor"
  x$rank[which(stringr::str_detect(x$title, "Professor") & is.na(x$rank))] <- "Professor"
  x$rank[which(stringr::str_detect(x$title, "Fellow") & is.na(x$rank))] <- "Fellow"
  x$rank[which(stringr::str_detect(x$title, "Librarian") & is.na(x$rank))] <- "Librarian"
  x$rank[which(stringr::str_detect(x$title, "Instructor") & is.na(x$rank))] <- "Instructor"
  x$rank[which(stringr::str_detect(x$title, "Lecturer") & is.na(x$rank))] <- "Lecturer"
  x$rank[which(stringr::str_detect(x$title, "Artist.+in.+Residence") & is.na(x$rank))] <- "Artist-in-Residence"


  ## Now add status as one of "Visiting", "Part-time", Tenure-track", or "Tenured"
  ## The course catalogs do not give information about "tenure-track" or "tenured".
  ## We assume that all "associate" or "full" professors are "tenured".
  ## All "assisstant" professors are "tenure-track"
  x$status <- NA
  x$status[which(stringr::str_detect(x$title, "Part-time") | stringr::str_detect(x$title, "Part-Time"))] <- "Part-time"
  x$status[which(x$rank == "Associate Professor" | x$rank == "Professor")] <- "Tenured"
  x$status[which(x$rank == "Assistant Professor")] <- "Tenure-track"
  x$status[which(stringr::str_detect(x$title, "Visiting"))] <- "Visiting"

  ## Some error checking
  stopifnot(all(x$title != ""))

  x
}
