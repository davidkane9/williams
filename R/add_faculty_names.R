#' @title Add Faculty Names
#'
#' @description This function takes as input a data frame which includes the raw
#'   text associated with each faculty member. It returns that data frame along with
#'   two new columns: first.name and last.name. If \code{complete} is FALSE, it also
#'   returns full.name.
#'
#' @param x data frame with raw.text column
#' @param complete logical to indicate how many variables to add to x. Default is FALSE.
#'
#' @return the input data frame along with two new columns.
#'
#' @format \describe{
#'  \item{first.name}{Faculty's first name.}
#'  \item{last.name}{Faculty's last name.}
#'  }
#'
#' @importFrom dplyr %>%
#'
#' @export

add_faculty_names <- function(x, complete = FALSE){

  stopifnot(is.data.frame(x))
  stopifnot("raw.text" %in% names(x))
  stopifnot(is.character(x$raw.text))

  ## Grab all names: these are always preceding the first '#' in the raw text
  ## Take care to remove leave information (that is the "*"s and "+"s)
  ## Also, remove any leading/trailing white spaces

  full.name <- stringr::str_replace_all(x$raw.text, "\\*|\\+", "")
  full.name <- stringr::str_split(full.name, "#", simplify = TRUE)[ ,1] %>% stringr::str_trim()
  names <- stringr::str_split(full.name, " ")

  ## For faculty, middle names (if present) seem to be collapsed into a single alphabet after the first name.
  ## We will assume assume that the first word in the name is always the first name.
  ## All preceding words (except collapsed middle names) in the name will be the last name.

  x$first.name <- names %>% purrr::map_chr(1)
  x$last.name  <- names %>% purrr::map_chr(fetch_lastname)

  if(complete){
    x$full.name <- full.name
  }

  ## Ought to do more robust error-checking, but this is not bad.

  stopifnot(all(x$first.name != ""))

  ## Take note of " Ju-Yu, Scarlett Jang Professor of Art" edge case. She doesn't seem
  ## to have a lastname by our definition. Will add back following error check once we
  ## decide what to do with this edge case.

  # stopifnot(all(x$last.name != ""))

  x
}

## Notice: mapping names to x$last.name is slightly more complicated than mapping names to x$first.name.
## We can no longer just use indexing, as we need to deal with the middle name. Instead, we map using
## a function.

## I have included this function here, instead of giving it its seperate file, because we use it locally
## for purposes of add_faculty_names, and it has no relevance whatsoever to other functionality.

#' @title Fetch Lastname
#' @description Given a list of words in a name, this function will find the last name
#'              (defined as all words in the name after the first, not including collapsed middlenames if any).
#' @param name list of words in a name
#' @return last name (as defined above)
fetch_lastname <- function(name) {
  ## Dealing with the case where no lastname is present ("Ju-Yu")
  if(length(name) < 2) { return ("")}

  ## If a collapsed middlename is present, ignore it.
  if(substr(name[2], 2, 2) == '.'){
    return(paste(name[3:length(name)], collapse = " "))
  }

  ## else return everything but the firstname
  return(paste(name[2:length(name)], collapse = " "))
}
