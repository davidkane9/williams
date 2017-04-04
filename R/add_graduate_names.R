#' @title Add Graduate Names
#'
#' @description This function takes as input a data frame which includes the raw
#'   text associated with each graduate. It returns that data frame along with
#'   two new columns: first.name and last.name. We assume that both the first
#'   name and the last name consist of a single string. That is, even if
#'   someone's true last name is González del Riego, we record the last name as
#'   Riego. If \code{complete} is FALSE, it also returns full.name.
#'
#' @param x data frame with raw.text column
#' @param complete logical to indicate how many variables to add to x. Default is FALSE.
#'
#' @return the input data frame along with two new columns.
#'
#' @format \describe{ \item{first.name}{Graduate's first name.}#'
#'   \item{last.name}{Graduate's last name.} }
#' @export

add_graduate_names <- function(x, complete = FALSE){

  stopifnot(is.data.frame(x))
  stopifnot("raw.text" %in% names(x))
  stopifnot(is.character(x$raw.text))

  ## Grab all the names --- which are always before the first comma, if there is
  ## one. Then split the names by spaces into a list of lists, after getting rid
  ## of the */+ that signify Phi Beta Kappa and Sigma Xi.

  full.name <- stringr::str_replace_all(x$raw.text, "\\*|\\+", "")
  full.name <- stringr::str_split(full.name, ",", simplify = TRUE)[ ,1]
  names <- stringr::str_split(full.name, " ")

  ## Big problem is that, although the vast majority of people have 3 names,
  ## hundreds have just 2 or 4 and a handful have 5, e.g., Alfonso Rodrigo
  ## González del Riego! One (so far!) even has 6: Chloë Iambe Naomi Illyria
  ## Feldman Emison. purrr seems the perfect approach. Trickest part is dealing
  ## with last names that (obviously?) include more than one word, like González
  ## del Riego.

  ## First name is easy. And so is last name, if you (mistakenly!) assume that
  ## last names are always length 1. For now, we will not worry about middle
  ## names, although they might be helpful in gender identifications. We also
  ## ignore complications associated with last names that are of length more
  ## than one.

  x$first.name <- names %>% map_chr(1)
  x$last.name  <- names %>% map_chr(tail, 1)

  if(complete){
    x$full.name <- full.name
  }

  ## Ought to do more robust error-checking, but this is not bad.

  stopifnot(all(x$first.name != ""))
  stopifnot(all(x$last.name != ""))

  x
}
