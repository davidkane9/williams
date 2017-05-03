#' @title Add Graduate Names
#'
#' @description This function takes as input a data frame which includes the raw
#'   text associated with each graduate. It returns that data frame along with
#'   four new columns: first.name, middle.name, last.name and full.name. We assume
#'   that the first/middle/last names consist of a single string. That is, even if
#'   someone's true last name is González del Riego, we record the last name as
#'   Riego.
#'
#' @param x data frame with raw.text column
#'
#' @return the input data frame along with three new columns.
#'
#' @format \describe{
#'  \item{first.name}{Graduate's first name.}
#'  \item{middle.name}{Graduate's middle name. Defined as the second separate string
#'        in full.name, if there are more than two names total.}
#'  \item{last.name}{Graduate's last name.}
#'  \item{full.name}{Graduate's entire name.}
#'  }
#'
#' @importFrom dplyr %>%
#'
#' @export

add_graduate_names <- function(x){

  stopifnot(is.data.frame(x))
  stopifnot("raw.text" %in% names(x))
  stopifnot(is.character(x$raw.text))

  ## Grab all the names --- which are always before the first comma, if there is
  ## one. Then split the names by spaces into a list of lists, after getting rid
  ## of the */+ that signify Phi Beta Kappa and Sigma Xi. names is a a list of
  ## lists.

  full.name <- stringr::str_replace_all(x$raw.text, "\\*|\\+", "")
  full.name <- stringr::str_split(full.name, ",", simplify = TRUE)[ ,1]
  names <- stringr::str_split(full.name, " ")

  ## Big problem is that, although the vast majority of people have 3 names,
  ## hundreds have just 2 or 4 and a handful have 5, e.g., Alfonso Rodrigo
  ## González del Riego! One (so far!) even has 6: Chloë Iambe Naomi Illyria
  ## Feldman Emison. purrr seems the perfect approach. Trickest part is dealing
  ## with last names that (obviously?) include more than one word, like González
  ## del Riego.

  ## In 2016 (for the first time?!), the College began including suffixes like
  ## III or Jr. We need to delete these, otherwise we will deem them the last
  ## name of a graduate. Must be a better way! Rows 8406 and 8584 are examples.

  for(i in seq_along(names)){
    for(j in seq_along(names[[i]])){
      names[[i]][[j]] <- ifelse(names[[i]][[j]] %in% c("III", "Jr."), NA, names[[i]][[j]])
    }
  }

  names <- lapply(names, function(x) x[! is.na(x)])

  ## First name is easy.

  x$first.name <- names %>% purrr::map_chr(1)

  ## And so is last name, if you (mistakenly!) assume that last names are always
  ## length 1. For now, ignore that complication.

  x$last.name  <- names %>% purrr::map_chr(utils::tail, 1)

  ## Middle name is the second name. If there is no third name --- i.e., the
  ## person has only two names like Vu Le --- we NA middle.name. Must be a more
  ## elegant way to code this up.

  x$middle.name <- names %>% purrr::map_chr(2)
  x$middle.name <- ifelse(x$middle.name == x$last.name, NA, x$middle.name)


  x$full.name <- full.name

  ## Ought to do more robust error-checking, but this is not bad.

  stopifnot(all(x$first.name != ""))
  stopifnot(all(x$last.name  != ""))
  stopifnot(all(x$full.name  != ""))

  x
}
