#' @title Add Graduate Honors
#'
#' @description This function takes as input a data frame which includes the raw
#'   text associated with each graduate. It returns that data frame along with
#'   six new columns associated with department honors.
#'
#' @param x data frame with raw.text column
#' @param complete logical to indicate how many variables to add to x. Default is FALSE.
#'
#' @return the input data frame along with four new columns.
#'
#' @format \describe{
#'     \item{honor.1}{details of distinction of primary thesis honor (if any)
#'                    We define the "primary" thesis honor as the first honor
#'                    appearing alongside grad's name}
#'     \item{major.1}{major in which graduate completed primary honor}
#'     \item{honor.2}{details of distinction of secondary thesis honor (if any)
#'                   We define the "secondary" thesis honor as the second honor
#'                  appearing alongside grad's name}
#'     \item{major.2}{major in which graduate completed secondary honor}
#'     \item{major}{Simplified major with the highest honor}
#'     \item{honor}{Highest honor received, if any}
#'   }
#'
#' @export

add_graduate_honors <- function(x, complete = FALSE){

  stopifnot(is.data.frame(x))
  stopifnot("raw.text" %in% names(x))
  stopifnot(is.character(x$raw.text))

  ## Grab both possible honors  --- which are always after the first comma, if
  ## there is one. Never figured out a cool way to deal with the comma in the
  ## two versions of WGES in the data, so just replace the whole thing at the
  ## start.

  raw <- stringr::str_replace_all(x$raw.text, "Women's, Gender and Sexuality Studies|Women's and Gender Studies", "WGES")

  raw <- stringr::str_split(raw, ",", simplify = TRUE)

  raw[raw == ""] <- NA

  ## Create the individual raw honors and majors

  h.1 <- stringr::str_replace_all(raw[ ,2],  " with | in", "")
  h.2 <- stringr::str_replace_all(raw[ ,3],  " with | in", "")

  honor.1 <- stringr::str_match(h.1, "^.*honors")[ ,1]  ## Recall that str_match returns a matrix.
  honor.2 <- stringr::str_match(h.2, "^.*honors")[ ,1]

  major.1 <- stringr::str_replace(h.1, "^.*honors ", "")
  major.2 <- stringr::str_replace(h.2, "^.*honors ", "")

  ## Now that we have the raw information, we want to simplify so that the user
  ## can just consider one major and one honor, which is the most that most
  ## people want.

  ## For honors, we are just going to keep the first one. This is also the
  ## highest one in all but two cases.

  x$honor <- honor.1

  ## For majors, we are also just going to use the first major. But, we are
  ## going to standardize it across years and do some simplifications.

  x$major <- major.1

  ## All types of Contract Majors become just Contract.

  x$major[stringr::str_detect(x$major, "Contract")] <- "Contract"
  x$major[stringr::str_detect(x$major, "Art and Computer Science")] <- "Contract" ## Probably a typo in the catalog.
  x$major[stringr::str_detect(x$major, "Africa|Afro")] <- "Africana"
  x$major[stringr::str_detect(x$major, "Enviro")] <- "Environmental"

  ## Tricky case involves Women’s, Gender and Sexuality Studies,
  ## as a major, because it includes a comma. However, there are no WGES honor
  ## students who also get honors in another major. But should we really just
  ## assume that? For now, let's just hack:


  if(complete){
    x$honor.1 <- honor.1
    x$honor.2 <- honor.2
    x$major.1 <- major.1
    x$major.2 <- major.2
  }

  x
}
