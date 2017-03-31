#' @title Add Graduate Honors
#'
#' @description This function takes as input a data frame which includes the raw
#'   text associated with each graduate. It returns that data frame along with
#'   four new columns associated with department honors.
#'
#' @param x data frame with raw.text column
#'
#' @return the input data frame along with four new columns.
#'
#' @format \describe{
#'     \item{honor}{details of distinction of primary thesis honor (if any)
#'                    We define the "primary" thesis honor as the first honor
#'                    appearing alongside grad's name}
#'     \item{major}{major in which graduate completed primary honor}
#'     \item{honor.2}{details of distinction of secondary thesis honor (if any)
#'                   We define the "secondary" thesis honor as the second honor
#'                  appearing alongside grad's name}
#'     \item{major.2}{major in which graduate completed secondary honor}
#'   }
#' @export

add_graduate_honors <- function(x){

  stopifnot(is.data.frame(x))
  stopifnot("raw.text" %in% names(x))
  stopifnot(is.character(x$raw.text))

  ## Grab both possible honors  --- which are always after the first comma, if
  ## there is one. Tricky case involves Women’s, Gender and Sexuality Studies,
  ## as a major, because it includes a comma. However, there are no WGES honor
  ## students who also get honors in another major. But should we really just
  ## assume that? For now, let's just hack:

  raw <- str_replace_all(x$raw.text, "Women's, Gender and Sexuality Studies|Women's and Gender Studies", "WGES")

  raw <- stringr::str_split(raw, ",", simplify = TRUE)

  raw[raw == ""] <- NA

  ## Ought to be a way to handle the two honors without so much code repetition.

  honor.1 <- str_replace_all(raw[ ,2],  " with | in", "")
  honor.2 <- str_replace_all(raw[ ,3],  " with | in", "")

  x$honor   <- str_match(honor.1, "^.*honors")
  x$honor.2 <- str_match(honor.2, "^.*honors")

  x$major   <- str_replace(honor.1, "^.*honors", "")
  x$major.2 <- str_replace(honor.2, "^.*honors", "")

  x
}
