#' @title Add Graduate Race
#'
#' @description This function takes as input a data frame which includes the
#'   \code{last.name} of each graduate. It returns that data frame along with a
#'   new column: \code{race}.
#'
#' @param x data frame with \code{last.name} column
#' @param complete logical to indicate how many variables to add to x. Default
#'   is FALSE. If TRUE, other diagnostic variables are also added.
#'
#' @return the input data frame along with new column(s).
#'
#' @format \describe{
#'   \item{race}{Graduate's race as determined by the wru package.}
#'   }
#'
#' @export

add_graduate_race <- function(x, complete = FALSE){

  stopifnot(is.data.frame(x))
  stopifnot(all(c("last.name") %in% names(x)))
  stopifnot(is.character(x$last.name))

  ## We need the wru package. I am still confused about how to handle this
  ## requirement in DESCRIPTION.

  x$surname <- x$last.name

  x <- wru::merge_surnames(x) %>%
    select(-surname, -surname.match)

  z <- x[c("p_whi", "p_bla", "p_his", "p_asi", "p_oth")]

  x$race <- colnames(z)[max.col(z)]

  x <- x %>% mutate(race = forcats::fct_recode(race,
                                    "White" = "p_whi",
                                    "Black" = "p_bla",
                                    "Hispanic" = "p_his",
                                    "Asian" = "p_asi",
                                    "Other" = "p_oth"
                                    ))

  ## Keep only race unless complete = TRUE.

  if(! complete){
    x <- x %>% select(-p_whi, -p_bla, -p_his, -p_asi, -p_oth)
  }

  x
}
