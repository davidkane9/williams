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
#' @importFrom dplyr %>%
#'
#' @export

add_graduate_race <- function(x, complete = FALSE){

  stopifnot(is.data.frame(x))
  stopifnot(all(c("last.name") %in% names(x)))
  stopifnot(is.character(x$last.name))

  ## I think that the wru package is suspect and/or that merge_surnames works in
  ## weird ways. Or perhaps I don't understand the correct way to use
  ## Imports/Depends.

  x$surname <- x$last.name
  x <- wru::merge_surnames(x)

  ## Manipulation to make things nice.


  x <- x %>%  dplyr::select(-surname, -surname.match)

  z <- x[c("p_whi", "p_bla", "p_his", "p_asi", "p_oth")]

  x$race <- colnames(z)[max.col(z)]

  x <- x %>% dplyr::mutate(race = forcats::fct_recode(race,
                                    "White" = "p_whi",
                                    "Black" = "p_bla",
                                    "Hispanic" = "p_his",
                                    "Asian" = "p_asi",
                                    "Other" = "p_oth"
                                    ))

  ## Keep only race unless complete = TRUE.

  if(! complete){
    x <- x %>% dplyr::select(-p_whi, -p_bla, -p_his, -p_asi, -p_oth)
  }

  x
}
