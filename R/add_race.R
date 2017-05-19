#' @title Add Graduate Race
#'
#' @description This function takes as input a data frame which includes the
#'   \code{last.name} of each graduate. It returns that data frame along with a
#'   new column: \code{race}.
#'
#' @param x data frame with \code{last.name} column
#'
#' @return the input data frame along with new column(s).
#'
#' @format \describe{
#'   \item{p_whi}{Prosterior probability that the individual's race is White, as determined by the wru package}
#'   \item{p_bla}{Prosterior probability that the individual's race is Black, as determined by the wru package}
#'   \item{p_asi}{Prosterior probability that the individual's race is Asian, as determined by the wru package}
#'   \item{p_his}{Prosterior probability that the individual's race is Hispanic, as determined by the wru package}
#'   \item{p_oth}{Prosterior probability that the individual's race is none of the above, as determined by the wru package}
#'   \item{race}{Graduate's race as the racial category with the highest prosterior probability.}
#'   }
#'
#' @importFrom dplyr %>%
#' @importFrom wru merge_surnames
#'
#' @export

add_race <- function(x){

  stopifnot(is.data.frame(x))
  stopifnot(all(c("last.name") %in% names(x)))
  stopifnot(is.character(x$last.name))

  ## I think that the wru package is suspect and/or that merge_surnames works in
  ## weird ways. Or perhaps I don't understand the correct way to use
  ## Imports/Depends.

  x$surname <- x$last.name
  x <- wru::merge_surnames(x)

  ## Manipulation to make things nice.

  x <- x %>% tibble::as_tibble() %>%
    dplyr::select(-surname, -surname.match)

  z <- x[c("p_whi", "p_bla", "p_his", "p_asi", "p_oth")]

  x$race <- colnames(z)[max.col(z)]

  x <- x %>%
    dplyr::mutate(race = forcats::fct_recode(race,
                                             "White" = "p_whi",
                                             "Black" = "p_bla",
                                             "Hispanic" = "p_his",
                                             "Asian" = "p_asi",
                                             "Other" = "p_oth"))

  x
}
