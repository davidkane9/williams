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
  
  ## Add some hacks for specific people. Probably should separate this out into
  ## separate functions for graduates and faculty. 
  
  ## Students
  
  x$race[x$last.name == "Hall" & x$first.name == "Todd" & x$year == 2016] <- "Black"
  x$race[x$last.name == "Wosen" & x$first.name == "Jonathan" & x$year == 2013] <- "Black"
  x$race[x$last.name == "Martin" & x$first.name == "Naya-Joi" & x$year == 2009] <- "Black"
  
  x$race[x$last.name == "Mackall" & x$first.name == "Blake" & x$year == 2016] <- "White"
  x$race[x$last.name == "Whidbee" & x$first.name == "Paige" & x$year == 2015] <- "White"
  x$race[x$last.name == "Frett" & x$first.name == "Barry" & x$year == 2012] <- "White"
  x$race[x$last.name == "Jackson" & x$first.name == "Steven" & x$year == 2010] <- "White"
  x$race[x$last.name == "Blackshear" & x$first.name == "Chloe" & x$year == 2010] <- "White"
  x$race[x$last.name == "Williams" & x$first.name == "Erika" & x$year == 2008] <- "White"
  x$race[x$last.name == "Williams" & x$first.name == "Martin" & x$year == 2007] <- "White"
  x$race[x$last.name == "Grier" & x$first.name == "Alexandra" & x$year == 2006] <- "White"
  x$race[x$last.name == "Williams" & x$first.name == "Catherine" & x$year == 2000] <- "White"
  x$race[x$last.name == "Trice" & x$first.name == "Laura" & x$year == 2000] <- "White"
  
  ## Faculty. I assume this will work even when we start processing all the years again.
  
  x$race[x$last.name == "De Veaux" & x$first.name == "Richard"] <- "White"
  
  x$race[x$last.name == "Foias" & x$first.name == "Antonia"] <- "Hispanic"
  x$race[x$last.name == "Fox" & x$first.name == "Soledad"] <- "Hispanic"
  x$race[x$last.name == "Whalen" & x$first.name == "Carmen"] <- "Hispanic"
  
  x$race[x$last.name == "Ali" & x$first.name == "Laylah"] <- "Black"
  x$race[x$last.name == "James" & x$first.name == "Joy"] <- "Black"
  x$race[x$last.name == "Munemo" & x$first.name == "Ngonidzashe"] <- "Black"
  
  ## Note the difficulty of the two David Smiths on the faculty. We want to use
  ## "department to identify the one we want but that variable does not exist in
  ## the student data frame. For now, we ignore.
  
  ## x$race[x$last.name == "Smith" & x$first.name == "David" & department == "English"] <- "Black"
  
  x
}
