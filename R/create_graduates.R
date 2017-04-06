#' @title Create Data Frame of Graduates Data
#'
#' @description This function compiles information for all graduates and returns a
#'       dataframe.
#'
#' @param complete logical to indicate how many variables to return in the data frame.
#'  Default is FALSE, which returns the same information as in the \code{graduates}
#'  data frame distributed with the package.
#'
#' @return a dataframe with a row for each graduating senior and 9 associated variables.
#'
#' @format
#' \describe{
#'     \item{first.name}{First name of graduate}
#'     \item{last.name}{Last name of graduate}
#'     \item{year}{year of graduation}
#'     \item{latin.honor}{latin honor recieved by graduate (if any)}
#'     \item{honor}{details of distinction of primary thesis honor (if any)
#'                    We define the "primary" thesis honor as the first honor
#'                    appearing alongside grad's name}
#'     \item{major}{major in which graduate completed primary honor}
#'     \item{gender}{male or female, based on the \code{gender} package}
#'     }
#'
#' @export

create_graduates <- function(complete = FALSE){

  x <- gather_graduates()
  x <- add_graduate_names(x, complete = complete)
  x <- add_graduate_honors(x, complete = complete)
  x <- add_graduate_gender(x, complete = complete)
  x <- add_graduate_race(x, complete = complete)


  if(! complete){
    x$raw.text <- NULL
  }

  x <- tibble::as_tibble(x)

  ## Could have lots of error checking. Start with:

  stopifnot(all(unique(x$year) > 2000 & unique(x$year) < 2020))
  stopifnot(all(table(x$year) > 500 & table(x$year) < 600))

  stopifnot(sum(!is.na(x$major))   == sum(!is.na(x$honor)))

  stopifnot(length(unique(x$latin.honors)) == 4)
  stopifnot(length(unique(x$honor)) == 3)

  x
}
