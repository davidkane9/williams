#' @title Create Data Frame of Graduates Data
#'
#' @description This function compiles information for all graduates and returns a
#'       dataframe.
#'
#' @param complete logical to indicate how many variables to return in the data frame.
#'  Default is FALSE, which returns the same information as in the \code{graduates}
#'  data frame distributed with the package. If complete = TRUE, other variables will
#'  be included, which can be helpful for diagnostics.
#'
#' @return a dataframe with a row for each graduating senior and 9 associated variables.
#'
#' @format
#' \describe{
#'     \item{year}{year of graduation}
#'     \item{first.name}{First name of graduate}
#'     \item{last.name}{Last name of graduate}
#'     \item{gender}{male or female, based on the \code{gender} package}
#'     \item{race}{White, Black, Hispanic, Asian or Other, based on \code{wru} package}
#'     \item{latin.honors}{latin honor recieved by graduate (if any)}
#'     \item{Phi.Beta.Kappa}{TRUE/FALSE indicating membership in Phi Beta Kappa}
#'     \item{Sigma.Xi}{TRUE/FALSE indicating membership in Sigma Xi}
#'     \item{honor}{details of distinction of primary thesis honor (if any)
#'                    We define the "primary" thesis honor as the first honor
#'                    appearing alongside grad's name}
#'     \item{major}{major in which graduate completed primary honor}

#'     }
#'
#' @export

create_graduates <- function(complete = FALSE){

  x <- gather_graduates()
  x <- add_graduate_names(x)
  x <- add_graduate_honors(x)
  x <- add_gender(x)
  x <- add_race(x)


  if(! complete){
    x <- x %>%
            select(year, first.name, last.name, gender, race,
                   latin.honors, Phi.Beta.Kappa, Sigma.Xi, honor, major)
  }


  ## Could have lots of error checking. Start with:

  stopifnot(all(unique(x$year) > 1999 & unique(x$year) < 2020))
  stopifnot(all(table(x$year) > 500 & table(x$year) < 600))

  stopifnot(sum(!is.na(x$major))   == sum(!is.na(x$honor)))

  stopifnot(length(unique(x$latin.honors)) == 4)
  stopifnot(length(unique(x$honor)) == 3)

  x
}
