#' @title Create Data Frame of Graduates Data
#'
#' @description This function compiles information for all graduates and returns a
#'       dataframe. Details on this process can be found in the vignette.
#'
#' @param complete logical to indicate how many variables to return in the data frame.
#'  Default is FALSE.
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
#'     \item{honor.2}{details of distinction of secondary thesis honor (if any)
#'                   We define the "secondary" thesis honor as the second honor
#'                  appearing alongside grad's name}
#'     \item{major.2}{major in which graduate completed secondary honor}
#'     \item{raw.text}{graduate details as appearing in course catalog}
#'     \item{gender}{male or female, based on the \code{gender} package}
#'     }
#' @export

create_graduates <- function(complete = FALSE){

  x <- gather_graduates()
  x <- add_graduate_names(x)
  x <- add_graduate_honors(x, complete = complete)

  if(! complete){
    x$raw.text <- NULL
  }

  ## Dealing with gender and ethnicity is much trickier. We need the genderdata
  ## package to run the gender command. I am not sure how to handle this in
  ## DESCRIPTION. This is a bit messy but not a bad way to start.

  x$birth.year <- x$year - 22

  z <- gender::gender_df(x, name_col = "first.name", year_col = "birth.year")
  z <- z %>%
    dplyr::select(name, year_min, gender) %>%
    rename(year = year_min, first.name = name) %>%
    mutate(year = year + 22)

  ## For now, we are only keep a simple gender. We might play with the
  ## probabilities at some point. Biggest problem is the more than 8% of names
  ## for which gender is NA. We should both explore better ways to deal with
  ## foreign names and see about using middle names.

  x <- left_join(x, z)

  x$birth.year <- NULL

  ## Deal with ethnicity




  ## Could have lots of error checking. Start with:

  stopifnot(all(unique(x$year) > 2000 & unique(x$year) < 2020))
  stopifnot(all(table(x$year) > 500 & table(x$year) < 600))

  stopifnot(sum(!is.na(x$major))   == sum(!is.na(x$honor)))

  stopifnot(length(unique(x$latin.honors)) == 4)
  stopifnot(length(unique(x$honor)) == 3)

  x
}
