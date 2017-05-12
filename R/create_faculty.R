#' @title Create Data Frame of Faculty Data
#'
#' @description This function compiles information for all graduates and returns a
#'       dataframe.
#'
#' @param complete logical to indicate how many variables to return in the data frame.
#'  Default is FALSE, which returns the same information as in the \code{graduates}
#'  data frame distributed with the package.
#'
#' @return a dataframe with a row for each graduating senior and 16 associated variables.
#'
#' @format
#' \describe{
#'     \item{first.name}{First name of faculty}
#'     \item{last.name}{Last name of faculty}
#'     \item{year}{catalog year from which information is extracted}
#'     \item{birth.year}{inferred birth year for faculty member}
#'     \item{leave.full.year}{Is faculty on leave for full academic year?}
#'     \item{leave.first.sem}{Is faculty on leave for only first semester?}
#'     \item{leave.first.sem}{Is faculty on leave for only second semester?}
#'     \item{leave.first.sem}{Is faculty on leave for full calendar year?}
#'     \item{first.degree}{First degree held by faculty; This is generally a bachelors degree}
#'     \item{first.degree.year}{Year the first degree was conferred}
#'     \item{first.degree.school}{School that conferred first degree}
#'     \item{last.degree}{First degree held by faculty; This is generally the highest degree held by the faculty.}
#'     \item{last.degree.year}{Year the last degree was conferred}
#'     \item{last.degree.school}{School that conferred last degree.}
#'     \item{title}{title of faculty}
#'     }
#'
#' @examples
#'
#' \donttest{
#'    x <- create_faculty()
#' }
#'
#' @export

create_faculty <- function(complete = FALSE){

  x <- gather_faculty()
  x <- add_faculty_names(x)
  x <- add_faculty_degrees(x)
  x$birth.year <- x$first.degree.year - 22
  x <- add_faculty_titles(x)
  x <- add_gender_faculty(x)
  x <- add_faculty_department(x)
  ## But, for the 2015-2016 catalog, titles are weird: we are provided with departments, instead of a title.
  ## Now that we have added the department column, let's make title, rank, and status for all faculty from 2015 because
  ## we have no information on these.
  ## Admittedly, this is a stupid hack.. But it is the only way around I can think of without major refactoring
  x$title[which(x$year == 2015)] <- NA
  x$rank[which(x$year == 2015)] <- NA
  x$status[which(x$year == 2015)] <- NA

  x <- add_race(x)


  if(! complete){
    x <- x %>%
              dplyr::select(year, first.name, last.name, title, department, rank, status, leave, birth.year,
                     first.degree, first.degree.school, first.degree.year, last.degree,
                     last.degree.school, last.degree.year, gender, race)
  }

  x <- tibble::as_tibble(x)

  ## Some initial error checking:
  stopifnot(all(unique(x$year) >= 2000 & unique(x$year) < 2020))
  stopifnot(all(table(x$year) > 300 & table(x$year) < 500))

  x
}
