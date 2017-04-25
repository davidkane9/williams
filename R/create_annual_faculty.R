#' @title Create Current Faculty
#' @description This function accepts the faculty.detail data frame, and returns information about the college's faculty for
#'              the current year (the last year for which information is available).
#' @param x the faculty.detail data set with year, leave, first.name, last.name, first.degree, first.degree.year, first.degree.school,
#'                                           last.degree, last.degree.year, last.degree.school, birth.year, title, rank, status, department
#'                                           gender, and race columns
#' @return data set for faculty information for the last year
#'
#' @importFrom dplyr %>%
#'
#' @export
create_annual_faculty <- function(x) {
  ## Make sure that x has required columns in the required format
  stopifnot(is.data.frame(x))
  stopifnot("year" %in% names(x))
  stopifnot("leave" %in% names(x))
  stopifnot("first.name" %in% names(x))
  stopifnot("last.name" %in% names(x))
  stopifnot("first.degree" %in% names(x))
  stopifnot("first.degree.year" %in% names(x))
  stopifnot("first.degree.school" %in% names(x))
  stopifnot("last.degree" %in% names(x))
  stopifnot("last.degree.year" %in% names(x))
  stopifnot("last.degree.school" %in% names(x))
  stopifnot("birth.year" %in% names(x))
  stopifnot("department" %in% names(x))
  stopifnot("title" %in% names(x))
  stopifnot("rank" %in% names(x))
  stopifnot("status" %in% names(x))
  stopifnot("gender" %in% names(x))
  stopifnot(is.character(x$first.name))
  stopifnot(is.character(x$last.name))
  stopifnot(is.character(x$department))
  stopifnot(is.numeric(x$year))
  stopifnot(is.character(x$leave))
  stopifnot(is.character(x$first.degree))
  stopifnot(is.character(x$first.degree.school))
  stopifnot(is.character(x$last.degree))
  stopifnot(is.character(x$last.degree.school))
  stopifnot(is.numeric(x$first.degree.year))
  stopifnot(is.numeric(x$last.degree.year))
  stopifnot(is.character(x$title))
  stopifnot(is.character(x$rank))
  stopifnot(is.character(x$gender))
  ## Add once race stuff is fixed:

  # stopifnot(is.character(x$race))
  # stopifnot("race" %in% names(x))

  ## We need to fetch the rows of the current year
  this_year <- max(x$year)
  df <- dplyr::filter(x, year == this_year)

  ## Add additional columns
  df$start.year <- NA

  ## Now, some columns (like first.name, last.name, etc) are good as they are. However, we might be missing some information from
  ## previous year, which we would like to fill in.
  for(i in 1:nrow(df)){
    first.name <- df$first.name[i]
    last.name <- df$last.name[i]
    df$start.year[i] <- min(x$year[which(x$first.name == first.name & x$last.name == last.name)])

    ## Fetch information that might be missing for current year, but present in previous years
    df$first.degree[i] <- fetch_missing_information(first.name = first.name, last.name = last.name,
                                                    x = x, column = "first.degree")
    df$first.degree.year[i] <- fetch_missing_information(first.name = first.name, last.name = last.name,
                                                         x = x, column = "first.degree.year")
    df$first.degree.school[i] <- fetch_missing_information(first.name = first.name, last.name = last.name,
                                                    x = x, column = "first.degree.school")
    df$last.degree[i] <- fetch_missing_information(first.name = first.name, last.name = last.name,
                                                    x = x, column = "last.degree")
    df$last.degree.year[i] <- fetch_missing_information(first.name = first.name, last.name = last.name,
                                                         x = x, column = "last.degree.year")
    df$last.degree.school[i] <- fetch_missing_information(first.name = first.name, last.name = last.name,
                                                           x = x, column = "last.degree.school")
    df$first.degree[i] <- fetch_missing_information(first.name = first.name, last.name = last.name,
                                                    x = x, column = "first.degree")
    df$birth.year[i] <- fetch_missing_information(first.name = first.name, last.name = last.name,
                                                  x = x, column = "birth.year")

  }

  df
}
