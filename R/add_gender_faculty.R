#' @title Add Gender
#'
#' @description This function takes as input a data frame which includes the
#'   \code{first.name} and \code{birth.year} for each row. It returns that data
#'   frame along with new column \code{gender}, and other supporting
#'   information.
#'
#' @param x data frame with \code{birth.year}, \code{first.name} and
#'   \code{middle.name} columns.
#'
#' @return the input data frame along with new column(s).
#'
#' @format
#' \describe{
#'   \item{gender}{gender as determined by the \code{gender} package.}
#'   }
#'
#' @importFrom dplyr %>%
#'
#' @export
add_gender_faculty <- function(x){

  stopifnot(is.data.frame(x))
  stopifnot(all(c("birth.year", "first.name") %in% names(x)))
  stopifnot(is.character(x$first.name))
  stopifnot(is.numeric(x$birth.year))

  ## For some rows (esp. in faculty dataset) we don't know the exact birth year.
  ## We will stick to our earlier approach of using the average birth year here.
  
  index <- is.na(x$birth.year)
  x$birth.year[index] <- round(mean(x$birth.year, na.rm = TRUE))

  ## We need the genderdata package (not just the gender package) to run the
  ## gender command. However, requiring this package in the DESCRIPTION causes
  ## problems for CRAN (since genderdata is not on CRAN), so one needs to
  ## install by hand to run the below commands.

  z <- gender::gender_df(x, name_col = "first.name", year_col = "birth.year")

  z <- z %>%
    dplyr::select(name, year_min, gender) %>%
    dplyr::rename(birth.year = year_min, first.name = name,
                  gender = gender)


  x <- dplyr::left_join(x, z,  by = c("birth.year", "first.name"))


  ## But this still leaves us with hundreds of NAs for gender, overwhelmingly
  ## caused by non-English names. The genderizeR package provides much more
  ## global coverage than the gender package, but it uses a paid service that
  ## limits free access. So, we used it once and then saved the results.

  ## The genderizeR.faculty object includes first names for
  ## faculty for which we can not determine a gender above.

  x <- dplyr::left_join(x, genderizeR.faculty,
                        by  = c("first.name" = "name"))

  x$gender <- ifelse(is.na(x$gender), x$g.gender, x$gender)

  ## Now, clean up the data set
  ## (1) Make NAs for faculty whose birth.year was initially unknown
  
  x$birth.year[index] <- NA

  ## (2) Remove g.gender column
  
  x$g.gender <- NULL

  x
}
