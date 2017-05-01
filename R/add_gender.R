#' @title Add Gender
#'
#' @description This function takes as input a data frame which includes the
#'   \code{first.name} and \code{birth.year} for each row. It returns that data frame along with a
#'   new column: \code{gender}.
#'
#' @param x data frame with \code{first.name} column
#'
#' @return the input data frame along with new column(s).
#'
#' @format \describe{
#'   \item{gender}{gender as determined by the gender package.}
#'   }
#'
#' @importFrom dplyr %>%
#'
#' @export

add_gender <- function(x){

  stopifnot(is.data.frame(x))
  stopifnot(all(c("birth.year", "first.name") %in% names(x)))
  stopifnot(is.character(x$first.name))
  stopifnot(is.numeric(x$birth.year))

  ## For some rows (esp. in faculty dataset) we don't know the exact birth year.
  ## We will use the mean of the dataset for these rows.

  index <- which(is.na(x$birth.year))
  x$birth.year[index] <- mean(x$birth.year, na.rm = TRUE)

  ## We need the genderdata package (not just the gender package) to run the
  ## gender command. I am not sure how to handle this in DESCRIPTION.

  z <- gender::gender_df(x, name_col = "first.name", year_col = "birth.year")
  z <- z %>%
    dplyr::select(name, year_min, proportion_male, proportion_female, gender) %>%
    dplyr::rename(birth.year = year_min, first.name = name)

  x <- dplyr::left_join(x, z)

  # Remove artificial birth years.

  x$birth.year[index] <- NA

  x
}
