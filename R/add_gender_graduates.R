#' @title Add Gender Graduates
#'
#' @description This function takes as input a data frame for graduates which includes the
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
#'   \item{gender}{character, gender as determined by the \code{gender} package.}
#'     \item{p_male.first}{numeric, probability that the individual is a male given their first name, as determined by the
#'     \code{gender} packege}
#'     \item{p_female.first}{numeric, probability that the individual is a female given their first name, as determined by the
#'     \code{gender} packege}
#'     \item{gender.first}{character,gender of an individual given their first name, as determined by the  \code{gender} packege}
#'     \item{p_male.middle}{numeric, probability that the individual is a male given their middle name, as determined by the
#'     \code{gender} packege}
#'     \item{p_female.middle}{numeric, probability that the individual is a female given their middle name, as determined by the
#'     \code{gender} packege}
#'     \item{gender.middle}{character,gender of an individual given their middle name, as determined by the  \code{gender} packege}
#'     \item{gender.g}{character, gender of an individual, as determined by \code{genderizeR} package}
#'   }
#'
#' @importFrom dplyr %>%
#'
#' @export

add_gender_graduates <- function(x){

  stopifnot(is.data.frame(x))
  stopifnot(all(c("birth.year", "first.name", "middle.name") %in% names(x)))
  stopifnot(is.character(x$first.name) & is.character(x$middle.name))
  stopifnot(is.numeric(x$birth.year))

  ## For some rows (esp. in faculty dataset) we don't know the exact birth year.
  ## We will use the mean of the dataset for these rows. I don't like this
  ## approach. Can't we just pass in an NA value for birth year to the gender
  ## package? Apparently not. We could pass in a year range, but that is too
  ## much of a bother for now.

  x$birth.year[is.na(x$birth.year)] <- mean(x$birth.year, na.rm = TRUE)

  ## We need the genderdata package (not just the gender package) to run the
  ## gender command. We do this twice: for first names and for middle names.

  z.first <- gender::gender_df(x, name_col = "first.name", year_col = "birth.year")

  z.middle <- gender::gender_df(x, name_col = "middle.name", year_col = "birth.year")

  ## The problem is that this only returns data for the

  z.first <- z.first %>%
    dplyr::select(name, year_min, proportion_male, proportion_female, gender) %>%
    dplyr::rename(birth.year = year_min, first.name = name,
                  gender.first = gender,
                  p_male.first = proportion_male,
                  p_female.first = proportion_female)

  z.middle <- z.middle %>%
    dplyr::select(name, year_min, proportion_male, proportion_female, gender) %>%
    dplyr::rename(birth.year = year_min, middle.name = name,
                  gender.middle = gender,
                  p_male.middle = proportion_male,
                  p_female.middle = proportion_female)

  x <- dplyr::left_join(x, z.first,  by = c("birth.year", "first.name"))
  x <- dplyr::left_join(x, z.middle, by = c("birth.year", "middle.name"))

  ## There is a lot of interesting data here that we might want to play with.
  ## For now, we keep it simple. Inspection suggests that we should use the
  ## gender from the first name if it is available. If it is not, we use gender
  ## from the middle name.

  x$gender <- x$gender.first
  x$gender <- ifelse(is.na(x$gender), x$gender.middle, x$gender)

  ## But this still leaves us with hundreds of NAs for gender, overwhelmingly
  ## caused by non-English names. The genderizeR package provides much more
  ## global coverage than the gender package, but it uses a paid service that
  ## limits free access. So, we used it once and then saved the results.

  ## The genderizeR.graduates object includes all the first and middle names for
  ## graduates for which we can not determine a gender above.

  x <- dplyr::left_join(x, genderizeR.graduates,
                        by  = c("first.name" = "name"))

  x$gender <- ifelse(is.na(x$gender), x$gender.g, x$gender)

  ## Remove birth year, which is a variable we no longer need.

  x$birth.year <- NULL

  x
}
