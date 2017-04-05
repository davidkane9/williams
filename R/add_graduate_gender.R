#' @title Add Graduate Gender
#'
#' @description This function takes as input a data frame which includes the
#'   \code{first.name} of each graduate. It returns that data frame along with a
#'   new column: \code{gender}.
#'
#' @param x data frame with \code{first.name} column
#' @param complete logical to indicate how many variables to add to x. Default
#'   is FALSE. If TRUE, other diagnostic variables are also added.
#'
#' @return the input data frame along with new column(s).
#'
#' @format \describe{
#'   \item{gender}{Graduate's gender as determined by the gender package.}
#'   }
#'
#' @export

add_graduate_gender <- function(x, complete = FALSE){

  stopifnot(is.data.frame(x))
  stopifnot(all(c("year", "first.name") %in% names(x)))
  stopifnot(is.character(x$first.name))

  ## We need the genderdata package (not just the gender package) to run the
  ## gender command. I am not sure how to handle this in DESCRIPTION.

  x$birth.year <- x$year - 22

  z <- gender::gender_df(x, name_col = "first.name", year_col = "birth.year")
  z <- z %>%
    dplyr::select(name, year_min, proportion_male, proportion_female, gender) %>%
    rename(year = year_min, first.name = name) %>%
    mutate(year = year + 22)

  x <- left_join(x, z) %>% select(-birth.year)

  ## Keep only gender unless complete = TRUE.

  if(! complete){
    x <- x %>% select(-proportion_male, -proportion_female)
  }

  x
}
