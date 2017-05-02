#' @title Gather Faculty
#'
#' @description This function brings together the information on all Williams
#'   faculty for the years that we have downloaded the data in the package. It
#'   also adds information about faculty on leave.
#'
#' @return a dataframe for the specified years with a row for each faculty
#'   member and six variables.
#'
#' @format \describe{
#'   \item{year}{year}
#'   \item{raw.text}{Raw text from the Course Catlog associated with each faculty member}
#'   \item{leave}{Faculty leave information; one of "Academic Year", "First Semester", "Second Semester", "Calendar Year", or "None"}
#'   }
#' @export





gather_faculty <- function(){

  x <- tibble::data_frame()

  ## If we decide to provide optional year argument in gather_graduates, we should add it here as well.

  files <- list.files(paste0(system.file(package = "williamsmetrics"), "/extdata"), pattern = "faculty")

  for(i in seq_along(files)){
    year <- as.numeric(stringr::str_sub(files[i], 9, 12))
    name <- paste0("extdata/faculty-", year, "-", (year + 1), ".txt", sep = "")


    ## read in raw text from file, and add as column to the dataframe.
    ## Also, add in column for year.
    filename <- system.file(name, package = "williamsmetrics")
    raw <- readr::read_lines(filename)

    df <- tibble::data_frame(year = rep(year, length(raw)),
                             raw.text = raw)

    ## Now figure out faculty on leave
    df$leave <- ifelse(stringr::str_count(df$raw.text, "\\*") == 1, "Academic Year",
                       ifelse(stringr::str_count(df$raw.text, "\\*") == 2, "First Semester",
                              ifelse(stringr::str_count(df$raw.text, "\\*") == 3, "Second Semester",
                                     ifelse(stringr::str_count(df$raw.text, "\\*") == 4, "Calendar Year", "None"))))

    x <- rbind(x, df)
  }
  x
}

