#' @title Gather Graduates
#'
#' @description This function brings together the information on all Williams
#'   graduates for the years that we have downloaded the data in the package. It
#'   also adds Latin honors information.
#'
#' @return a dataframe for the specified years with a row for each graduating
#'   senior and five variables.
#'
#' @format \describe{
#'   \item{year}{Graduation year}
#'   \item{raw.text}{Raw text from the Course Catlog associated with each graduate}
#'   \item{latin.honors}{Latin honors}
#'   \item{Phi.Beta.Kappa}{Boolean value indicating membership in Phi Beta Kappa}
#'   \item{Sigma.Xi}{Boolean value indicating membership in Sigma Xi}
#'   }
#' @export

gather_graduates <- function(){

  x <- data.frame(NULL)

  ## Maybe provide an optional year argument that could be used to get just one file?

  files <- list.files(paste0(system.file(package = "williamsmetrics"), "/extdata"), pattern = "graduates")

  for(i in seq_along(files)){
    year <- as.numeric(stringr::str_sub(files[i], 16, 19))
    name <- paste0("extdata/graduates-", (year - 1), "-", year, ".txt", sep = "")
    filename <- system.file(name, package = "williamsmetrics")
    raw <- readr::read_lines(filename)

    clean <- raw[! stringr::str_detect(raw, "Bachelor of Arts")]

    df <- tibble::data_frame(year = rep(year, length(clean)),
                             raw.text = clean)

    ## Dealing with the category lines is the main annoyance. First, we grab the
    ## raw text and then assign the Latin Honors. Need to do this before you
    ## change the order of things. Might add error checking which takes advantage
    ## of the alphabetical listing of last names within honors categories.

    br <- which(stringr::str_detect(raw, "Bachelor of Arts"))

    df$latin.honors <- c(rep("Summa Cum Laude", br[2] - br[1] - 1),
                   rep("Magna Cum Laude", br[3] - br[2] - 1),
                   rep("Cum Laude", br[4] - br[3] - 1),
                   rep(NA, length(raw) - br[4]))

    ## Also figure out Phi Beta Kappa and Sigma Xi.

    df$Phi.Beta.Kappa <- stringr::str_detect(df$raw.text, "\\*")
    df$Sigma.Xi <- stringr::str_detect(df$raw.text, "\\+")

    x <- rbind(x, df)
  }

  x
}
