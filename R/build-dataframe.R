#' @title Build Dataframe for Given Year
#' @description The function loads in the graduate information for the
#'              given year, scrapes appropriate information, & organizes
#'              it into a dataframe.
#'
#'              The function first scrapes information about students with different latin honors
#'              ("Suma", "Magna", "Cum", and "None") as different lists using appropriate scraping
#'              functions from "extract-from-source.R".
#'
#'              Then, from each entry in these lists, we extract information about the students', their
#'              latin honors, and their theses and maintain them as corresponding lists. Finally, we
#'              organize the data into a dataframe in the required format.
#'
#' @param year year to build data frame for <YYYY>
#' @return dataframe with # rows = # graduates for the year,
#'                        # variabels = 9
#' @format
#' \describe{
#'     \item{firstname}{firstname of graduate}
#'     \item{middlename}{middlename(s) of graduate}
#'     \item{lastname}{lastname of graduate}
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
#' }
build_annual_dataframe <- function(year) {
  print(year)
  filename <- paste(getwd(), "/inst/extdata/graduates-", year, "-", (year + 1), ".txt", sep = "")

  # scraping information about students with different latin honors
  # ("Suma", "Magna", "Cum", and "None") as different lists using appropriate scraping
  # functions from "extract-from-source.R"
  suma_grads <- scrape_suma_cum_laude(filename)
  magna_grads <- scrape_magna_cum_laude(filename)
  cum_grads <- scrape_cum_laude(filename)
  no_latin_honors_grads <- scrape_graduates(filename)

  # scraping information about students using appropriate functions from "scrape-graduation-info.R"
  firstnames <- c(sapply(suma_grads, scrape_first_name),
                  sapply(magna_grads, scrape_first_name),
                  sapply(cum_grads, scrape_first_name),
                  sapply(no_latin_honors_grads, scrape_first_name))

  lastnames <- c(sapply(suma_grads, scrape_last_name),
                 sapply(magna_grads, scrape_last_name),
                 sapply(cum_grads, scrape_last_name),
                 sapply(no_latin_honors_grads, scrape_last_name))

  middlenames <- c(sapply(suma_grads, scrape_middle_name),
                   sapply(magna_grads, scrape_middle_name),
                   sapply(cum_grads, scrape_middle_name),
                   sapply(no_latin_honors_grads, scrape_middle_name))

  years <- rep(year, length(middlenames))

  # scraping information about students' latin honors using appropriate functions from
  # "scrape-graduation-info.R"
  latin.honors <- c(rep("SUMA", length(suma_grads)),
                    rep("MAGNA", length(magna_grads)),
                    rep("CUM", length(cum_grads)),
                    rep("NONE", length(no_latin_honors_grads)))

  # scraping information about students' theses using appropriate functions from "scrape-graduation-info.R"
  honors <- c(sapply(suma_grads, scrape_primary_honors),
              sapply(magna_grads, scrape_primary_honors),
              sapply(cum_grads, scrape_primary_honors),
              sapply(no_latin_honors_grads, scrape_primary_honors))

  majors <- c(sapply(suma_grads, scrape_primary_honors_major),
              sapply(magna_grads, scrape_primary_honors_major),
              sapply(cum_grads, scrape_primary_honors_major),
              sapply(no_latin_honors_grads, scrape_primary_honors_major))

  honors.2 <- c(sapply(suma_grads, scrape_secondary_honors),
                sapply(magna_grads, scrape_secondary_honors),
                sapply(cum_grads, scrape_secondary_honors),
                sapply(no_latin_honors_grads, scrape_secondary_honors))

  majors.2 <- c(sapply(suma_grads, scrape_secondary_honors_major),
                sapply(magna_grads, scrape_secondary_honors_major),
                sapply(cum_grads, scrape_secondary_honors_major),
                sapply(no_latin_honors_grads, scrape_secondary_honors_major))

  # compiling into dataframe
  df <- data.frame(firstname = firstnames,
                   middlename = middlenames,
                   lastname = lastnames,
                   year = years,
                   latin.honor = latin.honors,
                   honor = honors,
                   major = majors,
                   honor.2 = honors.2,
                   major.2 = majors.2)

  return(df)
}


#' @title Build Dataframe for all Available Years
#' @description The function loads in the graduate information for the
#'              given years, scrapes appropriate information, & organizes
#'              it into a dataframe.
#'
#'              The function first scrapes information about students with different latin honors
#'              ("Suma", "Magna", "Cum", and "None") as different lists using appropriate scraping
#'              functions from "extract-from-source.R".
#'
#'              Then, from each entry in these lists, we extract information about the students', their
#'              latin honors, and their theses and maintain them as corresponding lists. Finally, we
#'              organize the data into a dataframe in the required format.
#'
#' @param year year to build data frame for <YYYY>
#' @return dataframe with # rows = # graduates for the year,
#'                        # variabels = 9
#' @format
#' \describe{
#'     \item{firstname}{firstname of graduate}
#'     \item{middlename}{middlename(s) of graduate}
#'     \item{lastname}{lastname of graduate}
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
#' }
build_dataframes <- function() {
  years <- 2000:2015
  do.call(rbind, lapply(years, build_annual_dataframe) )
}




