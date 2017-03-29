#' @title Build Annual Dataframe
#' @description This function compiles information for the given year's degrees and graduates and presents
#'              it as a dataframe. Details on this process can be found in the vignette.
#'
#'
#' @param year year to build dataframe for
#' @return a dataframe for the specified year of
#'                        # rows = # graduates for the year,
#'                        # variabels = 10
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
#'     \item{raw.text}{graduation details as appearing in course catalog}
#'     }
#'

build_annual_dataframe <- function(year){
  filename <- paste(getwd(), "/inst/extdata/graduates-", year, "-", (year + 1), ".txt", sep = "")
  data <- read_file(filename)

  # scraping information about students with different latin honors
  # ("Suma", "Magna", "Cum", and "None") as different lists
  suma_grads <- slice_and_delimit(source_string = data, start_str = "Bachelor of Arts, Summa Cum Laude\n",
                                  stop_str = "Bachelor of Arts, Magna Cum Laude\n", delim = "\n")
  magna_grads <- slice_and_delimit(source_string = data, start_str = "Bachelor of Arts, Magna Cum Laude\n",
                                   stop_str = "Bachelor of Arts, Cum Laude\n", delim = "\n")
  cum_grads <- slice_and_delimit(source_string = data, start_str = "Bachelor of Arts, Cum Laude\n",
                                stop_str = "Bachelor of Arts\n", delim = "\n")
  no_latin_honors_grads <- slice_and_delimit(source_string = data, start_str = "Bachelor of Arts\n",
                                            stop_str = "", delim = "\n")

  # Standardize scraped info for discrepencies
  suma_grads.standard <- standardize_graduation_details(suma_grads)
  magna_grads.standard <- standardize_graduation_details(magna_grads)
  cum_grads.standard <- standardize_graduation_details(cum_grads)
  no_latin_honors_grads.standard <- standardize_graduation_details(no_latin_honors_grads)

  # scraping information about students using appropriate functions from "scrape-graduation-info.R"
  firstnames <- c(sapply(suma_grads.standard, scrape_first_name),
                  sapply(magna_grads.standard, scrape_first_name),
                  sapply(cum_grads.standard, scrape_first_name),
                  sapply(no_latin_honors_grads.standard, scrape_first_name))

  lastnames <- c(sapply(suma_grads.standard, scrape_last_name),
                 sapply(magna_grads.standard, scrape_last_name),
                 sapply(cum_grads.standard, scrape_last_name),
                 sapply(no_latin_honors_grads.standard, scrape_last_name))

  middlenames <- c(sapply(suma_grads.standard, scrape_middle_name),
                   sapply(magna_grads.standard, scrape_middle_name),
                   sapply(cum_grads.standard, scrape_middle_name),
                   sapply(no_latin_honors_grads.standard, scrape_middle_name))

  years <- rep(year, length(middlenames))

  # scraping information about students' latin honors using appropriate functions from
  # "scrape-graduation-info.R"
  latin.honors <- c(rep("SUMA", length(suma_grads.standard)),
                    rep("MAGNA", length(magna_grads.standard)),
                    rep("CUM", length(cum_grads.standard)),
                    rep("NONE", length(no_latin_honors_grads.standard)))
  # scraping information about students' theses using appropriate functions from "scrape-graduation-info.R"
  honors <- c(sapply(suma_grads.standard, scrape_primary_thesis_distinction),
              sapply(magna_grads.standard, scrape_primary_thesis_distinction),
              sapply(cum_grads.standard, scrape_primary_thesis_distinction),
              sapply(no_latin_honors_grads.standard, scrape_primary_thesis_distinction))

  majors <- c(sapply(suma_grads.standard, scrape_primary_thesis_major),
              sapply(magna_grads.standard, scrape_primary_thesis_major),
              sapply(cum_grads.standard, scrape_primary_thesis_major),
              sapply(no_latin_honors_grads.standard, scrape_primary_thesis_major))

  honors.2 <- c(sapply(suma_grads.standard, scrape_secondary_thesis_distinction),
                sapply(magna_grads.standard, scrape_secondary_thesis_distinction),
                sapply(cum_grads.standard, scrape_secondary_thesis_distinction),
                sapply(no_latin_honors_grads.standard, scrape_secondary_thesis_distinction))

  majors.2 <- c(sapply(suma_grads.standard, scrape_secondary_thesis_major),
                sapply(magna_grads.standard, scrape_secondary_thesis_major),
                sapply(cum_grads.standard, scrape_secondary_thesis_major),
                sapply(no_latin_honors_grads.standard, scrape_secondary_thesis_major))

  # compiling into dataframe
  df <- data.frame(firstname = firstnames,
                   middlename = middlenames,
                   lastname = lastnames,
                   year = years,
                   latin.honor = latin.honors,
                   honor = honors,
                   major = majors,
                   honor.2 = honors.2,
                   major.2 = majors.2,
                   stringsAsFactors = FALSE)
  # remove row names and add as column for "raw text"
  df$raw.text <- row.names(df)
  row.names(df) <- c()
  return(df)
}
