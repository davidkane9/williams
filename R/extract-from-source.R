#' @title Summa Cum Laude Scraper
#' @description Scrapes data for Summa Cum Laude graduates for given year.
#'              The graduates information is presented in each catalog as:
#'              Bachelor of Arts, Summa Cum Laude
#'              <Summa Cum Laude graduate 1>
#'              <Summa Cum Laude graduate 2>
#'              ...
#'              ...
#'              ...
#'              Bachelor of Arts, Magna Cum Laude
#'              ...
#'
#'              This function will first extract the lines between "Bachelor of Arts, Summa Cum Laude"
#'              and "Bachelor of Arts, Magna Cum Laude". Notice that these are exactly the lines
#'              with information about Suma Cum Laude graduates, with each line containing
#'              information about a particular graduate.
#'
#'              Subequently, we extract each line (that is, slice the String object at "\n" delimiter),
#'              and return each line as an entry in a list.
#'
#' @param data data in string format to scrape information from
#' @return A list of Summa Cum Laude graduates w/ honors information (if any)
scrape_suma_cum_laude <- function(data){

  # Substring data for Summa Cum Laude graduates
  START_STRING <- "Bachelor of Arts, Summa Cum Laude"
  END_STRING <- "Bachelor of Arts, Magna Cum Laude"
  start_pos <- regexpr(START_STRING, data) + nchar(START_STRING) + 1
  end_pos <- regexpr(END_STRING, data)-1
  summa_grads <- substr(data, start_pos, end_pos)

  # Split string into a list of Summa Cum Laude grads, and return it
  trimws(strsplit(summa_grads, "\n")[[1]])
}

#' @title Magna Cum Laude Scraper
#' @description Scrapes data for Magna Cum Laude graduates for given year
#'              The graduates information is presented in each catalog as:
#'              Bachelor of Arts, Magna Cum Laude
#'              <Magna Cum Laude graduate 1>
#'              <Magna Cum Laude graduate 2>
#'              ...
#'              ...
#'              ...
#'              Bachelor of Arts, Cum Laude
#'              ...
#'
#'              This function will first extract the lines between "Bachelor of Arts, Magna Cum Laude"
#'              and "Bachelor of Arts, Cum Laude". Notice that these are exactly the lines
#'              with information about Magna Cum Laude graduates, with each line containing
#'              information about a particular graduate.
#'
#'              Subequently, we extract each line (that is, slice the String object at "\n" delimiter),
#'              and return each line as an entry in a list.
#'
#' @param data data in string format to scrape information from
#' @return A list of Magna Cum Laude graduates w/ honors information (if any)
scrape_magna_cum_laude <- function(data){
  # Substring data for Magna Cum Laude graduates
  START_STRING <- "Bachelor of Arts, Magna Cum Laude"
  END_STRING <- "Bachelor of Arts, Cum Laude"
  start_pos <- regexpr(START_STRING, data) + nchar(START_STRING) + 1
  end_pos <- regexpr(END_STRING, data)-1
  magna_grads <- substr(data, start_pos, end_pos)

  # Split string into a list of Magna Cum Laude grads, and return it
  trimws(strsplit(magna_grads, "\n")[[1]])
}

#' @title Cum Laude Scraper
#' @description Scrapes data for Cum Laude graduates for given year
#'              The graduates information is presented in each catalog as:
#'              Bachelor of Arts, Cum Laude
#'              <Summa Cum Laude graduate 1>
#'              <Summa Cum Laude graduate 2>
#'              ...
#'              ...
#'              ...
#'              Bachelor of Arts
#'              ...
#'
#'              This function will first extract the lines between "Bachelor of Arts, Cum Laude"
#'              and "Bachelor of Arts\n". Notice that these are exactly the lines
#'              with information about Cum Laude graduates, with each line containing
#'              information about a particular graduate.
#'
#'              Subequently, we extract each line (that is, slice the String object at "\n" delimiter),
#'              and return each line as an entry in a list.
#'
#' @param filepath file to scrape data from
#' @return A list of Cum Laude graduates w/ honors information (if any)
scrape_cum_laude <- function(data){
  # Substring data for Cum Laude graduates
  START_STRING <- "Bachelor of Arts, Cum Laude"
  END_STRING <- "Bachelor of Arts\n"
  start_pos <- regexpr(START_STRING, data) + nchar(START_STRING) + 1
  end_pos <- regexpr(END_STRING, data)-1
  cum_grads <- substr(data, start_pos, end_pos)

  # Split string into a list of Cum Laude grads, and return it
  trimws(strsplit(cum_grads, "\n")[[1]])
}


#' @title Bachelor of Arts Scraper
#' @description Scrapes data for graduates w/ no latin honors for given year
#'              The graduates information is presented in each catalog as:
#'
#'              Bachelor of Arts
#'              <Bachelor of Arts graduate 1>
#'              <Bachelor of Arts graduate 2>
#'              ...
#'              ...
#'              <EOF>
#'
#'              This function will first extract the lines after "Bachelor of Arts\n", till the end
#'              of the data body. Notice that these are exactly the lines
#'              with information about graduates with no latin honors, with each line containing
#'              information about a particular graduate.
#'
#'              Subequently, we extract each line (that is, slice the String object at "\n" delimiter),
#'              and return each line as an entry in a list.
#'
#' @param filepath file to scrape data from
#' @return A list of graduates (w/ no latin honors) w/ honors information (if any)
scrape_graduates <- function(data){

  # Substring data for Cum Laude graduates
  START_STRING <- "Bachelor of Arts\n"
  start_pos <- regexpr(START_STRING, data) + nchar(START_STRING)
  end_pos <- nchar(data)
  grads <- substr(data, start_pos, end_pos)

  # Split string into a list of Cum Laude grads, and return it
  trimws(strsplit(grads, "\n")[[1]])
}

#' @title Read File
#' @description reads in text from a file and returns it as a String
#' @param filepath filepath of file to read from
#' @return contents of file in string format
readFile <- function(filepath){
  # Load in file from specified file path as a character string
  if(!file.exists(filepath)){
    stop("ERROR: No file found at specified file path")
  }
  data <- readChar(filepath, file.info(filepath)$size)
  return(data)
}
