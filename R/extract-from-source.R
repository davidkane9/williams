#' @title Summa Cum Laude Scraper
#' @description Scrapes data for Summa Cum Laude graduates for given year
#' @param filepath file to scrape data from
#' @return A list of Summa Cum Laude graduates w/ honors information (if any)
scrape_suma_cum_laude <- function(filepath){
  # Load in file from specified file path as a character string
  if(!file.exists(filepath)){
    stop("ERROR: No file found at specified file path")
  }
  text <- readChar(filepath, file.info(filepath)$size)

  # Substring text for Summa Cum Laude graduates
  START_STRING <- "Bachelor of Arts, Summa Cum Laude"
  END_STRING <- "Bachelor of Arts, Magna Cum Laude"
  start_pos <- regexpr(START_STRING, text) + nchar(START_STRING) + 1
  end_pos <- regexpr(END_STRING, text)-1
  summa_grads <- substr(text, start_pos, end_pos)

  # Split string into a list of Summa Cum Laude grads, and return it
  trimws(strsplit(summa_grads, "\n")[[1]])
}

#' @title Magna Cum Laude Scraper
#' @description Scrapes data for Magna Cum Laude graduates for given year
#' @param filepath file to scrape data from
#' @return A list of Magna Cum Laude graduates w/ honors information (if any)
scrape_magna_cum_laude <- function(filepath){
  # Load in file from specified file path as a character string
  if(!file.exists(filepath)){
    stop("ERROR: No file found at specified file path")
  }
  text <- readChar(filepath, file.info(filepath)$size)

  # Substring text for Magna Cum Laude graduates
  START_STRING <- "Bachelor of Arts, Magna Cum Laude"
  END_STRING <- "Bachelor of Arts, Cum Laude"
  start_pos <- regexpr(START_STRING, text) + nchar(START_STRING) + 1
  end_pos <- regexpr(END_STRING, text)-1
  magna_grads <- substr(text, start_pos, end_pos)

  # Split string into a list of Magna Cum Laude grads, and return it
  trimws(strsplit(magna_grads, "\n")[[1]])
}

#' @title Cum Laude Scraper
#' @description Scrapes data for Cum Laude graduates for given year
#' @param filepath file to scrape data from
#' @return A list of Cum Laude graduates w/ honors information (if any)
scrape_cum_laude <- function(filepath){
  # Load in file from specified file path as a character string
  if(!file.exists(filepath)){
    stop("ERROR: No file found at specified file path")
  }
  text <- readChar(filepath, file.info(filepath)$size)

  # Substring text for Cum Laude graduates
  START_STRING <- "Bachelor of Arts, Cum Laude"
  END_STRING <- "Bachelor of Arts\n"
  start_pos <- regexpr(START_STRING, text) + nchar(START_STRING) + 1
  end_pos <- regexpr(END_STRING, text)-1
  cum_grads <- substr(text, start_pos, end_pos)

  # Split string into a list of Cum Laude grads, and return it
  trimws(strsplit(cum_grads, "\n")[[1]])
}


#' @title Bachelor of Arts Scraper
#' @description Scrapes data for graduates w/ no latin honors for given year
#' @param filepath file to scrape data from
#' @return A list of graduates (w/ no latin honors) w/ honors information (if any)
scrape_graduates <- function(filepath){
  # Load in file from specified file path as a character string
  if(!file.exists(filepath)){
    stop("ERROR: No file found at specified file path")
  }
  text <- readChar(filepath, file.info(filepath)$size)

  # Substring text for Cum Laude graduates
  START_STRING <- "Bachelor of Arts\n"
  start_pos <- regexpr(START_STRING, text) + nchar(START_STRING) + 1
  end_pos <- nchar(text)
  grads <- substr(text, start_pos, end_pos)

  # Split string into a list of Cum Laude grads, and return it
  trimws(strsplit(grads, "\n")[[1]])
}
