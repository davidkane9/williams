#' @title Read File
#' @description reads in text from a file and returns it as a String
#' @param filepath filepath of file to read from
#' @return contents of file in string format
read_file <- function(filepath){
  # Load in file from specified file path as a character string
  if(!file.exists(filepath)){
    stop("ERROR: No file found at specified file path")
  }
  data <- readChar(filepath, file.info(filepath)$size)
  return(data)
}

