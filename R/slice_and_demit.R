#' @title Slice and Delimit
#' @description This function slices the \code{source_str} at \code{start_string}
#'              till \code{stop_str}, and seperates the string in between as
#'              a list seperated by delimiter \code{delim}
#'@param source_str String containing target text
#'@param start_str This function will delimit text after first ocurrance of \code{start_str}
#'@param stop_str This function will delimit text till before the first occurance of \code{stop_str}
#'                default: empty; equivalent to EOF.
#'@param delim character to delimite string on
#'             default: "\n"
#'@return target text in \code{source_str} delimited at character \code{delim}
slice_and_delimit <- function(source_string, start_str, stop_str = "", delim = "\n"){
  start_pos <- regexpr(start_str, source_string) + nchar(start_str)
  if(nchar(stop_str) == 0){ # go till end of file
    end_pos <- nchar(source_string)
  } else {
    end_pos <- regexpr(stop_str, source_string) - 1
  }
  sliced_text <- substr(source_string, start_pos, end_pos)

  # Split sliced string into a list at delim (delimiter), and return it
  trimws(strsplit(sliced_text, delim)[[1]])
}
