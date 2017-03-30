#' @title Slice and Delimit
#' @description This function slices the \code{source_str} at \code{start_str}
#'              till \code{stop_str}, and seperates the string in between as
#'              a list seperated by delimiter \code{delim}
#'@param source_str String containing target text
#'@param start_str String at which process should start
#'@param stop_str String at which process should end. The default is empty, which is equivalent to EOF.
#'@param delim character to delimite string on
#'             default: "\n"
#'@return target text in \code{source_str} delimited at character \code{delim}
#' @export

slice_and_delimit <- function(source_str, start_str, stop_str = "", delim = "\n"){
  start_pos <- regexpr(start_str, source_str) + nchar(start_str)
  if(nchar(stop_str) == 0){ # go till end of file
    end_pos <- nchar(source_str)
  } else {
    end_pos <- regexpr(stop_str, source_str) - 1
  }
  sliced_text <- substr(source_str, start_pos, end_pos)

  # Split sliced string into a list at delim (delimiter), and return it
  trimws(strsplit(sliced_text, delim)[[1]])
}
