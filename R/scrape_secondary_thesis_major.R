#' @title Scrape Secondary Thesis Major
#' @description A student's graduation details is provided in the catalog in the following format:
#'              (1) <firstname> <midlename(s)> <lastname>, <theses details>
#'              or, (2) <firstname> <midlename(s)> <lastname>
#'
#'              This function concerns itself with the <theses details>. In turn, <theses details> are
#'              provided in the following format:
#'              <thesis 1 details>, <thesis 2 details>
#'              where each <thesis N detail> is in the format "with <distinction> honors in <major>"
#'
#'              Here we define the second thesis as the "Secondary Thesis". This function will scrape the
#'              major for the Secondary Thesis.
#'              For example, in the case of "DoHyun Tony Chung, with honors in Political Economy, with
#'              honors in Mathematics", we infer "Mathematics" as the student's secondary thesis major.
#'@param grad_details A student's graduation details as appearing in the catalog.
#'@return Student's secondary thesis major
#' @export

scrape_secondary_thesis_major <- function(grad_details){
  comma_indices <- gregexpr(",", grad_details)[[1]]
  if(length(comma_indices) < 2){ # format (1) or no secondary thesis??
    return("NONE")
  }
  # => student wrote a secondary thesis --> extract major
  secondary_honors_start_index <- comma_indices[2] + 1
  secondary_honors_end_index <- nchar(grad_details)
  grad_details <- substr(grad_details, secondary_honors_start_index, secondary_honors_end_index)
  major_start_index <- regexpr(" in ", grad_details) + nchar(" in ")
  return(substr(grad_details, major_start_index, nchar(grad_details)))
}
