#' @title Scrape Primary Thesis Major
#' @description A student's graduation details is provided in the catalog in the following format:
#'              (1) <firstname> <midlename(s)> <lastname>, <theses details>
#'              or, (2) <firstname> <midlename(s)> <lastname>
#'
#'              This function concerns itself with the <theses details>. In turn, <theses details> are
#'              provided in the following format:
#'              <thesis 1 details>, <thesis 2 details>
#'              where each <thesis N detail> is in the format "with <distinction> honors in <major>"
#'
#'              Here we define the first thesis as the "Primary Thesis". This function will scrape the
#'              major for the Primary Thesis.
#'              For example, in the case of "DoHyun Tony Chung, with honors in Political Economy",
#'              we infer "Political Economy" as the student's primary thesis major.
#'@param grad_details A student's graduation details as appearing in the catalog.
#'@return Student's primary thesis major
scrape_primary_thesis_major <- function(grad_details){
  prim_honors_start_index <- regexpr(",", grad_details) + 1
  if(prim_honors_start_index <= 0){ # format (1) ?? If yes, student didn't write a thesis
    return("NONE")
  }
  #  => format (2) --> extract thesis major
  grad_details <- substr(grad_details, prim_honors_start_index, nchar(grad_details))
  prim_honors_end_index <- regexpr(",", grad_details)

  # remove secondary thesis details (if any)
  if(prim_honors_end_index > 0){
    grad_details <- substr(grad_details, 1, prim_honors_end_index - 1)
  }
  major_start_index <- regexpr(" in ", grad_details) + nchar(" in ")
  return(substr(grad_details, major_start_index, nchar(grad_details)))
}
