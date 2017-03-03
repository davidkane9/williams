#' @title First Name Scraper
#' @description Scrapes graduates first name for honor details
#' @param grad_details a graduates graduation details
#'                     supplied as: (1) <firstname> <midlename(s)> <lastname>, <honors details>
#'                              or, (2) <firstname> <midlename(s)> <lastname>
#' @return graduate's first name
scrape_first_name <- function(grad_details){
  start_pos <- 1
  end_pos <- regexpr(' ', grad_details) - 1
  return(substr(grad_details, start_pos, end_pos))
}

#' @title Last Name Scraper
#' @description Scrapes graduates last name for honor details
#' @param grad_details a graduates graduation details
#'                     supplied as: (1) <firstname> <midlename(s)> <lastname>, <honors details>
#'                              or, (2) <firstname> <midlename(s)> <lastname>
#' @return graduate's last name
scrape_last_name <- function(grad_details){
  # delete honors details
  honors_index <- regexpr(",", grad_details)
  if(honors_index > 0){
    grad_details <- substr(grad_details, 1, honors_index - 1)
  }
  # now, substring
  space_indices <- gregexpr(" ", grad_details)[[1]]
  last_space_index <- space_indices[length(space_indices)]
  return(substr(grad_details, last_space_index + 1, nchar(grad_details)))
}

#' @title Middle Name Scraper
#' @description Scrapes graduates middle name for honor details
#' @param grad_details a graduates graduation details
#'                     supplied as: (1) <firstname> <midlename(s)> <lastname>, <honors details>
#'                              or, (2) <firstname> <midlename(s)> <lastname>
#' @return graduate's middle name
scrape_middle_name <- function(grad_details){
  # delete honors details
  honors_index <- regexpr(",", grad_details)
  if(honors_index > 0){
    grad_details <- substr(grad_details, 1, honors_index - 1)
  }
  # now, substring
  space_indices <- gregexpr(" ", grad_details)[[1]]
  first_space_index <- space_indices[1]
  last_space_index <- space_indices[length(space_indices)]
  return(substr(grad_details, first_space_index + 1, last_space_index - 1))
}

#' @title Primary Honors Scraper
#' @description Scrapes primary honor details
#'              We define the "primary" honor as the first honor appearing alongside grad's name
#' @param grad_details a graduates graduation details
#'                     supplied as: (1) <firstname> <midlename(s)> <lastname>, <honors details>
#'                              or, (2) <firstname> <midlename(s)> <lastname>
#' @return (1) "NONE" if no honors
#'         (2) "HIGHEST" if grad recieved (highest) honors for thesis
#'         (3) "HONORS" if grad received (ordinary) honors for thesis
scrape_primary_honors <- function(grad_details) {
  prim_honors_start_index <- regexpr(",", grad_details) + 1
  if(prim_honors_start_index <= 0){
    return("NONE")
  }
  grad_details <- substr(grad_details, prim_honors_start_index, nchar(grad_details))
  prim_honors_end_index <- regexpr(",", grad_details)
  if(prim_honors_end_index > 0){
    grad_details <- substr(grad_details, 1, prim_honors_end_index - 1)
  }
  if(regexpr("highest", grad_details) > 0){
    return("HIGHEST")
  } else {
    return("HONORS")
  }
}

#' @title Primary Honors Major Scraper
#' @description Scrapes primary honor details
#'              We define the "primary" honor as the first honor appearing alongside grad's name
#' @param grad_details a graduates graduation details
#'                     supplied as: (1) <firstname> <midlename(s)> <lastname>, <honors details>
#'                              or, (2) <firstname> <midlename(s)> <lastname>
#' @return (1) "NONE" if no honors
#'         (2) <major> if grad did a thesis
scrape_primary_honors_major <- function(grad_details) {
  prim_honors_start_index <- regexpr(",", grad_details) + 1
  if(prim_honors_start_index <= 0){
    return("NONE")
  }
  grad_details <- substr(grad_details, prim_honors_start_index, nchar(grad_details))
  prim_honors_end_index <- regexpr(",", grad_details)
  if(prim_honors_end_index > 0){
    grad_details <- substr(grad_details, 1, prim_honors_end_index - 1)
  }
  major_start_index <- regexpr(" in ", grad_details) + nchar(" in ")
  return(substr(grad_details, major_start_index, nchar(grad_details)))
}

#' @title Secondary Honors Scraper
#' @description Scrapes secondary honor details
#'              We define the "secondary" honor as the second honor appearing alongside grad's name
#' @param grad_details a graduates graduation details
#'                     supplied as: (1) <firstname> <midlename(s)> <lastname>, <honors details>
#'                              or, (2) <firstname> <midlename(s)> <lastname>
#' @return (1) "NONE" if no honors
#'         (2) "HIGHEST" if grad recieved (highest) honors for thesis
#'         (3) "HONORS" if grad received (ordinary) honors for thesis
scrape_secondary_honors <- function(grad_details) {
  comma_indices <- gregexpr(",", grad_details)[[1]]
  if(length(comma_indices) < 2){
    return("NONE")
  }
  secondary_honors_start_index <- comma_indices[2] + 1
  secondary_honors_end_index <- nchar(grad_details)
  grad_details <- substr(grad_details, secondary_honors_start_index, secondary_honors_end_index)
  if(regexpr("highest", grad_details) > 0){
    return("HIGHEST")
  } else {
    return("HONORS")
  }
}

#' @title Secondary Honors Major Scraper
#' @description Scrapes secondary honor details
#'              We define the "secondary" honor as the second honor appearing alongside grad's name
#' @param grad_details a graduates graduation details
#'                     supplied as: (1) <firstname> <midlename(s)> <lastname>, <honors details>
#'                              or, (2) <firstname> <midlename(s)> <lastname>
#' @return (1) "NONE" if no honors
#'         (2) <major> if grad did a second thesis
scrape_secondary_honors_major <- function(grad_details) {
  comma_indices <- gregexpr(",", grad_details)[[1]]
  if(length(comma_indices) < 2){
    return("NONE")
  }
  secondary_honors_start_index <- comma_indices[2] + 1
  secondary_honors_end_index <- nchar(grad_details)
  grad_details <- substr(grad_details, secondary_honors_start_index, secondary_honors_end_index)
  major_start_index <- regexpr(" in ", grad_details) + nchar(" in ")
  return(substr(grad_details, major_start_index, nchar(grad_details)))
}
