#' @title Enrollment Links
#' @description Acts as the domain for links to the Williams Course Catalogs found on the
#'              college's website.
#' @param start_year YYYY year as integer from when links are required
#' @param end_year YYYY year as integer to when links are required
#' @return a list of links to course catalogs from \code{start_year} to \code{end_year}
get_catalog_links <- function(start_year, end_year) {
  # Earliest year we have catalogs for
  start_year_MIN <- 2000
  # Latest year we have catalogs for
  end_year_MAX <- 2015
  if(start_year < start_year_MIN || end_year > end_year_MAX || start_year > end_year){
    stop("ERROR in get_catalog_links: start_year or end_year parameter are illegal")
  }
  links <- c(
    "http://web.williams.edu/admin/registrar/catalog/depts0001/catalog.pdf",
    "http://web.williams.edu/admin/registrar/catalog/depts0102/catalog.pdf",
    "http://web.williams.edu/admin/registrar/catalog/depts0203/catalog.pdf",
    "http://web.williams.edu/admin/registrar/catalog/depts0304/catalog.pdf",
    "http://web.williams.edu/admin/registrar/catalog/depts0405/catalog0405.pdf",
    "http://web.williams.edu/admin/registrar/catalog/catalog0506.pdf",
    "http://web.williams.edu/admin/registrar/catalog/catalog0607.pdf",
    "http://web.williams.edu/admin/registrar/catalog/catalog0708.pdf",
    "http://web.williams.edu/admin/registrar/catalog/catalog0809.pdf",
    "http://web.williams.edu/admin/registrar/catalog/catalog0910.pdf",
    "http://web.williams.edu/admin/registrar/catalog/bulletin2010-11.pdf",
    "http://web.williams.edu/admin/registrar/catalog/bulletin2011_12.pdf",
    "http://web.williams.edu/admin/registrar/catalog/bulletin12_13.pdf",
    "http://web.williams.edu/admin/registrar/catalog/bulletin2013_14.pdf",
    "http://web.williams.edu/admin/registrar/catalog/bulletin2014_15.pdf",
    "http://web.williams.edu/admin/registrar/catalog/bulletin2015_16.pdf"
  )
  return(links[(start_year - start_year_MIN + 1):
           ((end_year_MAX - start_year_MIN + 1) - (end_year_MAX - end_year))])

}

#' @title Download File from Link
#' @description Download file at link and save it in inst/extdata with appropriate name
#' @param link link to file to download
#' @param filename file will be saved as "name"
#' @return TRUE iff download is successful
download_file <- function(link, filename) {
  # REQUIRES FIXING
  package_path <- getwd()
  download_path <- paste(package_path, "/inst/extdata/", filename, sep = "")
  status <- download.file(link, destfile = download_path)
  return(status == 0)
}

#' @title Download Catalogs ('00 -- '15)
#' @description Downloads all catalogs from 2000 to 2015
#' @return True iff all downloads are successful
download_catalogs <- function() {
  links <- get_catalog_links(start_year = 2000, end_year = 2015)
  names <- paste(paste("catalog", 2000:2015, 2001:2016, sep="-"), ".pdf", sep = "")
  results <- mapply(download_file, links, names)
  return(all(results))
}


