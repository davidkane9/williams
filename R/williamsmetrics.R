#' @title williamsmetrics: A package of data associated with Williams College and tools for creating it.
#'
#' @description
#' The heart of \code{williamsmetrics} is the dataset \code{graduates}. This documents
#' describes how this data was collected, and provides instructions for adding data in subsequent
#' years.
#'
#' \bold{The Course Catalogs} \cr
#' The college maintains an [archive](http://web.williams.edu/admin/registrar/catalog/archive.html) of
#' annual course catalogs that serves as a rich basis for information on graduating students.
#' More specifically, under the "Degrees Conferred" section in each course catalog,
#' we find a list of names for graduating students (catagorized by Latin honor conferred), along with information
#' about their senior thesis, and any related distinctions. \cr
#' For each course catalog, we copy-paste the "Degrees Conferred" section into a text file. We save this in
#' the \code{inst/extdata} directory of the package, using the naming convention
#' "graduates-<year - 1>-<year>.txt". \cr
#' For example, for course catalog for the 2016-2017 academic year, we save the "Degrees Conferred"
#' section into a text file named "graduates-2015-2016.txt" in the \code{inst/extdata} directory. \cr
#' \cr
#' Please note: Due to copy-pasting difficulties from the PDFs, the "copy-paste" step is sometimes tedious.
#' often, details about several graduates our clumped onto a
#' single line (that is, they appear without line breaks). Here, it is essential to manually seperate these
#' lines out, and ensure that a single line contains information only about a single graduate.
#' For example,
#' "DoHyun Tony Chung, with honors in Political EconomyRebecca Tamar Cover, with highest honors in AstrophysicsAmanda Bouvier Edmonds"
#' should be separated into: \cr
#' DoHyun Tony Chung, with honors in Political Economy \cr
#' Rebecca Tamar Cover, with highest honors in Astrophysics \cr
#' Amanda Bouvier Edmonds \cr
#'
#' Another complexity that we handle by hand is the apostrophe in "Women's" as it is used in both
#' Women's and Gender Studies and in Women's, Gender and Sexuality Studies. We had trouble handling this
#' apostrophe, because it has a strange encoding. So, we simply changed it to a simple apostrophe by hand.
#' Future years will need to be handled similarly.
#'
#' Once the new file is added, and the package rebuilt, running \code{x <- create_graduates()} will generate
#' a new data frame with all the relevant data. Use the \code{complete = TRUE} argument to provide more detailed
#' information.
#'
#' @docType package
#' @name williamsmetrics
NULL
