#' Write Author Names in Title Case.
#'
#' This function capitalizes author names (given and family names), while setting all other characters to lower case.
#'
#' @importFrom tools toTitleCase
#' @param ref A \code{\link{BibEntry}} object.
#' @export
#' @return A BibEntry object with author names in Title Case.
#' @author F. Rodriguez-Sanchez

capitalize_authors <- function(ref){

  authors <- tools::toTitleCase(tolower(ref$author))

  ref$author <- paste(ref$author, collapse = " and ")

  ref
}


