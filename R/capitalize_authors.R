#' Write Author Names in Title Case.
#'
#' This function capitalizes author names (given and family names), while setting all other characters to lower case.
#'
#' @importFrom tools toTitleCase
#' @param ref A \code{\link{BibEntry}} object.
#' @export
#' @return A BibEntry object with author names in Title Case.
#' @examples \dontrun{
#'
#'}


capitalize_authors <- function(ref){

  authors <- strsplit(paste(tolower(ref$author)), split = " ")

  au <- lapply(authors, Hmisc::capitalize)

  ref$author <- paste(lapply(au, paste, collapse = " "), collapse = " and ")

  ref
}


