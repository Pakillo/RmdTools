#' Write Author Names in Title Case.
#'
#' This function capitalizes author names (given and family names), while setting all other characters to lower case.
#'
#' @import RefManageR
#' @importFrom tools toTitleCase
#' @param ref A \code{\link[RefManageR]{BibEntry}} object.
#' @export
#' @return A BibEntry object with author names in Title Case.
#' @author F. Rodriguez-Sanchez

capitalize_authors <- function(ref){

  for (i in 1:length(ref)){
    authors <- tools::toTitleCase(tolower(ref[[i]]$author))
    ref[[i]]$author <- paste(authors, collapse = " and ")
  }

  return(ref)

}


