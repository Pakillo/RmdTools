#' Fix capitalization of reference titles.
#'
#' This function changes capitalization of reference titles in a \code{\link[RefManageR]{BibEntry}} object to Title Case (first letter of each word is capitalized), or only first word capitalized.
#'
#' @import RefManageR
#' @importFrom tools toTitleCase
#' @importFrom Hmisc capitalize
#' @param ref A \code{\link[RefManageR]{BibEntry}} object.
#' @param titlecase Logical. Use Title Case (first letter of each word is capitalized)? Default is FALSE (only first letter of the title capitalized).
#' @export
#' @return A BibEntry object with titles capitalized according to \code{titlecase}.
#' @author F. Rodriguez-Sanchez
#' @seealso \code{\link[tools]{toTitleCase}} and \code{\link[Hmisc]{capitalize}}.

capitalize_titles <- function(ref, titlecase = FALSE){

  for (i in 1:length(ref)){
    title.old <- gsub("\\{|\\}", "", tolower(ref[[i]]$title))
    ref[[i]]$title <- ifelse(isTRUE(titlecase),
                             tools::toTitleCase(title.old),
                             Hmisc::capitalize(title.old))
  }

  return(ref)

}


