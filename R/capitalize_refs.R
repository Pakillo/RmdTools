#' Fix capitalization of bibliographic references.
#'
#' This function is just a wrapper of \code{\link{capitalize_authors}} and  \code{\link{capitalize_titles}}, aimed at fixing the capitalizacion of all references contained in a \code{\link[RefManageR]{BibEntry}} object all at once. Author names will appear with capitalized first letters, and titles will appear as Sentence case or Title Case, depending on \code{TitleCase} parameter.
#'
#' @export
#' @inheritParams capitalize_titles
#' @param TitleCase Logical. Use Title Case (first letter of each word is capitalized)? Default is FALSE (only first letter of the title capitalized).
#' @return A BibEntry object.

capitalize_refs <- function(refs, TitleCase = FALSE){

  refs.au <- capitalize_authors(refs)
  refs.title <- capitalize_titles(refs.au, titlecase = TitleCase)

  refs.title

}


#' Write Author Names in Title Case.
#'
#' This function capitalizes author names (given and family names), while setting all other characters to lower case.
#'
#' @import RefManageR
#' @importFrom tools toTitleCase
#' @param refs A \code{\link[RefManageR]{BibEntry}} object.
#' @export
#' @return A BibEntry object with author names in Title Case.
#' @author F. Rodriguez-Sanchez
#' @seealso \code{\link{capitalize_refs}}

capitalize_authors <- function(refs){

  for (i in 1:length(refs)){
    authors <- tools::toTitleCase(tolower(refs[[i]]$author))
    refs[[i]]$author <- paste(authors, collapse = " and ")
  }

  return(refs)

}





#' Fix capitalization of reference titles.
#'
#' This function changes capitalization of reference titles in a \code{\link[RefManageR]{BibEntry}} object to Title Case (first letter of each word is capitalized), or only first word capitalized.
#'
#' @import RefManageR
#' @importFrom tools toTitleCase
#' @importFrom Hmisc capitalize
#' @param refs A \code{\link[RefManageR]{BibEntry}} object.
#' @param titlecase Logical. Use Title Case (first letter of each word is capitalized)? Default is FALSE (only first letter of the title capitalized).
#' @export
#' @return A BibEntry object with titles capitalized according to \code{titlecase}.
#' @author F. Rodriguez-Sanchez
#' @seealso \code{\link{capitalize_refs}}, and \code{\link[tools]{toTitleCase}} and \code{\link[Hmisc]{capitalize}} for details.

capitalize_titles <- function(refs, titlecase = FALSE){

  for (i in 1:length(refs)){
    title.old <- gsub("\\{|\\}", "", tolower(refs[[i]]$title))
    refs[[i]]$title <- ifelse(isTRUE(titlecase),
                             tools::toTitleCase(title.old),
                             Hmisc::capitalize(title.old))
  }

  return(refs)

}


