#' Encode author names in UTF-8.
#'
#' Encode author names in BibEntry objects as UTF-8. Specially useful when working in Windows systems that do not support UTF-8.
#'
#' @import RefManageR
#' @param refs A BibEntry object.
#' @export
#' @return A BibEntry object.
#' @examples \dontrun{
#' library(knitcitations)
#' cleanbib()
#' cite_options(citation_format = "pandoc")
#' #citet("10.1111/nph.12929") # doesn't work
#' citep("10.1016/j.tree.2006.09.010")
#' citet("10.1111/j.1461-0248.2007.01060.x")
#' ref <- knitcitations:::get_bib()
#' ref.utf8 <- BibEntry_to_UTF8(ref)
#'
#'}


BibEntry_to_UTF8 <- function(refs){

  for (i in 1:length(refs)){
    authors <- paste(refs[[i]]$author, collapse = " and ")
    refs[[i]]$author <- iconv(authors, to = "UTF-8")
  }

  for (i in 1:length(refs)){
    refs[[i]]$title <- iconv(refs[[i]]$title, to = "UTF-8")
  }


  for (i in 1:length(refs)){
    refs[[i]]$journal <- iconv(refs[[i]]$journal, to = "UTF-8")
  }

  refs

}


