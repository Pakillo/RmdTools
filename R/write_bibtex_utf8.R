#' Encode authors names in UTF-8 and write BibTeX file.
#'
#' Modified from \code{\link[knitcitations]{write.bibtex}} to overcome problems with non-UTF-8 encoding in Windows systems.
#'
#' @param entry a 'bibentry' object or list of bibentry objects.
#'        If NULL, writes all that have currently been cited.
#' @param file output bibtex file. Will automatically append '.bib' if not
#'  added. if 'NULL' will use stdout.
#' @param append a logical indicating that bibtex entries be added the the
#'  file.  If FALSE (default), the file is overwritten.
#' @param ... additional arguments to WriteBib
#' @return a list of citation information, invisibly
#' @import RefManageR
#' @examples
#'  write_bibtex_utf8(c(citation("knitr"),
#'                 citation("knitcitations"),
#'                 citation("RCurl")))
#' @export
#' @author Carl Boettiger, slightly modified by F. Rodriguez-Sanchez.

write_bibtex_utf8 <- function(entry = NULL,
                         file = "references.bib",
                         append = FALSE,
                         ...){

  if(is.null(entry)){
    entry <- get_bib()

  } else if(is(entry, "bibentry")){
    entry <- as.BibEntry(entry)

  } else {
    stop(paste("entry object of class", class(entry), "not recognized"))

  }

  entry.utf8 <- BibEntry_to_UTF8(entry)

  WriteBib(entry.utf8, file=file, append=append, ...)
}



