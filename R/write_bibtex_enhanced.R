#' Fix references and write BibTeX file.
#'
#' An enhanced version of \code{\link[knitcitations]{write.bibtex}} aimed at fixing capitalization styles of references, and overcoming problems with non-UTF-8 encoding (particularly in Windows systems).
#'
#' @param entry a 'bibentry' object or list of bibentry objects.
#'        If NULL, writes all that have currently been cited.
#' @param file output bibtex file. Will automatically append '.bib' if not
#'  added. if 'NULL' will use stdout.
#' @param append a logical indicating that bibtex entries be added the the
#'  file.  If FALSE (default), the file is overwritten.
#' @param utf8 Logical. If TRUE, convert author names to UTF-8 encoding.
#' @param capitalize Logical. If TRUE, fix capitalization of references (see \code{\link{capitalize_refs}}).
#' @param ... additional arguments to WriteBib
#' @return a list of citation information, invisibly
#' @import RefManageR
#' @import knitcitations
#' @examples
#'  write_bibtex_utf8(c(citation("knitr"),
#'                 citation("knitcitations"),
#'                 citation("RCurl")))
#' @export
#' @author Carl Boettiger, modified by F. Rodriguez-Sanchez.
#' @seealso \code{\link{capitalize_refs}} and \code{\link{BibEntry_to_UTF8}}.

write_bibtex_enhanced <- function(entry = NULL,
                         file = "references.bib",
                         append = FALSE,
                         utf8 = FALSE,
                         capitalize = TRUE,
                         ...){

  if(is.null(entry)){
    entry <- knitcitations:::get_bib()

  } else if(is(entry, "bibentry")){
    entry <- as.BibEntry(entry)

  } else {
    stop(paste("entry object of class", class(entry), "not recognized"))

  }

  if (utf8){
    entry <- BibEntry_to_UTF8(entry)
  }

  if (capitalize){
    entry <- capitalize_refs(entry)
  }

  WriteBib(entry, file=file, append=append, ...)
}



