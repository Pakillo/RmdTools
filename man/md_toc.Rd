% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/md_toc.R
\name{md_toc}
\alias{md_toc}
\title{Make linked table of contents in markdown}
\usage{
md_toc(path = {     (x <- dir())[tools::file_ext(x) == "md"] })
}
\arguments{
\item{path}{Character. Path to the markdown file.}
}
\value{
A markdown file.
}
\description{
This function takes a markdown file and creates links for every section in the table of contents (TOC).
}
\author{
Tyler Rinker
}
\seealso{
\link{https://github.com/trinker/testing_Rmd} & \link{http://stackoverflow.com/questions/29997263/linked-table-of-contents-toc-in-md-using-rmarkdown}
}

