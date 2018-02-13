
#' Return a list of references is contained in a set of tex files and write that list to a new bibtex file.
#' @param bibtex character path to bibtex file
#' @param write_bib a logical value. If TRUE, writes a bibtex file with the references contained in the tex file.
#' @param path character vector of full path names
#' @param pattern a regular expression. File names matching the regular expression are returned.
#' @param recursive a logical value. Should subdirectories be searched for tex files as well?
#' @param ... further arguments passed to write.bib in the bibtex package
#' @export
contained_in_tex <- function(bibtex, write_bib = FALSE, path = ".", pattern = "\\.tex$", recursive = TRUE, ...){
  x = bibtex::read.bib(bibtex)

  y = sapply(
    names(x),
    grepl,
    paste(
      sapply(
        list.files(
          path = path,
          pattern = pattern,
          recursive = recursive,
          full.names = TRUE
        ),
        readChar,
        nchars = 1e7
      ),
      collapse = ' '
    )
  )

  if(write_bib){
    bibtex::write.bib(x[names(x) %in% names(y[which(y)])], ...)
  }

  return(y)
}

