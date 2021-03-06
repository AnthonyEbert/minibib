
#' Return a list of references is contained in a set of tex files and write that list to a new bibtex file.
#' @param bibtex character path to bibtex file.
#' @param write_bib a logical value. If TRUE, writes a bibtex file with the references contained in the tex file.
#' @param path character vector of full path names.
#' @param pattern a regular expression. File names matching the regular expression are returned.
#' @param recursive a logical value. Should subdirectories be searched for tex files as well?
#' @param file character filename of output bibtex file.
#' @param type character specification of type of reference matching. Available choices are \code{"tex"} and \code{"Rmd"}.
#' @param ... further arguments passed to write.bib in the bibtex package.
#' @export
minibib <- function(bibtex, write_bib = TRUE, path = ".", pattern = "\\.tex$", recursive = TRUE, file = "main.bib", type = "tex", ...){
  x = bibtex::read.bib(bibtex)

  if(type == "tex"){
    input <- paste0("(\\{|,|\\s)", names(x), "(,|\\s|\\})")
  } else if(type == "Rmd"){
    input <- paste0("@", names(x), "(,|\\s|\\])")
  }

  y = sapply(
    input,
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

  names(y) <- names(x)

  if(write_bib){
    bibtex::write.bib(x[names(x) %in% names(y)[which(y)]], file = file, ...)
  }

  return(y)
}

