
minibib
=======

Is your bibtex file massive compared to the number of references in your tex file? Try minibib!

Installation
------------

You can install minibib from github with:

``` r
# install.packages("devtools")
devtools::install_github("AnthonyEbert/minibib")
```

Usage
-----

Open an R session in the directory of your latex project. The single function of minibib, `minibib()` takes the bibtex file path as its argument, the function will search through your latex documents for citations and match those against the bibtex file supplied. The default filename of the output file is `main.bib`. It will be a subset of your original bibtex file for the references actually found in the text.
