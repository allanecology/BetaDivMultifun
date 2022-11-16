#' set nonpublic variables
#'
#' This function sets some global variables which are useful
#' for a lazy use of this package. Set those variables once and
#' they will be used as defaults in the package.
#'
#' @param accessBexispath the path to the file "accesBexis.R".
#' This file is available at BExIS and contains code to import data 
#' directly from R.
#' @param pathtoRAWdata path to the data used for this analysis. This
#' is where the raw data, downloaded from BExIS is stored. It can be
#' the same folder as pathtoASSEMBLEDdata or not.
#' @param pathtoASSEMBLEDdata path to the data used for this analysis. 
#' This path points to data which is not originally from BExIS but 
#' has been assembled by someone already. It can be
#' the same folder as pathtoRAWdata or not.
#' @return a message that the variables have been set
#' @export
setNonpublicVariables <- function(accessBexispath = ".",
                                  pathtoRAWdata=".",
                                  pathtoASSEMBLEDdata = ".") {
  accessBexispath <<- accessBexispath
  pathtoRAWdata <<- pathtoRAWdata
  pathtoASSEMBLEDdata <<- pathtoASSEMBLEDdata
  return(paste("global variables have been set to ", "accessBexispath: ",
        accessBexispath, "pathtoRAWdata: ", pathtoRAWdata, "pathtoASSEMBLEDdata: ", pathtoASSEMBLEDdata))
}



#' Sourcing a Rmarkdown document
#'
#' Function extracted from : https://stackoverflow.com/questions/10966109/how-to-source-r-markdown-file-like-sourcemyfile-r
#' Sources an Rmd file by creating a temporary R file.
#' Aim : source Rmd files as .R files (not possible at this moment)
#'
#' Example use (not run) : ksource("vignettes/prepare_and_run_GDM.Rmd")
#'
#' @param x path to the Rmd file
#' @return the output of the sourced file
#' @export
ksource <- function(x, ...) {
  source(purl(x, output = tempfile()), ...)
}

NULL