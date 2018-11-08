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
