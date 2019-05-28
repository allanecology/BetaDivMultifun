#' Prepare for beta.part
#'
#' This function prepares the diversity data stored in a data.table for calculation
#' of betadiversity by `betapart::beta.pair`. 
#' 
#' @param datatable Is a data.table in which the following 3 columns are present : 
#' Species, Plot and value. Species describes the taxon whose presence or absence is 
#' assessed. Plot is the (useful) Name of the site, in the Biodiversity 
#' Exploratories, there are maximim 150 different Plots possible. Value is either
#' 0 or 1, where 1 means that the Species in the according row is present in the 
#' according Plot. In case the data table has more columns, this does not matter.
#' @return A data.frame which corresponds to the input requirements of `beta.pair`.
#' This means, rownames are Plots, Column names are species. And all data in the 
#' data frame is numeric.
#' @export
prepare_for_betapair <- function(datatable){
  datatable <- data.table::dcast.data.table(datatable, Plot ~ Species)
  datatable <- as.data.frame(datatable)
  rownames(datatable) <- datatable$Plot
  datatable$Plot <- NULL
  return(datatable)
}
NULL