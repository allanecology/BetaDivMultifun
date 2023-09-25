#' Change Exploratories plot names without zeros to plot names with zeros
#
#' This function adds the sortable plot names to an existing dataset.
#' Based on a column with Bexis plot names (e.g. AEG1), the function creates a column
#' with sortable plot names (e.g. AEG01). 
#' Note: This function does the opposite of the "BEplotNonZeros.R" function
#'
#' @param dat a dataset (data.frame) with at least one column with Bexis plot names (e.g. "AEG1)
#' @param column the column name of the column containing the Bexis plot names
#' @param plotnam the desired name of the new column with sortable plot names. Default is "PlotSTD"
#' @return the same dataset with an extra column containing the sortable plot names (e.g. "AEG01)
#' @author Caterina Penone
#' 
#' @examples
#' #create a dataset with a plot name column
#' dat <- data.frame(Plot_name = c("AEG1", "AEG2", "HEW4", "SEG8", "SEW10"), Values=1:5)
#' dat <- BEplotZeros(dat, "Plot_name", plotnam = "Sorted_plot_name")
#' 
#' @export
BEplotZeros <- function (dat, column, plotnam="PlotSTD"){
  dat <- as.data.frame(dat)
  funz <- function(x) ifelse((nchar(as.character(x))==4), gsub("(.)$", "0\\1", x),as.character(x))
  dat[,plotnam] <- sapply(dat[,column],funz)
  
  return(dat)
}
