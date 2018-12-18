#' BetaDivMultifun
#'
#' @name BetaDivMultifun
#' @docType package
NULL



#' load and clean LUI dataset
#'
#' This function loads the LUI dataset and calculates the mean of a given
#' column (e.g. "LUI")
#' among all given years. The years are given in a column named "Year".
#' The input data needs to be standardised among years and plots.
#' It outputs a vector with the land-use intensity averages.
#' This code is written by Eric.
#'
#' @param dataset input file
#' @param path path to input file, ending in "/"
#' @return a vector with the land-use intensity averages.
#' @export
loadAndCleanLUI <- function(dataset="LUI06to10update20.07.16.txt", path) {
  lui5y  <<- read.table(paste(path, dataset, sep=""), header=T)
  with(lui5y, tapply(LUI, Plot, mean))
}
NULL


#' combine columns
#'
#' This function combines columns. It creates a new column which is the mean (or any given function)
#' of several columns. The old columns are deleted.
#' It can be used e.g. to combine data from several years.
#'
#' This code is written by Eric.
#'
#' @param xx input file
#' @param oldcols names of the columns to be combined, given as a vector
#' @param newcolname the name which the newly generated, combined column should have
#' @param merge.fun the name of the function to be used for column combination. e.g. "mean".
#' @return a merged column in the same dataset as given as input.
#' @export
colComb <- function(xx, oldcols, newcolname, merge.fun = "mean"){

  meanvals <- apply(xx[, match(oldcols, colnames(xx))], 1, match.fun(merge.fun))

  xx2 <- xx[, -match(oldcols, colnames(xx))]
  xx3 <- cbind(xx2, meanvals)
  colnames(xx3)[which(colnames(xx3) == "meanvals")] <- newcolname

  return(xx3)

}
NULL


#' load and clean functions dataset
#'
#' This function loads the functions dataset.
#' It removes all columns mentioned, by default those are all services
#' and the two functions "P loss 2015" and "soil C stock".
#'
#' The function automatically calculates shoot Nitrogen stocks and
#' shoot Phosphorous stocks, both from years 2009 and 2011. Both are calculated
#' by multiplication of the Biomass measured in the given year and the shoot
#' Nitrogen/ Phosphorous stock.
#'
#' The function further takes the means of variables measured in several
#' years. By default, those are : Nitrogen and Phosphorous stock,
#' Biomass, Phosphorous loss, Phoshporous shoot, Phoshporous in mycorrhiza
#'
#' Colcomb is used to calculate the mean level of functions which are
#' measured in more than one year. Those are: shootPstock, shootNstock,
#' Biomass, P_loss, Pshoot, Pmic.
#'
#' This code is written by Eric.
#'
#' @param dataset input file
#' @param path path to input file, ending in "/"
#' @param remove vector of column names to remove. Defaults to all services
#' and the two functions "P loss 2015" and "soil C stock".
#' @return a cleaned functions dataframe
#' @export
loadAndCleanEcosystemFunctions <- function(dataset="EP grass functions & services.txt", path,
                            remove = c("Total.flower.cover", "Charism.butterfly.Abund",
                                         "Charism.butterfly.SR", "SR.birds", "forage.quality",
                                         "daily.temp.range","daily.moisture.range", "P_loss2015", "Soil.C.stock")) {
  funct <- read.table(paste(path, dataset, sep=""), header=T)
  funct <- funct[order(funct$Plot),]

  # removing the mentioned columns
  funct2 <- funct[,-match(remove, names(funct))]

  # calculate aboveground stocks of N and P
  funct2$shootNstock09 <- funct2$Biomass2009 * funct2$Nshoot.2009
  funct2$shootNstock11 <- funct2$Biomass2011 * funct2$Nshoot.2011
  funct2$shootPstock09 <- funct2$Biomass2009 * funct2$Pshoot.2009
  funct2$shootPstock11 <- funct2$Biomass2011 * funct2$Pshoot.2011

  # take out plot and exploratory column
  remove <- c("Plot", "Exploratory")
  fonly <- funct2[, which(names(funct2) != remove)]

  # calculate means of functions which are measured in more than one year and combine to column.
  # delete old columns
  fonly2 <- colComb(fonly, oldcols = c("shootPstock09", "shootPstock11"), newcolname = "shootPstock")
  fonly <- colComb(fonly2, oldcols = c("shootNstock09", "shootNstock11"), newcolname = "shootNstock")
  fonly2 <- colComb(fonly, oldcols = c("Biomass2009", "Biomass2011", "Biomass2014"), newcolname = "Biomass")
  fonly <- colComb(fonly2, oldcols = c("P_loss2009", "P_loss2011"), newcolname = "P_loss")
  fonly2 <- colComb(fonly, oldcols = c("Pshoot.2009", "Pshoot.2011"), newcolname = "Pshoot")
  fonly <- colComb(fonly2, oldcols = c("Pmic.2011", "Pmic.2014"), newcolname = "Pmic")

  return(fonly)
}
NULL




#' Test if column is identical with reference dataset
#'
#' Tests wether the column given is identical with another column from a reference dataset.
#' The order of rows is not identical in both columns, therefore a simple comparison with
#' \code{identical()} is not possible. 
#' The columns compared have the same name "name". 
#' Both datasets have a column (e.g. with the name "Plot"), by which they can be sorted.
#' Is used in BetaDivMultifun to test wether the dataset content is identical with 
#' the Synthesis dataset "Info_data_EP grass functions & services.xlsx", BExIS number
#' 
#'
#'
#' @param name names of the columns which will be compared.
#' @param tocompareset name of the dataset which is compared (dataset 1). Is a data.table.
#' @param refset name of the reference dataset (dataset 2). This dataset can be much larger
#' than \code{tocompareset}, only the two columns of interest are filtered out. 
#' Is a data.table.
#' @param sortcol name of the column which is used to sort rows
#' @return Boolean value which indicates wheter the columns are identical or not.
#' @export
CompareToReferenceDataset <- function(tocompareset, refset, name, sortcol="Plot"){
  refset <- refset[,c(sortcol, name), with=F]
  data.table::setnames(refset, old="Plot", new="Plotn")
  return(all.equal(refset, tocompareset[,c("Plotn", name), with=F], ignore.row.order=T))
}
NULL

