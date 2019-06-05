#' Multiple imputation loop
#'
#' *Note*: this is a very specific function, only useful in the context of the
#' analysis as described in the vignette of function imputation. It requires an
#' already prepared dataset (no negative values, log transformed). 
#' The names of columns with negative values are stored.
#' This function performes im imputations with missforest. The resulting iputed
#' dataset is stored in a list "impvals" of length im. The variable-wise imputation
#' error (missforest parameter variablewise = T) is stored in the list "imperr".
#' 
#' The following variables need to be defined before use of the function : 
#' `mindea` : from variable DEA, the minimum (below 0) before shifting the
#' whole variable to be positive.
#' `minno` : from variable NO3.2014, the minimum before shifting the whole 
#' column to be positive.
#' `mindung` : minimum dung.decomposition value to shift back the vector to
#' have values above zero.

#' @param dataset The dataset with the missing values which will be imputed.
#' All variables need to be logtransfomed and negative values need to be shifted,
#' according to the vignett.
#' @param numcols The names of the columns from `dataset` which are numeric.
#' @param i the numer of imputations
#' 
#' @return A list containing two lists, `impvals` and `imperr`, both containing data tables. 
#' `imperr` contains the variablewise imputation errors, which `impvals` 
#' contains the imputed values. Note that they are already back-transformed
#' (exp) and negative values are shifted back in.
#' 
#' @examples 
#' see vignette
#' 
#' @export
perform_missforest_imputations <- function(dataset, numcols, im=1){
  impvals <- list()
  imperr <- list()
  for(i in 1:im){
    print(i)
    current <- missForest::missForest(dataset, variablewise = T)
    imperr[[i]] <- data.table::data.table("columns" = colnames(dataset), "errors" =current$OOBerror)
    current <- data.table::data.table(current$ximp)
    # re-transform the numeric variables
    current[, (numcols) := lapply(.SD, function(x) (exp(x)-1)), .SDcols = numcols]
    # re-transform the negative values
    current$DEA <- current$DEA - abs(mindea)
    current$NO3.2014 <- current$NO3.2014 - abs(minno)
    current$dung.decomposition <- current$dung.decomposition - abs(mindung)
    # add back the "Plotn" column which was taken out for imputation
    current[, Plotn := befun$Plotn]
    # convert imputed data.table to matrix, as more handy for imputed values handling
    impvals[[i]] <- current
  }
  rm(current); rm(i)
  returnobj <- list("values" = impvals, "errors" =imperr)
  return(returnobj)
}

NULL