#' adjust rownames to lui or components
#'
#' If LUI is given, delta and sd is only shown for LUI itself. 
#' If components are given, delta F, delta M and delta G  as well as sd F, sd M and
#' sd G are given.
#' This function checks which lui input is given and adds the required placeholders
#' to the vector.
#' This will make possible the construction of restab accordingly.

#' @param luiinput a character vector, either "lui" or "components"
#' @return `plotsequence_abio`, a character vector containing the names for the LUI rows in `restab`, 
#' the table used for plotting.
#' 
#' @export
define_rownames_lui_or_components <- function(luiinput){
  if(luiinput == "lui"){
    plotsequence_abio <- c("LUI", "deltaLUI", "soil", "isolation", "geo")
  }else if(luiinput == "components"){
    plotsequence_abio <- c(paste(c("G", "M", "F"), "std", sep = ""), paste("delta", c("Gstd", "Mstd", "Fstd"), sep = ""),
                           "soil", "isolation", "geo")
  }
  return(plotsequence_abio)
}

NULL

#' Define plotsequence bio
#' 
#' Define the order of trophic levels used for plotting (manually)
#' 
#' 