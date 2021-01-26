#' Add back missing plots x species combinations
#' 
#' date of creation : 26.01.21
#' In order to save storage, the diversity dataset for bacteria and soil fungi o not include all combinations
#' of plots x species when a given species was not found in a given plot i.e. they do not contain zeros.
#' They need to be added back in order to gain the complete set of studied plots (missing plots x
#' species are not NA but measured, just the given species was not found).
#' Code from Caterina Penone, for further explanations and rationale
#' see https://github.com/biodiversity-exploratories-synthesis/Synthesis-dataset-manual/blob/main/Synthesis%20datasets%20%20How%20to%20use.md
#' Note : Only the columns Plot and Species are considered, any other columns will be filled with NA.
#' This can cause problems if the dataset is later filtered for e.g. Year.
#' 
#' @param diversity_dataset A data.table of soilfuni or bacteria in the format of the synthesis diversity 
#' dataset, with at least the columns Plot, Species and value.
#' @return A data.table in the same format as diversity_dataset, but with the previously
#' missing plots x species added back as 0. Note : Only the columns Plot, Species and value are
#' considered, any other columns will be filled with NA.
#' @export
add_back_missing_plots_species_combinations <- function(diversity_dataset){
  # Control : check if there are any NA values, although none would be expected
  if(length(diversity_dataset[is.na(value), value]) > 0){
    stop("There are NA values in the dataset. Missing combinations can not be added.")
  }
  # Control : the provided dataset needs to be a data.table.
  if(!data.table::is.data.table(diversity_dataset)){
    stop("The provided dataset is not a data.table. Please first convert it to data.table with
         data.table(diversity_dataset).")
  }
  # add back zeros
  diversity_dataset <- data.table::setDT(diversity_dataset)[data.table::CJ(Species = Species, Plot = Plot, unique = T), on=.(Species, Plot)]
  print(paste(nrow(diversity_dataset[is.na(value)]), "missing combinations were added."))
  diversity_dataset <- diversity_dataset[is.na(value), value := 0 ][] # note about the ending [] :
      # this is a reported bug from data.table, if not added, the function will not return the data.table
      # at first attempt (only at the second). https://stackoverflow.com/questions/32988099/data-table-objects-assigned-with-from-within-function-not-printed
  return(diversity_dataset)
}

#TODO : add this part to the adding plots
#  note : needs all_plots, a vector containing the whole list of explo plots.
length(unique(diversity_dataset$Plot)) # if > 0, not all plots included
# adding rows with the missing plots : 
# any random species
# value is 0
missing_plots <- all_plots[!all_plots %in% unique(diversity_dataset$Plot)]
random_species <- diversity_dataset$Species[1]
complementary_dataset <- data.table(Plot = missing_plots, Species = random_species, value = 0)

# add the complementary dataset to the original one
diversity_dataset <- rbindlist(list(diversity_dataset, complementary_dataset), fill = T, use.names = T)
diversity_dataset <- add_back_missing_plots_species_combinations(diversity_dataset)

length(unique(diversity_dataset$Plot))


NULL


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