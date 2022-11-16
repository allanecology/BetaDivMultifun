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
#' Note : special case if the input dataset does not contain all Plots, if certain plots do not 
#' contain any species. This special case is detected and solved in this function.
#' 
#' @param diversity_dataset A data.table of soilfuni or bacteria in the format of the synthesis diversity 
#' dataset, with at least the columns Plot, Species and value.
#' #' @param all_plots a character vector containing the complete set of plots from the 
#' Exploratories. (AEG 1-50, HEG 1-50 and SEG 1-50). Is needed for the subfunction
#' find_and_supplement_missing_plots.
#' @return A data.table in the same format as diversity_dataset, but with the previously
#' missing plots x species added back as 0. Note : Only the columns Plot, Species and value are
#' considered, any other columns will be filled with NA.
#' @export
add_back_missing_plots_species_combinations <- function(diversity_dataset, all_plots){
  # Control : check if there are any NA values, although none would be expected
  if(length(diversity_dataset[is.na(value), value]) > 0){
    stop("There are NA values in the dataset. Missing combinations can not be added.")
  }
  # Control : the provided dataset needs to be a data.table.
  if(!data.table::is.data.table(diversity_dataset)){
    stop("The provided dataset is not a data.table. Please first convert it to data.table with
         data.table(diversity_dataset).")
  }
  # check if the dataset contains all plots, or if plots need to be added manually
  if(length(unique(diversity_dataset$Plot)) < length(all_plots)){ 
    diversity_dataset <- find_and_supplement_missing_plots(diversity_dataset, all_plots)
  }
  
  # add back zeros
  diversity_dataset <- data.table::setDT(diversity_dataset)[data.table::CJ(Species = Species, Plot = Plot, unique = T), on=.(Species, Plot)]
  print(paste(nrow(diversity_dataset[is.na(value)]), "missing combinations were added."))
  diversity_dataset <- diversity_dataset[is.na(value), value := 0 ][] # note about the ending [] :
      # this is a reported bug from data.table, if not added, the function will not return the data.table
      # at first attempt (only at the second). https://stackoverflow.com/questions/32988099/data-table-objects-assigned-with-from-within-function-not-printed
  return(diversity_dataset)
}
NULL


#' Fill number of plots to 150
#' 
#' Subfunction for add_back_missing_plots_species_combinations
#' A dataset from soilfungi or bacteria can miss some plots. This happens if there are plots
#' without any species (e.g. for symbiont.soilfungi in 2011).
#' The dataset needs to be extended with the missing plots in order to use the function
#' `add_back_missing_plots_species_combinations`, because this function only adds the missing
#' plot x species combination, not completely missing plots.
#' 
#' @param diversity_dataset a species diversity data.table in the format of the
#' exploratories synthesis diversity dataset. It contains at least the columns Plot, 
#' Species and value.
#' @param all_plots a character vector containing the complete set of plots from the 
#' Exploratories. (AEG 1-50, HEG 1-50 and SEG 1-50).
#' @return a data.table consisting of the unchanged diversity_dataset as it has been provided 
#' as input, rbinded to a minimal data.table containing a random species and the 
#' missing plots.
find_and_supplement_missing_plots <- function(diversity_dataset, all_plots){
  if(length(unique(diversity_dataset$Plot)) == length(all_plots)){
    print("The dataset already contains the number of expected plots, no need to supplement missing ones.")
  } else {
    # find the set of missing plots and store to vector
    # get random species to be supplemented (only 1 is needed, all missing combinations
    # will be supplemented by add_back_missing_plots_species_combinations)
    # create a supplement dataset
    missing_plots <- all_plots[!all_plots %in% unique(diversity_dataset$Plot)]
    random_species <- diversity_dataset$Species[1]
    complementary_dataset <- data.table::data.table(Plot = missing_plots, Species = random_species, value = 0)
    diversity_dataset <- data.table::rbindlist(list(diversity_dataset, complementary_dataset), fill = T, use.names = T)
    # check if number of plots is the expected 150 now
    if(length(unique(diversity_dataset$Plot)) != length(all_plots)){
      stop("The number of plots is not the expected number. Check length of vector all_plots.")
    }
    return(diversity_dataset)
  }
}
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




#' beta.pair handle plots without species
#' 
#' The function `betapart::beta.pair()` can not handle plot without species.
#' The turnover and nestedness components following Baselga, 2010
#' are set to NaN due to division by 
#' zero in the formula. This applies to both cases (1) both plots do not contain
#' any species and (2) one of the plots does not contain any species.
#' This cases were not expected and therefore not addressed in the betapart
#' package.
#' Here, the two above described exceptions are addressed according to the
#' biological expectation behind.
#' 
#' This function aims to adress the special cases in beta.pair() without changing the 
#' overall behaviour of the function.
#' 
#' Betadiversity can be set to 0 or 1 in the described cases : 
#' 
#' If both plots do not contain any species, according to e.g. Carlo Ricotta
#' https://doi.org/10.1002/ece3.2980 , this can be interpreted as the
#' ultimate loss of betadiversity, and thus betadiversity and its components
#' can be set to 0.
#' e.g. Plot "P1" contains 3 species, "P2" and "P3" do not contain any species : 
#' P1 : 1 1 1 0 0 0 0
#' P2 : 0 0 0 0 0 0 0
#' P3 : 0 0 0 0 0 0 0
#' 
#' betadiversity(P2, P3) is set to beta.sor = 0 = beta.sne + beta.sim
#' 
#' If one plot contains species and the other not, the difference is 
#' pure nestedness, i.e. the maximal possible amount of nestedness, because
#' 100% of the difference among plots is nestedness.
#' betadiversity(P1, P2) is set to beta.sne = 1, beta.sim = 0, beta.sor = 1
#' 
#' What is different compared to the beta.pair function?
#' Cases of plots with zero species are catched by the output of betapart.core.
#' The cases are set to 0 or 1, as described above.
#' 
#' note that some tests of the function are provided as example.
#' 
#' @param x a data.set with same requirements as for beta.pair. *Note* for the project
#' BetaDivMultifun, the function `prepare_for_betapair` can be used. For more 
#' information, please read the documentation for beta.pair.
#' @param index.family is "sorensen" as set by default. Originally, it could be either
#' "sorensen" or "jaccard", but no additional functionality has been 
#' implemented for the "jaccard" index.
#' @return same as for function beta.pair. A list with three dissimilarity matrices,
#' being beta.sim, beta.sne and beta.sor. For more information, please read the
#' documentation for beta.pair.
#' @examples
#' TESTING
#' prepare test dataset
#' test <- data.table(Species = rep(c("S1", "S2", "S3", "S4", "S5"), 4), 
#'                    Plot = paste("P", sort(rep(seq(1, 4), 5)), sep = ""),
#'                    value = c(rep(0, 5), 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, rep(0, 5)))
#' test <- prepare_for_betapair(test)
#' old_res <- betapart::beta.pair(test, index.family = "sorensen") # producin NaN s
#' res <- beta.pair_zerospecies(test, index.family = "sorensen")
#' 
#' # all tests need to be TRUE
#' all(!any(is.nan(res$beta.sim)), !any(is.nan(res$beta.sne)), !any(is.nan(res$beta.sor))) # there are no more NaN values in the dataset any more.
#' all(res$beta.sne[is.nan(old_res$beta.sne)] %in% c(0, 1)) # previously NaN are now 0 or 1
#' all(res$beta.sor[is.nan(old_res$beta.sne)] %in% c(0, 1)) # previously NaN are now 0 or 1
#' all(res$beta.sim[is.nan(old_res$beta.sne)] %in% c(0)) # previously NaN are now 0 or 1
#' all(res$beta.sor[is.nan(old_res$beta.sne)]== res$beta.sne[is.nan(old_res$beta.sne)]) # beta.sor. and beta.sne are the same
#' @export
beta.pair_zerospecies <- function (x, index.family = "sorensen") 
{
  index.family <- match.arg(index.family, c("jaccard", "sorensen"))
  if (!inherits(x, "betapart")) {
    x <- betapart::betapart.core(x)
  }
  switch(index.family, sorensen = {
    # catch exceptions :
    double_zero <- x$min.not.shared == 0 & x$shared == 0 & x$max.not.shared == 0 # no unshared species, no shared species --> both plots don't have species
    one_zero <- x$shared == 0 & x$max.not.shared != 0 & x$min.not.shared == 0 # zero shared, something not shared : 1 plot without species
    
    beta.sim <- x$min.not.shared/(x$min.not.shared + x$shared)
    # EDIT special cases :
    beta.sim[double_zero] <- 0
    beta.sim[one_zero] <- 0
    
    beta.sne <- ((x$max.not.shared - x$min.not.shared)/((2 * 
                                                           x$shared) + x$sum.not.shared)) * (x$shared/(x$min.not.shared + 
                                                                                                         x$shared))
    # EDIT special cases
    beta.sne[double_zero] <- 0
    beta.sne[one_zero] <- 1
    
    beta.sor <- x$sum.not.shared/(2 * x$shared + x$sum.not.shared)
    # EDIT special cases
    beta.sor[double_zero] <- 0
    # beta.sor[one_zero] <- 1 # formula can handle one plot without species and calculate beta.sor.
    
    pairwise <- list(beta.sim = as.dist(beta.sim), beta.sne = as.dist(beta.sne), 
                     beta.sor = as.dist(beta.sor))
  }, jaccard = {
    print("There is no special case handling for jaccard index yet. Please use the original function
          beta.pair instead.")
    beta.jtu <- (2 * x$min.not.shared)/((2 * x$min.not.shared) + 
                                          x$shared)
    beta.jne <- ((x$max.not.shared - x$min.not.shared)/(x$shared + 
                                                          x$sum.not.shared)) * (x$shared/((2 * x$min.not.shared) + 
                                                                                            x$shared))
    beta.jac <- x$sum.not.shared/(x$shared + x$sum.not.shared)
    pairwise <- list(beta.jtu = as.dist(beta.jtu), beta.jne = as.dist(beta.jne), 
                     beta.jac = as.dist(beta.jac))
  })
  return(pairwise)
}
NULL






#' beta.pair.abund handle plots without species
#' 
#' Only minimal description is provided, More info in beta.pair_zerospecies. This function
#' was generated similarly.
#' Gradient corresponds to nestedness, balanced to turnover in respect of zeros.
#' 
#' @param x a data.set with same requirements as for beta.pair.abund *Note* for the project
#' BetaDivMultifun, the function `prepare_for_betapair` can be used. For more 
#' information, please read the documentation for beta.pair.
#' @param index.family is "bray" as set by default. Not implemented for "ruzicka"
#' @return same as for function beta.pair.abund A list with three dissimilarity matrices.
#' For more information, please read the
#' documentation for beta.pair.
beta.pair.abund_zerospecies <- function (x, index.family = "bray") 
{
  index.family <- match.arg(index.family, c("bray", "ruzicka"))
  if (!inherits(x, "betapart.abund")) {
    x <- betapart.core.abund(x)
  }
  switch(index.family, bray = {
    # EDIT catch exceptions :
    double_zero <- x$pair.shared.abund == 0 & x$pair.sum.not.shared.abund == 0 & x$pair.max.not.shared.abund == 0 # no unshared species, no shared species --> both plots don't have species
    one_zero <- x$pair.shared.abund == 0 & x$pair.max.not.shared.abund != 0 & x$pair.min.not.shared.abund == 0 # zero shared, something not shared : 1 plot without species
    
    beta.bray.bal <- x$pair.min.not.shared.abund/(x$pair.min.not.shared.abund + 
                                                    x$pair.shared.abund)
    # EDIT special cases :
    beta.bray.bal[double_zero] <- 0
    beta.bray.bal[one_zero] <- 0
    
    beta.bray.gra <- ((x$pair.max.not.shared.abund - x$pair.min.not.shared.abund)/((2 * 
                                                                                      x$pair.shared.abund) + x$pair.sum.not.shared.abund)) * 
      (x$pair.shared.abund/(x$pair.min.not.shared.abund + 
                              x$pair.shared.abund))
    # EDIT special cases
    beta.bray.gra[double_zero] <- 0
    beta.bray.gra[one_zero] <- 1
    
    beta.bray <- x$pair.sum.not.shared.abund/(2 * x$pair.shared.abund + 
                                                x$pair.sum.not.shared.abund)
    # EDIT special cases
    beta.bray[double_zero] <- 0
    # beta.bray[one_zero] # formula can handle one plot without species
    
    pairwise <- list(beta.bray.bal = as.dist(beta.bray.bal), 
                     beta.bray.gra = as.dist(beta.bray.gra), beta.bray = as.dist(beta.bray))
  }, ruzicka = {
    print("not defined for zerospecies - please use original function")
  })
  return(pairwise)
}
