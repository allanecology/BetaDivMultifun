# make example with soilfungi symbionts, there are 8 plots without species
test <- data.table(aggregate(value ~ Plot, symbiont.soilfungi, function(x) sum(x)))
barplot(test$value)
zeroplots <- test[value == 0, Plot] # 8 plots witout species
# prepare minimal example for re-coding betadiversity calculation and handling special cases
# tr <- symbiont.soilfungi[Plot %in% unique(c(plotNAset[1:4], zeroplots))]
tr <- symbiont.soilfungi[Plot %in% unique(c(plotNAset[1], zeroplots[1:2]))]
a <- unique(tr$Species)[1:3]
tr <- tr[Species %in% a, ]# filter species

b.tr <- prepare_for_betapair(tr)
b.tr[1, 2] <- 1
b.tr[1, 1] <- 1
b.tr[2, 1] <- 1

betapart::beta.pair(b.tr)


# AEG 2 vs. HEG 7 und vs. HEG11 : nestedness = 1, turnover = 0
# heg 7 vs. 11 : 000

# sim = turnover
require(betapart)


x <- b.tr
x <- prepare_for_betapair(symbiont.soilfungi)

# update of the function betapart, only sorensen part of formula
#TODO next : check with larger datasets, check for bugs
# change for plots without species : 
# - two plots without species : set all betadiversities to 0.
# - one plot without species : set beta nestedness to 1, beta turnover to 0 and beta.sor to 1 as well.

# catch cases of plots without species in betapart::beta.pair() which are NaN by default and set to  0 or 1.

beta.pair_zerospecies <- function (x, index.family = "sorensen") 
{
  index.family <- match.arg(index.family, c("jaccard", "sorensen"))
  if (!inherits(x, "betapart")) {
    x <- betapart.core(x)
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



# TESTING
# prepare test dataset
test <- data.table(Species = rep(c("S1", "S2", "S3", "S4", "S5"), 4), 
           Plot = paste("P", sort(rep(seq(1, 4), 5)), sep = ""),
           value = c(rep(0, 5), 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, rep(0, 5)))
test <- prepare_for_betapair(test)
old_res <- betapart::beta.pair(test, index.family = "sorensen") # producin NaN s
res <- beta.pair_zerospecies(test, index.family = "sorensen")

# all tests need to be TRUE
all(!any(is.nan(res$beta.sim)), !any(is.nan(res$beta.sne)), !any(is.nan(res$beta.sor))) # there are no more NaN values in the dataset any more.
all(res$beta.sne[is.nan(old_res$beta.sne)] %in% c(0, 1)) # previously NaN are now 0 or 1
all(res$beta.sor[is.nan(old_res$beta.sne)] %in% c(0, 1)) # previously NaN are now 0 or 1
all(res$beta.sim[is.nan(old_res$beta.sne)] %in% c(0)) # previously NaN are now 0 or 1
all(res$beta.sor[is.nan(old_res$beta.sne)]== res$beta.sne[is.nan(old_res$beta.sne)]) # beta.sor. and beta.sne are the same
        