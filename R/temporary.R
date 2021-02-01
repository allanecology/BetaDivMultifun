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

betapart::beta.pair(b.tr)


# AEG 2 vs. HEG 7 und vs. HEG11 : nestedness = 1, turnover = 0
# heg 7 vs. 11 : 000

# sim = turnover



x <- b.tr
x[1, 1] <- 1


# update of the function betapart
#TODO next : check with larger datasets, check for bugs

function (x, index.family = "sorensen") 
{
  index.family <- match.arg(index.family, c("jaccard", "sorensen"))
  if (!inherits(x, "betapart")) {
    x <- betapart.core(x)
  }
  switch(index.family, sorensen = {
    double_zero <- x$min.not.shared == 0 & x$shared == 0
        # no unshared species, no shared species --> both plots don't have species
    beta.sim <- x$min.not.shared/(x$min.not.shared + x$shared)
    beta.sim[double_zero] <- 0
    # zero shared, zero not shared : plot without species
    # zero shared, something not shared : 1 plot without species
    one_zero <- x$shared == 0 & x$max.not.shared
    
    beta.sne <- ((x$max.not.shared - x$min.not.shared)/((2 * 
                                                           x$shared) + x$sum.not.shared)) * (x$shared/(x$min.not.shared + 
                                                                                                         x$shared))
    beta.sne[one_zero] <- 1
    beta.sor <- x$sum.not.shared/(2 * x$shared + x$sum.not.shared)
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