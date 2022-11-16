
require(data.table)
require(missForest)
betas <- readRDS("master_beta_allplots.rds")
betas <- data.table(betas)

# betas : dataset with 32 columns (30 of trophic levels)


################
# Overview plot
################
# # is time intensive to plot all that
# visdat::vis_miss(betas)
# betas[, num_obs := Reduce(`+`, lapply(.SD,function(x) !is.na(x)))]
# hist(betas$num_obs)
# betas[, num_obs := NULL]


################
# check correlations
################
# include <- colnames(betas)[!colnames(betas) %in% c("Var1", "Var2")]
# m <- cor(betas[, ..include], use = "pairwise.complete.obs")
# 
# corrplot::corrplot(m,type="lower",addCoef.col = "black",method="color",diag=F, tl.srt=1, tl.col="black", mar=c(0,0,0,0), number.cex=0.6)
# tres <- 0.6
# m[which(m < tres & m > -tres)] <- 0
# corrplot::corrplot(m, type = "upper", tl.col="black", tl.srt=40, diag = F)
# 
# network <- igraph::graph_from_adjacency_matrix(m, weighted=T, mode="undirected", diag=F)
# plot(network)
# 
# rm(m); rm(network)

################
## Log transformation
################
# before changing values, store the original dataset. 
# Log transformation of the numeric columns. The natural logartithm of values smaller than 1 is negative. 
# Tehrefore, all values are shifted by 1 again to avoid any negative values in the log transformed dataset. 
# there are no negative values, no need to remove them. 

# betasbackup <- data.table::copy(betas)

numcols <- colnames(betas)[!colnames(betas) %in% c("Var1", "Var2")]

# log transform the values + 1, later shift them back.
betas[, (numcols) := lapply(.SD, function(x) log(x+1)), .SDcols = numcols]

# record NA value positions for visualistation
naloc <- is.na(betas)



################
# create factor column
################
# Plots would ideally been left in the imputation as factors, they contain very important information. 
# But missforest cannot handle categorical variables with more than 53 categories. 
# Plots have 150 categories. at least include "region comparisons" categories : 
# - A A
# - A H
# - A S
# - H H
# - H S
# - S S
# create region categories
betas[, regprep1 := substr(Var1, 1, 1)]
betas[, regprep2 := substr(Var2, 1, 1)]
betas[, region_comparison := paste(regprep1, regprep2)]
betas[, region_comparison := as.factor(region_comparison)]
betas[, regprep1 := NULL]; betas[, regprep2 := NULL]
include <- c(numcols, "region_comparison")



################
# imputation test
################
# Test with one imputation round: 
#   checking the errors and deciding if the imputation looks promising.
# current <- missForest::missForest(betas[1:100, ..include], variablewise = T)
# imperr <- data.table::data.table("columns" = colnames(betas[, ..include]), "errors" =current$OOBerror)
# ggplot(imperr, aes(x = columns, y = errors)) + 
#   geom_bar(stat = "identity") + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   ylim(0, 0.3) + geom_hline(yintercept=0.25, linetype="dashed", color = "red") +
#   ggtitle("Betadiversity imputation with 1000 plot comparisons")


################
# Imputation
################
# The imputation is repeated 50 times, and the mean of the imputed values is taken. 
impvals <- list()
imperr <- list()
dataset <- betas[, ..include]

for(i in 1:2){
  print(i)
  current <- missForest::missForest(dataset, variablewise = T)
  imperr[[i]] <- data.table::data.table("columns" = colnames(dataset), "errors" =current$OOBerror)
  current <- data.table::as.data.table(current$ximp)
  # re-transform the numeric variables
  current[, (numcols) := lapply(.SD, function(x) (exp(x)-1)), .SDcols = numcols]
  # add back the "Plotn" column which was taken out for imputation
  current[, Var1 := betas$Var1]
  current[, Var2 := betas$Var2]
  impvals[[i]] <- current
}
rm(current); rm(i)
# USER : may change filename
saveRDS(impvals, file="raw_imputed_betadiversity_values_complete.rds")
saveRDS(imperr, file = "raw_imputed_betadiversity_errors_complete.rds")