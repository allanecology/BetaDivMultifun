require(gdm)
require(car)
library(geosphere)
library(RColorBrewer)
library(viridis)

# diversities, functions, covariate soil loaded as dissimilarity matrices
funs <- "edis_glsoil"
names_of_matrices <- c("edis_glsoil")
matrices_for_formatsitepair <- list(soil="edis_glsoil")
names_of_matrices <- c(names_of_matrices, "edis_fungrl")
matrices_for_formatsitepair["functions"] <- "edis_fungrl"

names_betas <- c()
for(t in trlevels){
  names_betas <- c(names_betas, paste(t, "$beta.sim", sep=""))
  names_betas <- c(names_betas, paste(t, "$beta.sor", sep=""))
}


names_of_matrices <- c(names_of_matrices, names_betas) # attention, I will later take out "predictors"
matrices_for_formatsitepair <-  c(list(betas=names_betas), matrices_for_formatsitepair)

meltset <- get(names_of_matrices[1])
meltset[!lower.tri(meltset)] <- NA
meltset <- melt(meltset, value.name = names_of_matrices[1])
meltset <- meltset[!is.na(meltset[,3]),]



for(i in names_of_matrices[-1]){
  if(grepl("\\$" ,i)){
    split_i <- strsplit(i,split="\\$")
    data <- get(as.character(split_i[[1]][2]),get(split_i[[1]][1]))
  } else{
    data <- get(i)
  }
  data[!lower.tri(data)] <- NA
  data <- as.matrix(data)
  data <- melt(data, value.name=i)
  data <- data[!is.na(data[,3]),]
  meltset <- merge(meltset,data,by=c("Var1","Var2"))
}




