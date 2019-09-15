#' Create the table for plotting
#'
#' 
#' @return A data.table ready for plotting
#' @examples
#' blabla TODO
#' 
#' @export
create_restab <- function(x=1){
  require(data.table)
  #TODO : once all models are read, use effect size sequence and not alphabetically
  plotsequence_bio <- c("autotroph", "bacteria.RNA", "belowground.herbivore", "belowground.predator", "herbivore", "plant.pathogen", "pollinator", "protist.bacterivore", "protist.eukaryvore", "protist.omnivore", "protist.plant.parasite", "secondary.consumer", "soilfungi.decomposer", "soilfungi.pathotroph", "soilfungi.symbiont", "tertiary.consumer")
  #TODO : add characteristic color for each trophic level
  plotsequence_abio <- c("LUI", "deltaLUI", "soil", "isolation", "geo")
  restab <- data.table::data.table("names" = names(maxsplines), "maxsplines" = maxsplines)
  if(permut == T){
    del <- data.table::data.table("names" = names(sign), "sign" = sign)
    restab <- data.table::data.table(merge(restab, del, by = "names")); rm(del)
  }
  # get bios
  bios <- grep(paste(plotsequence_bio, collapse = "|"), names(maxsplines), value = T)
  restab[names %in% bios, type := "bio"]
  # get abios
  abios <- grep(paste(plotsequence_bio, collapse = "|"), names(maxsplines), value = T, invert = T)
  restab[names %in% abios, type := "abio"]
  # get nestedness
  sne <- grep("sne", names(maxsplines), value = T)
  restab[names %in% sne, component := "nestedness"]
  # get turnover
  to <- grep("sim", names(maxsplines), value = T)
  restab[names %in% to, component := "turnover"]
  # get only significant
  if(permut == T){restab[, maxsplines := (1-sign) * maxsplines]}
  # add nice names
  restab <- data.table::data.table(merge(nicenames, restab, by = "names"))
  # order by above-belowground and alphabetically
  # set the levels of the factor in the wanted order
  data.table::setorder(restab, ground, names)
  # restab[, names := factor(names, levels = names)]
  # order <- restab[type == "bio", names]
  # bring colors into right format
  restab[, color := as.character(color)]
  # print("hello")
  return(data.frame(restab))
}




create_restab2 <- function(x=1){
  require(data.table)
  # unscaled
  unscaled <- data.table(aggregate(maxsplines ~ ground, restab, sum))
  unscaled2 <- data.table(aggregate(maxsplines ~ ground, restab, length))
  setnames(unscaled2, old = "maxsplines", new = "weights")
  unscaled <-merge(unscaled, unscaled2); rm(unscaled2)
  
  unscaled[, type := "unscaled"]
  unscaled[, nicenames := "x"]
  unscaled[ground == "a", nicenames := "aboveground"]; unscaled[ground == "b", nicenames := "belowground"]; unscaled[ground == "x", nicenames := "abiotic"]
  
  unscaled[, color := "x"]
  unscaled[nicenames == "aboveground", color := "#66A61E"]; unscaled[nicenames == "belowground", color := "#A65628"]; unscaled[nicenames == "abiotic", color := "#666666"]
  restab2 <- data.table::copy(unscaled)
  
  # averaged but not scaled
  temp <- data.table::copy(unscaled) 
  temp[, maxsplines := maxsplines / weights]
  temp[, type := "averaged"]
  restab2 <- rbindlist(list(restab2, temp)) # add to unscaled (rbind)
  
  # scale but not averaged
  temp <- data.table::copy(unscaled)
  temp[, maxsplines := maxsplines / sum(temp$maxsplines)]
  temp[, type := "scaled"]
  restab2 <- rbindlist(list(restab2, temp)) # add to unscaled (rbind)
  
  # scale and averaged
  temp <- data.table::copy(unscaled)
  temp[, maxsplines := maxsplines / weights]
  temp[, maxsplines := maxsplines / sum(temp$maxsplines)]
  temp[, type := "averaged_scaled"]
  restab2 <- rbindlist(list(restab2, temp)) # add to unscaled (rbind)
  return(restab2)
}






create_restab_3 <- function(x=1){
  require(data.table)
  # unscaled
  unscaled <- data.table(aggregate(maxsplines ~ component, restab, sum))
  unscaled2 <- data.table(aggregate(maxsplines ~ component, restab, length))
  setnames(unscaled2, old = "maxsplines", new = "weights")
  unscaled <-merge(unscaled, unscaled2); rm(unscaled2)
  
  unscaled[, type := "unscaled"]
  unscaled[, nicenames := "x"]
  unscaled[component == "turnover", nicenames := "turnover"]; unscaled[component == "nestedness", nicenames := "nestedness"]; unscaled[component == "abio", nicenames := "abiotic"]
  
  unscaled[, color := "x"]
  unscaled[nicenames == "turnover", color := "#E6AB02"]; unscaled[nicenames == "nestedness", color := "#984EA3"]; unscaled[nicenames == "abiotic", color := "#666666"]
  restab3 <- data.table::copy(unscaled)
  
  # averaged but not scaled
  temp <- data.table::copy(unscaled) 
  temp[, maxsplines := maxsplines / weights]
  temp[, type := "averaged"]
  restab3 <- rbindlist(list(restab3, temp)) # add to unscaled (rbind)
  
  # scale but not averaged
  temp <- data.table::copy(unscaled)
  temp[, maxsplines := maxsplines / sum(temp$maxsplines)]
  temp[, type := "scaled"]
  restab3 <- rbindlist(list(restab3, temp)) # add to unscaled (rbind)
  
  # scale and averaged
  temp <- data.table::copy(unscaled)
  temp[, maxsplines := maxsplines / weights]
  temp[, maxsplines := maxsplines / sum(temp$maxsplines)]
  temp[, type := "averaged_scaled"]
  restab3 <- rbindlist(list(restab3, temp)) # add to unscaled (rbind)
  return(restab3)
}
