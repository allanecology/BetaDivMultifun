#' BetaDivMultifun
#'
#' @name BetaDivMultifun
#' @docType package
NULL


#' Create the table for plotting
#'
#' 
#' @return A data.table ready for plotting
#' @examples
#' blabla TODO
#' @import data.table
#' 
#' @export
create_restab <- function(x=1){
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
NULL


#' Create the table 2 for plotting
#'
#' 
#' @return A data.table ready for plotting
#' @examples
#' blabla TODO
#' @import data.table
#' 
#' @export
create_restab2 <- function(x=1){
  # divide plants by 2 and half to each overview bar above- and belowground
  auto <- restab[legendnames == "autotroph",]
  auto[ ,ground := "b"]
  auto <- rbind(restab[legendnames == "autotroph"], auto)
  auto[, maxsplines := maxsplines / 2]
  restab <- rbindlist(list(auto, restab[legendnames != "autotroph",]))
  
  lui_restab <- data.table::copy(restab)
  lui_restab[names %in% c("LUI", "deltaLUI"), ground := "lui"]
  
  # unscaled
  unscaled <- data.table::data.table(aggregate(maxsplines ~ ground, lui_restab, sum))

  unscaled[, type := "unscaled"]
  unscaled[, nicenames := "x"]
  unscaled[ground == "a", nicenames := "aboveground"]; unscaled[ground == "b", nicenames := "belowground"]; unscaled[ground == "lui", nicenames := "LUI"]; unscaled[ground == "x", nicenames := "abiotic"]
  
  unscaled[, color := "x"]
  unscaled[nicenames == "aboveground", color := "#66A61E"]; unscaled[nicenames == "belowground", color := "#A65628"]; unscaled[nicenames == "abiotic", color := "#666666"]; unscaled[nicenames == "LUI", color := "#D95F02"]
  restab2 <- data.table::copy(unscaled)
  
  # mean (average)
  temp <- data.table(aggregate(maxsplines ~ ground, lui_restab, mean)) # get means from original restab
  temp <- cbind(temp, unscaled[, .(type, nicenames, color)]) # add color and stuff
  temp[, type := "average"]
  restab2 <- rbindlist(list(restab2, temp)) # add to unscaled (rbind)
  
  # scale
  temp <- data.table::copy(unscaled)
  temp[, maxsplines := maxsplines / sum(temp$maxsplines)]
  temp[, type := "scaled"]
  restab2 <- rbindlist(list(restab2, temp)) # add to unscaled (rbind)
  
  # scale the averaged
  temp <- data.table::copy(restab2[type == "average",])
  temp[, maxsplines := maxsplines / sum(temp$maxsplines)]
  temp[, type := "averaged_scaled"]
  restab2 <- rbindlist(list(restab2, temp)) # add to unscaled (rbind)
  return(restab2)
}
NULL




#' Create the table 3 for plotting
#'
#' 
#' @return A data.table ready for plotting
#' @examples
#' blabla TODO
#' 
#' @import data.table
#' 
#' @export
create_restab_3 <- function(x=1){
  # unscaled
  unscaled <- data.table(aggregate(maxsplines ~ component, restab, sum))
  
  unscaled[, type := "unscaled"]
  unscaled[, nicenames := "x"]
  unscaled[component == "turnover", nicenames := "turnover"]; unscaled[component == "nestedness", nicenames := "nestedness"]; unscaled[component == "abio", nicenames := "abiotic"]
  
  unscaled[, color := "x"]
  unscaled[nicenames == "turnover", color := "#E6AB02"]; unscaled[nicenames == "nestedness", color := "#984EA3"]; unscaled[nicenames == "abiotic", color := "#666666"]
  restab3 <- data.table::copy(unscaled)
  
  # mean (average)
  temp <- data.table(aggregate(maxsplines ~ component, restab, mean)) # get means from original restab
  temp <- cbind(temp, unscaled[, .(type, nicenames, color)]) # add color and stuff
  temp[, type := "average"]
  restab3 <- rbindlist(list(restab3, temp)) # add to unscaled (rbind)
  
  # scale
  temp <- data.table::copy(unscaled)
  temp[, maxsplines := maxsplines / sum(temp$maxsplines)]
  temp[, type := "scaled"]
  restab3 <- rbindlist(list(restab3, temp)) # add to unscaled (rbind)
  
  # scale the averaged
  temp <- data.table::copy(restab3[type == "average",])
  temp[, maxsplines := maxsplines / sum(temp$maxsplines)]
  temp[, type := "averaged_scaled"]
  restab3 <- rbindlist(list(restab3, temp)) # add to unscaled (rbind)
  
  return(restab3)
}
NULL
