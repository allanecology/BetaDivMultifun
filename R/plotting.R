#' Create the table for plotting
#'
#' 
#' @return A data.table ready for plotting
#' @examples
#' blabla TODO
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

