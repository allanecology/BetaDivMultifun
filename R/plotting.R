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
  restab <- data.table::data.table(merge(nicenames, restab[, .(names, maxsplines)], by = "names"))
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
create_restab2 <- function(x=1, restab = restab){
  if(permut == T){
    print("removing non-significant effects from summary ...")
    restab[sign > 0.05 , maxsplines := 0]
  }
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
  unscaled[nicenames == "aboveground", color := "#66A61E"]; unscaled[nicenames == "belowground", color := "#A65628"]; unscaled[nicenames == "abiotic", color := "#666666"]; unscaled[nicenames == "LUI", color := "#0072B2"]
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
  lui_restab <- data.table::copy(restab)
  if(permut == T){
    print("removing non-significant effects from summary ...")
    lui_restab[sign > 0.05 , maxsplines := 0]
  }
  lui_restab[names %in% c("LUI", "deltaLUI"), component := "lui"]
  # unscaled
  unscaled <- data.table(aggregate(maxsplines ~ component, lui_restab, sum))
  
  unscaled[, type := "unscaled"]
  unscaled[, nicenames := "x"]
  unscaled[component == "turnover", nicenames := "turnover"]; unscaled[component == "nestedness", nicenames := "nestedness"]; unscaled[component == "abio", nicenames := "abiotic"]; unscaled[component == "lui", nicenames := "LUI"]
  
  unscaled[, color := "x"]
  unscaled[nicenames == "turnover", color := "#E6AB02"]; unscaled[nicenames == "nestedness", color := "#984EA3"]; unscaled[nicenames == "abiotic", color := "#666666"]; unscaled[nicenames == "LUI", color := "#0072B2"]
  restab3 <- data.table::copy(unscaled)
  
  # mean (average)
  temp <- data.table(aggregate(maxsplines ~ component, lui_restab, mean)) # get means from original restab
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


#' Create the overview table for single functions models
#'
#' create overview bars of single functions, above- belowground and turnover/ nestedness
#' instead of maxsplines as for single models, here we work with all functions. names of all
#' functions are stored in vector `singleEFnames`
#' 
#' @return A list with 2 data.tables ready for plotting
#' @param restab2 the input table, created in chunk for single functions heatmap
#' @examples
#' blabla TODO
#' 
#' @import data.table
#' 
#' @export
### FUNCTION
create_single_funs_overviewbars <- function(restab2){
  restab <- data.table::copy(restab2)
  
  # add LUI as ground and as component
  restab[names %in% c("LUI", "deltaLUI"), ground := "lui"]
  restab[names %in% c("LUI", "deltaLUI"), component := "lui"]
  
  # divide plants by 2 and half to each overview bar above- and belowground
  auto <- restab[legendnames == "autotroph", ]
  auto[, ground := "b"]
  auto <- rbind(restab[legendnames == "autotroph"], auto)
  # divide all effects by 2
  auto[, (singleEFnames) := lapply(.SD, FUN = function(x) x / 2), .SDcols = singleEFnames]
  restab <- rbindlist(list(auto, restab[legendnames != "autotroph",]))
  
  # backup <- data.table::copy(restab)
  
  ###
  # ABOVE- BELOWGROUND
  # get scaled effects
  f <- singleEFnames[1]
  d <- data.table::data.table(aggregate(get(f) ~ ground, restab, sum))
  d[, `get(f)` := `get(f)`/ sum(d$`get(f)`)] # scale to 0 1
  setnames(d, old = "get(f)", new = f)
  ov_ab_singleEFmods <- data.table::copy(d)
  # add colors for plotting
  ov_ab_singleEFmods[ground == "a", color := "#66A61E"]
  ov_ab_singleEFmods[ground == "b", color := "#A65628"]
  ov_ab_singleEFmods[ground == "x", color := "#666666"]
  ov_ab_singleEFmods[ground == "lui", color := "#0072B2"]
  
  for(f in singleEFnames[-1]){
    d <- data.table::data.table(aggregate(get(f) ~ ground, restab, mean))
    d[, `get(f)` := `get(f)`/ sum(d$`get(f)`)] # scale to 0 1
    setnames(d, old = "get(f)", new = f)
    ov_ab_singleEFmods <- merge(ov_ab_singleEFmods, d, by = "ground")
  }
  rm(d); rm(f)
  # create sequence in barplot with ordered levels : below - above - lui - abiotic
  ov_ab_singleEFmods$color <- factor(ov_ab_singleEFmods$color, 
                                     levels = rev(c("#A65628", "#66A61E", "#0072B2", "#666666")))
  
  ###
  # TURNOVER NESTEDNESS
  # get scaled effects
  f <- singleEFnames[1]
  d <- data.table::data.table(aggregate(get(f) ~ component, restab, sum))
  d[, `get(f)` := `get(f)`/ sum(d$`get(f)`)] # scale to 0 1
  setnames(d, old = "get(f)", new = f)
  ov_tn_singleEFmods <- data.table::copy(d)
  # add colors for plotting
  ov_tn_singleEFmods[component == "abio", color := "#666666"]
  ov_tn_singleEFmods[component == "turnover", color := "#E6AB02"]
  ov_tn_singleEFmods[component == "nestedness", color := "#984EA3"]
  ov_tn_singleEFmods[component == "lui", color := "#0072B2"]
  
  for(f in singleEFnames[-1]){
    d <- data.table::data.table(aggregate(get(f) ~ component, restab, mean))
    d[, `get(f)` := `get(f)`/ sum(d$`get(f)`)] # scale to 0 1
    setnames(d, old = "get(f)", new = f)
    ov_tn_singleEFmods <- merge(ov_tn_singleEFmods, d, by = "component")
  }
  rm(d); rm(f)
  # create sequence in barplot with ordered levels : turnover - nestedness - lui - abiotic
  ov_tn_singleEFmods$color <- factor(ov_tn_singleEFmods$color, 
                                     levels = rev(c("#E6AB02", "#984EA3", "#0072B2", "#666666")))
  
  # check
  test <- all(all(apply(ov_ab_singleEFmods[, -c("ground", "color"), with = F], 2, sum) -1 <= 0.001), 
              all(apply(ov_ab_singleEFmods[, -c("ground", "color"), with = F], 2, sum) -1 <= 0.001))
  if(!test){
    stop('simple check not passed, please check the function again')
  }
  return(list("above_below" = ov_ab_singleEFmods, 
              "turnover_nestedess" = ov_tn_singleEFmods))
}
NULL


#' Create the overview plots for single functions models
#'
#' create overview barplot of single functions, above- belowground and turnover/ nestedness
#' 
#' @return a ggplot element containing the plot
#' @param singleF_restab the input table, created with `create_single_funs_overviewbars`
#' 
#' @import data.table
#' @import ggplot2
#' 
#' @export
### FUNCTION
create_single_funs_overviewbar_plot <- function(singleF_restab, legend = F){
  if(legend){
  # return a plot with legend
    print("not implemented yet.")
  } else {
    sf_ov <- ggplot(singleF_restab, aes(x = variable, y = value, fill = color)) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_identity("", labels = singleF_restab$ground, 
                          breaks = singleF_restab$color,  guide = "legend") +
      # coord_flip() +
      scale_y_reverse() +
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.x = element_text(angle = 90),
            legend.position = "none")
  }
  return(sf_ov)
}
