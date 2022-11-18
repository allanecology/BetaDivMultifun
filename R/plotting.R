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


#' Create restab for overview bars
#' 
#' calculates scaled averages OR summed effect sizes per category, either (1) above- or belowground
#' or (2) turnover or nestedness, as well as LUI and abiotic effects. The categories correspond
#' to the categories given in the table `nicenames` in columns`lui_ground` and `lui_component`.
#' 
#' *Note for developers* : This function was built from `create_restab2()` and `create_restab_3()`
#' which were deleted (available in git history, date : 22.06.22)
#' 
#' **averaged scaled** : calculating the average, scaled effect. The mean 
#' contribution of a given group, represented as percentage of the total 
#' contribution of 100%.
#'
#' **summed scaled** : calculates the summed effect, scaled to the total effect.
#' There are more biotic than abiotic effects, but the biotic effect is "distilled"
#' across more variables. Summing the effects shows which groups are driving the
#' patterns dominantly.
#'
#' Note : Here, the mean values for each component are calculated. Summing the mean scaled 
#' contribution of above + belowground **is not the same as** summing the mean scaled
#' contribution of turnover + nestedness. The overviewbars do not show the same length of
#' LUI and abiotic, because it's the relative contributions.
#' 
#' @return a data.table containing values ready for plotting
#' @import data.table
#' @param restab exactly the output from the function `create_restab0()`
#' @param fun the function for aggregating. Is either "mean" or "sum".
#' 
#' @export
create_overviewbar_restab <- function(restab, fun = c("mean", "sum")){
  if(permut == T){
    print("please implement me...")
    # lui_restab[sign > 0.05 , maxsplines := 0]
  }
  
  #########
  # NESTEDNESS- TURNOVER
  #
  ovtab_tn <- data.table(aggregate(maxsplines ~ lui_component, restab, FUN = fun))
  ovtab_tn[, maxsplines := maxsplines / sum(ovtab_tn$maxsplines)] # convert to percent
  setnames(ovtab_tn, old = "lui_component", new = "component")
  # add colors
  ovtab_tn[component == "turnover", color := "#E6AB02"]   # yellow
  ovtab_tn[component == "nestedness", color := "#984EA3"] # purple
  ovtab_tn[component == "abiotic", color := "#666666"] # gray
  ovtab_tn[component == "LUI", color := "#0072B2"] # blue
  ovtab_tn[, type := fun]
  ovtab_tn[, groups := "turn_nes"]
  
  #########
  # ABOVE- BELOWGROUND
  #
  # Divide plants to above- and belowground
  auto <- restab[legendnames == "autotroph",]
  auto[, ground := "b"]
  auto[, lui_ground := "b"]
  auto[, lui_ground_nicenames := "belowground"]
  auto <- rbind(restab[legendnames == "autotroph"], auto)
  auto[, maxsplines := maxsplines / 2]
  ovtab_ab <- rbindlist(list(auto, restab[legendnames != "autotroph",]))
  rm(auto)
  # calculate average scaled effects
  ovtab_ab <- data.table(aggregate(maxsplines ~ lui_ground_nicenames, ovtab_ab, FUN = fun))
  ovtab_ab[, maxsplines := maxsplines / sum(ovtab_ab$maxsplines)]
  setnames(ovtab_ab, old = "lui_ground_nicenames", new = "component")
  ovtab_ab[, type := fun]
  ovtab_ab[, groups := "above_below"]
  # add colors
  ovtab_ab[component == "aboveground", color := "#66A61E"] # green
  ovtab_ab[component == "belowground", color := "#A65628"] # brown
  ovtab_ab[component == "abiotic", color := "#666666"] # gray
  ovtab_ab[component == "LUI", color := "#0072B2"] # blue
  
  #########
  # RETURN
  res <- rbindlist(list(ovtab_ab, ovtab_tn), use.names = T, fill = T)
  res[, bar_name := paste(type, groups, sep = "_")]
  return(res)
}




#' Create the table 2 for plotting
#'
#' 
#' @return A data.table ready for plotting
#' @examples
#' blabla TODO
#' @import data.table
#' 
#' @export
create_restab2 <- function(x=1, restab){
  if(permut == T){
    print("removing non-significant effects from summary ...")
    restab[sign > 0.05 , maxsplines := 0]
  }
  # divide plants by 2 and half to each overview bar above- and belowground
  auto <- restab[legendnames == "autotroph",]
  auto[, ground := "b"]
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
#' @param rel_colnames the names of the columns containing GDM results, for single functions e.g. `singleEFnames`
#' @examples
#' blabla TODO
#' 
#' @import data.table
#' 
#' @export
### FUNCTION
create_single_funs_overviewbars <- function(restab2, rel_colnames){
  restab <- data.table::copy(restab2)
  
  # add LUI as ground and as component
  restab[names %in% c("LUI", "deltaLUI"), ground := "lui"]
  restab[names %in% c("LUI", "deltaLUI"), component := "lui"]
  
  # divide plants by 2 and half to each overview bar above- and belowground
  auto <- restab[legendnames == "autotroph", ]
  auto[, ground := "b"]
  auto <- rbind(restab[legendnames == "autotroph"], auto)
  # divide all effects by 2
  auto[, (rel_colnames) := lapply(.SD, FUN = function(x) x / 2), .SDcols = rel_colnames]
  restab <- rbindlist(list(auto, restab[legendnames != "autotroph",]))
  
  # backup <- data.table::copy(restab)
  
  ###
  # ABOVE- BELOWGROUND
  # get scaled effects
  f <- rel_colnames[1]
  d <- data.table::data.table(aggregate(get(f) ~ ground, restab, sum))
  d[, `get(f)` := `get(f)`/ sum(d$`get(f)`)] # scale to 0 1
  setnames(d, old = "get(f)", new = f)
  ov_ab_singleEFmods <- data.table::copy(d)
  # add colors for plotting
  ov_ab_singleEFmods[ground == "a", color := "#66A61E"]
  ov_ab_singleEFmods[ground == "b", color := "#A65628"]
  ov_ab_singleEFmods[ground == "x", color := "#666666"]
  ov_ab_singleEFmods[ground == "lui", color := "#0072B2"]
  
  for(f in rel_colnames[-1]){
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
  f <- rel_colnames[1]
  d <- data.table::data.table(aggregate(get(f) ~ component, restab, sum))
  d[, `get(f)` := `get(f)`/ sum(d$`get(f)`)] # scale to 0 1
  setnames(d, old = "get(f)", new = f)
  ov_tn_singleEFmods <- data.table::copy(d)
  # add colors for plotting
  ov_tn_singleEFmods[component == "abio", color := "#666666"]
  ov_tn_singleEFmods[component == "turnover", color := "#E6AB02"]
  ov_tn_singleEFmods[component == "nestedness", color := "#984EA3"]
  ov_tn_singleEFmods[component == "lui", color := "#0072B2"]
  
  for(f in rel_colnames[-1]){
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
#' @param pos defines the bar position, either stack for bars on top of each other (stacked bars)
#' or "dodge" for bars next to each other
#' 
#' @import data.table
#' @import ggplot2
#' 
#' @export
### FUNCTION
create_single_funs_overviewbar_plot <- function(singleF_restab, legend = F, pos = "stack"){
  if(legend){
  # return a plot with legend
    print("not implemented yet.")
  } else {
    sf_ov <- ggplot(singleF_restab, aes(x = variable, y = value, fill = color)) +
      geom_bar(stat = "identity", color = "black", position = pos) +
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
