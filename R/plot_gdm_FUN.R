
#' create GDM input file path
#' 
#' pastes together the path where to get the gdm input file (.rds) which has been
#' created before
paste_gdm_input_path_together <- function(pathtoout, name){
  path_to_gdmin <- paste(pathtodata, "/analysis/output_datasets/", name, "_output.Rds", sep = "")
  return(path_to_gdmin)
}


#' create restab 0
#' 
#' Function containing code to create the first restab for plotting
#' 
#' @import data.table
#' 
#' @export
#' 
create_restab0 <- function(){
  restab <- data.table::data.table("names" = names(maxsplines), "maxsplines" = maxsplines)
  if(permut == T){
    restab <- data.table::data.table(merge(restab, sign, by = "names"))
  }
  # get bios
  bios <- grep(paste(plotsequence_bio, collapse = "|"), names(maxsplines), value = T)
  restab[names %in% bios, type := "bio"]
  # get abios
  abios <- grep(paste(plotsequence_bio, collapse = "|"), names(maxsplines), value = T, invert = T)
  restab[names %in% abios, type := "abio"]
  restab[, component := "abio"]
  # get nestedness
  sne <- grep("sne", names(maxsplines), value = T)
  restab[names %in% sne, component := "nestedness"]
  # get turnover
  to <- grep("sim", names(maxsplines), value = T)
  restab[names %in% to, component := "turnover"]
  # get only significant
  # if(permut == T){restab[sign > 0.05, maxsplines := 0]} # only keep 0.05 and smaller
  #TODO take this back? remove non-significant?
  # add nice names
  restab <- data.table::data.table(merge(nicenames, restab, by = c("names", "type", "component")))
  # order by above-belowground and alphabetically
  # set the levels of the factor in the wanted order
  data.table::setorder(restab, ground, names)
  # restab[, names := factor(names, levels = names)]
  # order <- restab[type == "bio", names]
  # bring colors into right format
  if(permut == T){
    print("setting non-significant colours to gray")
    restab[sign > 0.05, color := "gray91"]
  }
  restab[, color := as.character(color)]
  
  restab[component %in% c("turnover", "abio"), linetypet := "solid"]
  restab[component == "nestedness", linetypet := "dotted"]
  
  return(restab)
}
NULL


#' create plot "p"
#' 
#' code to create plot p : bio and aboveground
#' 
#' @param type is either grouped, where turnover and nestedness components
#' are shown in individual bars, or "stacked", where turnover and 
#' nestedness components are stacked on top of each other.
#' @import ggplot2
#' @import cowplot
#' 
#' @export
create_bio_aboveground_barplot <- function(type = c("grouped", "stacked")){
  df <- restab[type == "bio" & ground == "a", ]
  if(type == "grouped"){
    # plot by nicenmaes, unique names for each trophic level and each component
    p<-ggplot(data = df, aes(x=nicenames, y=maxsplines, fill = color, linetype = linetypet)) +
      geom_bar(stat="identity", color = "black")
    
  } else if(type == "stacked"){
    # plot by legendnames if stacked - unique names for trophic group, but no unique names for
    # turnover and nestedness
    p<-ggplot(data = df, aes(x=legendnames, y=maxsplines, fill = color, linetype = linetypet)) +
      geom_bar(stat="identity", color = "black")
    
  }
  p <- p + coord_flip() + 
    scale_fill_identity() + scale_linetype_identity() + # the best option for customizing linetype, color,...!
    ylim(0, 0.44) + 
    ggtitle(model_name) +
    theme(legend.position = "none", axis.title = element_blank(),
          axis.text.y = element_text(size=9, angle = 0),
          plot.margin = margin(l = 50), 
          axis.text.x = element_blank(),
          axis.line.x=element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_line(color = "grey"))
  
  return(p)
}
NULL

#' create plot bio belowground
#' 
#' code to create plot p : bio and belowground
#' 
#' @import ggplot2
#' @import cowplot
#' 
#' @export
create_bio_belowground_barplot <- function(){
  df <- restab[type == "bio" & ground == "b", ]
  b <- ggplot(data = df, aes(x=nicenames, y=maxsplines, fill = color, linetype = linetypet)) +
    geom_bar(stat="identity", color = "black") + 
    coord_flip() +
    scale_fill_identity() + scale_linetype_identity() +
    ylim(0, 0.44) +
    theme(legend.position = "none", 
          axis.title = element_blank(),
          axis.text.y = element_text(size=9, angle = 0),
          plot.margin = margin(l = 50),
          axis.text.x = element_blank(),
          axis.line.x=element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_line(color = "grey"))
  # aes(x=stringr::str_wrap(nicenames, 10), y=onlysign, fill = names))
  return(b)
}
NULL

#' create plot abio
#' 
#' code to create plot abio
#' 
#' @import ggplot2
#' @import cowplot
#' 
#' @export
create_abio_barplot <- function(){
  df <- restab[ground == "x"]
  q <- ggplot(data = df, aes(x=nicenames, y=maxsplines, fill = color, linetype = linetypet)) +
    geom_bar(stat="identity", color = "black") +
    coord_flip() +
    # scale_fill_manual(values = as.character(df$color)) +
    scale_fill_identity() +
    scale_linetype_identity() +
    ylim(0, 0.44) +
    theme(legend.position = "none", axis.title = element_blank(),
          axis.text.y = element_text(size=9),
          panel.grid.major.x = element_line(color = "grey"))
  return(q)
}
NULL




#' DEPRECIATED create plot Overview : Aboveground - belowground - abiotic
#' TODO delete this function, use now : `create_overview_barplot` 18.11.22
#' 
#' code to create overview
#' @param df data frame to be used for plotting. Is output from `create_overviewbar_restab`.
#' @param legend boolean T or F. if T, a plot with legend is created.
#' The legend can be exctracted with cowplot.1
#' 
#' @import ggplot2
#' @import cowplot
#' 
#' @export
create_overview_above_below_abiotic_barplot <- function(df, legend = F){
  if(legend){
    # create a plot with dummy values (all 1) to extract legend from
    df <- as.data.table(df)
    ldf <- unique(df[, .(color, lui_ground_nicenames)])
    ldf[, testvals := 1]
    ov1L <- ggplot(ldf, aes(x = lui_ground_nicenames, y = testvals, 
                            fill = factor(color, levels=c("#666666", "#0072B2", "#66A61E", "#A65628")))) +
      # note : the colors are just given individually in order to control the sequence
      #    of names in the legend. The colors themselves are read from the dataset
      geom_bar(stat = "identity", color = "black") +
      scale_fill_identity("", labels = ldf$lui_ground_nicenames, breaks = ldf$color, guide = "legend")
    ov1L <- cowplot::get_legend(ov1L)
    return(ov1L)
    
  } else {
    # create the overviewbar plot
    ov1 <- ggplot(data = df, aes(x=type, y=maxsplines, fill = factor(color, levels=c("#666666", "#0072B2", "#66A61E", "#A65628")))) +
      # note : the colors are just given individually in order to control the sequence
      #    of names in the legend. The colors themselves are read from the dataset
      geom_bar(stat="identity", color = "black", linetype = "solid") +
      coord_flip() +
      scale_fill_identity() +
      theme(legend.position = "none", axis.title = element_blank(),
            axis.text.y = element_text(size=9),
            panel.grid.major.x = element_line(color = "grey"))
    return(ov1)
  }
}
NULL


#' DEPRECIATED create plot Overview : turnover - newtedness - abiotic
#' TODO delete this function, use now : `create_overview_barplot` 18.11.22
#' code to create overview
#' 
#' @param df input dataframe or data.table, 
#' is the output of `create_overviewbar_restab`
#' @param legend boolean, if T, only legend is returned, if F, 
#' only plot is returned.
#' 
#' @import ggplot2
#' @import cowplot
#' 
#' @export
create_overview_turnover_nestedness_abiotic_barplot <- function(df, legend = F){
  if(legend){
    # create dummy barplot with `values 1 for all groups and extract colors from there.
    # reason : check if the colors are assigned correctly (visual test)
    df <- as.data.table(df)
    ldf <- unique(df[, .(color, lui_component)])
    ldf[, testvals := 1]
    ov1L <- ggplot(ldf, aes(x = lui_component, y = testvals, fill = factor(color, levels=c("#666666", "#0072B2", "#984EA3", "#E6AB02")))) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_identity("", labels = ldf$lui_component, breaks = ldf$color,  guide = "legend")
    ov1L <- cowplot::get_legend(ov1L)
    return(ov1L)
  } else {
    ov2 <- ggplot(data = df, aes(x=type, y=maxsplines, fill = factor(color, levels=c("#666666", "#0072B2", "#984EA3", "#E6AB02")))) +
      geom_bar(stat="identity", color = "black", linetype = "solid") +
      coord_flip() +
      scale_fill_identity() +
      theme(legend.position = "none", axis.title = element_blank(),
            axis.text.y = element_text(size=9),
            panel.grid.major.x = element_line(color = "grey"))
    
    return(ov2)
  }
}
NULL



#' Create Overview Barplot
#' 
#' Creates overview barplots from the data.table outputted by `create_overviewbar_restab()`. 
#' Plots all overviewbars provided by the input data, in order
#' to cut bars, please take out the respective lines.
#' 
#' @param df input dataset for plotting, taken from `create_overviewbar_restab()`.
#' @param legend if set to T, only the legend is the output. Defaults to F.
#' @return A barplot of the overviewbars. If `legend=T`, legend extracted with `cowplot::get_legend` which can be plotted
#' using `cowplot::plot_grid(legend)`.
#'
#' @import ggplot2
#' @import cowplot
#' @import data.table
#'
#' @export
create_overview_barplot <- function(df, legend = F){
  if(legend){
    # create dummy barplot with values 1 for all groups and extract colors from there.
    # reason : check if the colors are assigned correctly (visual test)
    df <- as.data.table(df)
    ldf <- unique(df[, .(color, component)])
    ldf[, testvals := 1]
    ov1L <- ggplot(ldf, aes(x = component, y = testvals, 
                            fill = factor(color, levels=c("#666666", "#0072B2", "#984EA3", "#E6AB02", "#66A61E", "#A65628")))) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_identity("", labels = ldf$component, breaks = ldf$color,  guide = "legend")
    ov1L <- cowplot::get_legend(ov1L)
    return(ov1L)
  } else {
    ov2 <- ggplot(data = df, aes(x=bar_name, y=maxsplines, 
                                 fill = factor(color, levels=c("#666666", "#0072B2", "#984EA3", "#E6AB02", "#66A61E", "#A65628")))) +
      geom_bar(stat="identity", color = "black", linetype = "solid") +
      coord_flip() +
      scale_fill_identity() +
      theme(legend.position = "none", axis.title = element_blank(),
            axis.text.y = element_text(size=9),
            panel.grid.major.x = element_line(color = "grey"))
    
    return(ov2)
  }
}
NULL


#' get legends of plots
#' 
#' based on the overview table `ǹicenames.csv`, the legend shows colors
#' for the different predictors.
#' the legend is split into abiotic and biotic predictors, as there are
#' 16 different colors, but 28 different predictors.
#' TODO : add the overview bar colors as well, to produce legend for them.
#' @param type character, is either "biotic" or "abiotic".
#' @return `legend`, legend extracted with `cowplot::get_legend` which can be plotted
#' using `cowplot::plot_grid(legend)`.
#'
#' @import ggplot2
#' @import cowplot
#' @import data.table
#'
#' @export
get_nice_legend <- function(type){
  # type needs to be either biotic or abiotic
  nicenames[, testvals := 1]
  short_nicenames <- unique(nicenames[, .(legendnames, testvals, color, ground)])
  short_nicenames[, legendnames := as.factor(legendnames)]
  if(type == "biotic"){
    dat <- short_nicenames[ground %in% c("a", "b")]
  }
  if(type == "abiotic"){
    dat <- short_nicenames[ground %in% c("x")]
  }
  if(!(type %in% c("biotic", "abiotic"))){
    print("type must be either biotic or abiotic.")
  }
  p <- ggplot(dat, aes(x = legendnames, y = testvals, fill = color)) +
    geom_bar(stat = "identity", color = "black") +
    scale_fill_identity(name = "", guide = "legend",
                        breaks = dat[, color],
                        labels = dat[, legendnames]) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("betadiversity colors")
  suppressWarnings(legend <- get_legend(p)) # because of delta characters in legendnames
  return(legend)
}

NULL

#' create lineplots
#' 
#' create lineplots based on the I splines extracted from a gdm model
#' with the function `gdm::isplineExtract`.
#' Used the overview table `ǹicenames.csv` for graphical parameters as
#' line types and color.
#' Can plot a legend if legend is set to T, but the legend is recommended
#' to be produced by the function `get_nice_legend`.
#' 
#' TODO : add vertical grid lines, remove x axis for above plots
#' @param data data.table, produced by `gdm::isplineExtract` and further
#' cleaning (as shown in `plot_gdm.Rmd`).
#' @return a ggplot2 plot element, with lineplots.
#'
#' @import ggplot2
#' @import cowplot
#' @import data.table
#'
#' @export
create_gdm_lineplot <- function(data, legend = F, ymax = 1){
  test <- unique(data[, .(color, nicenames)])
  p <- ggplot(data, aes(x = xaxis, y = value, fill = names, linetype = linetypeto)) +
    geom_line(aes(linetype=linetypeto, color=color), size = 1.6) +
    ylim(0, ymax) +
    scale_linetype_identity() + 
    scale_color_identity(name = "", guide = "legend", 
                         breaks = test$color,
                         labels = test$nicenames) +
    theme(axis.title = element_blank()) +
    background_grid()
  if(!legend){
    p <- p + theme(legend.position = "none")
  }
  return(p)
}
NULL


#' calculate average number of biotic/abiotic/LUI drivers of single functions
#' 
#' calculate the following average numbers to report from analysis :
#' Number of biotic/LUI/abiotic drivers of each single function separately.
#' Subsequently take average of all single functions, in order to get average
#' number of drivers per function per category.
#' To answer e.g. the following question : "How many biotic groups do
#' affect a single function on average?"
#' Note that all parameters have very specific arguments, which are produced in the
#' script `plot_GDM.Rmd`.
#' 
#' @param restab2 a data.table created in `plot_GDM.Rmd` creating a summary table of 
#' all maxspline effects for single functions.
#' @param singleEFnames a vector created in `plot_GDM.Rmd` containing the names of 
#' single functions.
#' @param name_of_summary is "bio_abio_lui", no other argument is accepted
calc_avg_number_of_biotic_abiotic_drivers_of_single_functions <- function(restab2, 
                                                                          singleEFnames, 
                                                                          name_of_summary = "bio_abio_lui"){
  # basic input check
  if(!name_of_summary %in% c("bio_abio_lui")){
    stop('name of summary not the expected argument "bio_abio_lui". Any new argument needs to be programmed newly.')
  }
  
  # create summary result for first single function "Biomass" which is first element in vector singleEFnames
  i <- 1
  summary_n_of_drivers_per_singlefun <- data.table(
    table(
      restab2[, .(get(name_of_summary), get(singleEFnames[i]))][V2 > 0, .(V1)]))
  
  colnames(summary_n_of_drivers_per_singlefun) <- c(name_of_summary, singleEFnames[i])
  
  # create summary result for all other single functions
  for(i in 1:(length(singleEFnames)-1)){
    res <- data.table(table(
      restab2[, .(get(name_of_summary), get(singleEFnames[i]))][V2 > 0, .(V1)]))
    colnames(res) <- c(name_of_summary, singleEFnames[i])
    summary_n_of_drivers_per_singlefun <- merge(summary_n_of_drivers_per_singlefun, 
                                                res, 
                                                by = name_of_summary, all = T)
  }
  rm(i); rm(res)
  # calculate mean number of drivers per single function to report in results
  res <- data.table("category" = summary_n_of_drivers_per_singlefun[, get(name_of_summary)],
                    "mean number of drivers per function" = rowMeans(summary_n_of_drivers_per_singlefun[, -1], na.rm = T))
  return(res)
}
NULL