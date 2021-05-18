
#' create GDM input file path
#' 
#' pastes together the path where to get the gdm input file (.rds) which has been
#' created before
paste_gdm_input_path_together <- function(pathtoout, name){
  # path_to_gdmin <- paste(pathtoout, "/GDM/", name, "_output.Rds", sep = "")
  # edit : the gdm output is stored now in planteco
  print("data is searched in planteco/.../analysis/output_datasets/")
  path_to_gdmin <- paste(pathtoout, "/analysis/output_datasets/", name, "_output.Rds", sep = "")
  return(path_to_gdmin)
}



#' adjust rownames to lui or components
#'
#' If LUI is given, delta and sd is only shown for LUI itself.
#' If components are given, delta F, delta M and delta G  as well as sd F, sd M and
#' sd G are given.
#' This function checks which lui input is given and adds the required placeholders
#' to the vector.
#' This will make possible the construction of restab accordingly.

#' @param luiinput a character vector, either "lui" or "components"
#' @return `plotsequence_abio`, a character vector containing the names for the LUI rows in `restab`,
#' the table used for plotting.
#'
#' @export
define_rownames_lui_or_components <- function(luiinput) {
  if (luiinput == "lui") {
    plotsequence_abio <-
      c("LUI", "deltaLUI", "soil", "isolation", "geo")
  } else if (luiinput == "components") {
    plotsequence_abio <-
      c(paste(c("G", "M", "F"), "std", sep = ""),
        paste("delta", c("Gstd", "Mstd", "Fstd"), sep = ""),
        "soil",
        "isolation",
        "geo")
  } else {
    stop("Error: input is not either lui or components.")
  }
  return(plotsequence_abio)
}

NULL

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
#' @import ggplot2
#' @import cowplot
#' 
#' @export
create_bio_aboveground_barplot <- function(){
  df <- restab[type == "bio" & ground == "a", ]
  p<-ggplot(data = df, aes(x=nicenames, y=maxsplines, fill = color, linetype = linetypet)) +
    geom_bar(stat="identity", color = "black") + 
    coord_flip() + 
    scale_fill_identity() + scale_linetype_identity() + # the best option for customizing linetype, color,...!
    ylim(0, 0.44) + 
    ggtitle(model_name) +
    theme(legend.position = "none", axis.title = element_blank(),
          axis.text.y = element_text(size=9, angle = 0),
          plot.margin = margin(l = 50), 
          axis.text.x = element_blank(),
          axis.line.x=element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_line(color = "grey")
    )
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




#' create plot Overview : Aboveground - belowground - abiotic
#' 
#' code to create overview
#' @param legend boolean T or F. if T, a plot with legend is created.
#' The legend can be exctracted with cowplot.
#' 
#' @import ggplot2
#' @import cowplot
#' 
#' @export
create_overview_above_below_abiotic_barplot <- function(legend = F){
  if(legend){
    df <- as.data.table(df)
    ldf <- unique(df[, .(color, nicenames, ground)])
    ldf[, testvals := 1]
    ov1L <- ggplot(ldf, aes(x = nicenames, y = testvals, fill = factor(color, levels=c("#666666", "#0072B2", "#66A61E", "#A65628")))) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_identity("", labels = ldf$nicenames, breaks = ldf$color, guide = "legend")
    ov1L <- cowplot::get_legend(ov1L)
    return(ov1L)
    
  } else {
    ov1 <- ggplot(data = df, aes(x=type, y=maxsplines, fill = factor(color, levels=c("#666666", "#0072B2", "#66A61E", "#A65628")))) +
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


#' create plot Overview : turnover - newtedness - abiotic
#' 
#' code to create overview
#' 
#' @param legend boolean, if T, only legend is returned, if F, 
#' only plot is returned.
#' 
#' @import ggplot2
#' @import cowplot
#' 
#' @export
create_overview_turnover_nestedness_abiotic_barplot <- function(legend = F){
  if(legend){
    df <- as.data.table(df)
    ldf <- unique(df[, .(color, nicenames, component)])
    ldf[, testvals := 1]
    ov1L <- ggplot(ldf, aes(x = nicenames, y = testvals, fill = factor(color, levels=c("#666666", "#0072B2", "#984EA3", "#E6AB02")))) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_identity("", labels = ldf$nicenames, breaks = ldf$color,  guide = "legend")
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

