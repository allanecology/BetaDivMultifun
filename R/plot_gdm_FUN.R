
#' create GDM input file path
#' 
#' pastes together the path where to get the gdm input file (.rds) which has been
#' created before
paste_gdm_input_path_together <- function(pathtoout, name){
  path_to_gdmin <- paste(pathtoout, "/cluster/", name, "_output.Rds", sep = "")
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
  
  restab[type == "abio", component := "abio"]
  
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
#' @improt cowplot
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
  return(b)
}
NULL