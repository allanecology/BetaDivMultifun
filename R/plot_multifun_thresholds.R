#' create lineplots of all thresholds
#' 
#' Creat lineplots for the weighted average models over all thresholds.
#' The basic shape and appearance is the same as in GDM lineplots, when
#' using the function `create_gdm_lineplot`.
#' 
#' The shown lines are either (1) the extracted Isplines from the GDM
#' models, or (2) the weighted averages over all thresholds. The weighted
#' averages are calculated at each X value, per predictor, per model type
#' (turnover / nestedness). The predicted Y values are weighted by their inverse 
#' standard deviation.
#' 
#' Used the overview table `ǹicenames.csv` for graphical parameters as
#' line types and color.
#' Can plot a legend if legend is set to T, but the legend is recommended
#' to be produced by the function `get_nice_legend`.
#' 
#' TODO : add vertical grid lines, remove x axis for above plots
#' @param data data.table, produced in `GDM_multifun_thresholds.Rmd`).
#' @param legend logical T or F, indicating if the plot or the legend should be returned.
#' @param ymax the maximum y value shown. Defaults to 1.
#' @param plottitle a character giving the title of the plot. Defaults to no title.
#' @return a ggplot2 plot element, with lineplots.
#'
#' @import ggplot2
#' @import cowplot
#' @import data.table
#'
#' @export
create_gdm_lineplot_predictorwise_thresholds <- function(data, legend = F, ribbon = T, ymax = 1, plottitle = ""){
  test <- unique(data[, .(color, nicenames)])
  
  p <- ggplot(data, aes(x = xaxis, y = value, fill = model_name, color = model_name)) +
    {if(ribbon)geom_ribbon(aes(ymin = minusSD_Y, ymax = plusSD_Y), alpha = 0.1, linetype = 0)} +
    geom_line(aes(linewidth = lwd)) +
    scale_colour_brewer(palette = "PuOr") +
    coord_cartesian(ylim = c(0, ymax)) +
    scale_linewidth_identity() +
    theme(axis.title = element_blank(),
          legend.title = element_blank()) +
    background_grid() +
    ggtitle(plottitle)
    
  if(!legend){
    p <- p + theme(legend.position = "none")
  }
  return(p)
}
NULL
