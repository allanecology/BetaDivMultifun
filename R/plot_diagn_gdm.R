#' Plot diagnistic Plot from gdm package
#'
#' This function plots the diagnostic plots from a fitted gdm model.
#' The package function does the plots, although only together with
#' all the indivdiual effects, which results in 2+(number of explanatory)
#' plots.
#' The function uses 200 samples along the ecological gradient to 
#' get the line. (similar as used in gdm package.)
#' Note that the var expl. is taken directly from the fitted model.
#'
#' @param x gdm model (output from gdm::gdm())
#' @param title character string. The title to show on the plot, e.g. the model name.
#' Defaults to empty.
#' @return the diagnostic plots from GDM model.
#' @export
plot_diagn_gdm <- function(x, title = ""){
  plot.color <- "blue"
  PSAMPLE <- 200
  plot.linewidth <- 2.0
  
  par(mfrow = c(1, 2))
  ##plots the compositional dissimilarity spline plot
  plot(x$ecological, x$observed, xlab="Predicted Ecological Distance", 
       ylab="Observed Compositional Dissimilarity", 
       type="n", ylim=c(0,1), main = title)
  points(x$ecological, x$observed, pch=20, cex=0.25, col=plot.color)
  overlayX <- seq( from=min(x$ecological), to=max(x$ecological), length=PSAMPLE )
  overlayY <- 1 - exp( - overlayX )
  lines(overlayX, overlayY, lwd=plot.linewidth )
  
  ##plots the second compositional dissimilarity spline plot
  plot(x$predicted, x$observed, xlab="Predicted Compositional Dissimilarity", 
       ylab="Observed Compositional Dissimilarity", type="n", ylim=c(0,1), 
       main = paste("%var expl =", round(x$explained, 2)))
  points( x$predicted, x$observed, pch=20, cex=0.25, col=plot.color )
  overlayX <- overlayY <- seq( from=min(x$predicted), to=max(x$predicted), length=PSAMPLE )
  lines( overlayX, overlayY, lwd=plot.linewidth )
}

NULL
