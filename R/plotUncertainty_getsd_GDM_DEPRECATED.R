#' plotUncertainty_getsd : Get GDM sd at max height DEPRECATED
#' 
#' *Depreciation note* : This function is no longer used, because (1) the new version
#' of the package `gdm` allows extracting output data from this function, and
#' (2) because we need the uncertainty not only at the maximum value, but throughout
#' the 200 x values.
#'
#' To get an uncertainty at the maximum x value of a GDM model (corresponds to max y), 
#' the function `plotUncertainty` from the `gdm` package was slightly modified : 
#' all input stays the same. New : a table is generated which contains
#' the estimate at maximum x and its standard deviation for each predictor separately.
#' The plotting has been outcommented, as there is no plot output needed in this case.
#' All edited placed are marked with `#EDIT` in the code
#' 
#' @import data.table
#' @return a data.table with 3 columns : the names of the predictors, the maximum effect
#' size and its standard deviation
#' @param spTable same as in gdm::plotUncertainty
#' @param sampleSites same as in gdm::plotUncertainty. 0.3 was used in Gossner 2016.
#' @param bsIters same as in gdm::plotUncertainty. 100 were used in Gossner 2016.
#' @param geo same as in gdm::plotUncertainty
#' @param splines same as in gdm::plotUncertainty
#' @examples 
#' # read in gdminput from `results_nonpublic.R`.
#' test <- plotUncertainty_getsd(gdminput, sampleSites = 0.3, bsIters = 5, geo = T)
#' 
#' @export
plotUncertainty_getsd <- function (spTable, sampleSites, bsIters, geo = FALSE, splines = NULL, 
                                   knots = NULL, splineCol = "blue", errCol = "grey80", plot.linewidth = 2, 
                                   plot.layout = c(2, 2), parallel = FALSE, cores = 2) 
{
  if (class(spTable)[1] != "gdmData") {
    warning("table class does not include type 'gdmData'. Make sure your data is in site-pair format. See the formatsitepair fucntion for help.")
  }
  if (!(class(spTable)[1] == "gdmData" | class(spTable)[1] == 
        "matrix" | class(spTable)[1] == "data.frame")) {
    stop("table argument needs to be a matrix or a table frame")
  }
  if (ncol(spTable) < 6) {
    stop("Not enough columns in table. (Minimum need: Observed, weights, X0, Y0, X1, Y1)")
  }
  if (nrow(spTable) < 1) {
    stop("Not enough rows in table")
  }
  if (!(geo == TRUE | geo == FALSE)) {
    stop("geo argument must be either TRUE or FALSE")
  }
  if (is.null(splines) == FALSE & class(splines) != "numeric") {
    stop("argument splines needs to be a numeric data type")
  }
  if (is.null(knots) == FALSE & class(knots) != "numeric") {
    stop("argument knots needs to be a numeric data type")
  }
  if (!(parallel == TRUE | parallel == FALSE)) {
    stop("parallel argument must be either TRUE or FALSE")
  }
  if (parallel == TRUE & is.null(cores) == TRUE) {
    stop("If parallel==TRUE, the number of cores must be specified")
  }
  if ((is.null(cores) == FALSE & is.numeric(cores) == FALSE) | 
      cores < 1) {
    stop("argument cores needs to be a positive integer")
  }
  if ((is.null(bsIters) == FALSE & is.numeric(bsIters) == FALSE) | 
      bsIters < 1) {
    stop("argument bsIters needs to be a positive integer")
  }
  if (is.numeric(sampleSites) == FALSE) {
    stop("sampleSites must be a number between 0 and 1")
  }
  if (sampleSites < 0) {
    stop("sampleSites must be a number between 0 and 1")
  }
  if (sampleSites > 1) {
    stop("sampleSites must be a number between 0 and 1")
  }
  if (sampleSites == 0) {
    stop("a sampleSites value of 0 will remove all sites from the analysis")
  }
  cores <- as.integer(cores)
  bsIters <- as.integer(bsIters)
  k <- NULL
  lstSP <- lapply(1:bsIters, function(i) {
    spTable
  })
  if (parallel == TRUE) {
    cl <- makeCluster(cores, outfile = "")
    registerDoParallel(cl)
    subSamps <- foreach(k = 1:length(lstSP), .verbose = F, 
                        .packages = c("gdm")) %dopar% gdm::removeSitesFromSitePair(spTable[[k]], 
                                                                              sampleSites = sampleSites)
    gdmMods <- foreach(k = 1:length(subSamps), .verbose = F, 
                       .packages = c("gdm")) %dopar% gdm(subSamps[[k]], 
                                                         geo = geo, splines = splines, knots = knots)
    stopCluster(cl)
  } else {
    subSamps <- lapply(lstSP, gdm::removeSitesFromSitePair, sampleSites = sampleSites)
    gdmMods <- lapply(subSamps, gdm, geo = geo, splines = splines, 
                      knots = knots)
  }
  fullGDMmodel <- gdm(spTable, geo = geo, splines = splines, 
                      knots = knots)
  #EDIT : here, an error is caused if the list gdmMods contains NULL models
  if(length(which(sapply(gdmMods, is.null))) > 0){
    gdmMods <- gdmMods[-which(sapply(gdmMods, is.null))] # risky - maybe it's enough to just remove the null.
  }
  exUncertSplines <- lapply(gdmMods, isplineExtract)
  fullGDMsplines <- isplineExtract(fullGDMmodel)
  predVars <- colnames(exUncertSplines[[1]][[1]])
  thisplot <- 0
  one_page_per_plot <- FALSE
  if ((plot.layout[1] == 1) && (plot.layout[2] == 1)) {
    one_page_per_plot <- TRUE
  } else {
    par(mfrow = plot.layout)
  }
  totalYmin <- Inf
  totalYmax <- -Inf
  for (p in 1:length(predVars)) {
    predV <- predVars[p]
    for (nm in 1:length(exUncertSplines)) {
      selPlot <- exUncertSplines[[nm]]
      spYmax <- max(selPlot[[2]][, predV])
      spYmin <- min(selPlot[[2]][, predV])
      totalYmax <- max(c(totalYmax, spYmax))
      totalYmin <- min(c(totalYmin, spYmin))
    }
  }
  # EDIT : create table to store yatmax and sd of the max value
  restab <- data.table::data.table(varname = predVars, yatmax = as.numeric(NA), sdy = as.numeric(NA))
  #
  for (p in 1:length(predVars)) {
    predV <- predVars[p]
    totalXmin <- Inf
    totalXmax <- -Inf
    for (nm in 1:length(exUncertSplines)) {
      selPlot <- exUncertSplines[[nm]]
      spXmax <- max(selPlot[[1]][, predV])
      spXmin <- min(selPlot[[1]][, predV])
      if (spXmax > totalXmax) {
        totalXmax = spXmax
      }
      if (spXmin < totalXmin) {
        totalXmin = spXmin
      }
    }
    if (totalYmax != 0) {
      plotX <- NULL
      plotY <- NULL
      byVarMatX <- NULL
      byVarMatY <- NULL
      for (nn in 1:length(exUncertSplines)) {
        plotX[[nn]] <- exUncertSplines[[nn]][[1]]
        plotY[[nn]] <- exUncertSplines[[nn]][[2]]
        byVarMatY <- cbind(byVarMatY, plotY[[nn]][, predV])
        byVarMatX <- cbind(byVarMatX, plotX[[nn]][, predV])
      }
      fullPlotX <- fullGDMsplines[[1]]
      fullPlotX <- fullPlotX[, predV]
      fullPlotY <- fullGDMsplines[[2]]
      fullPlotY <- fullPlotY[, predV]
      sdX <- apply(as.matrix(byVarMatX), 1, sd)
      sdY <- apply(as.matrix(byVarMatY), 1, sd)
      # EDIT : the plotting values don't need to be generated, because we don't produce a plot any more
      # highBoundX <- fullPlotX + sdX
      # lowBoundX <- fullPlotX - sdX
      # highBoundY <- fullPlotY + sdY
      # lowBoundY <- fullPlotY - sdY
      # if (predV == "Geographic") {
      #   rugData <- unique(sqrt(((spTable$s1.xCoord - 
      #                              spTable$s2.xCoord)^2) + ((spTable$s1.yCoord - 
      #                                                          spTable$s2.yCoord)^2)))
      # } else {
      #   varDat <- grep(predV, colnames(spTable))
      #   rugData <- unique(c(spTable[, c(varDat[1])], 
      #                       spTable[, c(varDat[2])]))
      # }
      # if (one_page_per_plot) {
      #   dev.new()
      #   dev.next()
      # } else {
      #   thisplot <- thisplot + 1
      #   if (thisplot > (plot.layout[1] * plot.layout[2])) {
      #     thisplot <- 1
      #     par(mfrow = plot.layout)
      #   }
      # }
      # EDIT : no need for a plot any more
      # plot(NULL, xlim = c(totalXmin, totalXmax), ylim = c(totalYmin, 
      #                                                     totalYmax), xlab = predV, ylab = "Partial Ecological Distance")
      # polygon(c(lowBoundX, rev(highBoundX)), c(lowBoundY, 
      #                                          rev(highBoundY)), col = errCol, border = NA)
      # lines(fullPlotX, fullPlotY, col = splineCol, lwd = plot.linewidth)
      # rug(rugData)
      
      # EDIT : fill output table with values
      # get max value and sd of it (mean +- sd)
      restab[varname == predV, yatmax := fullPlotY[length(fullPlotY)]]
      restab[varname == predV, sdy := sdY[length(sdY)]]
    }
  }
  # EDIT : return results table
  return(restab)
}
NULL
