#' plotUncertainty_slim : Get spline uncertainty
#' 
#' This function is a "slim" version of the `gdm::plotUncertainty` function.
#' 
#' Two changes have been applied to the functio : (1)
#' The plotting has been disabled, because the function is runned on a cluster
#' and no plotting output is desired (or can later be done based on the extracted)
#' values.
#' (2) The saving option is edited, such that the output of the function is returned
#' to a variable rather than saved as .csv. This is more appropriate in the
#' cluster surrounding.
#' 
#' All parameters are the same as in the original function, but a few of them are 
#' depreciated :
#' unneeded plotting variables are : `splineCol`, `errCol`, `plot.linewidth`, `plot.layout`
#' unneeded saving variables are : `save`, `fileName`
#' Please consult the original description of the function by the `gdm` package authors,
#' because this is really just a simple edit.
#' 
#' @return a data.frame containing "6 values for each predictor, three each for the x and
#' y values. For each x and y, there are the full model values, the upper and the lower bound."
#' (taken from the original `gdm::plotUncertainty` function description)
#' @param spTable see `gdm::plotUncertainty` function description
#' @param sampleSites see `gdm::plotUncertainty` function description. 0.3 was used in Gossner 2016.
#' @param bsIters see `gdm::plotUncertainty` function description. 100 were used in Gossner 2016.
#' @param geo is set to FALSE by default, see `gdm::plotUncertainty` function description
#' @param splines is set to NULL by default, see `gdm::plotUncertainty` function description
#' @param knots is set to NULL by default, see `gdm::plotUncertainty` function description
#' @param parallel is set to FALSE by default, see `gdm::plotUncertainty` function description
#' @param cores is set to 2 by default, see `gdm::plotUncertainty` function description
#' 
#' @examples
#' # read in gdminput from `results_nonpublic.R`.
#' test <- plotUncertainty_slim(gdminput, sampleSites = 0.3, bsIters = 5, geo = T)
#' 
#' @export
plotUncertainty_slim <- function (spTable, sampleSites, bsIters, geo = FALSE, splines = NULL, 
                                  knots = NULL, splineCol = "blue", errCol = "grey80", plot.linewidth = 2, 
                                  plot.layout = c(2, 2), parallel = FALSE, cores = 2, save = FALSE, 
                                  fileName = "gdm.plotUncertainy.csv"){
  # below : list of parameters. If you are a user, please ignore.
  #    if you are a developer, the list serves to set the function parameters
  #    manually in case the functino is worked on.
  # spTable = gdminput
  # sampleSites <- 0.3
  # bsIters = 5
  # geo = T
  # splines = NULL
  # knots = NULL
  # parallel = FALSE
  # cores=2
  # save = F
  # fileName = "gdm.plotUncertainy.csv"
  # plot.layout = c(2, 2)
  
  if (!is(spTable, "gdmData")) {
    warning("The spTable object is not of class 'gdmData'. See the formatsitepair function for help.")
  }
  if (!(is(spTable, "gdmData") | is(spTable, "matrix") | is(spTable, 
                                                            "data.frame"))) {
    stop("The spTable object must be of class 'gdmData', 'matrix', or 'data.frame'.")
  }
  if (ncol(spTable) < 6) {
    stop("spTable object requires at least 6 columns: Observed, weights, s1.xCoord, s1.yCoord, s2.xCoord, s2.yCoord")
  }
  if (nrow(spTable) < 1) {
    stop("spTable object has < 1 rows.")
  }
  if (!(geo == TRUE | geo == FALSE)) {
    stop("geo argument must be either TRUE or FALSE")
  }
  if (is.null(splines) == FALSE & !is(splines, "numeric")) {
    stop("splines object must of of class = 'numeric'.")
  }
  if (is.null(knots) == FALSE & !is(knots, "numeric")) {
    stop("knots object must of of class = 'numeric'.")
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
    stop("a sampleSites value of 0 will remove all sites from the analysis (bad).")
  }
  if (save & is.null(fileName)) {
    stop("Save is TRUE, but no fileName provided.")
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
                        .packages = c("gdm")) %dopar% subsample.sitepair(lstSP[[k]], 
                                                                         sampleSites = sampleSites)
    gdmMods <- foreach(k = 1:length(subSamps), .verbose = F, 
                       .packages = c("gdm")) %dopar% gdm(subSamps[[k]], 
                                                         geo = geo, splines = splines, knots = knots)
    stopCluster(cl)
  } else {
    subSamps <- lapply(lstSP, subsample.sitepair, sampleSites = sampleSites) # subsample the tables to form subsampled (reduced) datasets
    gdmMods <- lapply(subSamps, gdm, geo = geo, splines = splines, 
                      knots = knots) # create a GDM model for each subsampled dataset
  }
  fullGDMmodel <- gdm(spTable, geo = geo, splines = splines, 
                      knots = knots) # the model on the full dataset
  devExps <- lapply(gdmMods, function(x) {
    x$explained
  }) # collect deviance explained of each subsampled dataset
  devExps <- unlist(devExps)
  exUncertSplines <- lapply(gdmMods, isplineExtract) # extract splines from each model of the subsampled datasets
  fullGDMsplines <- isplineExtract(fullGDMmodel)
  predVars <- colnames(exUncertSplines[[1]][[1]]) # create vector of names of predictors
  # edit below : outcommented plotting code, just setting the layout of the resulting plot
  # thisplot <- 0
  # one_page_per_plot <- FALSE
  # if ((plot.layout[1] == 1) && (plot.layout[2] == 1)) {
  #   one_page_per_plot <- TRUE
  # } else {
  #   par(mfrow = plot.layout)
  # }
  totalYmin <- Inf
  totalYmax <- -Inf
  # the plotting is done on 200 values which are equally distributed across the 
  # range of the predictor. Set variables to capture those 200 variables
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
  # create empty outdata table and fill it (with the loop)
  outData <- data.frame(matrix(nrow = 200, ncol = length(predVars) * 6)) 
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
      highBoundX <- fullPlotX + sdX
      lowBoundX <- fullPlotX - sdX
      highBoundY <- fullPlotY + sdY
      lowBoundY <- fullPlotY - sdY
      if (p == 1) {
        start <- p
        end <- p * 6
      } else {
        start <- end + 1
        end <- p * 6
      }
      # fill in values to outData
      outData[, start:end] <- cbind(lowBoundX, fullPlotX, 
                                    highBoundX, lowBoundY, fullPlotY, highBoundY)
      colnames(outData)[start:end] <- paste(predV, c("minusSD_X", 
                                                     "fullModel_X", "plusSD_X", "minusSD_Y", "fullModel_Y", 
                                                     "plusSD_Y"), sep = "_")
      if (predV == "Geographic") {
        rugData <- unique(sqrt(((spTable$s1.xCoord - 
                                   spTable$s2.xCoord)^2) + ((spTable$s1.yCoord - 
                                                               spTable$s2.yCoord)^2)))
      } else {
        varDat <- grep(predV, colnames(spTable))
        rugData <- unique(c(spTable[, c(varDat[1])], 
                            spTable[, c(varDat[2])]))
      }
      # edit below : outcommented plotting part
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
      # plot(NULL, xlim = c(totalXmin, totalXmax), 
      #      ylim = c(totalYmin, totalYmax), 
      #      xlab = predV, ylab = "Partial Ecological Distance")
      # polygon(c(lowBoundX, rev(highBoundX)), c(lowBoundY, 
      #                                          rev(highBoundY)), col = errCol, border = NA)
      # lines(fullPlotX, fullPlotY, col = splineCol, lwd = plot.linewidth)
      # rug(rugData)
    }
  }
  # saving is not done within the function
  # if (save) {
  #   write.csv(outData, fileName, row.names = F)
  # }
  return(outData)
}























#' plotUncertainty_getsd : Get GDM sd at max height DEPRECIATED
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
