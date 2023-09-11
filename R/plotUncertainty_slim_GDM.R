#' plotUncertainty_slim : Get spline uncertainty
#' 
#' This function is a "slim" version of the `gdm::plotUncertainty` function.
#' 
#' Two changes have been applied to the function : (1)
#' The plotting has been disabled, because the function is runned on a cluster
#' and no plotting output is desired (or can later be done based on the extracted)
#' values. Additionally, the number of points for predictions is enhanced from 200
#' to 8350 points (the number of observations in the dataset). Note that this change
#' is hard-coded, and is also changed in the function `isplineExtract.edit`
#' (2) The saving option is edited, such that the output of the function is returned
#' to a variable rather than saved as .csv. This is more appropriate in the
#' cluster surrounding.
#' 
#' All parameters are the same as in the original function, but a few of them are 
#' deprecated :
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
    subSamps <- lapply(lstSP, gdm::subsample.sitepair, sampleSites = sampleSites) # subsample the tables to form subsampled (reduced) datasets
    gdmMods <- lapply(subSamps, gdm::gdm, geo = geo, splines = splines, 
                      knots = knots) # create a GDM model for each subsampled dataset
  }
  print("checkpoint 1")
  fullGDMmodel <- gdm(spTable, geo = geo, splines = splines, 
                      knots = knots) # the model on the full dataset
  devExps <- lapply(gdmMods, function(x) {
    x$explained
  }) # collect deviance explained of each subsampled dataset
  devExps <- unlist(devExps)
  # Capture case if NULL model is returned : with function isplineExtract.edit
      # The algorithm was unable to fit a model to your data.
      # The sum of all spline coefficients = 0 and deviance explained = NULL.
      # Returning NULL object.
  exUncertSplines <- lapply(gdmMods, isplineExtract.edit) # extract splines from each model of the subsampled datasets
  print("checkpoint 2")
  fullGDMsplines <- isplineExtract.edit(fullGDMmodel)
  print("checkpoint 2 b")
  predVars <- colnames(exUncertSplines[[1]][[1]]) # create vector of names of predictors
  print("checkpoint 3")
  # -- temporal edit below
  # saveRDS(predVars, file = "~/Desktop/gdm_plotuncertainty_testout.Rds")
  # saveRDS(exUncertSplines, file = "~/Desktop/gdm_plotuncertainty_testoutexsp.Rds")
  #TODO delete this edit
  # -- temporal edit end
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
  # new : the resolution is enhanced to ALL 8350 rows!
  for (p in 1:length(predVars)) {
    predV <- predVars[p]
    for (nm in 1:length(exUncertSplines)) {
      selPlot <- exUncertSplines[[nm]]
      # edit : added handling of NULL cases here
      if(is.null(selPlot[[1]])){
        print("element is NULL. Returning NA")
        spYmax <- NA
        spYmin <- NA
        # neither totalYmax nor totalYmin will change
      } else {
        spYmax <- max(selPlot[[2]][, predV], na.rm = T)
        spYmin <- min(selPlot[[2]][, predV], na.rm = T)
        totalYmax <- max(c(totalYmax, spYmax), na.rm = T)
        totalYmin <- min(c(totalYmin, spYmin), na.rm = T)
      }
    }
  }
  print("checkpoint 4")
  # create empty outdata table and fill it (with the loop)
  outData <- data.frame(matrix(nrow = 8350, ncol = length(predVars) * 8)) 
  for (p in 1:length(predVars)) {
    predV <- predVars[p]
    totalXmin <- Inf
    totalXmax <- -Inf
    print("checkpoint 5")
    for (nm in 1:length(exUncertSplines)) {
      selPlot <- exUncertSplines[[nm]]
      # note : added the if clause below to handle NULL objects.
      if(is.null(selPlot[[1]])){
        spXmax <- NA
        spXmin <- NA
        #TODO keep NA, or better NULL?
      } else {
        spXmax <- max(selPlot[[1]][, predV], na.rm = T)
        spXmin <- min(selPlot[[1]][, predV], na.rm = T)
        if (spXmax > totalXmax) {
          totalXmax = spXmax
        }
        if (spXmin < totalXmin) {
          totalXmin = spXmin
        }
      }
    }
    print("checkpoint 6")
    if (totalYmax != 0) {
      plotX <- NULL
      plotY <- NULL
      byVarMatX <- NULL
      byVarMatY <- NULL
      for (nn in 1:length(exUncertSplines)) {
        if(is.null(exUncertSplines[[nn]][[1]])){
          # edit : handled NULL cases
          plotX[[nn]] <- NULL
          plotY[[nn]] <- NULL
          byVarMatY <- cbind(byVarMatY, NULL)
          byVarMatX <- cbind(byVarMatX, NULL)
        } else {
          plotX[[nn]] <- exUncertSplines[[nn]][[1]]
          plotY[[nn]] <- exUncertSplines[[nn]][[2]]
          byVarMatY <- cbind(byVarMatY, plotY[[nn]][, predV])
          byVarMatX <- cbind(byVarMatX, plotX[[nn]][, predV])
        }
      }
      print("checkpoint 7")
      fullPlotX <- fullGDMsplines[[1]]
      fullPlotX <- fullPlotX[, predV]
      fullPlotY <- fullGDMsplines[[2]]
      fullPlotY <- fullPlotY[, predV]
      sdX <- apply(as.matrix(byVarMatX), 1, sd)
      sdY <- apply(as.matrix(byVarMatY), 1, sd)
      # edit below ---
      # record number of values used to calculate sd
      sdXn <- apply(as.matrix(byVarMatX), 1, function(x) sum(!is.na(x)))
      sdYn <- apply(as.matrix(byVarMatY), 1, function(x) sum(!is.na(x)))
      # edit above ---
      highBoundX <- fullPlotX + sdX
      lowBoundX <- fullPlotX - sdX
      highBoundY <- fullPlotY + sdY
      lowBoundY <- fullPlotY - sdY
      if (p == 1) {
        # edit : changed 6 to 8, because 2 additional columns with number of 
        #    non-null models used to calculate sd.
        start <- p
        end <- p * 8
      } else {
        start <- end + 1
        end <- p * 8
      }
      print("checkpoint 8")
      # fill in values to outData
      outData[, start:end] <- cbind(lowBoundX, fullPlotX, 
                                    highBoundX, lowBoundY, fullPlotY, highBoundY,
                                    sdXn, sdYn)
      colnames(outData)[start:end] <- paste(predV, c("minusSD_X", 
                                                     "fullModel_X", "plusSD_X", "minusSD_Y", "fullModel_Y", 
                                                     "plusSD_Y",
                                                     "SDn", "SDn"), sep = "_")
      if (predV == "Geographic") {
        rugData <- unique(sqrt(((spTable$s1.xCoord - 
                                   spTable$s2.xCoord)^2) + ((spTable$s1.yCoord - 
                                                               spTable$s2.yCoord)^2)))
      } else {
        varDat <- grep(predV, colnames(spTable))
        rugData <- unique(c(spTable[, c(varDat[1])], 
                            spTable[, c(varDat[2])]))
      }
      print("checkpoint 9")
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
