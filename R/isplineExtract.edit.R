#' isplineExtract.edit
#' 
#' This is an edit of the function in order to "handle" NULL input
#'  returns NULL output for NULL input.
#'  
#'  
#'  

isplineExtract.edit <- function (model){
  if (!is(model, "gdm")) {
    if(is.null(model)){
      outData <- list(NULL)
      print("function isplineExtract.edit : special case of NULL model. returning NULL")
      return(outData)
    } else {
      stop("model argument must be of class = 'gdm'.")
    }
  }
  options(warn.FPU = FALSE)
  PSAMPLE <- 8350 # edit: why is this not 200 any more?
  #                had been set to 8350 to have higher xaxis resolution in june 2022.
  #                also changed in plotUncertainty edit to have 8350 rows.
  preddata <- rep(0, times = PSAMPLE)
  pn <- model$predictors
  nPreds <- length(pn)
  yDat <- xDat <- matrix(0, PSAMPLE, nPreds) # matrix with nrow = nPredictors
  colnames(yDat) <- colnames(xDat) <- pn
  pmin <- 1
  pmax <- PSAMPLE
  splineindex <- 1
  for (i in 1:nPreds) {
    numsplines <- model$splines[i]
    z <- .C("GetPredictorPlotData", pdata = as.double(preddata), 
            as.integer(PSAMPLE), as.double(model$coefficients[splineindex:(splineindex + numsplines - 1)]), 
            as.double(model$knots[splineindex:(splineindex + numsplines - 1)]), 
            as.integer(numsplines), 
            PACKAGE = "gdm")
    yDat[, i] <- z$pdata
    pmin <- pmin + PSAMPLE
    pmax <- pmax + PSAMPLE
    parabol <- 1 + (cumsum(model$splines)[i] - model$splines[i])
    parabola <- cumsum(model$splines)[i]
    xDat[, i] <- seq(from = model$knots[parabol], to = model$knots[parabola], 
                     length = PSAMPLE)
    splineindex <- splineindex + numsplines
  }
  outData <- list(x = xDat, y = yDat)
  return(outData)
}
