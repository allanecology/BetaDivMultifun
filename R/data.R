# This file contains documentation of the package datasets stored in data/

#' GDM model names
#'
#' A helper dataset to keep an overview about the models, and creating automated input.
#'
#' @format ## `who`
#' A data frame with 7,240 rows and 60 columns:
#' \describe{
#'   \item{modelname}{Name of a given model as required as input for reading, writing data and naming plots.}
#'   \item{luiinput}{DEPRECIATED Is land-use given as index LUI or as individual component Grazing, Fertilisation or Mowing?}
#'   \item{permut}{Indicates wether p-values have been permuted and wether they are available for the given model.}
#'   \item{funs}{Indicates the model component "function" which is read from the raw files "EFmaster".
#'    Is the name of the response variable used in the respective model.}
#'   \item{lui}{again indicates the land-use component given, see luiinput. Having 2 columns is a remnant of analysis
#'   writing, I have not been aware that I used 2 naming systems. lui is the current one.}
#'   \item{model_class}{Indicates the class of the models, because not all possible models are used to produce all output.}
#' }
#' @source this package
"model_names"
