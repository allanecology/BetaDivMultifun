#' Calculate Presence-Absence from continuous
#'
#' based on treshold
#' chose between 50% Quantile OR 50% of maximum 5 values
#' TODO :add more explanation
#' maximum_observed_level = average of top 5 sites
#' calculate the multifunctionality of a (standardized) vector

#' @param x input data.table
#' @param threshold the threshold for calculation, between 0 and 1, e.g. 0.5
#' @return a vector of scaled values to be between 0 and 1.
#' 
#' @examples
#' x <- c(0,0.1,0.4,0.5,0.8,0.9,1)
#' calc_presenceabsence(x,treshold=0.3)
#' 
#' @export
calc_presenceabsence <- function(x, threshold = 0.5, type = "max5"){
  # x is a vector
  max_obs <- mean(sort(x,decreasing = T, na.last=T)[1:5])
  new_vector <- x>(max_obs*threshold)
  return(new_vector)
}

NULL
