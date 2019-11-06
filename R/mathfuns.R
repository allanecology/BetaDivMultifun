#' Normalisation / scale to 0 and 1
#'
#' This function normalises a vector to be between 0 and 1. The minimum
#' is substracted from a sample and the result is divided by the range
#' of the vector. As used in Soliveres 2016.
#' NA values are ignored.
#' \deqn{ \frac{ x - min(x) }{ max(x) - min(x) } = \frac{x - min(x)}{range(x)}}{(x - min(x) / (max(x) - min(x)) )}

#' @param x an input vector of values to be scaled.
#' @return a vector of scaled values to be between 0 and 1.
#' 
#' @examples
#' x = sample(-100:100, 50) # create sample
#' normalized = (x-min(x))/(max(x)-min(x)) # normalized Data
#' par(mfrow=c(1,2)) # plot both example data and normalized data
#' plot(x)
#' plot(normalized) # the shape is not affected, only the range changed
#' # with data.table:
#' datatable[, (mysubset) := lapply(.SD, function(cols) calc_presenceabsence(cols, treshold=treshold, type = "max5")),.SDcols=mysubset]
#' 
#' @export
scale01 <- function(x){
  return((x - (min(x,na.rm=T)))/(max(x,na.rm=T)-min(x,na.rm=T)))
}

NULL




#' Get random vector from a truncated normal distribution
#' 
#' Get a vector of random numbers, drawn from a normal distribution, 
#' which is truncated (e.g. between 0 and 1).
#' Has been used to generate the example dataset.
#' The mean is given, the standard deviation is the mean / 3. 
#' @param length_of_vector number, length of returned vector
#' @param mean number, the mean of the normal distribution. 
#' @param max number, the maximum which is assigned to all values which
#' exceed the limits
#' @param min number, the minimum which is assigned to all values which 
#' are below the given limit.
#' @return a numeric vector of randomly generated numbers

get_random_number_from_truncated_normal_dist <- function(length_of_vector, mean, max, min){
  random_vector <- rnorm(length_of_vector, mean, mean/3)
  random_vector[random_vector > max] <- max
  random_vector[random_vector < min] <- min
  return(random_vector)
}

NULL
