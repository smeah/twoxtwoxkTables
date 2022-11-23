#' Common Odds Ratio of 2x2xk Table
#'
#' Function to calculate the common odds ratio of a 2x2xk table.
#'
#' @param x A 3 dimensional 2x2xk table/array of 2x2 tables.
#'
#' @return The common odds ratio of the 2x2xk table.
#'
#' @example
#' a <- array(c(1,3,5,6,
#'              2,7,3,1,
#'              2,4,9,2),
#'            dim = c(2,2,3))
#' common.odds.ratio(a)
#'
#' @export
common.odds.ratio <- function(x){

  # Checking passed argument is valid
  # Must be an array of multiple 2x2 tables
  if(length(dim(x)) != 3){
    stop("Please pass a 3-dimensional table")
  }
  if(dim(x)[1] != 2 | dim(x)[2] != 2){
    stop("Dimension of tables in array is not 2x2")
  }

  # Iteratively calculating the common odds ratio
  # Iterating through each 2x2 table and adding each partial result of the numerator and denominator, both of which are summations
  numerator <- 0
  denominator <- 0
  for(i in 1:dim(x)[3]){
    numerator <- numerator + x[1,1,i] * x[2,2,i] / sum(x[,,i])
    denominator <- denominator + x[1,2,i] * x[2,1,i] / sum(x[,,i])
  }

  # Checking if common odds ratio can be calculated
  # If so, return common odds ratio
  if(denominator > 0 & numerator >= 0){
    return(numerator/denominator)
  } else {
    stop("Common odds ratio is undefined")
  }
}
