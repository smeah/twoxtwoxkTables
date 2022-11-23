#' Conditional Odds Ratios of 2x2 or 2x2xk Table
#'
#' Function to calculate the odds ratio of a single 2x2 table or conditional odds ratios of a 2x2xk table.
#'
#' @param x Either a two dimensional 2x2 table or a 3 dimensional 2x2xk table/array of 2x2 tables
#'
#' @return A scalar of the single odds ratio if a single 2x2 table is passed, or a vector of conditional odds ratios if a 2x2xk table/array of 2x2 tables is passed
#'
#' @examples
#'
#'
#' @export
cond.odds.ratios <- function(x){

  # Checking passed argument is valid
  # Can be either a single 2x2 table or an array of multiple 2x2 tables
  if(length(dim(x)) != 2 & length(dim(x)) != 3){
    stop("Please pass a single 2x2 table or array of 2x2 tables")
  }
  if(dim(x)[1] != 2 | dim(x)[2] != 2){
    stop("Dimension of tables is not 2x2")
  }

  # Calculating the odds ratio of a single 2x2 table if that is the passed argument
  if(length(dim(x)) == 2){
    return(x[1,1] * x[2,2] / (x[1,2] * x[2,1]))
  }

  # Calculating the odds ratio of each 2x2 table if an array of multiple 2x2 tables is passed
  else if(length(dim(x)) == 3) {
    # Pre-allocating vector where each odds ratio will be stored
    res <- rep(NA_real_, dim(x)[3])
    # Calculating the odds ratio of each 2x2 table in a for loop
    for(i in 1:dim(x)[3]){
      res[i] = x[1,1,i] * x[2,2,i] / (x[1,2,i] * x[2,1,i])
    }
    return(res)
  }

  else {
    # If input has neither 2 or 3 dimensions, it is an illegal input
    # Should be taken care of at beginning, but including it as a stopping condition here as well just in case
    stop("Please pass a single 2x2 table or array of 2x2 tables")
  }
}

# Function to calculate the common odds ratio of an array of multiple 2x2 tables
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

# Function to perform the Cochran–Mantel–Haenszel test on an array of multiple 2x2 tables
# Returns the common odds ratio (sample estimate), CMH chi-squared test statistic, and associated p-value
#' @export
mantelhaenzel.test <- function(x){

  # Checking passed argument is valid
  # Must be an array of multiple 2x2 tables
  if(length(dim(x)) != 3){
    stop("Please pass a 3-dimensional table")
  }
  if(dim(x)[1] != 2 | dim(x)[2] != 2){
    stop("Dimension of tables in array is not 2x2")
  }

  # Iteratively calculating the test statistic
  # Iterating through each 2x2 table and adding each partial result of the numerator and denominator, both of which are summations
  # Numerator is squared summation, so square after for loop
  numerator <- 0
  denominator <- 0
  for(i in 1:dim(x)[3]){
    row_tot1 <- sum(x[1,,i])
    row_tot2 <- sum(x[2,,i])
    col_tot1 <- sum(x[,1,i])
    col_tot2 <- sum(x[,2,i])
    grand_tot <- sum(x[,,i])
    numerator <- numerator + (x[1,1,i] - row_tot1 * col_tot1 / grand_tot)
    denominator <- denominator + (row_tot1 * row_tot2 * col_tot1 * col_tot2 / (grand_tot ^ 2 * (grand_tot - 1)))
  }
  numerator <- numerator ^ 2

  # Checking if valid CMH statistic can be calculated
  if(denominator <= 0 | numerator < 0){
    stop("Mantel-Haenszel test statistic is undefined")
  }

  # Calling common odds ratio function to get common odds ratio of the array 2x2 tables
  common.or <- common.odds.ratio(x)
  names(common.or) <- "Common Odds Ratio"

  # Calcuating test statistic from numerator and denominator calculated iteratively
  chi.sq.stat <- numerator/denominator
  names(chi.sq.stat) <- "Chi-Square Test Statistic"

  # Calculating p-value from test statistic and chi-squared distribution with 1 degree of freedom
  p.val <- dchisq(chi.sq.stat, df = 1)
  names(p.val) <- "p-value"

  # Returning vector of common odds ratio, test statistic, and p-value
  # All of which are named
  return(c(common.or, chi.sq.stat, p.val))
}






