#' Cochran–Mantel–Haenszel Chi-Squared Test for a 2x2xk Table
#'
#' Performs the Cochran–Mantel–Haenszel chi-squared test without a continuity correction on a 2x2xk table, testing the null hypothesis that the common odds ratio is 1 and the alternative that it is non-zero.
#'
#' @param x A 3 dimensional 2x2xk table/array of 2x2 tables.
#'
#' @return A list including the common odds ratio, chi-squared statistic, and p-value from the test.
#'
#' @examples
#' a <- array(c(1,3,5,6,
#'              2,7,3,1,
#'              2,4,9,2),
#'            dim = c(2,2,3))
#' mantelhaenzel.test(a)
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

  # Calculating test statistic from numerator and denominator calculated iteratively
  chi.sq.stat <- numerator/denominator
  names(chi.sq.stat) <- "Chi-Square Test Statistic"

  # Calculating p-value from test statistic and chi-squared distribution with 1 degree of freedom
  p.val <- pchisq(chi.sq.stat, df = 1, lower.tail = FALSE)
  names(p.val) <- "p-value"

  # Returning list of common odds ratio, test statistic, and p-value
  # All of which are named
  #return(c(common.or, chi.sq.stat, p.val))
  return(list(common.or = common.or, chi.sq.stat = chi.sq.stat, p.val = p.val))
}
