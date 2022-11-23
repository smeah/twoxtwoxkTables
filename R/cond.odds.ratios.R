#' Conditional Odds Ratios of 2x2 or 2x2xk Table
#'
#' Function to calculate the odds ratio of a single 2x2 table or conditional odds ratios of a 2x2xk table.
#'
#' @param x Either a two dimensional 2x2 table or a 3 dimensional 2x2xk table/array of 2x2 tables.
#'
#' @return A scalar of the single odds ratio if a single 2x2 table is passed, or a vector of conditional odds ratios if a 2x2xk table/array of 2x2 tables is passed.
#'
#' @examples
#' ## A single 2x2 table
#' a <- array(c(1,3,5,6),
#'            dim = c(2,2))
#' cond.odds.ratios(a)
#'
#' ## A 2x2xk table or array of 2x2 tables
#' b <- array(c(1,3,5,6,
#'              2,7,3,1),
#'            dim = c(2,2,2))
#' cond.odds.ratios(b)
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
