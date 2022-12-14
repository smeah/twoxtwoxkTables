% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cond.odds.ratios.R
\name{cond.odds.ratios}
\alias{cond.odds.ratios}
\title{Conditional Odds Ratios of a 2x2 or 2x2xk Table}
\usage{
cond.odds.ratios(x)
}
\arguments{
\item{x}{Either a two dimensional 2x2 table or a 3 dimensional 2x2xk table/array of 2x2 tables.}
}
\value{
A scalar of the single odds ratio if a single 2x2 table is passed, or a vector of conditional odds ratios if a 2x2xk table/array of 2x2 tables is passed.
}
\description{
Calculates the odds ratio of a single 2x2 table or conditional odds ratios of a 2x2xk table.
}
\examples{
## A single 2x2 table
a <- array(c(1,3,5,6),
           dim = c(2,2))
cond.odds.ratios(a)

## A 2x2xk table or array of 2x2 tables
b <- array(c(1,3,5,6,
             2,7,3,1),
           dim = c(2,2,2))
cond.odds.ratios(b)
}
