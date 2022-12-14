# twoxtwoxkTables

<!-- badges: start -->
[![R-CMD-check](https://github.com/smeah/twoxtwoxkTables/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/smeah/twoxtwoxkTables/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

### R package for analysis of 2x2xk tables

Commonly used in epidemiology, 2x2xk tables are a way to express the relationship between a binary independent variable (often an exposure or treatment) and a binary dependent variable (often an outcome or disease).
This package provides functions that generate common epidemiological measures for 2x2xk tables.  These functions are in many cases more conveinent and faster than current methods to calcuate these measures using functions in CRAN.

A comparison in usage, convenience, and speed to existing functions published on CRAN is provided in the package's vignette.

This package is not published to CRAN, but can be installed (without the vignette included) by running the line `devtools::install_github("smeah/twoxtwoxkTables")` in the R console.  

To install the package with the vignette included, run `devtools::install_github("smeah/twoxtwoxkTables", build_vignettes = TRUE)`.  **Note that to build the vignette, you will need to download the `samplesizeCMH` and `bench` packages if you have not already.**  To view the vignette, run `browseVignettes("twoxtwoxkTables")`.

This package was created by Sabir Meah (smeah@umich.edu) as a part of BIOSTAT-625: Computing with Big Data at the University of Michigan.

Functions:

* `cond.odds.ratios()` - Conditional Odds Ratios

Calculates the conditional odds ratios for each of the k strata in a 2x2xk table.

* `common.odds.ratio()` - Common Odds Ratios

Calculates the common odds ratio across all k strata in a 2x2xk table.

* `mantelhaenzel.test()` - Cochran–Mantel–Haenszel Test

Performs the Cochran–Mantel–Haenszel chi-squared test (two-sided without continuity correction) on a 2x2xk table.  
Tests the null hypothesis that the common odds ratio is 1 against the alternative hypothesis that it is not 1.
