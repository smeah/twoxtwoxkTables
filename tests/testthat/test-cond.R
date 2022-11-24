library(twoxtwoxkTables, quietly = TRUE, warn.conflicts = FALSE)
library(testthat, quietly = TRUE, warn.conflicts = FALSE)

data("Titanic")
partial_tables <- margin.table(Titanic, c(2,4,1))

a <- array(c(1,3,5,6),
           dim = c(2,2))

test_that("conditional odds ratios", {
  expect_equal(cond.odds.ratios(partial_tables),
               as.numeric(apply(partial_tables, 3, samplesizeCMH::odds.ratio)))
  expect_equal(cond.odds.ratios(a),
               as.numeric(samplesizeCMH::odds.ratio(a)))
})
