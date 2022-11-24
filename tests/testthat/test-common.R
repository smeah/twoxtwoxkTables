library(twoxtwoxkTables, quietly = TRUE, warn.conflicts = FALSE)
library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(stats, quietly = TRUE, warn.conflicts = FALSE)

data("Titanic")
partial_tables <- margin.table(Titanic, c(2,4,1))

a <- array(c(1,3,5,6,
             2,7,3,1,
             2,4,9,2),
           dim = c(2,2,3))

test_that("common odds ratios", {
  expect_equal(common.odds.ratio(partial_tables),
               as.numeric(mantelhaen.test(partial_tables)$estimate))
  expect_equal(common.odds.ratio(a),
               as.numeric(mantelhaen.test(a)$estimate))
})
