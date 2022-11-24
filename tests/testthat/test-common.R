library(twoxtwoxkTables, quietly = TRUE, warn.conflicts = FALSE)
library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(stats, quietly = TRUE, warn.conflicts = FALSE)
data("Titanic")
partial_tables <- margin.table(Titanic, c(2,4,1))
test_that("common odds ratios", {
  expect_equal(common.odds.ratio(partial_tables),
               as.numeric(mantelhaen.test(partial_tables)$estimate))
})
