library(twoxtwoxkTables, quietly = TRUE, warn.conflicts = FALSE)
library(testthat, quietly = TRUE, warn.conflicts = FALSE)
library(stats, quietly = TRUE, warn.conflicts = FALSE)
data("Titanic")
partial_tables <- margin.table(Titanic, c(2,4,1))
test_that("Mantel-Haenzel test", {
  expect_equal(as.numeric(mantelhaenzel.test(partial_tables)$common.or),
               as.numeric(mantelhaen.test(partial_tables, correct = FALSE)$estimate))
  expect_equal(as.numeric(mantelhaenzel.test(partial_tables)$chi.sq.stat),
               as.numeric(mantelhaen.test(partial_tables, correct = FALSE)$statistic))
  expect_equal(as.numeric(mantelhaenzel.test(partial_tables)$p.val),
               as.numeric(mantelhaen.test(partial_tables, correct = FALSE)$p.value))
})
