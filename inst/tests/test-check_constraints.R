context("check_constraints with ColumnChecks")

gt0 <- function(x) {x > 0}
lt10 <- function(x) {x < 10}

checks1 <- ColumnChecks()
checks2 <- ColumnChecks(classtype="numeric",
                        missings=FALSE, unique=TRUE,
                        constraints=FunctionList(a=gt0, lt10))

test_that("column_constraints works with no restrictions", {
  expect_equal(check_constraints(letters, checks1),
              TRUE)
})

test_that("column_constraints works with no restrictions, empty data frame", {
  expect_equal(check_constraints(numeric(), checks1), TRUE)
})

test_that("column_constraints works with valid column", {
  foo <- 1:20
  expect_equal(check_constraints(foo, checks2), TRUE)
})

test_that("column constraints catches bad class", {
  foo <- letters
  expect_equal(check_constraints(foo, checks2),
               "‘object’ does not inherit from “numeric”")
})

test_that("column constraints catches missing", {
  foo <- c(NA, 1:5)
  expect_equal(check_constraints(foo, checks2),
               "‘object’ has missing values")
})

test_that("column constraints catches duplicates", {
  foo <- c(1, 1, 2)
  expect_equal(check_constraints(foo, checks2),
               "‘object’ has duplicated values")
})

test_that("arbitrary named constraints works", {
  foo <- -1:1
  expect_equal(check_constraints(foo, checks2), "Failed constraint “a”")
})

test_that("arbitrary unnamed constraints works", {
  foo <- 9:11
  expect_equal(check_constraints(foo, checks2), "Failed constraint #2")
})

