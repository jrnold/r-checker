context("check_constraints with ColumnChecks")

gt0 <- function(x) {x > 0}

checks1 <- ColumnChecks()
checks2 <- ColumnChecks(type="numeric",
                        missings=FALSE, unique=TRUE,
                        constraints=FunctionList(a=gt0))

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




                        


