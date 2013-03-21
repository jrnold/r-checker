context("check_constraints with ColumnCheckList")

columns <- ColumnCheckList(foo = ColumnChecks(classtype="numeric"))
tablechecks0 <- TableChecks()

test_that("works with default args", {
  expect_equal(check_constraints(data.frame(foo=1:10), tablechecks0), TRUE)
})

test_that("works with default args and empty data frame", {
  expect_equal(check_constraints(data.frame(), tablechecks0), TRUE)
})

test_that("works with non-empty columns", {
  checks <- TableChecks(columns = columns)
  expect_equal(check_constraints(data.frame(bar=1:10, foo=1:10), checks),
               TRUE)
})

test_that("catches bad column type", {
  checks <- TableChecks(columns = columns)
  expect_equal(check_constraints(data.frame(foo=letters), checks),
               "Invalid column “foo”: ‘object’ does not inherit from “numeric”")
})

test_that("catches missing column", {
  checks <- TableChecks(columns = columns)
  expect_equal(check_constraints(data.frame(bar=letters), checks),
               "Column “foo” not present")

})

test_that("catches extra columns if exclusive=TRUE", {
  checks <- TableChecks(columns = columns, exclusive=TRUE)
  object <- data.frame(foo=1:26, bar=letters)
  expect_equal(check_constraints(object, checks),
               "Extra columns: “bar”")
})

test_that("catches out of order columns if ordered=TRUE", {
  checks <- TableChecks(columns = columns, ordered=TRUE)
  object <- data.frame(bar = letters, foo=1:26)
  expected <- "Columns not in order\nExpected order:“foo”\nOut of order columns:“bar”,“foo”"
  expect_equal(check_constraints(object, checks), expected)
})

test_that("catches excluded columns if excluded not empty", {
  checks <- TableChecks(columns = columns, exclude="bar")
  object <- data.frame(bar = letters, foo=1:26)
  excluded <- "Columns which should not be in the data.frame: “bar”"
  check_constraints(object, checks)
})

test_that("catches arbitrary constraints", {
  toomanyrows <- function(x) {nrow(x) < 5}
  checks <- TableChecks(columns = columns,
                        constraints=FunctionList(nrows=toomanyrows))
  object <- data.frame(bar = letters, foo=1:26)
  expect_equal(check_constraints(object, checks),
               "Failed constraint “nrows”")
})

test_that("catches arbitrary constraints", {
  toomanyrows <- function(x) {nrow(x) < 5}
  checks <- TableChecks(columns = columns,
                        constraints=FunctionList(toomanyrows))
  object <- data.frame(bar = letters, foo=1:26)
  expect_equal(check_constraints(object, checks),
               "Failed constraint #1")
})

test_that("constraints work if data is valid", {
  numberrows <- function(x) {nrow(x) > 5}
  checks <- TableChecks(columns = columns,
                        constraints=FunctionList(nrows=numberrows))
  object <- data.frame(bar = letters, foo=1:26)
  expect_equal(check_constraints(object, checks), TRUE)
})


