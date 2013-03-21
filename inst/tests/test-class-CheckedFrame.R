columns <- ColumnCheckList(foo = ColumnChecks(classtype="numeric"))
checks <- TableChecks(columns=columns)

test_that("initialize works", {
  foo <- new("CheckedFrame", data.frame(foo=1:10), constraints=checks)
  expect_is(foo, "CheckedFrame")
})

test_that("initialize checks validity", {
  expect_is(new("CheckedFrame", data.frame(foo=1:10)),
            "CheckedFrame")
})

test_that("initialize works", {
  foo <- CheckedFrame(data.frame(foo=1:10), constraints=checks)
  expect_is(foo, "CheckedFrame")
})

## Need to rewrite initialize
## test_that("initialize works with empty", {
##   new("CheckedFrame", constraints=checks)
## })

test_that("initialize wihout args works", {
  expect_is(new("CheckedFrame"), "CheckedFrame")
})

########

foo <- CheckedFrame(data.frame(foo=1:10, bar=1:10),
                    constraints=checks)

#################
# [ method

context("[,DataFrameConstr-method")

test_that("[,DataFrameConstr,missing,missing works", {
  expect_equal(foo[drop=FALSE], foo)
})

test_that("[,DataFrameConstr,missing,character with drop=missing  works", {
  expect_equal(foo[ , "foo"], as.numeric(1:10))
})

test_that("[,DataFrameConstr,missing,character with drop=FALSE works", {
  expect_equal(foo[ , "foo", drop=FALSE],
               CheckedFrame(data.frame(foo=1:10), constraints=checks))
})

test_that("[,DataFrameConstr,integer,missing works", {
  expected <- CheckedFrame(foo[1:2], constraints=foo@constraints)
  expect_equal(foo[1:2], expected)
})

test_that("[,DataFrameConstr,integer,missing: test #1", {
  expect_equal(foo[1:2, "foo"], 1:2)
})

test_that("[,DataFrameConstr,integer,missing: test #2", {
  expect_equal(foo[1:2, "bar", drop=FALSE], data.frame(bar=1:2))
})

test_that("[,DataFrameConstr,integer,mssing: test #3", {
  expected <- CheckedFrame(data.frame(foo=1:2, bar=1:2),
                           constraints=foo@constraints)
  expect_equal(foo[1:2, c("foo", "bar"), drop=FALSE], expected)
})

test_that("[,DataFrameConstr drops to data.frame if invalid subset", {
  expected <- data.frame(bar=1:10)
  expect_equal(foo[, c("bar"), drop=FALSE], expected)
})




