context("checked_frame_class")

columns <- ColumnCheckList(foo = ColumnChecks(classtype="numeric"))
checks <- TableChecks(columns=columns)
Bar <- checked_frame_class("Bar", checks)

test_that("creating objects works", {
  expect_is(Bar(foo = 1:10), "Bar")
})

test_that("creating objects works", {
  expect_error(Bar(foo = letters), "invalid class")
})

test_that("as(object, \"data.frame\") produces an S3 data.frame", {
  foo <- Bar(foo = 1:10)
  expect_false(isS4(as(foo, "data.frame")))
})
