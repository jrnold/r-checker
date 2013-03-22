context("checked_frame_class")

columns <- ColumnCheckList(foo = ColumnChecks(classtype="numeric"))
checks <- TableChecks(columns=columns)

test_that("creating objects works", {
  Bar <- checked_frame_class("Bar", checks)
  expect_is(Bar(foo = 1:10), "Bar")
})

test_that("creating objects works", {
  Bar <- checked_frame_class("Bar", checks)
  expect_error(Bar(foo = letters), "invalid class")
})

test_that("as(object, \"data.frame\") produces an S3 data.frame", {
  Bar <- checked_frame_class("Bar", checks)
  foo <- Bar(foo = 1:10)
  expect_false(isS4(as(foo, "data.frame")))
})

