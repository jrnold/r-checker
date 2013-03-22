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

