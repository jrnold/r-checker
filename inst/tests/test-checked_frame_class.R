context("checked_frame_class")

columns <- ColumnCheckList(foo = ColumnChecks(classtype="numeric"))
checks <- TableChecks(columns=columns)

test_that("creating objects works", {
  Foo <- checked_frame_class("Foo", checks)
  expect_is(Foo(foo = 1:10), "Foo")
})

test_that("creating objects works", {
  Foo <- checked_frame_class("Foo", checks)
  expect_error(Foo(foo = letters), "invalid class")
})

