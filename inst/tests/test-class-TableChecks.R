context("ColumnCheckList class")

test_that("ColumnCheckList works without args", {
  expect_is(ColumnCheckList(), "ColumnCheckList")
})

test_that("ColumnCheckList works with args", {
  expect_is(ColumnCheckList(alpha = ColumnChecks(),
                           beta = ColumnChecks(classtype="numeric")),
           "ColumnCheckList")
})

test_that("ColumnCheckList throws error if empty names", {
  expect_error(ColumnCheckList(ColumnChecks()), "invalid class")
})

test_that("ColumnCheckList throws error if bad types", {
  expect_error(ColumnCheckList("foo"), "invalid class")
})
