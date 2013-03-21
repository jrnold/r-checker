context("nlist")

test_that("nlist returns a namedList object", {
  foo <- nlist(a=1, b=2)
  expect_is(foo, "namedList")
  expect_equal(names(foo), c("a", "b"))
})
