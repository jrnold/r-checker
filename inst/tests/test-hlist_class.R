context("hlist_class")

test_that("hlist_class creates a subclass", {
  foo <- hlist_class("foo", "integer")
  expect_is(foo, "function")
  expect_true(isClass("foo"))
})

test_that("hlist_class works as expected", {
  Foo <- hlist_class("Foo", "numeric")
  foo <- Foo(a=1, b=2)
  expect_is(foo, "Foo")
  expect_equal(foo@.Data, list(1, 2))
  expect_equal(names(foo), c("a", "b"))
  expect_equal(foo@classtype, "numeric")
})

test_that("hlist_class new class gives error if classtype altered", {
  Foo <- hlist_class("Foo", "numeric")
  foo <- Foo(a=1, b=2)
  slot(foo, "classtype") <- "character"
  expect_error(validObject(foo), "invalid class")
})

###############

Foo <- hlist_class("Foo", "numeric")
foo <- Foo(a=1, b=2)

#######

context("Subclass of HList c method")

test_that("c-method: test #1", {
  bar <- Foo(c=3)
  baz <- c(foo, bar)
  expect_equal(baz, Foo(a=1, b=2, c=3))
})

test_that("c-method: test #2", {
  bar <- list(c=3)
  baz <- c(foo, bar)
  expect_equal(baz, Foo(a=1, b=2, c=3))
})

test_that("c-method: test #3", {
  bar <- list(d="4")
  expect_error(c(foo, bar), "invalid class")
})


#######

context("Subclass of HList [ method")

test_that("[-method integer works", {
  expect_equal(foo[2], Foo(b=2))
})

test_that("[-method character works", {
  expect_equal(foo["b"], Foo(b=2))
})

test_that("[-method missing works", {
  expect_equal(foo[], foo)
})

#######

context("Subclass of HList [<- method")

test_that("[<- with missing,vector works", {
  foo[] <- 3
  expected <- Foo(a=3, b=3)
  expect_equal(foo, expected)
})

test_that("[<- with missing,list works", {
  foo[] <- list(a=3, b=4)
  expected <- Foo(a=3, b=4)
  expect_equal(foo, expected)
})

test_that("[<- with character: test #1", {
  foo["a"] <- 100
  expected <- Foo(a=100, b=2)
  expect_equal(foo, expected)
})

test_that("[<- with integer: test #1", {
  foo[1] <- c(100)
  expected <- Foo(a=100, b=2)
  expect_equal(foo, expected)
})

##########
context("Subclass of HList [[<- method")

test_that("[[<- with missing throws error", {
  expect_error({foo[[]] <- 1}, regexp="missing subscript")
})

test_that("[[<- with character", {
  foo[["a"]] <- 100
  expect_equal(foo, Foo(a=100, b=2))
})

test_that("[[<- with numeric", {
  foo[[1]] <- 100
  expect_equal(foo, Foo(a=100, b=2))
})

#########

context("Subclass of HList $<- method")

test_that("$<-,HList works", {
  foo$a <- 100
  expect_equal(foo, Foo(a=100, b=2))
})

########

context("subclass HList names<- method")

test_that("names<- with character works", {
  names(foo) <- c("d", "e")
  expect_equal(foo, Foo(d=1, e=2))
})

test_that("names<- with NULL works", {
  names(foo) <- NULL
  expect_equal(foo, Foo(1, 2))
})

###########

context("subclass HList length<- method")

test_that("length<- works with value < length(object)", {
  length(foo) <- 1
  expect_equal(foo, Foo(a=1))
})

test_that("length<- works with value == length(object)", {
  length(foo) <- length(foo)
  expect_equal(foo, Foo(a=1, b=2))
})

test_that("length<- works with value > length(object)", {
  length(foo) <- length(foo) + 1
  expect_is(foo, "Foo")
  expect_equal(foo@.Data, list(1, 2, NULL))
  expect_equal(foo@names, c("a", "b", ""))
})
