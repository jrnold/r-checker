context("HList-class")

test_that("new(\"HList\", ...) with args works", {
    foo <- new("HList", list(a=1, b=2, c=3), classtype = "numeric")
    expect_is(foo, "HList")
    expect_equal(foo@.Data, list(1, 2, 3))
    expect_equal(names(foo), c("a", "b", "c"))
    expect_equal(foo@classtype, "numeric")
})

test_that("new(\"HList\", ...) without args works", {
    foo <- new("HList")
    expect_is(foo, "HList")
    expect_equal(foo@.Data, list())
    expect_equal(names(foo), character())
    expect_equal(foo@classtype, "ANY")
})

test_that("new(\"HList\") works with NULL elements", {
    foo <- new("HList", list(a=1, NULL, 2, NULL))
    expect_is(foo, "HList")
    expect_equal(names(foo), c("a", "", "", ""))
    expect_equal(foo@classtype, "ANY")
})


test_that("HList() works", {
  expect_identical(HList(list(a=1, b=2), "numeric"),
                   new("HList", list(a=1, b=2), "numeric"))
})

test_that("Error if bad classtype", {
    expect_error(new("HList", letters, classtype="integer"),
                 "Not all elements have class")
})

test_that("Error if length classtype > 1", {
    expect_error(new("HList", 1:4, classtype=c("integer", "character")),
                 "length != 1")
})

#############################################
## Methods

foo <- new("HList", list(a=1, b=2, c=3), classtype="numeric")

#######

context("c,HList method")

test_that("c-method: Homoglist,Homoglist works", {
  bar <- new("HList", list(d=4), classtype="numeric")
  baz <- c(foo, bar)
  expect_equal(baz, new("HList", list(a=1, b=2, c=3, d=4), classtype="numeric"))
})

test_that("c-method: Homoglist,list works", {
  bar <- new("HList", list(d=4), classtype="numeric")
  baz <- c(foo, bar)
  expect_equal(baz, new("HList", list(a=1, b=2, c=3, d=4), classtype="numeric"))
})

test_that("c-method: Homoglist,list throws error if bad", {
  bar <- list(d="4")
  expect_error(c(foo, bar), "invalid class")
})


#######

context("[,HList method")

test_that("[-method HList,integer works", {
  expect_equal(foo[2], new("HList", list(b=2), classtype="numeric"))
})

test_that("[-method HList,character works", {
  expect_equal(foo["b"], new("HList", list(b=2), classtype="numeric"))
})

test_that("[-method HList,missing works", {
  expect_equal(foo[], foo)
})

#######

context("[<-,HList method")

test_that("[<- with HList,missing,vector works", {
  foo[] <- 3
  expected <- new("HList", list(a=3, b=3, c=3), classtype="numeric")
  expect_equal(foo, expected)
})

test_that("[<- with HList,missing,list works", {
  foo[] <- list(a=3, b=2, c=1)
  expected <- new("HList", list(a=3, b=2, c=1), classtype="numeric")
  expect_equal(foo, expected)
})

test_that("[<- with HList,character: test #1", {
  foo["a"] <- 100
  expected <- new("HList", list(a=100, b=2, c=3), classtype="numeric")
  expect_equal(foo, expected)
})

test_that("[<- with HList,character: test #2", {
  foo[c("a", "b")] <- c(100, 200)
  expected <- new("HList", list(a=100, b=200, c=3), classtype="numeric")
  expect_equal(foo, expected)
})

test_that("[<- with HList,integer: test #1", {
  foo[1] <- c(100)
  expected <- new("HList", list(a=100, b=2, c=3), classtype="numeric")
  expect_equal(foo, expected)
})

test_that("[<- with HList,integer: test #2", {
  foo[2:3] <- c(100, 200)
  expected <- new("HList", list(a=1, b=100, c=200), classtype="numeric")
  expect_equal(foo, expected)
})

##########
context("[[<-,HList method")

test_that("[[<- with HList, missing throws error", {
  expect_error({foo[[]] <- 1}, regexp="missing subscript")
})

test_that("[[<-,HList,character: test #1", {
  foo[["a"]] <- 100
  expect_equal(foo, new("HList", list(a=100, b=2, c=3), classtype="numeric"))
})

test_that("[[<-,HList,character: test #2", {
  foo[[c("a", "b")]] <- 100
  expect_equal(foo, new("HList", list(a=c(1, b=100), b=2, c=3), classtype="numeric"))
})

test_that("[[<-,HList,numeric: test #1", {
  foo[[1]] <- 100
  expect_equal(foo, new("HList", list(a=100, b=2, c=3), classtype="numeric"))
})

test_that("[[<-,HList,numeric: test #2", {
  foo[[c(1, 2)]] <- 100
  expect_equal(foo, new("HList", list(a=c(1, 100), b=2, c=3), classtype="numeric"))
})

#########

context("$<-,HList method")

test_that("$<-,HList works", {
  foo$a <- 100
  expect_equal(foo, new("HList", list(a=100, b=2, c=3), classtype="numeric"))
})

########

context("names<-,HList method")

test_that("names<- with character works", {
  names(foo) <- c("d", "e", "f")
  expect_equal(foo, new("HList", list(d=1, e=2, f=3), classtype="numeric")) 
})

test_that("names<- with NULL works", {
  names(foo) <- NULL
  expect_equal(foo, new("HList", list(1, 2, 3), classtype="numeric"))
})

###########

context("length<-,HList method")

test_that("length<- works with value < length(object)", {
  length(foo) <- 1
  expect_equal(foo, new("HList", list(a=1), "numeric"))
})

test_that("length<- works with value == length(object)", {
  length(foo) <- length(foo)
  expect_equal(foo, new("HList", list(a=1, b=2, c=3), "numeric"))
})

test_that("length<- works with value > length(object)", {
  length(foo) <- length(foo) + 1
  expect_is(foo, "HList")
  expect_equal(foo@.Data, list(1, 2, 3, NULL))
  expect_equal(foo@names, c("a", "b", "c", ""))
})
