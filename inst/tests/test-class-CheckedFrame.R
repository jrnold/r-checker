columns <- ColumnCheckList(foo = ColumnChecks(classtype="numeric"))
checks <- TableChecks(columns=columns)

test_that("initialize works", {
  foo <- new("CheckedFrame", data.frame(foo=1:10), checks=checks)
  expect_is(foo, "CheckedFrame")
})

test_that("initialize without checks works", {
  foo <- new("CheckedFrame", data.frame(foo=1:10))
  expect_is(foo, "CheckedFrame")
})

test_that("initialize checks validity", {
  expect_error(new("CheckedFrame", data.frame(foo=letters), checks=checks),
               "invalid class")
})

test_that("initialize works", {
  foo <- CheckedFrame(data.frame(foo=1:10), checks=checks)
  expect_is(foo, "CheckedFrame")
})

## Need to rewrite initialize
## test_that("initialize works with empty", {
##   new("CheckedFrame", checks=checks)
## })

test_that("initialize wihout args works", {
  expect_is(new("CheckedFrame"), "CheckedFrame")
})

#################
# [ method
context("[,DataFrameConstr-method")

columns <- ColumnCheckList(foo = ColumnChecks(classtype="numeric"))
checks <- TableChecks(columns=columns)
foo <- CheckedFrame(data.frame(foo=1:10, bar=1:10),
                    checks=checks)

test_that("[,DataFrameConstr,missing,missing works", {
  expect_equal(foo[drop=FALSE], foo)
})

test_that("[,DataFrameConstr,missing,character with drop=missing  works", {
  expect_equal(foo[ , "foo"], as.numeric(1:10))
})

test_that("[,DataFrameConstr,missing,character with drop=FALSE works", {
  expect_equal(foo[ , "foo", drop=FALSE],
               CheckedFrame(data.frame(foo=1:10), checks=checks))
})

test_that("[,DataFrameConstr,integer,missing works", {
  expected <- CheckedFrame(foo[1:2], checks=foo@checks)
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
                           checks=foo@checks)
  expect_equal(foo[1:2, c("foo", "bar"), drop=FALSE], expected)
})

test_that("[,DataFrameConstr drops to data.frame if invalid subset", {
  expected <- data.frame(bar=1:10)
  expect_equal(foo[, c("bar"), drop=FALSE], expected)
})

################################
context("[<-,DataFramcConstr")

columns <- ColumnCheckList(foo = ColumnChecks(classtype="numeric"))
checks <- TableChecks(columns=columns)
foo <- CheckedFrame(data.frame(foo=1:10, bar=1:10),
                    checks=checks)

test_that("[<- missing,missing works", {
  foo[] <- 1
  expected <- CheckedFrame(data.frame(foo=rep(1, 10),
                                      bar=rep(1, 10)),
                           checks=checks)
  expect_equal(foo, expected)
})

test_that("[<- missing,missing throws error", {
  expect_error(foo[] <- "a", "invalid class")
})

test_that("[<- missing,ANY throws error", {
  expect_error(foo[1, ] <- "a", "invalid class")
})

test_that("[<- missing,ANY works", {
  foo[["foo"]] <- 1
  foo <- CheckedFrame(data.frame(foo=1, bar=1:10),
                      checks=checks)
})

test_that("[<- ANY,missing works", {
  foo[1:2, ] <- c(100, 200)
  expected <- CheckedFrame(data.frame(foo=c(100, 200, 3:10),
                                      bar=c(100, 200, 3:10)),
                           checks=checks)
  expect_equal(foo, expected)
})

test_that("[<- missing,ANY throws error", {
  expect_error(foo[["foo"]] <- "a", "invalid class")
})

test_that("[<- ANY,ANY works", {
  foo[1:2, "foo"] <- c(100, 200)
  expected <- CheckedFrame(data.frame(foo=c(100, 200, 3:10),
                                      bar=1:10),
                           checks=checks)
  expect_equal(foo, expected)
})

test_that("[<- missing,ANY throws error", {
  expect_error(foo[1:2, "foo"] <- "a", "invalid class")
})


################################
context("[[<-,DataFramcConstr")

columns <- ColumnCheckList(foo = ColumnChecks(classtype="numeric"))
checks <- TableChecks(columns=columns)
foo <- CheckedFrame(data.frame(foo=1:10, bar=1:10),
                    checks=checks)

test_that("[[<- missing,missing error", {
  expect_error(foo[[]] <- 1)
})

test_that("[[<- ANY,missing works", {
  foo[["foo"]] <- 1
  foo <- CheckedFrame(data.frame(foo=1, bar=1:10),
                      checks=checks)
})

test_that("[[<- ANY,missing throws error", {
  expect_error(foo[["foo"]] <- "a", "invalid class")
})

test_that("[[<- ANY,missing works", {
  foo[[1, "foo"]] <- 100
  expected <- CheckedFrame(data.frame(foo=c(100, 2:10),
                                      bar=1:10),
                           checks=checks)
  expect_equal(foo, expected)
})

test_that("[[<- ANY,missing throws error", {
  expect_error(foo[[1, "foo"]] <- "a", "invalid class")
})

#####################3
context("$<- CheckedFrame")

columns <- ColumnCheckList(foo = ColumnChecks(classtype="numeric"))
checks <- TableChecks(columns=columns)
foo <- CheckedFrame(data.frame(foo=1:10, bar=1:10),
                    checks=checks)

test_that("$<- works", {
  foo$foo <- 1:10 * 2
  expected <- CheckedFrame(data.frame(foo= (1:10 * 2), bar=1:10),
                           checks=checks)
  expect_equal(foo, expected)
})

test_that("$<- works", {
  expect_error(foo$foo <- "a", "invalid class")
})

###########
context("rbind CheckedFrame")

columns <- ColumnCheckList(foo = ColumnChecks(classtype="numeric"))
checks <- TableChecks(columns=columns)
foo <- CheckedFrame(data.frame(foo=1:4, bar=5:8), checks=checks)

test_that("rbind2 works", {
  expected <- CheckedFrame(data.frame(foo=c(1:4, 9), bar=c(5:8, 10)), checks=checks)
  expect_equal(rbind2(foo, data.frame(foo=9, bar=10)),
              expected)
})

test_that("rbind2 throws error", {
  expect_error(rbind2(foo, data.frame(foo="a", bar=10)), "invalid class")
})

###########
context("cbind2 CheckedFrame")

columns <- ColumnCheckList(foo = ColumnChecks(classtype="numeric"))
checks <- TableChecks(columns=columns, exclude="baz")
foo <- CheckedFrame(data.frame(foo=1:4, bar=5:8), checks=checks)

test_that("cbind2 works", {
  expected <- CheckedFrame(data.frame(foo=1:4, bar=5:8, qux=9:12),
                           checks=checks)
  expect_equal(cbind2(foo, data.frame(qux=9:12)),
              expected)
})

test_that("cbind2 throws error", {
  expect_error(cbind2(foo, data.frame(baz=9:12)), "invalid class")
})

#############
context("nanes CheckedFrame")

columns <- ColumnCheckList(foo = ColumnChecks(classtype="numeric"))
checks <- TableChecks(columns=columns)
foo <- CheckedFrame(data.frame(foo=1:4, bar=5:8, baz=9:12), checks=checks)

test_that("names<- works", {
  names(foo) <- c("foo", "a", "b")
  expected <- CheckedFrame(data.frame(foo=1:4,
                                      a=5:8, b=9:12), checks=checks)
  expect_equal(foo, expected)
})

test_that("names<- throws error", {
  expect_error(names(foo) <- c("c", "a", "b"), "invalid class")
})

test_that("colnames<- works", {
  colnames(foo) <- c("foo", "a", "b")
  expected <- CheckedFrame(data.frame(foo=1:4,
                                      a=5:8, b=9:12), checks=checks)
  expect_equal(foo, expected)
})

test_that("colnames<- throws error", {
  expect_error(colnames(foo) <- c("c", "a", "b"), "invalid class")
})

test_that("rownames<- works", {
  rownames(foo) <- letters[1:4]
  expect_is(foo, "CheckedFrame")
  expect_equal(rownames(foo), letters[1:4])
})

test_that("dimnames<- works", {
  dimnames(foo) <- list(letters[1:4], c("foo", "a", "b"))
  expect_is(foo, "CheckedFrame")
  expect_equal(rownames(foo), letters[1:4])
  expect_equal(colnames(foo), c("foo", "a", "b"))
})

test_that("dimnames<- throws error", {
  expect_error(dimnames(foo) <- list(letters[1:4], c("x", "a", "b")),
               "invalid class")
})

###################

context("CheckedFrame coercion")

columns <- ColumnCheckList(foo = ColumnChecks(classtype="numeric"))
checks <- TableChecks(columns=columns)
foo <- CheckedFrame(data.frame(foo=1:4, bar=5:8, baz=9:12), checks=checks)

test_that("as(object, \"data.frame\") produces an S3 data.frame", {
  expect_false(isS4(as(foo, "data.frame")))
})
