library(dotdotdot)
context("do_dots")

empty_dots = list("..." = set_names(list(), NA))

test_that("do_dots returns list", {
  expect_is(do_dots("sum"), "list")
})

test_that("do_dots accepts NULL inputs", {
  expect_error(do_dots(NULL))
  expect_error(do_dots(NULL, NULL))
  f = function() NULL
  expect_equal(do_dots(f), list(f = NULL))
})

test_that("do_dots handles unnamed args", {
  f = function(...) c(...)
  expect_equal(do_dots(f, 1:10), c(list(f = 1:10), empty_dots))
})

test_that("do_dots handles multiple functions", {
  f = function(x) x
  g = function(y) y
  expect_equal(do_dots(c(f, g), x = 1, y = 2), c(list(f = 1, g = 2), empty_dots))
})

test_that("do_dots handles multiple functions with matching arguments", {
  f = function(x) x
  g = function(x) x
  expect_equal(do_dots(c(f, g), x = 1), c(list(f = 1, g = 1), empty_dots))
  expect_error(do_dots(c(f, g), x = 1, consume = TRUE))
})

test_that("do_dots handles primitive functions with ... args", {
  expect_equal(do_dots(sum, 1, 1, 1), c(list(sum = 3), empty_dots))
  expect_equal(do_dots(c, a = "a", b = "b"), list("..." = list(a = "a", b = "b")))
})

test_that("do_dots handles mixed functions and function names", {
  expect_equal(do_dots(c("sum", c), 1:3), c(list(sum = 6, c = 1:3), empty_dots))
})

test_that("do_dots works with environments", {
  e = new.env(parent = emptyenv())
  e$f = function(x) x
  e$g = function(y) y
  expect_equal(do_dots(c(f, "g"), x  = 1, y = 2, envir = e),
               c(list(sum = 6, c = 1:3), empty_dots))
})
