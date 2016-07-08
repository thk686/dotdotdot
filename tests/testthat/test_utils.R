library(dotdotdot)
context("utils")

test_that("is_empty_string works", {
  expect_error(is_empty_string())
  expect_error(is_empty_string(pi))
  expect_true(is_empty_string(""))
  expect_true(is_empty_string(NA_character_))
  expect_false(is_empty_string("test"))
  expect_false(all(is_empty_string(letters)))
  expect_true(all(is_empty_string(rep("", 10))))
})

test_that("parse_deparse works", {
  expect_error(parse_deparse())
  expect_error(parse_deparse(NULL))
  expect_error(parse_deparse(""))
  expect_error(parse_deparse("x = 1"))
  expect_equal(parse_deparse("c(a = 1)"), set_names("1", "a"))
  expect_equal(parse_deparse("list(a = 1, b, c = 1:3)"),
               set_names(c("1", "b", "1:3"), c("a", NA_character_, "c")))
  expect_equal(parse_deparse("list(c(list(a = 1)))"), set_names("1", "a"))
})

test_that("trim_quotes works", {
  expect_error(trim_quotes())
  expect_equal(trim_quotes(NULL), character(0))
  expect_equal(trim_quotes(""), "")
  expect_equal(trim_quotes(1:2), c("1", "2"))
  expect_equal(trim_quotes(TRUE), "TRUE")
  expect_equal(trim_quotes(NA), NA_character_)
  expect_equal(trim_quotes("    \"x\"    "), "x")
  expect_equal(trim_quotes("\"'`"), "")
})

test_that("extract_enlisted works", {
  expect_error(extract_enlisted())
  expect_equal(extract_enlisted(NULL), character(0))
  expect_equal(extract_enlisted(""), NA_character_)
  expect_equal(extract_enlisted("c(a = 1)"), "a = 1")
  expect_equal(extract_enlisted("list(c(a = 1, b = 2, c))"), "a = 1, b = 2, c")
})

test_that("set_names works", {
  expect_error(set_names())
  expect_error(set_names(NULL))
  x = ""
  names(x) = NA_character_
  expect_equal(set_names("", NA), x)
  x = letters[1:5]
  x = set_names(x, x)
  expect_equal(names(x), letters[1:5])
  x = set_names(x, NA)
  expect_equal(names(x), rep(NA_character_, 5))
  names(x) = c(letters[1:3], "", NA)
  x = set_names(x, NA, keep_existing = TRUE)
  expect_equal(names(x), c(letters[1:3], NA, NA))
  names(x) = c(letters[1:3], "", NA)
  x = set_names(x, rev(letters[1:5]))
  expect_equal(names(x), rev(letters[1:5]))
  x = 1:3
  names(x) = 1:3
  expect_equal(set_names(1:3, 1:3, keep_existing = TRUE), x)
})

test_that("check_function works", {
  expect_error(check_function())
  expect_false(check_function(NULL))
  expect_false(check_function(NA))
  expect_false(check_function(""))
  expect_true(check_function("ls"))
  expect_true(check_function(ls))
  e = new.env(parent = emptyenv())
  expect_false(check_function(ls, e))
})

test_that("get_formals works", {
  expect_error(get_formals())
  expect_error(get_formals(NULL))
  expect_error(get_formals(""))
  expect_error(get_formals("_test_"))
  f = function(x) x
  expect_equal(get_formals(f), as.pairlist(alist(x =)))
  expect_equal(get_formals("cut"), as.pairlist(alist(x =, ... =)))
  e = new.env(parent = emptyenv())
  e$f = function(y) y
  expect_equal(get_formals("f", e), as.pairlist(alist(y =)))
  expect_error(get_formals(ls, e))
})

test_that("is_primitive works", {
  expect_error(is_primitive())
  expect_error(is_primitive(NULL))
  expect_error(is_primitive(""))
  expect_error(is_primitive(NA))
  expect_true(is_primitive(c))
  expect_true(is_primitive("c"))
  expect_true(is_primitive(sum))
  expect_true(is_primitive("sum"))
  e = new.env(parent = emptyenv())
  e$sum = NA
  expect_error(is_primitive(sum, e))
  e$sum = function(x) x
  expect_false(is_primitive(sum, e))
  expect_false(is_primitive("sum", e))
})
