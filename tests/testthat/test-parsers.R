test_that("parsing a single character works", {
  testthat::expect_equal(
  pchar('s')("slate")$match,
  's'
  )
  })

test_that("parsing with %then% works", {
  testthat::expect_equal(
    (pchar("s") %then% pchar("l"))("slate")$match,
    c('s', 'l')
    )
  })

test_that("parsing with %then%  three times works", {
  testthat::expect_equal(
    (pchar("s") %then% pchar("l") %then% pchar("a"))("slate")$match,
    c('s', 'l', 'a')
    )
  })

test_that("parsing with %or_else% works", {
  testthat::expect_equal(
    (pchar("s") %or_else% pchar("l") %or_else% pchar("a"))("slate")$match,
    c('s')
    )
  })

test_that("parsing with any_of works", {
  testthat::expect_equal(
    parse_digit("123")$match,
    "1"
  )
})

test_that("map_p applies a function", {
  testthat::expect_equal(
    (parse_digit %map_p% as.integer)("1234")$match,
    1)
  })

test_that("many1 works as planned", {
  testthat::expect_equal(
    many1(pchar("s"))("ssssdbg")$rest,
    "dbg")
  testthat::expect_equal(
    many1(pchar("s"))("sssdbg")$match,
    c("s", "s", "s")
    )
  })

test_that("parse_int_works", {
  testthat::expect_equal(
    parse_int("123")$match,
    123)
  testthat::expect_equal(
    parse_int("-123")$match,
    -123)
  })

test_that("keep left and right work", {
  testthat::expect_equal(
    (pchar("s") %keep_left% pchar("k"))("ska")$match,
    "s")
  testthat::expect_equal(
    (pchar("s") %keep_right% pchar("k"))("ska")$match,
    "k")
  })



