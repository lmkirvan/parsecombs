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

test_that("parsing with %or_else%  works", {
  testthat::expect_equal(
    (pchar("s") %then% pchar("l") %then% pchar("a"))("slate")$match,
    c('s', 'l', 'a')
  )
})

