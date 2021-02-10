library(RFM)

context("length of return value")

test_that("adding of two numbers returns single value", {
  expect_is(addFirstTwo(c(1,2)), "numeric")
})

test_that("adding of two single digits length is 1", {
  expect_length(addFirstTwo(c(2,5)), 1)
})

