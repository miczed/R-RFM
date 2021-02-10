library(RFM)

context("adding of positive numbers")

test_that("adding of positive numbers works", {
  expect_equal(addFirstTwo(c(2,2)), 4)
  expect_equal(addFirstTwo(c(1,5)), 6)
})

test_that("adding of negative numbers works", {
  expect_equal(addFirstTwo(c(-2,-5)), -7)
  expect_equal(addFirstTwo(c(-1,-20)), -21)
})

test_that("adding of zero numbers works", {
  expect_equal(addFirstTwo(c(0,-5)), -5)
  expect_equal(addFirstTwo(c(1,0)), 1)
})
