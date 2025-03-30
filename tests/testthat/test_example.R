library(testthat)
library(tmbExtract)
library(TMB)
runExample("simple")


test_that("ExtractParamsTmb", {
  expected <- c(0)
  observed <- c(0)

  expect_equal(observed, expected)
})

test_that("ExtractCorTmb", {
  expected <- c(0)
  observed <- c(0)
  
  expect_equal(observed, expected)
})

test_that("ExtractRandTmb", {
  expected <- c(0)
  observed <- c(0)
  
  expect_equal(observed, expected)
})

test_that("ExtractStdTmb", {
  expected <- c(0)
  observed <- c(0)
  
  expect_equal(observed, expected)
})


test_that("ExtractVarTmb", {
  expected <- c(0)
  observed <- c(0)
  
  expect_equal(observed, expected)
})

test_that("tmbExtract", {
  expected <- c(0)
  observed <- c(0)
  
  expect_equal(observed, expected)
})