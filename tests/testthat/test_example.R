library(testthat)
library(TMB)

test_that("ExtractParamsTmb", {
  
  out <- runExample("simple")
  setwd(system.file("examples",package="TMB"))
  tmbObj <- list()
  tmbObj$fit <- out$value
  
  expected <- ExtractParamsTmb(tmbObj, dllID="simple")
  
  observed <- c("beta"=52.01370232, "beta"=30.24058534,  
                "logsdu" =-0.15777145,"logsd0"=0.03326068)
  
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
