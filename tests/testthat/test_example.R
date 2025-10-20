library(testthat)

test_that("ExtractParamsTmb", {
  path <- file.path(system.file("examples", package = "TMB"))
  TMB::compile(file.path(path, "simple.cpp"), clean = TRUE)
  out <- TMB::runExample("simple")
  out_test <- list(fit = out$value)
  expected <- ExtractParamsTmb(out_test, dllID = "simple", path = path)
  observed <- c(
    "beta" = 52.01370232,
    "beta" = 30.24058534,
    "logsdu" = -0.15777145,
    "logsd0" = 0.03326068
  )
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
  
  path <- file.path(system.file("examples", package = "TMB"))
  TMB::compile(file.path(path, "simple.cpp"), clean = TRUE)
  out <- TMB::runExample("simple")
  out_test <- list(fit = out$value)
  expected <- ExtractVarTmb(out_test, 
                            params = c("logsdu","logsd0"),
                            dllID = "simple", 
                            path = path)
  observed <- c(
    "logsdu" = exp(-0.15777145)^2,
    "logsd0" = exp(0.03326068)^2
  )
  expect_equal(observed, expected)
})

