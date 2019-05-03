context("Test for summary measures")

test_that("tests for mean", {
  expect_length(aux_mean(2,0.3),1)
  expect_type(aux_mean(2,0.3),"double")
  expect_equal(aux_mean(2,0.3),0.6)
})

test_that("tests for variance", {
  expect_length(aux_variance(10,0.1),1)
  expect_type(aux_variance(10,0.1),"double")
  expect_equal(aux_variance(10,0.1),0.9)
})

test_that("tests for mode", {
  expect_length(aux_mode(5,0.5),1)
  expect_type(aux_mode(5,0.5),"integer")
  expect_equal(aux_mode(10,0.3),3)
})

test_that("tests for skewness", {
  expect_type(aux_skewness(10,0.3),"double")
  expect_length(aux_skewness(10,0.3),1)
  expect_gt(aux_skewness(10,0.3),0.27)
})

test_that("tests for kurtosis", {
  expect_type(aux_kurtosis(10,0.3),"double")
  expect_length(aux_kurtosis(10,0.3),1)
  expect_gt(aux_kurtosis(10,0.3),-2)
})
