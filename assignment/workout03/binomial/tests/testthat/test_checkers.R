context("Test for three checkers")

test_that("test for check_prob", {
  expect_error(check_prob(-1),"invaild prob value")
  expect_length(check_prob(1),1)
  expect_equal(check_prob(1),T)
})

test_that("test for check_trials", {
  expect_error(check_trials(3.5))
  expect_length(check_trials(5),1)
  expect_type(check_trials(3),"logical")
})

test_that("test for check_success", {
  expect_error(check_success(5,4))
  expect_type(check_success(2,3),"logical")
  expect_length(check_success(1:3,4),1)
})
