context("tests for binomial")

test_that("tests for bin_choose", {
  expect_error(bin_choose(2,3))
  expect_length(bin_choose(5,2),1)
  expect_type(bin_choose(5,2),"double")
})

test_that("tests for bin_probability", {
  expect_length(bin_probability(2,5,0.5),1)
  expect_type(bin_probability(2,5,0.5),"double")
  expect_error(bin_probability(2,5,1.3))
})

test_that("tests for bin_distribution", {
  expect_is(bin_distribution(5,0.5),"bindis")
  expect_type(bin_distribution(5,0.5)[1,1],"integer")
  expect_length(bin_distribution(5,0.5)[,1],6)
})

test_that("tests for bin_culmulative", {
  expect_is(bin_cumulative(5,0.5), "bincum")
  expect_equal(bin_cumulative(5,0.5)[6,3],1)
  expect_type(bin_cumulative(5,0.5)[1,1],"integer")
})
