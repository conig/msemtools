test_that("character matrix works", {
  mat <- msemtools:::character_matrix(c("male","female","male,female,other", "female,male", "male,other"))

  testthat::expect_equal(mat[,"male"],c(1,0,1,1,1))
  testthat::expect_equal(mat[,"female"],c(0,1,1,1,0))
  testthat::expect_equal(mat[,"other"],c(0,0,1,0,1))
  })

test_that("character matrix works with different sep", {
  mat <- msemtools:::character_matrix(c("male","female","male_female_other", "female_male", "male_other"), pattern = "_")

  testthat::expect_equal(mat[,"male"],c(1,0,1,1,1))
  testthat::expect_equal(mat[,"female"],c(0,1,1,1,0))
  testthat::expect_equal(mat[,"other"],c(0,0,1,0,1))
})
