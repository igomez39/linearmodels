test_that("correct coefficients", {
  #Obtain data from mtcars
  slr1 <- linearModels(mtcars, "wt","mpg")
  slr2 <- lm(mpg ~ wt, mtcars)
  testthat::expect_equal(slr1[,1],
                         round(slr2$coefficients,digits = 4))
})

test_that("correct coefficients", {
  #Obtain data from mtcars
  mlr1 <- linearModels(mtcars, c("wt","cyl"),"mpg")
  mlr2 <- lm(mpg ~ wt + cyl, mtcars)
  testthat::expect_equal(mlr1[,1],
                         round(mlr2$coefficients,digits = 4))
})

test_that("correct standard deviations", {
  #Obtain data from mtcars
  slr1 <- linearModels(mtcars, "wt","mpg")
  slr2 <- lm(mpg ~ wt, mtcars)
  testthat::expect_equal(slr1[,2],
                         round(summary(slr2)$coefficients[, 2],4))
})

test_that("correct standard deviations", {
  #Obtain data from mtcars
  mlr1 <- linearModels(mtcars, c("wt","cyl"),"mpg")
  mlr2 <- lm(mpg ~ wt + cyl, mtcars)
  testthat::expect_equal(mlr1[,2],
                         round(summary(mlr2)$coefficients[, 2],4))
})

test_that("correct t-statistic", {
  #Obtain data from mtcars
  slr1 <- linearModels(mtcars, "wt","mpg")
  slr2 <- lm(mpg ~ wt, mtcars)
  testthat::expect_equal(slr1[,3],
                         round(summary(slr2)$coefficients[, 3],4))
})

test_that("correct t-statistic", {
  #Obtain data from mtcars
  mlr1 <- linearModels(mtcars, c("wt","cyl"),"mpg")
  mlr2 <- lm(mpg ~ wt + cyl, mtcars)
  testthat::expect_equal(mlr1[,3],
                         round(summary(mlr2)$coefficients[, 3],4))
})




test_that("correct p-value", {
  #Obtain data from mtcars
  slr1 <- linearModels(mtcars, "wt","mpg")
  slr2 <- lm(mpg ~ wt, mtcars)
  testthat::expect_equal(slr1[,4],
                         round(summary(slr2)$coefficients[, 4],4))
})



test_that("correct p-value", {
  #Obtain data from mtcars
  mlr1 <- linearModels(mtcars, c("wt","cyl"),"mpg")
  mlr2 <- lm(mpg ~ wt + cyl, mtcars)
  testthat::expect_equal(mlr1[,4],
                         round(summary(mlr2)$coefficients[, 4],4))
})
