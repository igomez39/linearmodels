#' Fitting Linear Models
#'
#' @description linearModels is a mimic of lm() that is used to fit linear models.
#'
#' @param data The data set for multiple linear regression. It should be in the form of a 2d matrix
#' @param x A string array of all the predictors' names.
#' @param y A single string of the response.
#' @importFrom stats median pf pt quantile
#' @return
#'
#' @export
#'
#' @examples
#' mt = mtcars
#' linearModels(mtcars, "mpg", "cyl")


linearModels <- function(data,x,y) {
  if (is.matrix(data) == FALSE && is.matrix(as.matrix(data)) == TRUE ){
  matrix = as.matrix(data)
  }
  else {
    stop("ERROR: Please input something that is convertible to a matrix")
  }
  #calculating beta
  outcome = as.matrix(data[y])
  predictors = as.matrix(data[x])
  betas = cbind(rep(1,length(outcome)),predictors)
  beta_hat = round(solve(t(betas) %*% betas) %*% t(betas) %*% outcome,2)

  # number of columns and total sample size for df
  n = nrow(betas)
  p = ncol(betas)

  #Calculate Residuals: MIN, 1Q,MEDIAN,3Q,MAX

  ## First step is to calculate the predicted values
  predicted = betas%*%betas_r

  ### do predicted - actuals
  residuals = outcome - predicted
  sigma2 = t(residuals)%*%residuals/(n-p)

  #Variance
  variance = diag(solve(t(betas)%*%betas))*c(sigma2)
  standard_error = sqrt(variance)
  #CALCULATE T-VALUE
  t_stat = beta_hat/standard_error

  #CALCULATE P-VALUE
  p_value = 2*pt(-abs(t_stat),df=n-p)

  #MULTIPLE R-SQUARED
  ssr = sum((predicted - mean(outcome))^2)
  sst = sum((outcome - mean(outcome))^2)
  r_square = ssr/sst

  #ADJUSTED
  r_square_adjusted = 1 - ((1-r_square)* (n-1)/ (n-p-1))

  #F-STATISTIC
  f_stat = (r_square/(1-r_square)) * ((n-p)/(p-1))

  #P-VALUE OF F-STATISTIC
  pf(f_stat, (p-1), (n-p), lower.tail = FALSE)

  print("Residuals:")
  residuals_mat <- cbind(Min = min(residuals),
                      "1Q" = quantile(residuals,.25),
                      "Median" = median(residuals),
                      "3Q" = quantile(residuals,.75),
                      "Max" = max(residuals))

  print(residuals_mat)

  print("Coefficients:")
  output_mat <- cbind(Estimate = beta_hat,
                      Std_Err = standard_error,
                      "T-Statistic" = t_stat,
                      "P-value" = p_value)
  rownames(output_mat) <- c("(Intercept)",x)
  colnames(output_mat) <- c("Estimate", "Std. Error", "t value", "P value")
  print(output_mat)
  }



