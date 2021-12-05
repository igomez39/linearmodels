## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(linearModels)
library(bench)

## ---- include=FALSE-----------------------------------------------------------
linearModels(mtcars, "wt","mpg")

## ---- include=FALSE-----------------------------------------------------------
linearModels(mtcars, c("wt","cyl"),"mpg")

## -----------------------------------------------------------------------------
mlr1 <- linearModels(mtcars, c("wt","cyl"),"mpg")
mlr2 <- lm(mpg ~ wt + cyl, mtcars)
all.equal(mlr1[,1], round(mlr2$coefficients,digits = 4))

## -----------------------------------------------------------------------------
mark(linearModels(mtcars,c("wt","cyl"),"mpg"))

## -----------------------------------------------------------------------------
mark(lm(mpg ~ wt + cyl, mtcars))

