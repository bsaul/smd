## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(SugarMaryDenver)

## ----numeric-------------------------------------------------------------
xn <- rnorm(100)
gg <- rep(c("A", "B"), each = 50)
smd(x = xn, g = gg)

## ----integer-------------------------------------------------------------
xi <- sample(1:20, 100, replace = TRUE)
smd(x = xi, g = gg)

## ----character-----------------------------------------------------------
xc <- unlist(replicate(2, sort(sample(letters[1:3], 50, replace = TRUE)), simplify = FALSE))
smd(x = xc, g = gg)

## ----factor--------------------------------------------------------------
xf <- factor(xc)
smd(x = xf, g = gg)

## ----logical-------------------------------------------------------------
xl <- as.logical(rbinom(100, 1, prob = 0.5))
smd(x = xl, g = gg)

## ----matrix--------------------------------------------------------------
mm <- cbind(xl, xl, xl, xl)
smd(x = mm, g = gg)

## ----list----------------------------------------------------------------
ll <- list(xn, xi, xf, xl)
smd(x = ll, g = gg)

## ----data.frame----------------------------------------------------------
df <- data.frame(xn, xi, xc, xf, xl)
smd(x = df, g = gg)

