## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(RKelly)

# A bet to back at price 2.1 and objective probability of 0.5 and 5% commision
kelly_back_dec(price = 2.1, p=0.5, commision_rate = 0.05)

## ------------------------------------------------------------------------
# A bet to lay at price 1.9 and objective probability of 0.5 and 5% commision
kelly_lay_dec(price = 1.9, p = 0.5, commision_rate = 0.05)

## ------------------------------------------------------------------------
kelly_back_dec(price = 1.9, p=0.5, commision_rate = 0.0)

