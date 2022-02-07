
library(usethis)
use_git_config(user.name = "cmacche", user.email = "cmaccheyne@gmail.com")


## produce 100 random numbers that follows a normal distribution
x <- rnorm(100, mean = 0, sd = 1)

## estimate mean
median(x)

## estimate SD
var(x)
