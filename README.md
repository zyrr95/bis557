[![Build Status](https://travis-ci.org/zyrr95/bis557.svg?branch=master)](https://travis-ci.org/zyrr95/bis557)

BIS557
===

This is a repository for storing all code, documentation, and digital 
artifacts for BIS557.

So far the first thing we've done is creating and document a function that
calls `lm`. You can use it like this:

```{R}
library(bis557)
fit <- linear_model(Sepal.Length ~., iris)
summary(fit)
```

The second function is ridge regression. An example is like this:

```{R}
library(bis557)
fit <- ridge_reg(Sepal.Length ~., 1,iris)
summary(fit)
```
