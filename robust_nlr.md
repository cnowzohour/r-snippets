Robust non-linear regression example
================

Generate Data

``` r
set.seed(2)
n <- 100
a1 <- 1
a2 <- 2
b1 <- -0.5
b2 <- 1.5
f <- function(x, a1, a2, b1, b2) a1 * exp(b1 * x) + a2 * exp(b2 * x)

x <- rnorm(n)
eps <- rt(n, 1)
y <- f(x, a1, a2, b1, b2) + eps
dat <- data.frame(x=x, y=y)
```

Fit model

``` r
library(robustbase)
res <- nlrob(y ~ f(x, a1, a2, b1, b2), data = dat, start = c(a1 = 1, a2 = 1, b1 = -1, b2 = 1))
```

Plot

``` r
xlim <- c(-2, 2)
grid <- seq(xlim[1], xlim[2], len = 1000)
y0 <- f(grid, a1, a2, b1, b2)
yhat <- f(grid, res$coefficients['a1'], res$coefficients['a2'], res$coefficients['b1'], res$coefficients['b2'])
plot(x, y, xlim = xlim, ylim = range(c(y0, y)))
lines(grid, y0, col = "blue", lwd = 2)
lines(grid, yhat, col = "red", lty = 2)
legend("bottomright", legend = c("Ground Truth", "Robust Estimate"), col = c("blue", "red"), lty = c(1, 2), lwd = c(2, 1))
```

![](robust_nlr_files/figure-markdown_github/unnamed-chunk-6-1.png)
