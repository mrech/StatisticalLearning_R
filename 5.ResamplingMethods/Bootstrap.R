## Bootstrap
require(ISLR)

## Minimum risk investment - Section 5.2
# Boostrap returns the sampling distribution of statistics
# the sampling variability of alpha/Standard error of alpha

alpha=function(x,y){
  vx = var(x)
  vy = var(y)
  cxy = cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}

alpha(Portfolio$X, Portfolio$Y)

## What is the standard error of alpha?
# wrapper that allows a bootstrap to work
alpha.fn=function(data, index){ # index row > observations used in the bootstrap sampling
  with(data[index,], alpha(X,Y)) # with allows to use named valuables X, Y that are in the dataframe
}

# Try to run it once
alpha.fn(Portfolio,1:100)

# Bootstrap involve random sampling with replacement
set.seed(1) # to get reproducible results
alpha.fn(Portfolio, sample(1:100,100,replace = TRUE))

boot.out=boot(Portfolio, alpha.fn, R=1000) # 1000 bootstraps
boot.out
plot(boot.out)

# The second plot is a qqplot, which plots the ordered values against the ordered
# statistics of a Gaussian. If it lines up on a straight line, you may say it looks
# close to Gaussian, maybe slightly bigger tail on the right.