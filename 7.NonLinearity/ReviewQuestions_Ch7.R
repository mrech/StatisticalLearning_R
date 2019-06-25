# 7.R.R1: Slope coefficient in a linear regression of y on x (-0.6748 )
setwd('~/MachineLearning/StatisticalLearning_R/7.NonLinearity/')

data = load('7.R.RData')

plot(x,y)

fit = lm(y ~ x)

# 7.R.R2: For the model y ~ 1+x+x^2, what is the coefficient of x (7.771e+01 == 77.71)
fit1 = lm(y ~ 1 + x + I(x^2))
summary(fit1)

# 7.Q.1

# Fitting a generalized additive model for y against X1 and X2, using cubic spline with 4 knots for
# each variables will NOT get the same fitted values as we would if we fit the additive model jointly 
# or separatly even if X1 and X2 are uncorrelated, because the nonlinear basis functions might be. 

