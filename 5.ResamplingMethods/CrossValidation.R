require(ISLR)
require(boot)
# Cross-Validation for Generalized Linear Model
?cv.glm
plot(mpg~horsepower, data = Auto)

## LOOCV
glm.fit=glm(mpg~horsepower, data=Auto) # if family is not specify glm run a linear regression
cv.glm(Auto,glm.fit)$delta # brute force: pretty slow (doesnt use formula (5.2) on page 180)

# delta are the cross-validated prediction errors
#1. number is the raw leave-one-out
#2. is the bias-corrected version, since we train it on is slightly smaller (n-1) dataset.
# The latest has more of an effect for k-fold cross-validation, where training is even smaller (K-k)

# Lets write a simple function to use formula (5.2)
# short cut ONLY for linear regression
loocv=function(fit){
  h=lm.influence(fit)$h # post-processor for lm.fit and extract the element h (diagonal element Hii)
  mean((residuals(fit)/(1-h))^2) # residuals of the full fit. Divide does element by element division in that vector.
}

loocv(glm.fit) # very quickly it produce the same results

# Now, we are going to use it and create the sampling distribution of an estimator
# we can fit a polynimials of different degrees
cv.error=rep(0,5) # vector for collecting the errors
degree=1:5
for (d in degree) {
  glm.fit=glm(mpg~poly(horsepower,d), data = Auto)
  cv.error[d] = loocv(glm.fit)
}

plot(degree,cv.error,type = 'b')
# Degree 2 jumps down from 24 to just above 19.
# Then higher degrees really don't make much difference.
# Looking at the initial plot, the intuition was that
# a quadratic predictor would do a good job.

## 10-fold CV
cv.error10=rep(0,5)
for (d in degree) {
  glm.fit=glm(mpg~poly(horsepower,d), data = Auto)
  cv.error10[d] = cv.glm(Auto, glm.fit, K=10)$delta[1] # fitting the model 10 times each times
}

lines(degree,cv.error10, type = 'b', col='red')

# 10-fold and leave-one-out cross-validation
# tell us the same story.
# In general we favor 10-fold cross-validation for computing errors.
# It tends to be a more stable measure than leave-one-out cross-validation
# and most of the time, it's cheaper to compute.
