library(MASS)
library(ISLR)

### Simple linear regression
names(Boston)
?Boston
plot(medv~lstat, Boston)
fit1=lm(medv~lstat, Boston)
fit1
summary(fit1)
abline(fit1, col='red')
names(fit1)
confint(fit1)
predict(fit1, data.frame(lstat=c(5,10,15)), interval='confidence')

### Multiple linear regression
fit2=lm(medv~lstat+age, data=Boston)
summary(fit2)
fit3=lm(medv~.,Boston)
summary(fit3)
par(mfrow=c(2,2))
# 1. Non-linearity: by the curve in the residuals, 
#we can see that the model is not quite capturing everything.
plot(fit3)
fit4=update(fit3,~.-age-indus) # we remove two variables compared to before
summary(fit4)

### Nonlinear terms and Interactions
fit5=lm(medv~lstat*age,Boston)
summary(fit5)
# use identiy matrix if you just want to raise Istat to the power of two
fit6=lm(medv~lstat + I(lstat^2), Boston)
summary(fit6)
attach(Boston) # to make variable easily available
par(mfrow = c(1,1))
plot(medv~lstat)
#fitting a linear regression in a non linear model
points(lstat, fitted(fit6), col='red',pch=20)
# easier way to fit polynomials: poly of degree 4 in lstat
fit7=lm(medv~poly(lstat,4))
# getting wiggly: overfitting the model in sporadic observations
points(lstat, fitted(fit7), col='blue', pch=20)

# plotting characters available
plot(1:20,1:20, pch= 1:20, cex=2)

### Qualitative predictors
fix(Carseats)
names(Carseats)
summary(Carseats)
fit1=lm(Sales~.+Income:Advertising+Age:Price, Carseats)
summary(fit1)
contrasts(Carseats$ShelveLoc)

### Writing R functions
regplot = function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col='red')
}
attach(Carseats)
regplot(Price, Sales)
# Unnamed arguments
regplot = function(x,y,...){
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col='red')
}
regplot(Price,Sales,xlab='Price',ylab='Sales',col='blue', pch=20)
