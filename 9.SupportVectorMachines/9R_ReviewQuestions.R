# use simulation to evaluate (by Monte Carlo) the expected misclassification error rate given a particular generating model.
library(MASS)
library(e1071)

# we would like to know the expected test error rate if we fit an SVM to a sample of 50 
# random training points from class 1 and 50 more from class 0.
# Create a dataset of 100 observations (with 50 1's and 50 0's) with 10 predictors
# x it is a ten-dimensional Gaussian distribution with the following specific
# n is number of obs. mu is vector mean, sigma is Identity matrix (variance-covariance)

# 9.R.1 ===================================================================

counts = 100
errate = rep(0, counts)
for (i in 1:counts) {
  # 1. Generate a arandom training sample to train on
  x0 = mvrnorm(50, mu = c(0,0,0,0,0,0,0,0,0,0), Sigma = diag(10))
  x1 = mvrnorm(50, mu = c(1,1,1,1,1,0,0,0,0,0), Sigma = diag(10))
  x_train = matrix(rbind(x0, x1), 100, 10)

  # y is equally divided between classes 0 and 1. 
  y_train = matrix(rep(c(0,1), c(50,50)))

  #plot(x_train, col =y_train +3, pch=19)

  # Create a training dataset
  train_data = data.frame(x_train, y_train = as.factor(y_train))
  #dim(train_data)

  # 2. Evaluate the number of mistakes we make on a large test set of 1000 observations
  x0_test = mvrnorm(500, mu = rep(0,10), Sigma = diag(10))
  x1_test = mvrnorm(500, mu = rep(c(1,0), c(5,5)), diag(10))
  #dim(x1_test)

  x_test = matrix(rbind(x0_test,x1_test), 1000, 10)
  y_test = matrix(rep(c(0,1), c(500,500)))

  test_data = data.frame(x_test, y_test = as.factor(y_test))

  # Use svm in the e1071 package with the default settings (the default kernel is a radial kernel). 
  #  What is the expected test error rate of this method (to within 10%)?
  svmfit = svm(factor(y_train)~., data = train_data)
  #print(svmfit)

  y_pred = predict(svmfit, x_test)
  error_rate = mean(as.numeric(y_pred != factor(y_test)))
  
  errate[i] = error_rate}

#  expected test error rate
mean(errate)

# 9.R.2 =================================================================== 
# Now fit an svm with a linear kernel (kernel = "linear"). What is the expected test error rate to within 10%?

counts = 100
errate = rep(0, counts)
for(i in 1:counts){
  x = matrix(rnorm(100 * 10), ncol = 10)
  y = c(rep(0, 50), rep(1, 50))
  x[y == 1, 1:5] = x[y == 1, 1:5] + 1

  dat = data.frame(x = x, y = as.factor(y))
  svm.fit = svm(y ~ ., data = dat, kernel = 'linear')
  
  xtest = matrix(rnorm(100 * 10), ncol = 10)
  ytest = sample(c(0, 1), 100, rep = TRUE)
  xtest[ytest == 1, 1:5] = xtest[ytest == 1, 1:5] + 1
  testdat = data.frame(x = xtest, y = as.factor(ytest))
  
  ypred = predict(svm.fit, testdat)
  result = table(predict = ypred, truth = testdat$y)
  errate[i] = 1 - (result[1] + result[4]) / 100
}
mean(errate)

# 9.R.3 =================================================================== 
# What is the expected test error for logistic regression? (to within 10%)

counts = 100
errate = rep(0, counts)
for(i in 1:counts){
  x = matrix(rnorm(100 * 10), ncol = 10)
  y = c(rep(0, 50), rep(1, 50))
  x[y == 1, 1:5] = x[y == 1, 1:5] + 1
  
  dat = data.frame(x = x, y = as.factor(y))
  glm.fit=glm(y~., data=dat, family = binomial)
  
  xtest = matrix(rnorm(100 * 10), ncol = 10)
  ytest = sample(c(0, 1), 100, rep = TRUE)
  xtest[ytest == 1, 1:5] = xtest[ytest == 1, 1:5] + 1
  testdat = data.frame(x = xtest, y = as.factor(ytest))
  
  glm.probs=predict(glm.fit, testdat,type='response')
  glm.pred=ifelse(glm.probs>0.5, 1, 0)
  result = table(predict = glm.pred, truth = testdat$y)
  errate[i] = 1 - (result[1] + result[4]) / 100
}
mean(errate)

# Logistic regression is similar to SVM with a linear kernel.
