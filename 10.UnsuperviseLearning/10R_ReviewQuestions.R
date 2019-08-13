load('~/MachineLearning/StatisticalLearning_R/10.UnsuperviseLearning/10.R.RData')

# concatenate x and x.test using the rbind functions and perform a principal components analysis
x_all = rbind(x, x.test)
pca.out = prcomp(x_all, retx=TRUE, center=TRUE, scale=TRUE)

## 10.R.1 ===========================================================
# what proportion of the variance is explained by the first five principal components?

tot_var = sum(var(pca.out$x)) # sum(pca.out$sdev^2)
var_exp = sum(var(pca.out$x[, 1:5]))
PVE = var_exp/tot_var

## 10.R.2 ===========================================================
# What is the mean-squared test error if we regress y on the first five principal components,
# and use the resulting model to predict y.test? 

# dataframe where to predict
project.x = data.frame(predict(pca.out, x))
project.x_test = data.frame(predict(pca.out, x.test))

# Run a multi linear regression with first 5 PCs
model <-  lm(y ~ PC1 + PC2 + PC3 + PC4 + PC5, data = project.x)
summary(model)

# Mean-Squared test error - testing
y_pred = predict(model, project.x_test)
mean((y_pred- y.test)^2)

## 10.R.3 ===========================================================
# try an OLS linear regression of y on the matrix x. 
# What is the mean squared predition error if we use the fitted model to predict y.test from x.test?
fit = lm(y ~., x)
y_pred = predict(fit, x.test)
mean((y_pred- y.test)^2)
