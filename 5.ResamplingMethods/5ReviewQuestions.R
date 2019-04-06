# 5.R Review Questions

# 5.R.R1 
# load file
data = load('5.R.RData')

# run simple linear regression model
fit=lm(y~X1+X2,data=Xy)
summary(fit)$coefficients

# Standard error for beta1 is: 0.02593

# 5.R.R2
#plot the data using matplot(Xy,type="l")
matplot(Xy,type="l")
legend('top', colnames(Xy), fill=seq_len(ncol(Xy)))

# '''
# Looking at the graph our estimate of s.e.(beta1) is too low.                        
# there is very strong autocorrelation between consecutive rows of the data matrix.   
# Roughly speaking, if we filter the data by order, we have about 10-20 repeats
# of every data point.
# The sample size (Number of indipendent obs) is in effect much smaller than the number of rows.
# '''


# 5.R.R3 
# use the (standard) bootstrap to estimate beta1. To within 10%, what do you get?
estimator=function(x1, x2, y){
  fit=lm(y~x1+x2)
  summary(fit)$coefficients[2,1] # extract beta1 
}

estimator.fn=function(data, index){ 
  with(data[index,], estimator(X1, X2, y)) 
}

# Bootstrap involve random sampling with replacement
estimator.fn(Xy, sample(1:1000,1000,replace = TRUE))

require(boot)
boot.out=boot(Xy, estimator.fn, R=1000)
boot.out
plot(boot.out)

# Our result does not improve
# i.i.d = Independent and Identically Distributed
# When we do the i.i.d. bootstrap, we are relying on the original sampling having been i.i.d. 
# That is the same assumption that screwed us up when we used lm.

# 5.R.R4 
# use the block bootstrap to estimate s.e. beta1.
# Use blocks of 100 contiguous observations, and resample ten whole blocks with replacement 
# then paste them together to construct each bootstrap time series. 

# Resample 10 whole blocks with replacement
# Use blocks of 100 contiguous observations

index=function(data,contiguousObs){
  
  blocks=seq(1,nrow(data),contiguousObs)
  sampleBlock = sample(blocks, length(blocks), replace = TRUE)
  # Initiate an empty vector 
  new.rows = vector()
  
  for (elem in 1:length(sampleBlock)) {
    currentList = sampleBlock[elem]:(sampleBlock[elem]+(contiguousObs-1))
    new.rows = c(new.rows, currentList)
  }
  new.rows
}

# beta1 after 1 random draw
estimator.fn(Xy, index(Xy,100))
 
# We can't use the standard bootstrap for time series whcih are not iid (no serial autocorrelation)
# The standard bootstrap doesn't accurately mimic the real-world data-generating mechanism:
# The set of resampled time series from bootstrap very very different from the sort of time series
# we actually get in the real world.

# we store the beta1 value for 1000 draws

beta1 = rep(0,1000)
for (i in 1:length(beta1)){
  beta1[i] = estimator.fn(Xy, index(Xy,100))
}

# then we calculate the standard deviation
sd(beta1)


