## K-Nearest Neighbors
library(class)
?knn
attach(Smarket)
objects(2)

# build a matrix of lag1 and lag2
Xlag = cbind(Lag1, Lag2)
Xlag[1:5,]
train = Year<2005
# classify a new observation, based on the trining set in the x space
# you look for the trining obs closest to your test point in 
# Euclidean distance and classify to its class
knn.pred=knn(Xlag[train,],Xlag[!train,],Direction[train], k=1)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])

#0.5 is useless. 1NN did no better than flipping a coin
