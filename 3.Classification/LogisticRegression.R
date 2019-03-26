# GLM function and familyt equals binomial
require(ISLR)

# Query our data set
names(Smarket)
summary(Smarket)
?Smarket

# Make a plot
pairs(Smarket, col=Smarket$Direction)
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket, family = binomial)

summary(glm.fit)
# COMMENT:
# 1.none are significant
# 2. NULL DEVIANCE: log likelihood if you just use the mean model
# 3. RESIDUAL DEVIANCE: deviance for the model with all the predictors in

# make prediction on the training data that we use to fit the model
glm.probs=predict(glm.fit, type='response')

# we do not expect to have strong predictions
glm.probs[1:5]

# turn this probability into classification
glm.pred=ifelse(glm.probs>0.5, 'Up', 'Down')
attach(Smarket)
table(glm.pred, Direction)
# mean classification performances
# in the training data, we do slightly better than chance
mean(glm.pred==Direction)

# Devide the training and test set
train = Year<2005
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket, family = binomial, subset = train)
glm.probs=predict(glm.fit, newdata=Smarket[!train,],type='response')
glm.pred=ifelse(glm.probs>0.5, 'Up', 'Down')
Direction.2005=Smarket$Direction[!train]
# table in the test data
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)
# we are doing worst than the null rate

# we might be overfitting
# Fit smaller model
glm.fit=glm(Direction~Lag1+Lag2,
            data=Smarket, family = binomial, subset = train)
glm.probs=predict(glm.fit, newdata=Smarket[!train,],type='response')
glm.pred=ifelse(glm.probs>0.5, 'Up', 'Down')
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
summary(glm.fit)




