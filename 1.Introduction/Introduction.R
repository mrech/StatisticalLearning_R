### vectors, data, matrices, subsetting
x = c(2,7,5)
x
y = seq(from=4, length=3, by=3)
y
?seq

# vectors operations (elementwise)
x+y
x/y
x^y

# Accessing element of a vector
x[2]
x[2:3]
x[-2]
x[-c(1,2)]

# Matrices
z = matrix(seq(1,12),4,3)
# subset matrices
z[3:4,2:3]
z[,2:3]
z[,1] # became a vector
z[,1,drop=FALSE] # keep the matrix
# Matrices dimension
dim(z)

# list what is available in our workingdirectory
ls()
# remove variable from workingdirectory
rm(y)

# Generating random data (useful for simulations)
# random uniform distribution (0,1)
x = runif(50)
# random normal distribution  
y = rnorm(50)

# graphics
plot(x,y, xlab = 'Random Uniform', ylab = 'Random Normal', pch='*', col='blue')
# set graphics parameters
par(mfrow=c(2,1))
plot(x,y)
hist(y)
par(mfrow=c(1,1))

# Reading in data
names(iris)
dim(iris)
class(iris) # data.frame store matrix with columns in different format
summary(iris)
plot(iris$Sepal.Length, iris$Sepal.Width)
# Objects in the database can be accessed by simply giving their name
attach(iris)
search()
plot(Petal.Length, Species)
