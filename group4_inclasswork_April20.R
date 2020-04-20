data("cars")
str(cars)
# create a plot, speed Vs distance
plot(speed ~ dist, data = cars)
help("lowess")
# use the lowess() function
lowess(cars$speed ~ cars$dist)
lines(lowess(cars$speed ~ cars$dist, f=2/3), col="blue") 
#This gives the proportion of points in the plot which influence the smooth at each value.
# Larger values give more smoothness.
# Change the "f" value and observe the shape of the line.
# lines(lowess(cars$speed ~ cars$dist, f=0.75), col="gray")  # f = 0.75
lines(lowess(cars$speed ~ cars$dist, f=0.8), col="red")  # f = 0.8
lines(lowess(cars$speed ~ cars$dist, f=0.9), col="green")  # f = 0.9
lines(lowess(cars$speed ~ cars$dist, f=0.1), col= 5)  # f = 0.1
lines(lowess(cars$speed ~ cars$dist, f=0.01), col= 6)  # f = 0.01 


library(MASS)
names(iris)
dim(iris)
head(iris)
set.seed(555)
Train <- sample(1:nrow(iris), nrow(iris)/2)
iris_Train <- iris[Train,] # Traning dataset
irist_Test <- iris[-Train,] # Testing dataset
help(lda)
fit1 <- lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris_Train)
predict1 <- predict(fit1, iris_Train)
predict1_class <- predict1$class
table1 <- table(predict1_class, iris_Train$Species)
table1
# Calculating the Accuracy of the prediction
sum(diag(table1))/sum(table1)





