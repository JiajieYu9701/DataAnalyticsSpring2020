# Lab 3

rm(list=ls())
set.seed(1)
help(par)
par(mar = rep(0.2,4))
data_Matrix <-matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])
par(mar=rep(0.2,4))
heatmap(data_Matrix)

set.seed(2)
for(i in 1:40){
  # flipping a coin and getting the data
  coin_Flip <- rbinom(1, size = 1, prob = 0.5)
  # if the coin is "Heads", add a common pattern to that row,
  if(coin_Flip){
    data_Matrix[i, ] <- data_Matrix[i, ] + rep(c(0,3), each =5)
  }
}

par(mar= rep(0.2, 4))
image(1:10, 1:40, t(data_Matrix)[, nrow(data_Matrix):1])

par(mar=rep(0.2, 4))
heatmap(data_Matrix)

hh<- hclust((dist(data_Matrix)))
data_Matrix_Ordered <- data_Matrix[hh$order,]
par(mfrow = c(1,3))
image(t(data_Matrix_Ordered)[,nrow(data_Matrix_Ordered):1])
plot(rowMeans(data_Matrix_Ordered),40:1,xlab='The Row Mean',ylab='Row',pch=19)      
plot(colMeans(data_Matrix_Ordered),xlab='Column',ylab='Column Mean',pch=19)

rm(list = ls())
data("Titanic")
# Trees for the Titanic
dim(titanic_train)
head(titanic_train)
titanic_train_new <- na.omit(titanic_train)

# rpart
library(rpart)
titanic_train_new <- titanic_train_new[, -c(1,4,9,11)]
titanic_train_new <- titanic_train_new[titanic_train_new$Embarked != "",]
fit_rpart <- rpart(Survived ~ ., data = titanic_train_new)
summary(fit_rpart)
par(mfrow=c(1,1))
rpart.plot(fit_rpart)

# ctree
library(dummies)
titanic_train.dummy <- dummy.data.frame(titanic_train_new,sep=".")
head(titanic_train.dummy)
titanic_train.dummy <- titanic_train.dummy[, -c(3,9)]
require(party)
fit_ctree <- ctree(Survived ~ ., data = titanic_train.dummy)
summary(fit_rpart)
plot(fit_ctree)

# hclust
help(hclust)
fit_hclust <- hclust(dist(titanic_train.dummy))
titanic_train.dummy_Ordered <- titanic_train.dummy[fit_hclust$order,]
par(mfrow = c(1,3))
image(t(titanic_train.dummy_Ordered)[, nrow(titanic_train.dummy_Ordered):1])
plot(rowMeans(titanic_train.dummy_Ordered), xlab = "The Row Mean", yylab = "Column Mean", pch = 19)
plot(colMeans(titanic_train.dummy_Ordered), xlab = "Column", ylab = "Column Mean", pch =19)

# random forest
library(randomForest)
fit_rf <- randomForest(Survived ~ ., data = titanic_train.dummy)
plot(fit_rf)
