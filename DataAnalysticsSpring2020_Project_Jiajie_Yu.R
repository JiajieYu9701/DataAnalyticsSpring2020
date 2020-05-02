rm(list=ls())
setwd('C:\\RPI\\ITWS6600 Data Analytics\\Project')
df <- read.csv('NYSERDA_Low-_to_Moderate-Income_for_2013-2015.csv')

summary(df)
summary(df$Time.in.Home)
df <- df[-which(df$Time.in.Home == 'Unknown'),]

table(is.na(df))
sum(is.na(df$Mortgage.Indicator))
is.na(df$Mortgage.Indicator)
df[,19] <- as.character(df[,19])
df[is.na(df[,19]),19] <-'rented'
sum(is.na(df[,19]))
df[,19] <- as.factor(df[,19])



tapply(df$County...County.Group,df$County...County.Group,length)

df$Households.with.Elderly <- as.character(df$Households.with.Elderly)
df$Households.with.Elderly[df$Households.with.Elderly == 'Yes'] <- 1
df$Households.with.Elderly[df$Households.with.Elderly == 'No'] <- 0

df$Households.with.Children <- as.character(df$Households.with.Children)
df$Households.with.Children[df$Households.with.Children == 'Yes'] <- 1
df$Households.with.Children[df$Households.with.Children == 'No'] <- 0

table(df$Households.with.Elderly)
table(df$Households.with.Children)
hist(df$Households.with.Elderly,xlab = 'Househodls with Elderly', main='Househould with elderly')
hist(df$Households.with.Children,xlab = 'Househodls with Children', main='Househould with Children')
hist(table(df$Low.to.Moderate.Income.Group))
ggplot(df) + geom_boxplot(aes( y = df$Household.Weight,color = "Carat")) + xlab("Household Weight")+ylab("")+coord_flip()

df2 <-df
df$Low.to.Moderate.Income.Group <- as.character(df$Low.to.Moderate.Income.Group)
df$Low.to.Moderate.Income.Group[df$Low.to.Moderate.Income.Group == 'Very Low Income'] <- 'LMI Household'
df$Low.to.Moderate.Income.Group[df$Low.to.Moderate.Income.Group == 'Low Income'] <- 'LMI Household'
df$Low.to.Moderate.Income.Group[df$Low.to.Moderate.Income.Group == 'Moderate Income'] <- 'LMI Household'
df$Low.to.Moderate.Income.Group[df$Low.to.Moderate.Income.Group != 'LMI Household'] <- 0
df$Low.to.Moderate.Income.Group[df$Low.to.Moderate.Income.Group == 'LMI Household'] <- 1

df$Low.to.Moderate.Income.Group <- as.factor(df$Low.to.Moderate.Income.Group)

#Eliminate variables that reveal information and variables that not significant
df <- df[,c(-5,-6,-18)]

# Split data into train(70%) and test(30%)
set.seed(1)
train.rows <- sample(rownames(df), dim(df)[1]*0.7)
train_data<-df[train.rows,]
test.rows<-setdiff(rownames(df),train.rows)
test_data<-df[test.rows,]

#logistic regression
lm.fit <- glm(Low.to.Moderate.Income.Group ~ ., data= train_data, family = "binomial",control=list(maxit=100))
summary(lm.fit)
lgtrain_pred <- predict(lm.fit,  train_data, type = 'response')
pred <- predict(lm.fit,newdata=test_data,type = "response")
library(caret)
library(e1071)
confusionMatrix(as.factor(ifelse( lgtrain_pred > 0.5, 1, 0)), as.factor(train_data$Low.to.Moderate.Income.Group))
confusionMatrix(as.factor(ifelse(pred > 0.5, 1, 0)), as.factor(test_data$Low.to.Moderate.Income.Group))


#decision tree
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

decisionTreeModel <- rpart(train_data$Low.to.Moderate.Income.Group~., train_data)
decisionTreeModel
par(oma=c(0,0,1,0))
rpart.plot(decisionTreeModel)
fancyRpartPlot(decisionTreeModel,cex=0.5)
dt_pred <- predict(decisionTreeModel,newdata = test_data,type = "class")
table(dt_pred,test_data$Low.to.Moderate.Income.Group)
confusionMatrix(as.factor(dt_pred), as.factor(test_data$Low.to.Moderate.Income.Group))
prp(decisionTreeModel, type = 4, extra = 3, under = TRUE, split.font = 2, varlen = 10,  
    box.col=ifelse(decisionTreeModel$frame$var == "<leaf>", 'gray', 'white'))




# XGBoost
library(Matrix)
library(xgboost)

traindata1 <- data.matrix(train_data[,c(1:4,6:20)]) 
traindata2 <- Matrix(traindata1,sparse=T) 
traindata3 <- train_data[,5]
traindata3 <- as.numeric(as.character(traindata3))
traindata4 <- list(data=traindata2,label=traindata3)
dtrain <- xgb.DMatrix(data = traindata4$data, label = traindata4$label) 

testset1 <- data.matrix(test_data[,c(1:4,6:20)]) 

testset2 <- Matrix(testset1,sparse=T) 

testset3 <- test_data[,5]
testset3 <- as.numeric(as.character(testset3))

testset4 <- list(data=testset2,label=testset3) 

dtest <- xgb.DMatrix(data = testset4$data, label = testset4$label) 

xgb <- xgboost(data = dtrain,max_depth=6, eta=0.5,  objective='binary:logistic', nround=25)

pre_xgb = round(predict(xgb,newdata = dtest))

table(test_data$Low.to.Moderate.Income.Group,pre_xgb,dnn=c("Reference","Prediction"))
library(pROC)
xgboost_roc <- roc(test_data$Low.to.Moderate.Income.Group,as.numeric(pre_xgb))
confusionMatrix(as.factor(pre_xgb), as.factor(test_data$Low.to.Moderate.Income.Group))

plot(xgboost_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), 
     max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='xgboost ROC')

model <- xgb.dump(xgb, with.stats = T)
names <- dimnames(data.matrix(train_data[,-1]))[[2]]
importance_matrix <- xgb.importance(names, model = xgb)
xgb.plot.importance(importance_matrix[1:10,])

