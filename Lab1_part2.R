#Exercise 1
rm(list=ls())
setwd("C:\\RPI\\ITWS6600 Data Analytics")
EPI_data<- read.csv("2010EPI_data.csv", skip=1)
attach(EPI_data)
plot(ecdf(EPI),do.points=FALSE,verticals=TRUE)
help("qqnorm")
par(pty="s")
qqnorm(EPI);qqline(EPI)

x<-seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Qplot for t dsn")
qqline(x)

plot(ecdf(EPI_data$EPI),do.points=F,verticals = T)
plot(ecdf(EPI_data$EPI),do.points=T,verticals = T)
par(pty="s")
help("qqnorm")
help("qqplot")
qqnorm(EPI_data$EPI)
qqline(EPI_data$EPI)
x<-seq(30,95,1)
x
x2<-seq(30,95,2)
x2
x2<-seq(30,96,2)
x2
qqplot(qt(ppoints(250),df=5),x,xlab = "Q-Q plot")
qqline(X)

#Exercise 2
boxplot(EPI_data$EPI,EPI_data$DALY)

multivariate <- read.csv("multivariate.csv")
head(multivariate)
attach(multivariate)
help(lm)
mm <- lm(Homeowners~Immigrant)
summary(mm)$coef

plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm,col=2,lwd=3)

newImmigrantdata <- data.frame(Immigrant = c(0,  20))
mm %>% predict(newImmigrantdata)
abline(mm)
abline(mm,col=3,lwd=3)
attributes(mm)
mm$coefficients

plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()
plot(pressure$temperature, pressure$pressure, type="l")
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2,col="red")
points(pressure$temperature,pressure$pressure/2,col="red")
library(ggplot2)
qplot(pressure$temperature,pressure$pressure, geom="line")
qplot(temperature,pressure, data=pressure, geom = "line")
ggplot(pressure, aes(x=temperature, y=pressure))+geom_line()+geom_point()
ggplot(pressure, aes(x=temperature, y=pressure))+geom_line()+geom_point()

#Creating Bar graphs
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot((table(mtcars$cyl)))
qplot(mtcars$cyl)
qplot(factor((mtcars$cyl))
#Bar graph of counts
qplot(factor(cyl),data=mtcars)
ggplot(mtcars, aes(x=factor(cyl)))+geom_bar()

#Creating Histogram
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10)
hist(mtcars$mpg, breaks = 5)
hist(mtcars$mpg, breaks = 12)
qplot(mpg, data = mtcars, binwidth=4)
ggplot(mtcars, aes(x=mpg))+geom_histogram(binwidth = 4)
ggplot(mtcars, aes(x=mpg))+geom_histogram(binwidth = 5)

#Creating Box-plot
plot(ToothGrowth$supp, ToothGrowth$len)
boxplot(len~supp, data= ToothGrowth)
boxplot(len~supp + dose, data= ToothGrowth)
library(ggplot2)
qplot(ToothGrowth$supp, ToothGrowth$len, geom="boxplot")
qplot(ToothGrowth, aes(x=supp, y=len))+gepm_boxplot()
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose),ToothGrowth$len, geom="boxplot")
qplot(interaction(supp,dose)),len,data=ToothGrowth,geom="boxplot")
ggplot(ToothGrowth, aes(interaction(supp,dose),y=len))+geom_boxplot()
