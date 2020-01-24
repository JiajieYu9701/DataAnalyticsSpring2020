rm(list=ls())
setwd("C:\\RPI\\ITWS6600 Data Analytics")
GPW3 <- read.csv("GPW3_GRUMP_SummaryInformation_2010.csv")
library(readxl)
EPIxls<- read_xls("2010EPI_data.xls",sheet="EPI2010_onlyEPIcountries")
EPI_data<- read.csv("2010EPI_data.csv", skip=1)
View(EPI_data)
attach(EPI_data)
fix(EPI_data)
EPI
tf<- is.na(EPI)
E<- EPI[!tf]

#Exercise 1: exploring the distribution
summary(EPI)
fivenum(EPI, na.rm=TRUE)
stem(EPI)
hist(EPI)
hist(EPI,seq(30.,95.,1.0), prob=TRUE)
lines(density(EPI,na.rm = T,bw=1.))
lines(density(EPI,na.rm = T,bw="SJ"))
rug(EPI)

#Exercise 1: fitting a distribution beyond histograms
plot(ecdf(EPI),do.points=F, verticals = T)
par(pty="s")
qqnorm(EPI)
qqline(EPI)
x<- seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab= "Q-Q plot for dsn")
qqline(x)

plot(ecdf(DALY), do.points=F,verticals = T)
qqnorm(DALY)
qqline(DALY)
plot(ecdf(WATER_H),do.points=F, verticals = T)
qqnorm(WATER_H)
qqline(WATER_H)

boxplot(EPI,DALY)
qqplot(EPI,DALY)

boxplot(ENVHEALTH,ECOSYSTEM)
qqplot(ENVHEALTH,ECOSYSTEM)

boxplot(AIR_H,WATER_H)
qqplot(AIR_H, WATER_H)

help("distributions")

EPILand<-EPI[!Landlock]
Eland<- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30.,95.,1.0), prob=T)

EPI_South_Asia <- EPI[EPI_regions=="South Asia"]

detach(EPI_data)

attach(GPW3)
dim(GPW3)
str(GPW3)
